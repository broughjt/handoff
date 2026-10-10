#lang typed/racket/base

(require racket/list
         racket/path
         racket/string
         racket/system
         "error.rkt")

(provide (struct-out branch-head)
         git
         git!
         repository-path
         find-handoff-repository
         git-repository?
         find-project-repository
         project-branch-head
         list-arcs
         list-archived-arcs
         select-arc
         start-file
         checks-file
         state-files
         log-directory
         entry-file
         results-file
         arc?
         parse-log-entry-name
         log-entry-path
         list-log-entries
         pending-log-entry)

(define repository-path (build-path ".scratch" "handoff"))
(define archive-tag-prefix "archive/")

;; The expected layout of a handoff arc directory
(define start-file "start.toml")
(define checks-file "checks.toml")
(define state-files '("arc.md" "tasks.md" "do-not-reopen.md" "open-questions.md" "review.md"))
(define log-directory "log")
(define entry-file "entry.md")
(define results-file "results.toml")

;; Whether `path` is the top level of a Git repository or worktree, rather
;; than a directory inside one.
(: git-repository? (-> Path-String Boolean))
(define (git-repository? path)
  (and (directory-exists? path)
       (let ([output (git path "rev-parse" "--show-toplevel")])
         (and output
              (equal? (path->directory-path (normalize-path (string-trim output "\n" #:left? #f)))
                      (path->directory-path (normalize-path path)))))))

;; Like Git finds .git/, walk up from `start` to the nearest .scratch/handoff/
;; repository.
(: find-handoff-repository (-> Path-String Path))
(define (find-handoff-repository start)
  (let loop ([directory (simple-form-path start)])
    (define candidate (build-path directory repository-path))
    (cond
      [(git-repository? candidate) candidate]
      [else
       (define-values (parent _name _directory?) (split-path directory))
       (if (path? parent)
           (loop parent)
           (raise-handoff-error "no ~a/ at or above ~a" repository-path start))])))

;; The project repository enclosing the handoff repository.
(: find-project-repository (-> Path Path))
(define (find-project-repository repository)
  (define output
    (or (git (build-path (normalize-path repository) 'up) "rev-parse" "--show-toplevel")
        (raise-handoff-error "no project repository encloses ~a" repository)))
  (string->path (string-trim output "\n" #:left? #f)))

;; The branch head, consisting of the branch name (`#f` when detached) and its
;; latest commit (`#f` before first commit).
(struct branch-head ([branch : (U String #f)] [head : (U String #f)]) #:transparent)

(: project-branch-head (-> Path branch-head))
(define (project-branch-head project)
  (define (line [output : (U String #f)])
    (and output (string-trim output "\n" #:left? #f)))
  (branch-head (line (git project "symbolic-ref" "--quiet" "--short" "HEAD"))
               (line (git project "rev-parse" "--verify" "--quiet" "HEAD"))))

(: list-arcs (-> Path (Listof String)))
(define (list-arcs repository)
  (sort (for/list : (Listof String) ([name (in-list (directory-list repository))]
                                     #:when (arc? (build-path repository name)))
          (path->string name))
        string<?))

(: list-archived-arcs (-> Path (Listof String)))
(define (list-archived-arcs repository)
  (define output
    (or (git repository "tag" "--list" (string-append archive-tag-prefix "*"))
        (raise-handoff-error "git tag failed in ~a" repository)))
  (sort (for/list : (Listof String) ([tag (in-list (string-split output))])
          (string-trim tag archive-tag-prefix #:right? #f))
        string<?))

(: select-arc (-> Path (U String #f) String))
(define (select-arc repository name)
  (cond
    [name
     (when (or (member name '("" "." ".." ".git")) (regexp-match? #rx"/" name))
       (raise-handoff-error "specify a top-level arc directory name"))
     (unless (directory-exists? (build-path repository name))
       (raise-handoff-error "~a is not a directory in ~a" name repository))
     name]
    [else
     (define arcs (list-arcs repository))
     (cond
       [(empty? arcs) (raise-handoff-error "there are no active arcs")]
       [(empty? (rest arcs)) (first arcs)]
       [else (raise-handoff-error "specify the arc; active arcs: ~a" (string-join arcs ", "))])]))

;; Git's stdout, run in `directory`, or #f when it exits non-zero. Stdin is
;; empty and stderr is discarded.
(: git (-> Path-String String * (U String #f)))
(define (git directory . arguments)
  (define-values (succeeded? output _error) (apply run-git directory arguments))
  (and succeeded? output))

;; TODO: no, exclamation points are only to indicate mutation
;; Probably just do primed or starred name

;; Like `git`, but raises a handoff error carrying Git's stderr when it exits
;; non-zero, for commands whose failure the user has to see.
(: git! (-> Path-String String * String))
(define (git! directory . arguments)
  (define-values (succeeded? output error) (apply run-git directory arguments))
  (unless succeeded?
    (raise-handoff-error "git ~a failed in ~a~a"
                         (string-join arguments) directory
                         (if (string=? error "") "" (string-append ":\n" (string-trim error)))))
  output)

(: run-git (-> Path-String String * (Values Boolean String String)))
(define (run-git directory . arguments)
  (define program
    (or (find-executable-path "git") (raise-handoff-error "git not found on PATH")))
  (define output (open-output-string))
  (define error (open-output-string))
  (define succeeded?
    (parameterize ([current-input-port (open-input-string "")]
                   [current-output-port output]
                   [current-error-port error])
      (apply system* program "-C" directory arguments)))
  (values succeeded? (get-output-string output) (get-output-string error)))

;; Arcs are the top-level directories holding a start file.
(: arc? (-> Path Boolean))
(define (arc? path)
  (and (directory-exists? path) (file-exists? (build-path path start-file))))

;; Ah, a typical canonicity problem of string representations of data
;; types: "1" and "01" both represent 1. To avoid this, we require the
;; absence of leading zeros.
(: parse-log-entry-name (-> String (U Exact-Positive-Integer #f)))
(define (parse-log-entry-name name)
  (and (regexp-match? #px"^[1-9][0-9]*$" name)
       (let ([number (string->number name)])
         (and (exact-positive-integer? number) number))))

(: log-entry-path (-> Path Exact-Positive-Integer Path))
(define (log-entry-path arc number)
  (build-path arc log-directory (number->string number)))

;; Some entries in the log directory might not pass `parse-log-entry-name`, so
;; partition into successfully parsed entries and entries which are invalid.
(: list-log-entries (-> Path (Values (Listof Exact-Positive-Integer) (Listof String))))
(define (list-log-entries arc)
  (define log (build-path arc log-directory))
  (define names (if (directory-exists? log) (map path->string (directory-list log)) null))
  (define-values (numbered invalid) (partition parse-log-entry-name names))
  (values (sort (map (λ ([name : String]) (assert (parse-log-entry-name name))) numbered) <)
          (sort invalid string<?)))

;; The number of the arc's update in progress: its highest-numbered log entry,
;; when that is a directory not yet in the handoff repository's HEAD.
(: pending-log-entry (-> Path (U Exact-Positive-Integer #f)))
(define (pending-log-entry arc)
  (define-values (numbers _invalid) (list-log-entries arc))
  (and (pair? numbers)
       (let* ([number (last numbers)]
              [entry (log-entry-path arc number)])
         (and (directory-exists? entry)
              ;; `HEAD:./` names the tree at the current directory.
              (not (git entry "cat-file" "-e" "HEAD:./"))
              number))))

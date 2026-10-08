#lang typed/racket/base

(require racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         "error.rkt")

(provide git
         repository-path
         find-handoff-repository
         git-repository?
         find-project-repository
         list-arcs
         list-archived-arcs
         select-arc
         start-file
         checks-file
         state-files
         log-directory
         arc?
         parse-log-entry-name
         log-entry-path
         list-log-entries)

(define repository-path (build-path ".scratch" "handoff"))
(define archive-tag-prefix "archive/")

;; The expected layout of a handoff arc directory
(define start-file "start.toml")
(define checks-file "checks.toml")
(define state-files '("arc.md" "tasks.md" "do-not-reopen.md" "open-questions.md" "review.md"))
(define log-directory "log")

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
  (define program
    (or (find-executable-path "git") (raise-handoff-error "git not found on PATH")))
  (define output (open-output-string))
  (and (parameterize ([current-input-port (open-input-string "")]
                      [current-output-port output]
                      [current-error-port (open-output-nowhere)])
         (apply system* program "-C" directory arguments))
       (get-output-string output)))

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

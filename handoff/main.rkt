#lang typed/racket/base
;; Automation for the handoff skills.

(require racket/cmdline
         racket/list
         racket/match
         racket/path
         racket/string
         racket/system
         "error.rkt")

(module+ main
  (exit (main (current-command-line-arguments))))

(: main (-> (Vectorof String) Exit-Code))
(define (main argv)
  (status->exit-code
   (with-handlers ([exn:fail:handoff?
                    (λ ([error : exn]) : Status
                      (eprintf "handoff: ~a\n" (exn-message error))
                      'usage)]
                   [exn:fail:user?
                    (λ ([error : exn]) : Status
                      (eprintf "~a\n" (exn-message error))
                      'usage)])
     (match (parse-arguments argv)
       ;; Most commands execute from with the current working directory set to
       ;; the project repository, but there are few for which this doesn't make
       ;; sense.
       [(invocation _ (workflow-list-subcommand)) (handle-workflow-list)]
       [(invocation _ (guide-subcommand)) (handle-guide)]
       [(invocation option subcommand)
        (when (and option (not (git-repository? option)))
          (raise-handoff-error "~a is not a git repository" option))
        (define repository (or option
                               (find-handoff-repository (current-directory))))
        (match subcommand
          [(list-subcommand archived?) (handle-list repository archived?)]
          [(start-subcommand arc)
           (handle-start repository (select-arc repository arc))]
          [(check-subcommand arc)
           (handle-check repository (select-arc repository arc))]
          [(lint-subcommand arc)
           (handle-lint repository (select-arc repository arc))]
          [(update-subcommand 'begin arc)
           (handle-update-begin repository (select-arc repository arc))]
          [(update-subcommand 'commit arc)
           (handle-update-commit repository (select-arc repository arc))]
          [(new-subcommand arc workflow)
           (handle-new repository arc workflow)]
          [(workflow-install-subcommand name)
           (handle-workflow-install repository name)]
          [(workflow-fork-subcommand arc name source)
           (handle-workflow-fork repository
                                 (select-arc repository arc)
                                 name
                                 source)])]))))

(define-type Status (U 'success 'failure 'usage))
(define-type Exit-Code (U 0 1 2))

(: status->exit-code (-> Status Exit-Code))
(define (status->exit-code status)
  (case status
    [(success) 0]
    [(failure) 1]
    [(usage) 2]))

;;; Command line

(define-type Subcommand
  (U list-subcommand
     start-subcommand
     check-subcommand
     lint-subcommand
     update-subcommand
     new-subcommand
     workflow-list-subcommand
     workflow-install-subcommand
     workflow-fork-subcommand
     guide-subcommand))
(struct list-subcommand ([archived? : Boolean]) #:transparent)
(struct start-subcommand ([arc : (U String #f)]) #:transparent)
(struct check-subcommand ([arc : (U String #f)]) #:transparent)
(struct new-subcommand ([arc : String]
                        [workflow : (U String #f)]) #:transparent)
(struct workflow-list-subcommand () #:transparent)
(struct workflow-install-subcommand ([pattern : String]) #:transparent)
(struct workflow-fork-subcommand ([arc : String]
                                  ;; TODO: `name` or something else?
                                  [name : String]
                                  [source : String]) #:transparent)
(struct lint-subcommand ([arc : (U String #f)]) #:transparent)
(struct update-subcommand ([step : (U 'begin 'commit)]
                           [arc : (U String #f)]) #:transparent)
(struct guide-subcommand () #:transparent)

;; "Envisioning the hereafter, listenin' to Steve Wonder"
(struct invocation ([repository : (U Path #f)] [subcommand : Subcommand]) #:transparent)

(: parse-arguments (-> (Vectorof String) invocation))
(define (parse-arguments argv)
  (define repository : (U Path #f) #f)
  (command-line
   #:program "handoff"
   #:argv argv
   #:usage-help
   "Commands:"
   "  list      list handoff arcs"
   "  start     get a brief for starting work on a handoff arc"
   "  check     run the commands in checks.toml and report their results"
   "  lint      validate expected structure of a handoff arc"
   "  update    record a log entry: --begin runs the checks and records them, --commit commits"
   "  new       create an arc's files, copying its workflow"
   "  workflow  list, install, or fork reusable workflows"
   "  guide     print the path of HANDOFF.md, which documents concepts shared by the handoff skills"
   #:once-each
   [("--repository") path
                     "Handoff repository to use instead of the nearest .scratch/handoff/"
                     (set! repository (string->path (assert path string?)))]
   #:args (subcommand . arguments)
   ;; Typed Racket types the formal arguments of `#:args` as `Any`, so we do a
   ;; runtime check explicitly (that we expect will never fail).
   (invocation repository
               (parse-subcommand (assert subcommand string?)
                                 (map (λ (a) (assert a string?)) arguments)))))

(: parse-subcommand (-> String (Listof String) Subcommand))
(define (parse-subcommand subcommand arguments)
  (define program (string-append "handoff " subcommand))
  (: parse-arc-argument (-> String (Listof String) (U String #f)))
  (define (parse-arc-argument program arguments)
    (command-line
     #:program program
     #:argv arguments
     #:args ([arc #f])
     (and arc (assert arc string?))))
  (case subcommand
    [("list")
     (define archived? : Boolean #f)
     (command-line
      #:program program
      #:argv arguments
      #:once-each
      [("--archived") "List archived arcs instead" (set! archived? #t)]
      #:args ()
      (list-subcommand archived?))]
    [("start") (start-subcommand (parse-arc-argument program arguments))]
    [("check") (check-subcommand (parse-arc-argument program arguments))]
    [("lint") (lint-subcommand (parse-arc-argument program arguments))]
    [("update")
     (define step : (U 'begin 'commit #f) #f)
     (command-line
      #:program program
      #:argv arguments
      #:once-any
      [("--begin") "Run the checks and record their results in a pending log entry"
                   (set! step 'begin)]
      [("--commit") "Commit the pending log entry once its entry.md is written"
                    (set! step 'commit)]
      #:args ([arc #f])
      (update-subcommand (or step (raise-handoff-error "update requires --begin or --commit"))
                      (and arc (assert arc string?))))]
    [("new")
     (command-line
      #:program program
      #:argv arguments
      #:help-labels
      ;; TODO: name or something else?
      "<workflow> is a name from handoff workflow list, or a reference such as handoff:workflow-x.md"
      #:args (arc [workflow #f])
      (new-subcommand (assert arc string?) (and workflow (assert workflow string?))))]
    [("workflow")
     (command-line
      #:program program
      #:argv arguments
      #:usage-help
      "Commands:"
      ;; TODO: What is a "reusable" workflow as opposed to a "workflow file"?
      ;; TODO: Why not:
      ;; list workflows
      ;; copy a workflow verbatim to the root of the handoff repository
      ;; copy a workflow to the root of the handoff repository so it can be modified
      "  list                 list the reusable workflows"
      "  install <name>       copy a reusable workflow verbatim to the handoff repository's root"
      "  fork <arc> <name> <source>  copy a reusable workflow or workflow file into the arc as workflow-<name>.md"
      #:args (verb . verb-arguments)
      (parse-workflow-subcommand (assert verb string?)
                                 (map (λ (a) (assert a string?)) verb-arguments)))]
    [("guide") (command-line #:program program #:argv arguments #:args () (guide-subcommand))]
    [else (raise-handoff-error "unknown command ~a; expected list, start, check, lint, update, new, workflow, or guide"
                               subcommand)]))

;; TODO: inline
(: parse-workflow-subcommand (-> String (Listof String) Subcommand))
(define (parse-workflow-subcommand verb arguments)
  (define program (string-append "handoff workflow " verb))
  (case verb
    [("list")
     (command-line #:program program #:argv arguments #:args ()
                   (workflow-list-subcommand))]
    [("install")
     (command-line #:program program #:argv arguments #:args (pattern)
                   (workflow-install-subcommand (assert pattern string?)))]
    [("fork")
     (command-line #:program program #:argv arguments
                   #:help-labels
                   ;; TODO: Can this be more concise? Do we need to say both?
                   "<source> is a name from handoff workflow list, or a reference such as handoff:workflow-x.md"
                   #:args (arc name source)
                   (workflow-fork-subcommand (assert arc string?) (assert name string?) (assert source string?)))]
    [else (raise-handoff-error "unknown command workflow ~a; expected list, install, or fork" verb)]))

;;; Commands

(: handle-list (-> Path Boolean Status))
(define (handle-list repository archived?)
  (error 'handle-list "unimplemented"))

(: handle-check (-> Path String Status))
(define (handle-check repository arc)
  (error 'handle-check "unimplemented"))

(: handle-lint (-> Path String Status))
(define (handle-lint repository arc)
  (error 'handle-lint "unimplemented"))

(: handle-start (-> Path String Status))
(define (handle-start repository* name)
  (error 'handle-start "unimplemented"))

(: handle-update-begin (-> Path String Status))
(define (handle-update-begin repository name)
  (error 'handle-update-begin "unimplemented"))

(: handle-update-commit (-> Path String Status))
(define (handle-update-commit repository name)
  (error 'handle-update-commit "unimplemented"))

(: handle-workflow-list (-> Status))
(define (handle-workflow-list)
  (error 'handle-workflow-list "unimplemented"))

(: handle-guide (-> Status))
(define (handle-guide)
  (error 'handle-guide "unimplemented"))

(: handle-new (-> Path String (U String #f) Status))
(define (handle-new repository arc workflow)
  (error 'handle-new "unimplemented"))

(: handle-workflow-install (-> Path String Status))
(define (handle-workflow-install repository name)
  (error 'handle-workflow-install "unimplemented"))

(: handle-workflow-fork (-> Path String String String Status))
(define (handle-workflow-fork repository arc name workflow)
  (error 'handle-workflow-fork "unimplemented"))

;;; Repositories

(define repository-path (build-path ".scratch" "handoff"))
(define start-file "start.toml")

;; TODO: Can this be simplified in any way?
(: git-repository? (-> Path-String Boolean))
(define (git-repository? path)
  (and (directory-exists? path)
       (let ([output (git* path "rev-parse" "--show-toplevel")])
         (and output
              (equal? (path->directory-path
                       (normalize-path (string-trim output "\n" #:left? #f)))
                      (path->directory-path (normalize-path path)))))))

;; TODO: Can this be simplified in any way?
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

;;; Arc helpers

(: arc? (-> Path Boolean))
(define (arc? path)
  (and (directory-exists? path) (file-exists? (build-path path start-file))))

(: list-arcs (-> Path (Listof String)))
(define (list-arcs repository)
  ;; TODO: Is sorting necessary here? The documentation says `directory-list`
  ;; always sorts its outputs by `path<?`.
  (sort (for/list : (Listof String) ([entry (in-list (directory-list repository))]
                                     #:when (arc? (build-path repository entry)))
          (path->string entry))
        string<?))

;; TODO: The check can be consolodated into checking a single regex. Define the regex globally as `arc-name-regex` and inline the check against and raising the error.
(: check-arc-name (-> String Void))
(define (check-arc-name name)
  (when (or (member name '("" "." ".." ".git")) (regexp-match? #rx"/" name))
    (raise-handoff-error "specify a top-level arc directory name")))

(: select-arc (-> Path (U String #f) String))
(define (select-arc repository name)
  (cond
    [name
     (check-arc-name name)
     (unless (directory-exists? (build-path repository name))
       (raise-handoff-error "~a is not a directory in ~a" name repository))
     name]
    [else
     (define arcs (list-arcs repository))
     (cond
       [(empty? arcs) (raise-handoff-error "there are no active arcs")]
       [(empty? (rest arcs)) (first arcs)]
       [else (raise-handoff-error "specify the arc; active arcs: ~a" (string-join arcs ", "))])]))

;;; Git

;; TODO: Any incidental complexity in any of these?

(: git (-> Path-String String * String))
(define (git directory . arguments)
  (define-values (succeeded? output error) (apply run-git directory arguments))
  (unless succeeded?
    (raise-handoff-error "git ~a failed in ~a~a"
                         (string-join arguments) directory
                         (if (string=? error "") "" (string-append ":\n" (string-trim error)))))
  output)

(: git* (-> Path-String String * (U String #f)))
(define (git* directory . arguments)
  (define-values (succeeded? output _error) (apply run-git directory arguments))
  (and succeeded? output))

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

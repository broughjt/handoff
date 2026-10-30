#lang typed/racket/base
;; Automation for the handoff skills.

(require racket/cmdline
         racket/file
         racket/format
         racket/function
         racket/list
         racket/match
         racket/math
         racket/path
         racket/port
         racket/string
         racket/system
         "error.rkt")

(require/typed racket/base
  [current-inexact-monotonic-milliseconds (-> Flonum)]
  [(alarm-evt monotonic-alarm-evt) (-> Real True (Evtof Any))])

(require/typed racket/file
  [make-temporary-directory (-> Path)])

(require/typed toml
  [parse-toml (-> String Table)])

(require/typed gregor
  [#:opaque Moment moment?]
  [moment->iso8601 (-> Moment String)])

(module signal racket/base
  (require ffi/unsafe)
  (provide kill-group)
  (define kill (get-ffi-obj "kill" #f (_fun _int _int -> _int)))
  (define (kill-group pid)
    (kill (- pid) 9)
    (void)))

(require/typed (submod "." signal)
  [kill-group (-> Integer Void)])

(provide Moment
         (struct-out check)
         (struct-out check-observation)
         (struct-out check-result)
         (struct-out check-run)
         (struct-out check-added)
         (struct-out check-removed)
         (struct-out check-matched)
         check-result-outcome
         parse-checks
         parse-check-run
         write-check-run
         compare-check-results
         check-matched-change
         print-results
         print-comparisons
         (struct-out arc)
         (struct-out log-entry)
         (struct-out start)
         (struct-out read-first)
         (struct-out scope-path)
         parse-start
         parse-scope-path
         parse-log-entry-name
         find-last-committed-log-entry
         find-pending-log-entry
         find-baseline
         lint
         (struct-out project)
         (struct-out history)
         inlined-sections
         omit-finished-tasks
         print-sections
         print-documents
         print-project
         print-handoff-repository
         print-start)

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
          [(check-subcommand arc compare?)
           (handle-check repository (select-arc repository arc) compare?)]
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
          [(workflow-fork-subcommand arc source name)
           (handle-workflow-fork repository
                                 (select-arc repository arc)
                                 source
                                 name)])]))))

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
(struct check-subcommand ([arc : (U String #f)]
                          [compare? : Boolean])
  #:transparent)
(struct new-subcommand ([arc : String]
                        [workflow : (U String #f)])
  #:transparent)
(struct workflow-list-subcommand () #:transparent)
(struct workflow-install-subcommand ([name : String]) #:transparent)
(struct workflow-fork-subcommand ([arc : String]
                                  [source : String]
                                  [name : (U String #f)])
  #:transparent)
(struct lint-subcommand ([arc : (U String #f)]) #:transparent)
(struct update-subcommand ([step : (U 'begin 'commit)]
                           [arc : (U String #f)])
  #:transparent)
(struct guide-subcommand () #:transparent)

;; "Envisioning the hereafter, listenin' to Steve Wonder"
(struct invocation ([repository : (U Path #f)]
                    [subcommand : Subcommand])
  #:transparent)

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
    [("check")
     (define compare? : Boolean #f)
     (command-line
      #:program program
      #:argv arguments
      #:once-each
      [("--compare") "Compare the results with those of the last handoff"
                     (set! compare? #t)]
      #:args ([arc #f])
      (check-subcommand (and arc (assert arc string?)) compare?))]
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
      (update-subcommand
       (or step (raise-handoff-error "update requires --begin or --commit"))
       (and arc (assert arc string?))))]
    [("new")
     (command-line
      #:program program
      #:argv arguments
      #:help-labels
      "<workflow> is a name from handoff workflow list"
      #:args (arc [workflow #f])
      (new-subcommand (assert arc string?)
                      (and workflow (assert workflow string?))))]
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
      "  fork <arc> <source> [<name>]  copy a reusable workflow or workflow file into the arc as workflow-<name>.md"
      #:args (verb . verb-arguments)
      (let* ([verb (assert verb string?)]
             [arguments (map (λ (a) (assert a string?)) verb-arguments)]
             [program (string-append program " " verb)])
        (case verb
          [("list")
           (command-line #:program program #:argv arguments #:args ()
                         (workflow-list-subcommand))]
          [("install")
           (command-line #:program program #:argv arguments #:args (name)
                         (workflow-install-subcommand (assert name string?)))]
          [("fork")
           (command-line #:program program #:argv arguments
                         #:help-labels
                         "<source> is either a workflow name or a reference like handoff:workflow-x.md"
                         "<name> defaults to the source's name"
                         #:args (arc source [name #f])
                         (workflow-fork-subcommand
                          (assert arc string?)
                          (assert source string?)
                          (and name (assert name string?))))]
          [else (raise-handoff-error "unknown command workflow ~a; expected list, install, or fork"
                                     verb)])))]
    [("guide") (command-line #:program program
                             #:argv arguments
                             #:args ()
                             (guide-subcommand))]
    [else (raise-handoff-error "unknown command ~a; expected list, start, check, lint, update, new, workflow, or guide"
                               subcommand)]))

;;; Commands

(: handle-list (-> Path Boolean Status))
(define (handle-list repository archived?)
  (for ([arc-name (in-list ((if archived?
                                list-archived-arcs
                                list-arcs)
                            repository))])
    (displayln arc-name))
  'success)

(: handle-check (-> Path String Boolean Status))
(define (handle-check repository name compare?)
  (define checks (read-checks (build-path repository name checks-file)))
  (define project (find-project-repository repository))
  (define results
    (cond
      [compare?
       ;; Read the baseline first, so that if it is broken, we refuse with an
       ;; error before running the checks.
       (match-define (cons log-entry-index baseline)
         (or (find-baseline name (car (read-log repository name)))
             (raise-handoff-error "~a has no committed log entry to compare against"
                                  name)))
       (define results (run-checks project checks))
       (print-comparisons (compare-check-results (check-run-results baseline)
                                                 results)
                          log-entry-index
                          (check-run-host baseline)
                          (check-run-started baseline))
       results]
      [else
       (define results (run-checks project checks))
       (print-results results)
       results]))
  (if (andmap (λ ([result : check-result])
                (eq? (check-result-outcome result) 'true))
              results)
      'success
      'failure))

(: handle-lint (-> Path String Status))
(define (handle-lint repository name)
  (define parsed
    (read-arc repository (find-project-repository repository) name))
  (define diagnostics (if (arc? parsed) (lint parsed) parsed))
  (for ([diagnostic (in-list diagnostics)])
    (displayln diagnostic))
  (printf "~a: ~a\n" name (count-errors diagnostics))
  (if (empty? diagnostics) 'success 'failure))

(: handle-start (-> Path String Status))
(define (handle-start repository* name)
  (define repository (simple-form-path repository*))
  (define project-path (find-project-repository repository))
  (define parsed (read-arc repository project-path name))
  (cond
    [(arc? parsed)
     (define baseline (find-baseline name (arc-log parsed)))
     (define results (run-checks project-path (arc-checks parsed)))
     (define project
       (read-project project-path (and baseline (cdr baseline))))
     (define dirty
       (string-split
        (git repository "--no-optional-locks" "status" "--porcelain")
        "\n"))
     (print-start repository parsed baseline results project dirty)
     'success]
    [else
     (for ([message (in-list parsed)])
       (eprintf "handoff: ~a\n" message))
     'usage]))

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

(: handle-workflow-fork (-> Path String String (U String #f) Status))
(define (handle-workflow-fork repository arc source name)
  (error 'handle-workflow-fork "unimplemented"))

;;; Repositories

(define repository-path (build-path ".scratch" "handoff"))
(define start-file "start.toml")
(define arc-name-regex #px"^(?!(?:\\.|\\.\\.|\\.git)$)[^/]+$")

(: git-repository? (-> Path-String Boolean))
(define (git-repository? path)
  (define dot-git (build-path path ".git"))
  (or (directory-exists? dot-git) (file-exists? dot-git)))

(: find-handoff-repository (-> Path-String Path))
(define (find-handoff-repository start)
  (let loop ([directory (simple-form-path start)])
    (define candidate (build-path directory repository-path))
    (define-values (parent _name _directory?) (split-path directory))
    (cond
      [(git-repository? candidate) candidate]
      [(path? parent) (loop parent)]
      [else (raise-handoff-error "no ~a/ at or above ~a"
                                 repository-path
                                 start)])))

(: find-project-repository (-> Path Path))
(define (find-project-repository repository)
  (string->path
   (or (git* (build-path repository 'up) "rev-parse" "--show-toplevel")
       (raise-handoff-error "no project repository contains ~a" repository))))

;;; Arc helpers

(define archive-tag-prefix "archive/")

(: arc-directory? (-> Path Boolean))
(define (arc-directory? path)
  (and (directory-exists? path) (file-exists? (build-path path start-file))))

(: list-arcs (-> Path (Listof String)))
(define (list-arcs repository)
  ;; Note: `directory-list` sorts by `path<?`.
  (for/list : (Listof String)
            ([entry (in-list (directory-list repository))]
             #:when (arc-directory? (build-path repository entry)))
    (path->string entry)))

(: list-archived-arcs (-> Path (Listof String)))
(define (list-archived-arcs repository)
  (define output (git repository
                      "tag"
                      "--list"
                      (string-append archive-tag-prefix "*")))
  (sort (for/list : (Listof String) ([tag (in-list (string-split output))])
          (string-trim tag archive-tag-prefix #:right? #f))
        string<?))

(: select-arc (-> Path (U String #f) String))
(define (select-arc repository name)
  (cond
    [name
     (unless (regexp-match? arc-name-regex name)
       (raise-handoff-error "specify a top-level arc directory name"))
     (unless (directory-exists? (build-path repository name))
       (raise-handoff-error "~a is not a directory in ~a" name repository))
     name]
    [else
     (define arcs (list-arcs repository))
     (cond
       [(empty? arcs) (raise-handoff-error "there are no active arcs")]
       [(empty? (rest arcs)) (first arcs)]
       [else (raise-handoff-error "specify the arc; active arcs: ~a"
                                  (string-join arcs ", "))])]))

;;; Checks

(define checks-file "checks.toml")
(define results-file "results.toml")
(define default-timeout : Positive-Exact-Rational 60)
(define capture-limit : Natural (* 1024 1024)) ;; 1 MiB
(define output-lines 20)

(struct check ([id : String]
               [claim : String]
               [command : String]
               [timeout : Positive-Exact-Rational])
  #:transparent)

(struct check-result ([check : check]
                      [observation : check-observation])
  #:transparent)

;; `exit` is #f when the check timed out, and `truncated` counts the bytes
;; dropped from the start of `output` to keep it under `capture-limit`.
(struct check-observation ([exit : (U Integer #f)]
                           [duration : Nonnegative-Exact-Rational]
                           [output : String]
                           [truncated : Natural])
  #:transparent)

(struct check-run ([host : String]
                   [started : Moment]
                   [duration : Nonnegative-Exact-Rational]
                   [branch : (U String #f)]
                   [head : (U String #f)]
                   [results : (Listof check-result)])
  #:transparent)

(define-type Outcome (U 'true 'false 'unknown 'timeout))

(: exit-outcome (-> (U Integer #f) Outcome))
(define (exit-outcome code)
  (match code
    [#f 'timeout]
    [0 'true]
    [1 'false]
    [_ 'unknown]))

(: check-result-outcome (-> check-result Outcome))
(define check-result-outcome
  (compose1 (compose1 exit-outcome check-observation-exit)
            check-result-observation))

(: milliseconds->seconds (-> Nonnegative-Real Nonnegative-Exact-Rational))
(define (milliseconds->seconds milliseconds)
  (/ (exact-round milliseconds) 1000))

(: seconds->milliseconds (-> Nonnegative-Real Nonnegative-Real))
(define (seconds->milliseconds seconds)
  (* 1000 seconds))

;;; Reading and writing checks.toml and results.toml

(define check-fields (list 'id 'claim 'command))
(define check-optional-fields (list 'timeout))
(define observation-fields (list 'duration 'output))
(define observation-optional-fields (list 'exit 'truncated))

(: read-checks (-> Path (Listof check)))
(define (read-checks path)
  (unless (file-exists? path)
    (raise-handoff-error "~a does not exist" path))
  (parse-checks (file->string path) path))

(: parse-checks (-> String Path (Listof check)))
(define (parse-checks text source)
  (define source-string (path->string source))
  (define table (read-toml text source-string))
  (expect-fields table null (list 'check) source-string)
  (parse-check-tables table
                      source-string
                      (λ ([entry : Table] [context : String])
                        (expect-fields entry
                                       check-fields
                                       check-optional-fields
                                       context)
                        (table->check entry context))
                      check-id))

(: parse-check-run (-> String Path check-run))
(define (parse-check-run text source)
  (define source-string (path->string source))
  (define table (read-toml text source-string))
  (expect-fields table (list 'run 'project) (list 'check) source-string)
  (define run-context (format "~a: run" source-string))
  (define project-context (format "~a: project" source-string))
  (define run (expect-table (hash-ref table 'run) run-context))
  (define project (expect-table (hash-ref table 'project) project-context))
  (expect-fields run (list 'host 'started 'duration) null run-context)
  (expect-fields project null (list 'branch 'head) project-context)
  (define results
    (parse-check-tables
     table
     source-string
     (λ ([entry : Table] [context : String])
       (expect-fields entry
                      (append check-fields observation-fields)
                      (append check-optional-fields observation-optional-fields)
                      context)
       (check-result (table->check entry context)
                     (table->check-observation entry context)))
     (compose1 check-id check-result-check)))
  (check-run (expect-string run 'host run-context)
             (expect-datetime run 'started run-context)
             (expect-seconds run 'duration run-context)
             (and (hash-has-key? project 'branch)
                  (expect-string project 'branch project-context))
             (and (hash-has-key? project 'head)
                  (expect-string project 'head project-context))
             results))

(: parse-check-tables (All (A) (-> Table String (-> Table String A) (-> A String)
                                   (Listof A))))
(define (parse-check-tables table source parse id)
  (define entries (hash-ref table 'check (const null)))
  (unless (list? entries)
    (raise-handoff-error "~a: check must be an array of tables" source))
  (define parsed
    (for/list : (Listof A) ([entry (in-list entries)]
                            [index (in-naturals)])
      (define context (format "~a: check[~a]" source index))
      (parse (expect-table entry context) context)))
  (define duplicate ((inst check-duplicates A String) parsed #:key id))
  (when duplicate
    (raise-handoff-error "~a: check ~a appears more than once"
                         source
                         (id duplicate)))
  parsed)

(: table->check (-> Table String check))
(define (table->check table context)
  (check (expect-string table 'id context)
         (expect-string table 'claim context)
         (expect-string table 'command context)
         (expect-timeout table context)))

(: table->check-observation (-> Table String check-observation))
(define (table->check-observation table context)
  (define exit (hash-ref table 'exit #f))
  (unless (or (not exit) (exact-integer? exit))
    (raise-handoff-error "~a: exit must be an integer" context))
  (define output (hash-ref table 'output))
  (unless (string? output)
    (raise-handoff-error "~a: output must be a string" context))
  (check-observation exit
                     (expect-seconds table 'duration context)
                     output
                     (if (hash-has-key? table 'truncated)
                         (expect-natural table 'truncated context)
                         0)))

(: expect-timeout (-> Table String Positive-Exact-Rational))
(define (expect-timeout table context)
  (define seconds
    (if (hash-has-key? table 'timeout)
        (expect-seconds table 'timeout context)
        default-timeout))
  (if (positive? seconds)
      seconds
      (raise-handoff-error "~a: timeout must be a positive number of seconds"
                           context)))

(: expect-seconds (-> Table Symbol String Nonnegative-Exact-Rational))
(define (expect-seconds table key context)
  (define seconds (hash-ref table key #f))
  (if (and (rational? seconds) (>= seconds 0))
      (milliseconds->seconds (seconds->milliseconds seconds))
      (raise-handoff-error "~a: ~a must be a nonnegative number of seconds"
                           context
                           key)))

(: write-check-run (-> check-run Void))
(define (write-check-run run)
  (define (field [key : String] [value : String])
    (printf "~a = ~a\n" key value))
  (printf "[run]\n")
  (field "host" (toml-string (check-run-host run)))
  (field "started" (moment->iso8601 (check-run-started run)))
  (field "duration" (toml-number (check-run-duration run)))
  (printf "\n[project]\n")
  (let ([branch (check-run-branch run)])
    (when branch
      (field "branch" (toml-string branch))))
  (let ([head (check-run-head run)])
    (when head
      (field "head" (toml-string head))))
  (for ([result (in-list (check-run-results run))])
    (define check (check-result-check result))
    (define observation (check-result-observation result))
    (define exit (check-observation-exit observation))
    (define truncated (check-observation-truncated observation))
    (printf "\n[[check]]\n")
    (field "id" (toml-string (check-id check)))
    (field "claim" (toml-string (check-claim check)))
    (field "command" (toml-string (check-command check)))
    (field "timeout" (toml-number (check-timeout check)))
    (when exit
      (field "exit" (number->string exit)))
    (field "duration" (toml-number (check-observation-duration observation)))
    (unless (zero? truncated)
      (field "truncated" (number->string truncated)))
    (field "output" (toml-string (check-observation-output observation)))))

;;; Running checks

(: run-checks (-> Path (Listof check) (Listof check-result)))
(define (run-checks project checks)
  (define bash (or (find-executable-path "bash")
                   (raise-handoff-error "bash not found on PATH")))
  (define directory (make-temporary-directory))
  ;; Shutting the custodian down kills the process groups of the checks still
  ;; running if we exit early.
  (define custodian (make-custodian))
  (dynamic-wind
   void
   (thunk
     (parameterize ([current-custodian custodian]
                    [current-subprocess-custodian-mode 'kill]
                    [subprocess-group-enabled #t]
                    [current-directory project])
       (: start-check (-> check Path (Evtof check-result)))
       (define (start-check check output)
         (define started (current-inexact-monotonic-milliseconds))
         (define process
           (call-with-output-file output
             (λ ([out : Output-Port])
               (define-values (process _stdout stdin _stderr)
                 (subprocess out #f out bash "-c" (check-command check)))
               (close-output-port (assert stdin))
               process)))
         (define deadline (+ started
                             (seconds->milliseconds (check-timeout check))))
         (handle-evt
          (choice-evt process (monotonic-alarm-evt deadline #t))
          ;; `thunk` and `thunk*` cause type checking problems here
          (λ (_) (finish-check check process output started))))
       ;; A check is finished when its shell exits or its timeout deadline
       ;; lapses. Either way we kill its process group, which also stops
       ;; anything that the shell left running in the background.
       (: finish-check (-> check Subprocess Path Flonum check-result))
       (define (finish-check check process output started)
         (define duration
           (milliseconds->seconds
            (max 0.0 (- (current-inexact-monotonic-milliseconds) started))))
         (define status (subprocess-status process))
         (kill-group (subprocess-pid process))
         (subprocess-wait process)
         (define truncated
           (max 0 (- (file-size output) capture-limit)))
         (check-result
          check
          (check-observation (if (eq? status 'running) #f status)
                             duration
                             (call-with-input-file output
                               (λ ([in : Input-Port])
                                 (file-position in truncated)
                                 (port->string in)))
                             truncated)))
       (define events
         (for/hasheq : (Immutable-HashTable check (Evtof check-result))
                     ([check (in-list checks)]
                      [index (in-naturals)])
           (values
            check
            (start-check check (build-path directory (number->string index))))))
       (let loop : (Listof check-result)
            ([pending : (Immutable-HashTable check (Evtof check-result)) events]
             [finished : (Immutable-HashTable check check-result) (hasheq)])
         (cond
           [(hash-empty? pending)
            (for/list : (Listof check-result) ([check (in-list checks)])
              (hash-ref finished check))]
           [else
            (define result (apply sync (hash-values pending)))
            (loop (hash-remove pending (check-result-check result))
                  (hash-set finished (check-result-check result) result))]))))
   (thunk
     (custodian-shutdown-all custodian)
     (delete-directory/files directory))))

;;; Comparing checks

(struct check-added ([after : check-result]) #:transparent)
(struct check-removed ([before : check-result]) #:transparent)
(struct check-matched ([before : check-result]
                       [after : check-result])
  #:transparent)
(define-type Check-Comparison (U check-added check-removed check-matched))

(define-type Change
  (U 'redefined
     'unchanged
     'contradicted
     'unverified
     'verified
     'still-unverified))

(: compare-check-results (-> (Listof check-result)
                             (Listof check-result)
                             (Listof Check-Comparison)))
(define (compare-check-results before after)
  (: find (-> check-result (Listof check-result) (U check-result #f)))
  (define (find result results)
    (define id (check-id (check-result-check result)))
    (findf (λ ([other : check-result])
             (equal? (check-id (check-result-check other)) id))
           results))
  (append
   (for/list : (Listof Check-Comparison) ([result (in-list after)])
     (define earlier (find result before))
     (if earlier
         (check-matched earlier result)
         (check-added result)))
   (for/list : (Listof Check-Comparison) ([result (in-list before)]
                                          #:unless (find result after))
     (check-removed result))))

(: check-matched-change (-> check-matched Change))
(define (check-matched-change comparison)
  (match-define (check-matched before after) comparison)
  (cond
    [(not (check-definition-equivalent? (check-result-check before)
                                        (check-result-check after)))
     'redefined]
    [else (outcome-change (check-result-outcome before)
                          (check-result-outcome after))]))

;; The claim is a description of the command, so rewording it does not
;; redefine the check.
(: check-definition-equivalent? (-> check check Boolean))
(define (check-definition-equivalent? a b)
  (and (equal? (check-command a) (check-command b))
       (= (check-timeout a) (check-timeout b))))

(: outcome-change (-> Outcome Outcome Change))
(define (outcome-change before after)
  (define (definite? [outcome : Outcome])
    (and (memq outcome (list 'true 'false)) #t))
  (cond
    [(eq? before after) 'unchanged]
    [(and (definite? before) (definite? after)) 'contradicted]
    [(definite? before) 'unverified]
    [(definite? after) 'verified]
    [else 'still-unverified]))

;;; Printing results

(: print-results (-> (Listof check-result) Void))
(define (print-results results)
  (for ([result (in-list results)])
    (print-result-header result)
    (print-result-details result)
    (newline))
  (print-summary results))

(: print-comparisons (-> (Listof Check-Comparison)
                         Positive-Integer
                         String
                         Moment
                         Void))
(define (print-comparisons comparisons log-entry-index host started)
  (printf "compared against log/~a, recorded on ~a at ~a\n\n"
          log-entry-index
          host
          (moment->iso8601 started))
  (for ([comparison (in-list comparisons)])
    (match comparison
      [(check-added after)
       (print-result-header after)
       (printf "change: new since log/~a\n" log-entry-index)
       (print-result-details after)
       (newline)]
      [(check-matched before after)
       (define change (check-matched-change comparison))
       (define show-earlier-output?
         (and (not (memq change (list 'unchanged 'redefined)))
              (not (eq? (check-result-outcome before) 'true))))
       (print-result-header after)
       (case change
         [(unchanged) (void)]
         [(redefined)
          (printf "change: redefined since log/~a, where it was ~a\n"
                  log-entry-index
                  (check-result-outcome before))]
         [else
          (printf "change: ~a, was ~a at log/~a\n"
                  change
                  (check-result-outcome before)
                  log-entry-index)])
       (print-result-details after)
       (when show-earlier-output?
         (print-output (format "output at log/~a" log-entry-index)
                       (check-observation-output
                        (check-result-observation before))))
       (newline)]
      [(check-removed _) (void)]))
  (define removed
    (for*/list : (Listof check) ([comparison (in-list comparisons)]
                                 #:when (check-removed? comparison))
      (check-result-check (check-removed-before comparison))))
  (for ([check (in-list removed)])
    (printf "removed since log/~a: ~a (~a)\n"
            log-entry-index
            (check-id check)
            (check-claim check)))
  (unless (empty? removed)
    (newline))
  (print-summary
   (filter-map (λ ([comparison : Check-Comparison]) : (U check-result #f)
                 (match comparison
                   [(check-added after) after]
                   [(check-matched _ after) after]
                   [(check-removed _) #f]))
               comparisons)))

(: print-result-header (-> check-result Void))
(define (print-result-header result)
  (define check (check-result-check result))
  (printf "id: ~a\n" (check-id check))
  (printf "claim: ~a\n" (check-claim check))
  (printf "outcome: ~a\n" (check-result-outcome result)))

(: print-result-details (-> check-result Void))
(define (print-result-details result)
  (define check (check-result-check result))
  (define observation (check-result-observation result))
  (define outcome (check-result-outcome result))
  (case outcome
    [(true) (void)]
    [(timeout) (printf "timeout: ~as\n" (format-seconds (check-timeout check)))]
    [else (printf "exit: ~a\n" (check-observation-exit observation))])
  (printf "duration: ~as\n"
          (format-seconds (check-observation-duration observation)))
  (unless (eq? outcome 'true)
    (unless (zero? (check-observation-truncated observation))
      (printf "truncated: ~a bytes before the output were not kept\n"
              (check-observation-truncated observation)))
    (print-output "output" (check-observation-output observation))))

(: format-seconds (-> Nonnegative-Exact-Rational String))
(define (format-seconds seconds)
  (~r seconds #:precision '(= 1)))

(: print-output (-> String String Void))
(define (print-output label output)
  (define lines (string-split output "\n"))
  (unless (empty? lines)
    (define tail (take-right lines (min output-lines (length lines))))
    (printf "~a~a:\n"
            label
            (if (< (length tail) (length lines))
                (format " (last ~a of ~a lines)" (length tail) (length lines))
                ""))
    (for ([line (in-list tail)])
      (printf "  ~a\n" line))))

(: print-summary (-> (Listof check-result) Void))
(define (print-summary results)
  (define (count-with-outcome outcome)
    (count (λ ([result : check-result])
             (eq? (check-result-outcome result) outcome))
           results))
  (define counts
    (for*/list : (Listof String)
               ([outcome (in-list (list 'true 'false 'unknown 'timeout))]
                [count (in-value (count-with-outcome outcome))]
                #:unless (zero? count))
      (format "~a ~a" count outcome)))
  (printf "summary: ~a\n"
          (if (empty? counts)
              "no checks"
              (string-join counts ", "))))

;;; Reading arcs

(define state-files
  (list "arc.md"
        "tasks.md"
        "do-not-reopen.md"
        "open-questions.md"
        "review.md"))
(define log-directory "log")
(define entry-file "entry.md")

(struct arc ([name : String]
             [start : start]
             [checks : (Listof check)]
             [state-texts : (Immutable-HashTable String String)]
             [workflow-text : (U String #f)]
             [missing-references : (Listof scope-path)]
             [other-documents : (Listof (Pairof scope-path (U String #f)))]
             [log : (Listof log-entry)]
             [invalid-log-entries : (Listof String)])
  #:transparent)

(struct log-entry ([number : Positive-Integer]
                   [committed? : Boolean]
                   [text : (U String #f)]
                   ;; `results` is the error's message when results.toml does
                   ;; not parse.
                   [results : (U check-run String #f)])
  #:transparent)

;; Either the arc, or the error of every file that does not parse.
(: read-arc (-> Path Path String (U arc (Listof String))))
(define (read-arc repository project name)
  (define directory (build-path repository name))
  (define errors : (Listof String) null)
  (: attempt (All (A) (-> (-> A) (U A #f))))
  (define (attempt read)
    (with-handlers ([exn:fail:handoff?
                     (λ ([error : exn])
                       (set! errors (cons (exn-message error) errors))
                       #f)])
      (read)))
  (define texts
    (for/hash : (Immutable-HashTable String (U String #f))
              ([file (in-list (list* start-file checks-file state-files))])
      (define path (build-path directory file))
      (values file
              (attempt
               (thunk
                (if (file-exists? path)
                    (file->string path)
                    (raise-handoff-error "~a: expected a regular file"
                                         (build-path name file))))))))
  (define start-text (hash-ref texts start-file))
  (define start*
    (and start-text
         (attempt (thunk (parse-start start-text (build-path name start-file))))))
  (define checks-text (hash-ref texts checks-file))
  (define checks
    (and checks-text
         (attempt (thunk (parse-checks checks-text (build-path name checks-file))))))
  (define log* (attempt (thunk (read-log repository name))))
  (cond
    [(and start* checks log* (empty? errors))
     (match-define (cons entries invalid-log-entries) log*)
     (define state-texts
       (for/hash : (Immutable-HashTable String String)
                 ([file (in-list (cons start-file state-files))])
         (values file (assert (hash-ref texts file) string?))))
     (define workflow-path
       (resolve-scope-path repository name project (start-workflow start*)))
     (define workflow-text
       (and (file-exists? workflow-path) (file->string workflow-path)))
     (define missing-references
       (filter-not (λ ([path : scope-path])
                     (file-exists?
                      (resolve-scope-path repository name project path)))
                   (cons (start-workflow start*)
                         (map read-first-path (start-read-first start*)))))
     (define other-documents (read-arc-other-documents repository directory))
     (arc name
          start*
          checks
          state-texts
          workflow-text
          missing-references
          other-documents
          entries
          invalid-log-entries)]
    [else (reverse errors)]))

(: read-log (-> Path String (Pairof (Listof log-entry) (Listof String))))
(define (read-log repository name)
  (define directory (build-path repository name log-directory))
  (define committed
    (let ([paths (git* repository "ls-tree" "-z" "--name-only" "HEAD" "--"
                       (string-append name "/" log-directory "/"))])
      (if paths
          (for/list : (Listof String)
                    ([path (in-list (string-split paths "\u0000"))])
            (define-values (_base entry _directory?) (split-path path))
            (path->string (assert entry path?)))
          null)))
  (: read-entry (-> String (U log-entry #f)))
  (define (read-entry entry)
    (define number (parse-log-entry-name entry))
    (define path (build-path directory entry))
    (define text-path (build-path path entry-file))
    (define results-path (build-path path results-file))
    (and number
         (directory-exists? path)
         (log-entry number
                    (and (member entry committed) #t)
                    (and (file-exists? text-path) (file->string text-path))
                    (and (file-exists? results-path)
                         (with-handlers ([exn:fail:handoff? exn-message])
                           (parse-check-run (file->string results-path)
                                            (build-path name
                                                        log-directory
                                                        entry
                                                        results-file)))))))
  (cond
    [(directory-exists? directory)
     (define entries (map path->string (directory-list directory)))
     (cons ((inst sort log-entry Positive-Integer) (filter-map read-entry entries)
                                                   <
                                                   #:key log-entry-number)
           (filter-not read-entry entries))]
    [(or (file-exists? directory) (link-exists? directory))
     (raise-handoff-error "~a: expected a directory"
                          (build-path name log-directory))]
    [else (cons null null)]))

(: read-arc-other-documents (-> Path Path (Listof (Pairof scope-path (U String #f)))))
(define (read-arc-other-documents repository arc-directory)
  (define layout
    (map string->path
         (list* start-file checks-file log-directory state-files)))
  (define (markdown [path : Path])
    (and (path-has-extension? path #".md")
         (file-exists? path)
         (file->string path)))
  (append
   (for/list : (Listof (Pairof scope-path (U String #f)))
             ([entry (in-list (directory-list arc-directory))]
              #:unless (member entry layout))
     (define path (build-path arc-directory entry))
     (cons (scope-path 'arc (if (directory-exists? path)
                                (path->directory-path entry)
                                entry))
           (markdown path)))
   (for*/list : (Listof (Pairof scope-path (U String #f)))
              ([entry (in-list (directory-list repository))]
               [text (in-value (markdown (build-path repository entry)))]
               #:when text)
     (cons (scope-path 'handoff entry) text))))

(: find-last-committed-log-entry (-> (Listof log-entry) (U log-entry #f)))
(define (find-last-committed-log-entry entries)
  (findf log-entry-committed? (reverse entries)))

(: find-pending-log-entry (-> (Listof log-entry) (U log-entry #f)))
(define (find-pending-log-entry entries)
  (and (pair? entries)
       (not (log-entry-committed? (last entries)))
       (last entries)))

;; The baseline is the highest-numbered log entry committed to the handoff
;; repository. This means a pending entry is never the baseline.
(: find-baseline (-> String (Listof log-entry)
                     (U (Pairof Positive-Integer check-run) #f)))
(define (find-baseline name entries)
  (define entry (find-last-committed-log-entry entries))
  (define results (and entry (log-entry-results entry)))
  (cond
    [(not entry) #f]
    [(check-run? results) (cons (log-entry-number entry) results)]
    [(string? results) (raise-handoff-error "~a" results)]
    [else (raise-handoff-error "~a: expected a regular file"
                               (build-path name
                                           log-directory
                                           (number->string
                                            (log-entry-number entry))
                                           results-file))]))

;;; Parsing arcs

(struct start ([next : String]
               [workflow : scope-path]
               [read-first : (Listof read-first)])
  #:transparent)

(struct read-first ([path : scope-path] [reason : String]) #:transparent)

(define-type Scope (U 'arc 'handoff 'project))
(struct scope-path ([scope : Scope] [path : Path]) #:transparent)

(: parse-start (-> String Path start))
(define (parse-start text source)
  (define source-string (path->string source))
  (define table (read-toml text source-string))
  (expect-fields table (list 'next 'workflow 'read-first) null source-string)
  (define entries (hash-ref table 'read-first))
  (unless (list? entries)
    (raise-handoff-error "~a: read-first must be an array of tables"
                         source-string))
  (start (expect-string table 'next source-string)
         (expect-scope-path table 'workflow source-string)
         (for/list : (Listof read-first) ([entry (in-list entries)]
                                          [index (in-naturals)])
           (define context (format "~a: read-first[~a]" source-string index))
           (define read (expect-table entry context))
           (expect-fields read (list 'path 'reason) null context)
           (read-first (expect-scope-path read 'path context)
                       (expect-string read 'reason context)))))

(: expect-scope-path (-> Table Symbol String scope-path))
(define (expect-scope-path table key context)
  (define text (expect-string table key context))
  (or (parse-scope-path text)
      (raise-handoff-error "~a: ~a must be arc:, handoff: or project: followed by a relative path without .."
                           context
                           key)))

(: parse-scope-path (-> String (U scope-path #f)))
(define (parse-scope-path text)
  (match (regexp-match #px"^(arc|handoff|project):([^\u0000]+)$" text)
    [(list _ (? string? scope) (? string? relative))
     (define path (string->path relative))
     (and (relative-path? path)
          (not (memq 'up (explode-path path)))
          (scope-path (case scope
                        [("arc") 'arc]
                        [("handoff") 'handoff]
                        [else 'project])
                      path))]
    [_ #f]))

(: scope-path->string (-> scope-path String))
(define (scope-path->string path)
  (format "~a:~a"
          (scope-path-scope path)
          (path->string (scope-path-path path))))

(: resolve-scope-path (-> Path String Path scope-path Path))
(define (resolve-scope-path repository arc project path)
  (build-path (case (scope-path-scope path)
                [(arc) (build-path repository arc)]
                [(handoff) repository]
                [(project) project])
              (scope-path-path path)))

(: parse-log-entry-name (-> String (U Positive-Integer #f)))
(define (parse-log-entry-name name)
  (define number (and (regexp-match? #px"^[1-9][0-9]*$" name)
                      (string->number name)))
  (and (exact-positive-integer? number) number))

;;; Lint

(define placeholder-check-id "replace-me")

(: lint (-> arc (Listof String)))
(define (lint parsed)
  (define name (arc-name parsed))
  (define pending (find-pending-log-entry (arc-log parsed)))
  (append
   (for/list : (Listof String) ([path (in-list (arc-missing-references parsed))])
     (format "~a: ~a is not a regular file"
             (build-path name start-file)
             (scope-path->string path)))
   (for/list : (Listof String)
             ([check (in-list (arc-checks parsed))]
              #:when (equal? (check-id check) placeholder-check-id))
     (format "~a: check ~a is the placeholder from handoff new; replace it with the arc's checks"
             (build-path name checks-file)
             placeholder-check-id))
   (for*/list : (Listof String)
              ([document (in-list (arc-other-documents parsed))]
               [issue (in-value (let ([text (cdr document)])
                                  (and text
                                       (not (verbatim-copy? text))
                                       (summary-issue text))))]
               #:when issue)
     (define path (car document))
     (format "~a: ~a"
             (if (eq? (scope-path-scope path) 'arc)
                 (build-path name (scope-path-path path))
                 (scope-path-path path))
             issue))
   (append*
    (for/list : (Listof (Listof String)) ([entry (in-list (arc-log parsed))])
      (define path (build-path name
                               log-directory
                               (number->string (log-entry-number entry))))
      (define pending? (eq? entry pending))
      (define text (log-entry-text entry))
      (define results (log-entry-results entry))
      (append
       (cond
         [pending? null]
         [(not text) (list (format "~a: expected a regular file"
                                   (build-path path entry-file)))]
         [(not (entry-headline text))
          (list (format "~a: expected a heading on its first nonblank line"
                        (build-path path entry-file)))]
         [else null])
       (cond
         [(string? results) (list results)]
         [(or results pending?) null]
         [else (list (format "~a: expected a regular file"
                             (build-path path results-file)))]))))
   (for/list : (Listof String) ([stray (in-list (arc-invalid-log-entries parsed))])
     (format (if (parse-log-entry-name stray)
                 "~a: expected a directory"
                 "~a: expected a log entry number, a positive integer without leading zeros")
             (build-path name log-directory stray)))
   (budget-diagnostics parsed)))

(: budget-diagnostics (-> arc (Listof String)))
(define (budget-diagnostics parsed)
  (define sections
    (inlined-sections parsed
                      (or (find-pending-log-entry (arc-log parsed))
                          (find-last-committed-log-entry (arc-log parsed)))))
  (define sizes
    ((inst sort (Pairof String Natural) Natural)
     (for/list : (Listof (Pairof String Natural))
               ([section (in-list sections)])
       (cons (car section)
             (bytes-length (string->bytes/utf-8 (cdr section)))))
     >
     #:key cdr))
  (define total (apply + (map (inst cdr String Natural) sizes)))
  (if (> total (* 1000 report-budget))
      (list (format "~a: the report inlines ~a KB, over the ~a KB budget; largest: ~a"
                    (arc-name parsed)
                    (format-kilobytes total)
                    report-budget
                    (string-join
                     (for/list : (Listof String)
                               ([size (in-list (take sizes
                                                     (min 3 (length sizes))))])
                       (format "~a ~a KB"
                               (car size)
                               (format-kilobytes (cdr size))))
                     ", ")))
      null))

(: format-kilobytes (-> Natural String))
(define (format-kilobytes bytes)
  (~r (/ bytes 1000) #:precision '(= 1)))

(: count-errors (-> (Listof String) String))
(define (count-errors diagnostics)
  (case (length diagnostics)
    [(0) "no errors"]
    [(1) "1 error"]
    [else (format "~a errors" (length diagnostics))]))

;;; The start report

(define report-budget 24) ; in KB of 1000 bytes
(define commit-limit 20)

(struct project ([path : Path]
                 [branch : (U String #f)]
                 [head : (U String #f)]
                 [history : (U 'unresolvable history)])
  #:transparent)

(struct history ([only-recorded : (Listof String)]
                 [only-current : (Listof String)])
  #:transparent)

(: read-project (-> Path (U check-run #f) project))
(define (read-project path baseline)
  (define head (git* path "rev-parse" "--verify" "--quiet" "HEAD"))
  (define recorded (and baseline (check-run-head baseline)))
  (: commits (-> (U String #f) (U String #f) (Listof String)))
  (define (commits from to)
    (if to
        (string-split (git path "log" "--abbrev=12" "--format=%h %s"
                           (if from (string-append from ".." to) to))
                      "\n")
        null))
  (define branch (git* path "symbolic-ref" "--quiet" "--short" "HEAD"))
  (project path
           branch
           head
           (cond
             [(not baseline) (history null null)]
             [(and recorded
                   (not (git* path
                              "cat-file"
                              "-e"
                              (string-append recorded "^{commit}"))))
              'unresolvable]
             [else (history (commits head recorded)
                            (commits recorded head))])))

(: inlined-sections (-> arc (U log-entry #f) (Listof (Pairof String String))))
(define (inlined-sections parsed entry)
  (: text (-> String String))
  (define (text file)
    (hash-ref (arc-state-texts parsed) file))
  (define-values (tasks count) (omit-finished-tasks (text "tasks.md")))
  (filter-map
   (λ ([section : (Pairof String (U String #f))])
     (match section
       [(cons header (? string? text*)) (cons header text*)]
       [_ #f]))
   (list (cons "arc:start.toml" (text start-file))
         (cons "arc:arc.md" (text "arc.md"))
         (cons (scope-path->string (start-workflow (arc-start parsed)))
               (arc-workflow-text parsed))
         (cons "arc:tasks.md"
               (if (zero? count)
                   tasks
                   (format "~a(~a finished task~a left out; they are in arc:tasks.md)\n"
                           tasks
                           count
                           (if (= count 1) "" "s"))))
         (cons "arc:do-not-reopen.md" (text "do-not-reopen.md"))
         (cons "arc:open-questions.md" (text "open-questions.md"))
         (cons "arc:review.md" (text "review.md"))
         (cons (format "arc:~a/~a/~a"
                       log-directory
                       (and entry (log-entry-number entry))
                       entry-file)
               (and entry (log-entry-text entry))))))

(: omit-finished-tasks (-> String (Values String Natural)))
(define (omit-finished-tasks text)
  (define-values (kept count _omitting?)
    (for/fold ([kept : (Listof String) null]
               [count : Natural 0]
               [omitting? : Boolean #f])
              ([line (in-list (string-split text "\n" #:trim? #f))])
      (cond
        [(regexp-match? #px"^- \\[[xX]\\] " line)
         (values kept (add1 count) #t)]
        [(and omitting? (regexp-match? #px"^\\s+\\S" line))
         (values kept count #t)]
        [else
         (values (cons line kept) count #f)])))
  (values (string-join (reverse kept) "\n") count))

(: print-start (-> Path arc
                   (U (Pairof Positive-Integer check-run) #f)
                   (Listof check-result) project (Listof String)
                   Void))
(define (print-start repository parsed baseline results project dirty)
  (printf "project: ~a\n" (project-path project))
  (printf "handoff repository: ~a\n" repository)
  (print-sections (inlined-sections parsed (find-last-committed-log-entry (arc-log parsed))))
  (print-header "other documents")
  (print-documents parsed)
  (print-header "project")
  (print-project project baseline)
  (print-header "handoff repository")
  (print-handoff-repository (lint parsed) parsed dirty)
  (print-header "checks")
  (match baseline
    [(cons number run)
     (print-comparisons (compare-check-results (check-run-results run) results)
                        number
                        (check-run-host run)
                        (check-run-started run))]
    [#f
     (printf "no committed log entry to compare against\n\n")
     (print-results results)]))

(: print-header (-> String Void))
(define (print-header name)
  (printf "\n==> ~a <==\n" name))

(: print-sections (-> (Listof (Pairof String String)) Void))
(define (print-sections sections)
  (for ([section (in-list sections)])
    (define text (cdr section))
    (print-header (car section))
    (write-string text)
    (unless (or (string=? text "") (string-suffix? text "\n"))
      (newline))))

(: print-documents (-> arc Void))
(define (print-documents parsed)
  (define workflow (start-workflow (arc-start parsed)))
  (define documents
    (filter (λ ([document : (Pairof scope-path (U String #f))])
              (define text (cdr document))
              (not (or (equal? (car document) workflow)
                       (and text (verbatim-copy? text)))))
            (arc-other-documents parsed)))
  (when (empty? documents)
    (displayln "nothing"))
  (for ([document (in-list documents)])
    (define text (cdr document))
    (define summary
      (if text (string-join (map string-trim (first-paragraph text))) ""))
    (printf "- ~a~a\n"
            (scope-path->string (car document))
            (if (string=? summary "") "" (string-append ": " summary)))))

(: print-project (-> project (U (Pairof Positive-Integer check-run) #f) Void))
(define (print-project project baseline)
  (printf "now: ~a\n" (describe-head (project-branch project)
                                     (project-head project)))
  (when baseline
    (match-define (cons number run) baseline)
    (: commits (-> String (Listof String) Void))
    (define (commits heading lines)
      (displayln heading)
      (for ([line (in-list (take lines (min commit-limit (length lines))))])
        (printf "  ~a\n" line))
      (when (> (length lines) commit-limit)
        (printf "  and ~a more\n" (- (length lines) commit-limit))))
    (printf "at log/~a: ~a\n"
            number
            (describe-head (check-run-branch run) (check-run-head run)))
    (unless (equal? (check-run-branch run) (project-branch project))
      (printf "the branch differs from log/~a\n" number))
    (match (project-history project)
      ['unresolvable
       (displayln "recorded head does not resolve here: its history was rewritten and pruned, or it was committed on another machine")]
      [(history '() '()) (displayln "no new commits")]
      [(history '() only-current)
       (commits (format "commits since log/~a:" number) only-current)]
      [(history only-recorded only-current)
       (commits (format "history rewritten since log/~a; < marks commits only in the recorded history, > only in the current one:"
                        number)
                (append (map (λ ([line : String]) (string-append "< " line))
                             only-recorded)
                        (map (λ ([line : String]) (string-append "> " line))
                             only-current)))])))

(: describe-head (-> (U String #f) (U String #f) String))
(define (describe-head branch head)
  (if head
      (format "~a at ~a"
              (or branch "detached")
              (substring head 0 (min 12 (string-length head))))
      (format "~a with no commits" (or branch "detached"))))

(: print-handoff-repository (-> (Listof String) arc (Listof String) Void))
(define (print-handoff-repository diagnostics parsed dirty)
  (printf "lint: ~a\n" (count-errors diagnostics))
  (for ([diagnostic (in-list diagnostics)])
    (printf "  ~a\n" diagnostic))
  (define pending (find-pending-log-entry (arc-log parsed)))
  (cond
    [pending
     (define results (log-entry-results pending))
     (printf "pending log entry: log/~a was begun but not committed, so it is not the baseline\n"
             (log-entry-number pending))
     (printf "  results: ~a\n"
             (cond
               [(check-run? results)
                (format "recorded on ~a at ~a"
                        (check-run-host results)
                        (moment->iso8601 (check-run-started results)))]
               [results "does not parse; see lint"]
               [else "none"]))
     (printf "  entry.md: ~a\n"
             (if (log-entry-text pending) "written" "not written"))]
    [else (displayln "pending log entry: none")])
  (cond
    [(empty? dirty) (displayln "uncommitted: none")]
    [else
     (displayln "uncommitted:")
     (for ([line (in-list dirty)])
       (printf "  ~a\n" line))]))

;;; Markdown

;; The lines of the first paragraph after a document's provenance line and
;; opening headings.
(: first-paragraph (-> String (Listof String)))
(define (first-paragraph text)
  (define (skip? [line : String])
    (or (string=? (string-trim line) "") (regexp-match? #px"^ {0,3}#" line)))
  (define lines (string-split (strip-provenance text) "\n" #:trim? #f))
  (takef (dropf lines skip?) (negate skip?)))

(define non-prose-openings
  (list #px"^[-*+]\\s"
        #px"^[0-9]+[.)]\\s"
        #px"^(```|~~~)"
        #px"^\\|"
        #px"^<"))

(: summary-issue (-> String (U String #f)))
(define (summary-issue text)
  (define paragraph (first-paragraph text))
  (cond
    [(empty? paragraph)
     "expected a paragraph of prose after the opening headings"]
    [(let ([line (string-trim (first paragraph) #:right? #f)])
       (ormap (λ ([opening : Regexp]) (regexp-match? opening line))
              non-prose-openings))
     "expected a paragraph of prose after the opening headings, not a list, code block, table, or HTML"]
    [else #f]))

(: verbatim-copy? (-> String Boolean))
(define (verbatim-copy? text)
  (regexp-match? #px"^<!-- copied [^\n]*; verbatim -->\n" text))

(: strip-provenance (-> String String))
(define (strip-provenance text)
  (regexp-replace #px"^<!-- (copied|forked|created) [^\n]*-->\n" text ""))

;; The text of the heading on an entry's first nonblank line.
(: entry-headline (-> String (U String #f)))
(define (entry-headline text)
  (define line
    (findf (compose1 non-empty-string? string-trim) (string-split text "\n")))
  (define heading (and line
                       (regexp-match #px"^ {0,3}#{1,6}[ \t]+(.*\\S)" line)))
  (and heading (second heading)))

;;; TOML

(define-type Table (Immutable-HashTable Symbol Any))
(define-predicate table? Table)

(: read-toml (-> String String Table))
(define (read-toml text source)
  (with-handlers ([exn:fail? (λ ([error : exn])
                               (raise-handoff-error "~a: ~a"
                                                    source
                                                    (exn-message error)))])
    (parse-toml text)))

(: expect-fields (-> Table (Listof Symbol) (Listof Symbol) String Void))
(define (expect-fields table required optional context)
  (define (names [keys : (Listof Symbol)])
    (string-join (sort (map symbol->string keys) string<?) ", "))
  (define missing (remove* (hash-keys table) required))
  (define unknown (remove* (append required optional) (hash-keys table)))
  (unless (empty? missing)
    (raise-handoff-error "~a is missing ~a" context (names missing)))
  (unless (empty? unknown)
    (raise-handoff-error "~a has unknown fields ~a" context (names unknown))))

(: expect-table (-> Any String Table))
(define (expect-table value context)
  (if (table? value)
      value
      (raise-handoff-error "~a must be a table" context)))

(: expect-string (-> Table Symbol String String))
(define (expect-string table key context)
  (define value (hash-ref table key #f))
  (if (and (string? value) (non-empty-string? (string-trim value)))
      value
      (raise-handoff-error "~a: ~a must be a nonempty string" context key)))

(: expect-natural (-> Table Symbol String Natural))
(define (expect-natural table key context)
  (define value (hash-ref table key #f))
  (if (exact-nonnegative-integer? value)
      value
      (raise-handoff-error "~a: ~a must be a nonnegative integer" context key)))

(: expect-datetime (-> Table Symbol String Moment))
(define (expect-datetime table key context)
  (define value (hash-ref table key #f))
  (if (moment? value)
      value
      (raise-handoff-error "~a: ~a must be a datetime with an offset"
                           context
                           key)))

;; We escape strings ourselves, since toml-lib's writer uses Racket's escapes
;; and so output with an ANSI escape (\e) or a bell (\a) would not read back.
(: toml-string (-> String String))
(define (toml-string text)
  (define multiline? (string-contains? text "\n"))
  (define out (open-output-string))
  (write-string (if multiline? "\"\"\"\n" "\"") out)
  (for ([c (in-string text)])
    (cond
      [(char=? c #\\) (write-string "\\\\" out)]
      [(char=? c #\") (write-string "\\\"" out)]
      [(and multiline? (char=? c #\newline)) (write-char c out)]
      [(char=? c #\tab) (write-char c out)]
      [(or (char<? c #\space) (char=? c #\rubout))
       (write-string "\\u" out)
       (write-string (~r (char->integer c)
                         #:base (list 'up 16)
                         #:min-width 4
                         #:pad-string "0")
                     out)]
      [else (write-char c out)]))
  (write-string (if multiline? "\"\"\"" "\"") out)
  (get-output-string out))

(: toml-number (-> Exact-Rational String))
(define (toml-number number)
  (number->string (if (integer? number) number (exact->inexact number))))

;;; Git

(: git (-> Path-String String * String))
(define (git directory . arguments)
  (define-values (succeeded? output error) (apply run-git directory arguments))
  (unless succeeded?
    (raise-handoff-error "git ~a failed in ~a~a"
                         (string-join arguments) directory
                         (if (string=? error "")
                             ""
                             (string-append ":\n" (string-trim error)))))
  output)

(: git* (-> Path-String String * (U String #f)))
(define (git* directory . arguments)
  (define-values (succeeded? output _error) (apply run-git directory arguments))
  (and succeeded? output))

(: run-git (-> Path-String String * (Values Boolean String String)))
(define (run-git directory . arguments)
  (define program
    (or (find-executable-path "git")
        (raise-handoff-error "git not found on PATH")))
  (define output (open-output-string))
  (define error (open-output-string))
  (define succeeded?
    (parameterize ([current-input-port (open-input-string "")]
                   [current-output-port output]
                   [current-error-port error])
      (apply system* program "-C" directory arguments)))
  (values succeeded?
          (string-trim (get-output-string output) "\n" #:left? #f)
          (get-output-string error)))


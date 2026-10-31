#lang typed/racket/base
;; Automation for the handoff skills.

(require racket/file
         racket/format
         racket/function
         racket/list
         racket/match
         racket/math
         racket/path
         racket/port
         racket/string
         racket/system
         typed/file/sha1
         "cli.rkt"
         "error.rkt"
         "toml.rkt")

(require/typed racket/base
  [current-inexact-monotonic-milliseconds (-> Flonum)]
  [file-or-directory-type
   (-> Path-String (U 'file 'directory 'link 'directory-link #f))]
  [(alarm-evt monotonic-alarm-evt) (-> Real True (Evtof Any))])

(require/typed racket/file
  [call-with-atomic-output-file (-> Path-String (-> Output-Port Path Void) Void)]
  [make-temporary-directory (->* () (String #:base-dir (U Path-String #f)) Path)])

(require/typed basedir
  [list-data-dirs
   (->* () (#:program Path-String #:only-existing? Any) (Listof Path))]
  [list-data-files (->* (Path-String) (#:program Path-String) (Listof Path))])

(require/typed racket/os
  [gethostname (-> String)])

(require/typed gregor
  [adjust-timezone (-> Moment Integer Moment)]
  [~t (-> Moment String String)])

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
         next-log-entry-number
         find-baseline
         lint
         (struct-out project-status)
         (struct-out history)
         inlined-sections
         omit-finished-tasks
         print-sections
         print-other-documents
         print-project-repository
         print-handoff-repository
         (struct-out start-report)
         (struct-out checks-compared)
         (struct-out checks-uncompared)
         build-start-report
         print-start
         commit-subject-or-refuse
         workflow-name?
         workflow-copy-name
         (struct-out reusable-workflow)
         placeholder-workflow
         new-arc-files
         workflow-fork-name
         (struct-out workflow-provenance)
         format-workflow-provenance-line
         split-workflow-provenance
         workflow-install-status)

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
        (define handoff-repository
          (simple-form-path
           (or option (find-handoff-repository (current-directory)))))
        (match subcommand
          [(list-subcommand archived?)
           (handle-list handoff-repository archived?)]
          [(start-subcommand arc)
           (handle-start handoff-repository
                         (select-arc handoff-repository arc))]
          [(check-subcommand arc compare?)
           (handle-check handoff-repository
                         (select-arc handoff-repository arc)
                         compare?)]
          [(lint-subcommand arc)
           (handle-lint handoff-repository
                        (select-arc handoff-repository arc))]
          [(update-subcommand 'begin arc)
           (handle-update-begin handoff-repository
                                (select-arc handoff-repository arc))]
          [(update-subcommand 'commit arc)
           (handle-update-commit handoff-repository
                                 (select-arc handoff-repository arc))]
          [(new-subcommand arc workflow)
           (handle-new handoff-repository arc workflow)]
          [(workflow-install-subcommand name)
           (handle-workflow-install handoff-repository name)]
          [(workflow-fork-subcommand arc source name)
           (handle-workflow-fork handoff-repository
                                 (select-arc handoff-repository arc)
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

;;; Commands

(: handle-list (-> Path Boolean Status))
(define (handle-list handoff-repository archived?)
  (for ([arc-name (in-list ((if archived?
                                list-archived-arcs
                                list-arcs)
                            handoff-repository))])
    (displayln arc-name))
  'success)

(: handle-start (-> Path String Status))
(define (handle-start handoff-repository arc-name)
  (define project-repository (find-project-repository handoff-repository))
  (match (read-arc handoff-repository project-repository arc-name)
    [(? arc? arc)
     (define baseline (find-baseline arc-name (arc-log arc)))
     (define check-results (run-checks project-repository (arc-checks arc)))
     (define project-status
       (read-project-status project-repository (and baseline (cdr baseline))))
     (print-start (build-start-report handoff-repository
                                      arc
                                      baseline
                                      check-results
                                      project-status
                                      (read-dirty handoff-repository)))
     'success]
    [(? list? errors) (refuse-arc-errors arc-name errors)]))

(: handle-check (-> Path String Boolean Status))
(define (handle-check handoff-repository arc-name compare?)
  (define checks
    (read-checks (build-path handoff-repository arc-name checks-file)))
  (define project-repository (find-project-repository handoff-repository))
  (define check-results
    (cond
      [compare?
       ;; Read the baseline first, so that if it is broken, we refuse with an
       ;; error before running the checks.
       (match-define (cons log-entry-number baseline)
         (or (find-baseline arc-name
                            (car (read-log handoff-repository arc-name)))
             (raise-handoff-error "~a has no committed log entry to compare against"
                                  arc-name)))
       (define check-results (run-checks project-repository checks))
       (print-comparisons (compare-check-results (check-run-results baseline)
                                                 check-results)
                          log-entry-number
                          (check-run-host baseline)
                          (check-run-started baseline))
       check-results]
      [else
       (define check-results (run-checks project-repository checks))
       (print-results check-results)
       check-results]))
  (if (andmap (λ ([result : check-result])
                (eq? (check-result-outcome result) 'true))
              check-results)
      'success
      'failure))

(: handle-lint (-> Path String Status))
(define (handle-lint handoff-repository arc-name)
  (define diagnostics
    (match (read-arc handoff-repository
                     (find-project-repository handoff-repository)
                     arc-name)
      [(? arc? arc) (lint arc)]
      [(? list? errors) errors]))
  (for ([diagnostic (in-list diagnostics)])
    (displayln diagnostic))
  (printf "~a: ~a\n" arc-name (count-errors diagnostics))
  (if (empty? diagnostics) 'success 'failure))

(: handle-update-begin (-> Path String Status))
(define (handle-update-begin handoff-repository arc-name)
  (define project-repository (find-project-repository handoff-repository))
  (define arc (read-arc-and-lint handoff-repository project-repository arc-name))
  (define baseline (find-baseline arc-name (arc-log arc)))
  (define-values (branch head) (read-project-head project-repository))
  (define started (posix->moment (current-seconds) 0))
  (define clock (current-inexact-monotonic-milliseconds))
  (define check-results (run-checks project-repository (arc-checks arc)))
  (define duration
    (milliseconds->seconds
     (max 0.0 (- (current-inexact-monotonic-milliseconds) clock))))
  ;; The entry's directory is created only once the checks have run, so that
  ;; an interrupted run leaves nothing behind.
  (define log-entry-number (next-log-entry-number (arc-log arc)))
  (define directory (build-path handoff-repository
                                arc-name
                                log-directory
                                (number->string log-entry-number)))
  (make-directory* directory)
  (define results-path (build-path directory results-file))
  (call-with-atomic-output-file
   results-path
   (λ ([out : Output-Port] [_ : Path])
     (parameterize ([current-output-port out])
       (write-check-run
        (check-run (gethostname)
                   started
                   duration
                   branch
                   head
                   check-results)))))
  (print-checks (compare-with-baseline baseline check-results))
  (printf "\nrecorded: ~a\n" results-path)
  (printf "entry: ~a\n" (build-path directory entry-file))
  'success)

(: handle-update-commit (-> Path String Status))
(define (handle-update-commit handoff-repository arc-name)
  (define project-repository (find-project-repository handoff-repository))
  (define arc (read-arc-and-lint handoff-repository project-repository arc-name))
  (define-values (branch head) (read-project-head project-repository))
  (define subject (commit-subject-or-refuse arc branch head))
  ;; Fails on an unmerged index, which could not be committed anyway.
  (define index (git handoff-repository "write-tree"))
  (with-handlers ([exn:fail? (λ ([error : exn])
                               (git* handoff-repository "read-tree" index)
                               (raise error))])
    (git handoff-repository "add" "-A" "--" arc-name)
    (git handoff-repository "commit" "--quiet" "--message" subject))
  (printf "committed: ~a ~a\n"
          (git handoff-repository "rev-parse" "--short=12" "HEAD")
          subject)
  (print-dirty (read-dirty handoff-repository))
  'success)

(: handle-new (-> Path String (U String #f) Status))
(define (handle-new handoff-repository arc-name workflow-name)
  (unless (regexp-match? arc-name-regex arc-name)
    (raise-handoff-error "specify a top-level arc directory name"))
  (define directory (build-path handoff-repository arc-name))
  (when (file-or-directory-type directory)
    (raise-handoff-error "~a already exists in ~a" arc-name handoff-repository))
  (define project-repository (find-project-repository handoff-repository))
  (define workflow (and workflow-name (find-reusable-workflow workflow-name)))
  (define workflow-path
    (if workflow
        (scope-path 'handoff
                    (string->path (workflow-copy-name
                                   (reusable-workflow-name workflow))))
        placeholder-workflow))
  (define files (new-arc-files arc-name workflow-path))
  ;; The arc is written beside its final place and renamed into it, so that
  ;; an interrupted or failed run leaves no half-written arc.
  (define temporary
    (make-temporary-directory ".new-~a" #:base-dir handoff-repository))
  (dynamic-wind
   void
   (thunk
     (for ([file (in-list files)])
       (call-with-output-file (build-path temporary (car file))
         (λ ([out : Output-Port])
           (write-string (cdr file) out)
           (void))))
     (rename-file-or-directory temporary directory))
   (thunk
     (when (directory-exists? temporary)
       (delete-directory/files temporary))))
  (printf "created: ~a/\n" arc-name)
  (printf "files: ~a\n" (string-join (map (inst car String String) files) ", "))
  (when workflow
    (print-workflow-install workflow (install-workflow handoff-repository workflow)))
  (define diagnostics
    (match (read-arc handoff-repository project-repository arc-name)
      [(? arc? arc) (lint arc)]
      [(? list? errors) errors]))
  (displayln "lint, until the arc is written:")
  (for ([diagnostic (in-list diagnostics)])
    (printf "  ~a\n" diagnostic))
  'success)

(: handle-workflow-list (-> Status))
(define (handle-workflow-list)
  (define directories (workflow-directories))
  (define workflows (read-reusable-workflows directories))
  (cond
    [(empty? workflows)
     (printf "no reusable workflows in ~a\n"
             (string-join (map path->string directories) ", "))]
    [else
     (displayln "search order; install, fork and new take the first workflow with a name")
     (for ([workflow (in-list workflows)])
       (define path (reusable-workflow-path workflow))
       (newline)
       (printf "name: ~a\n" (reusable-workflow-name workflow))
       (printf "path: ~a\n" path)
       (printf "summary: ~a\n" (one-line-summary (file->string path))))])
  'success)

(: handle-workflow-install (-> Path String Status))
(define (handle-workflow-install handoff-repository name)
  (define workflow (find-reusable-workflow name))
  (print-workflow-install workflow (install-workflow handoff-repository workflow))
  'success)

(: handle-workflow-fork (-> Path String String (U String #f) Status))
(define (handle-workflow-fork handoff-repository arc-name source name)
  (define project-repository (find-project-repository handoff-repository))
  (define found : (U reusable-workflow scope-path)
    (if (string-contains? source ":")
        (expect-scope-path (hash 'source source)
                           'source
                           "handoff workflow fork")
        (find-reusable-workflow source)))
  (define-values (description source-path)
    (match found
      [(? reusable-workflow?)
       (values (format "workflow ~a" (reusable-workflow-name found))
               (reusable-workflow-path found))]
      [(? scope-path?)
       (values (scope-path->string found)
               (resolve-scope-path handoff-repository
                                   arc-name
                                   project-repository
                                   found))]))
  (unless (file-exists? source-path)
    (raise-handoff-error "~a is not a regular file" source))
  (define workflow-name (or name (workflow-fork-name found)))
  (unless (workflow-name? workflow-name)
    (raise-handoff-error "~a is not a workflow name: lowercase letters, digits and hyphens; give one as <name>"
                         workflow-name))
  (define file (workflow-copy-name workflow-name))
  (define path (build-path handoff-repository arc-name file))
  (when (file-or-directory-type path)
    (raise-handoff-error "arc:~a already exists; give another name as <name>"
                         file))
  (define contents (file->bytes source-path))
  (define-values (_provenance text)
    (split-workflow-provenance (bytes->string/utf-8 contents #\uFFFD)))
  (call-with-output-file path
    (λ ([out : Output-Port])
      (write-string (format-workflow-provenance-line 'forked
                                                     description
                                                     contents
                                                     (posix->moment (current-seconds) 0))
                    out)
      (write-string text out)
      (void))
    #:exists 'error)
  (printf "forked: arc:~a\n" file)
  (printf "from: ~a\n" (if (reusable-workflow? found) source-path source))
  'success)

(: handle-guide (-> Status))
(define (handle-guide)
  (match (list-data-files guide-file #:program program-name)
    [(cons path _) (displayln path)]
    ['()
     (raise-handoff-error "no ~a in ~a"
                          guide-file
                          (string-join (map path->string (handoff-data-directories))
                                       ", "))])
  'success)

;;; Repositories

(define handoff-repository-path (build-path ".scratch" "handoff"))
(define start-file "start.toml")
(define arc-name-regex #px"^(?!(?:\\.|\\.\\.|\\.git)$)[^/]+$")

(: git-repository? (-> Path-String Boolean))
(define (git-repository? path)
  (define dot-git (build-path path ".git"))
  (or (directory-exists? dot-git) (file-exists? dot-git)))

(: find-handoff-repository (-> Path-String Path))
(define (find-handoff-repository start)
  (let loop ([directory (simple-form-path start)])
    (define candidate (build-path directory handoff-repository-path))
    (define-values (parent _name _directory?) (split-path directory))
    (cond
      [(git-repository? candidate) candidate]
      [(path? parent) (loop parent)]
      [else (raise-handoff-error "no ~a/ at or above ~a"
                                 handoff-repository-path
                                 start)])))

(: find-project-repository (-> Path Path))
(define (find-project-repository handoff-repository)
  (string->path
   (or (git* (build-path handoff-repository 'up) "rev-parse" "--show-toplevel")
       (raise-handoff-error "no project repository contains ~a"
                            handoff-repository))))

;;; Arc helpers

(define archive-tag-prefix "archive/")

(: arc-directory? (-> Path Boolean))
(define (arc-directory? path)
  (and (directory-exists? path) (file-exists? (build-path path start-file))))

(: list-arcs (-> Path (Listof String)))
(define (list-arcs handoff-repository)
  ;; Note: `directory-list` sorts by `path<?`.
  (for/list : (Listof String)
            ([entry (in-list (directory-list handoff-repository))]
             #:when (arc-directory? (build-path handoff-repository entry)))
    (path->string entry)))

(: list-archived-arcs (-> Path (Listof String)))
(define (list-archived-arcs handoff-repository)
  (define output (git handoff-repository
                      "tag"
                      "--list"
                      (string-append archive-tag-prefix "*")))
  (sort (for/list : (Listof String) ([tag (in-list (string-split output))])
          (string-trim tag archive-tag-prefix #:right? #f))
        string<?))

(: select-arc (-> Path (U String #f) String))
(define (select-arc handoff-repository arc-name)
  (cond
    [arc-name
     (unless (regexp-match? arc-name-regex arc-name)
       (raise-handoff-error "specify a top-level arc directory name"))
     (unless (directory-exists? (build-path handoff-repository arc-name))
       (raise-handoff-error "~a is not a directory in ~a"
                            arc-name
                            handoff-repository))
     arc-name]
    [else
     (define arcs (list-arcs handoff-repository))
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
  (define check-results
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
  (check-run (expect-nonempty-string run 'host run-context)
             (expect-datetime run 'started run-context)
             (expect-seconds run 'duration run-context)
             (and (hash-has-key? project 'branch)
                  (expect-nonempty-string project 'branch project-context))
             (and (hash-has-key? project 'head)
                  (expect-nonempty-string project 'head project-context))
             check-results))

(: parse-check-tables (All (A) (-> Table String (-> Table String A) (-> A String)
                                   (Listof A))))
(define (parse-check-tables table source parse id)
  (define parsed (expect-tables table 'check source parse))
  (define duplicate ((inst check-duplicates A String) parsed #:key id))
  (when duplicate
    (raise-handoff-error "~a: check ~a appears more than once"
                         source
                         (id duplicate)))
  parsed)

(: table->check (-> Table String check))
(define (table->check table context)
  (check (expect-nonempty-string table 'id context)
         (expect-nonempty-string table 'claim context)
         (expect-nonempty-string table 'command context)
         (expect-timeout table context)))

(: table->check-observation (-> Table String check-observation))
(define (table->check-observation table context)
  (check-observation (and (hash-has-key? table 'exit)
                          (expect-integer table 'exit context))
                     (expect-seconds table 'duration context)
                     (expect-string table 'output context)
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
(define (run-checks project-repository checks)
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
                    [current-directory project-repository])
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
  (define (find result check-results)
    (define id (check-id (check-result-check result)))
    (findf (λ ([other : check-result])
             (equal? (check-id (check-result-check other)) id))
           check-results))
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
(define (print-results check-results)
  (for ([result (in-list check-results)])
    (print-result-header result)
    (print-result-details result)
    (newline))
  (print-summary check-results))

(: print-comparisons (-> (Listof Check-Comparison)
                         Positive-Integer
                         String
                         Moment
                         Void))
(define (print-comparisons comparisons log-entry-number host started)
  (printf "compared against log/~a, recorded on ~a at ~a\n\n"
          log-entry-number
          host
          (moment->iso8601 started))
  (for ([comparison (in-list comparisons)])
    (match comparison
      [(check-added after)
       (print-result-header after)
       (printf "change: new since log/~a\n" log-entry-number)
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
                  log-entry-number
                  (check-result-outcome before))]
         [else
          (printf "change: ~a, was ~a at log/~a\n"
                  change
                  (check-result-outcome before)
                  log-entry-number)])
       (print-result-details after)
       (when show-earlier-output?
         (print-output (format "output at log/~a" log-entry-number)
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
            log-entry-number
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
(define (print-summary check-results)
  (define (count-with-outcome outcome)
    (count (λ ([result : check-result])
             (eq? (check-result-outcome result) outcome))
           check-results))
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
             [documents : (Listof (Pairof scope-path (U String #f)))]
             [log : (Listof log-entry)]
             [invalid-log-entries : (Listof String)])
  #:transparent)

(struct log-entry ([number : Positive-Integer]
                   [committed? : Boolean]
                   [text : (U String #f)]
                   ;; `run` is the error's message when results.toml does
                   ;; not parse.
                   [run : (U check-run String #f)])
  #:transparent)

;; Either the arc, or the error of every file that does not parse.
(: read-arc (-> Path Path String (U arc (Listof String))))
(define (read-arc handoff-repository project-repository arc-name)
  (define directory (build-path handoff-repository arc-name))
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
                                         (build-path arc-name file))))))))
  (define start-text (hash-ref texts start-file))
  (define start*
    (and start-text
         (attempt
          (thunk (parse-start start-text (build-path arc-name start-file))))))
  (define checks-text (hash-ref texts checks-file))
  (define checks
    (and checks-text
         (attempt
          (thunk (parse-checks checks-text
                               (build-path arc-name checks-file))))))
  (define log* (attempt (thunk (read-log handoff-repository arc-name))))
  (cond
    [(and start* checks log* (empty? errors))
     (match-define (cons entries invalid-log-entries) log*)
     (define state-texts
       (for/hash : (Immutable-HashTable String String)
                 ([file (in-list (cons start-file state-files))])
         (values file (assert (hash-ref texts file) string?))))
     (define workflow-path
       (resolve-scope-path handoff-repository
                           arc-name
                           project-repository
                           (start-workflow start*)))
     (define workflow-text
       (and (file-exists? workflow-path) (file->string workflow-path)))
     (define missing-references
       (filter-not (λ ([path : scope-path])
                     (file-exists?
                      (resolve-scope-path handoff-repository
                                          arc-name
                                          project-repository
                                          path)))
                   (cons (start-workflow start*)
                         (map read-first-path (start-read-first start*)))))
     (define documents (read-documents handoff-repository directory))
     (arc arc-name
          start*
          checks
          state-texts
          workflow-text
          missing-references
          documents
          entries
          invalid-log-entries)]
    [else (reverse errors)]))

(: refuse-arc-errors (-> String (Listof String) Nothing))
(define (refuse-arc-errors arc-name messages)
  (raise-handoff-error "~a has ~a; fix them first:\n~a"
                       arc-name
                       (count-errors messages)
                       (string-join (map (λ ([message : String])
                                           (string-append "  " message))
                                         messages)
                                    "\n")))

(: read-log (-> Path String (Pairof (Listof log-entry) (Listof String))))
(define (read-log handoff-repository arc-name)
  (define directory (build-path handoff-repository arc-name log-directory))
  (define committed
    (let ([paths (git* handoff-repository
                       "ls-tree" "-z" "--name-only" "HEAD" "--"
                       (string-append arc-name "/" log-directory "/"))])
      (if paths
          (for/list : (Listof String)
                    ([path (in-list (string-split paths "\u0000"))])
            (define-values (_base entry _directory?) (split-path path))
            (path->string (assert entry path?)))
          null)))
  (: read-entry (-> String (U log-entry #f)))
  (define (read-entry entry)
    (define log-entry-number (parse-log-entry-name entry))
    (define path (build-path directory entry))
    (define text-path (build-path path entry-file))
    (define results-path (build-path path results-file))
    (and log-entry-number
         (directory-exists? path)
         (log-entry log-entry-number
                    (and (member entry committed) #t)
                    (and (file-exists? text-path) (file->string text-path))
                    (and (file-exists? results-path)
                         (with-handlers ([exn:fail:handoff? exn-message])
                           (parse-check-run (file->string results-path)
                                            (build-path arc-name
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
                          (build-path arc-name log-directory))]
    [else (cons null null)]))

(: read-documents (-> Path Path (Listof (Pairof scope-path (U String #f)))))
(define (read-documents handoff-repository arc-directory)
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
              ([entry (in-list (directory-list handoff-repository))]
               [text (in-value
                      (markdown (build-path handoff-repository entry)))]
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

(: next-log-entry-number (-> (Listof log-entry) Positive-Integer))
(define (next-log-entry-number entries)
  (match (find-pending-log-entry entries)
    [(? log-entry? pending) (log-entry-number pending)]
    [#f (if (empty? entries) 1 (add1 (log-entry-number (last entries))))]))

;; The baseline is the highest-numbered log entry committed to the handoff
;; repository. This means a pending entry is never the baseline.
(: find-baseline (-> String (Listof log-entry)
                     (U (Pairof Positive-Integer check-run) #f)))
(define (find-baseline arc-name entries)
  (define entry (find-last-committed-log-entry entries))
  (define run (and entry (log-entry-run entry)))
  (cond
    [(not entry) #f]
    [(check-run? run) (cons (log-entry-number entry) run)]
    [(string? run) (raise-handoff-error "~a" run)]
    [else (raise-handoff-error "~a: expected a regular file"
                               (build-path arc-name
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
  (start (expect-nonempty-string table 'next source-string)
         (expect-scope-path table 'workflow source-string)
         (expect-tables table
                        'read-first
                        source-string
                        (λ ([read : Table] [context : String])
                          (expect-fields read (list 'path 'reason) null context)
                          (read-first
                           (expect-scope-path read 'path context)
                           (expect-nonempty-string read 'reason context))))))

(: expect-scope-path (-> Table Symbol String scope-path))
(define (expect-scope-path table key context)
  (define text (expect-nonempty-string table key context))
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
(define (resolve-scope-path handoff-repository arc project-repository path)
  (build-path (case (scope-path-scope path)
                [(arc) (build-path handoff-repository arc)]
                [(handoff) handoff-repository]
                [(project) project-repository])
              (scope-path-path path)))

(: parse-log-entry-name (-> String (U Positive-Integer #f)))
(define (parse-log-entry-name name)
  (define number (and (regexp-match? #px"^[1-9][0-9]*$" name)
                      (string->number name)))
  (and (exact-positive-integer? number) number))

;;; Lint

(: lint (-> arc (Listof String)))
(define (lint arc)
  (define pending (find-pending-log-entry (arc-log arc)))
  (append
   (if (equal? (start-next (arc-start arc)) placeholder)
       (list (format "~a: next is the placeholder from handoff new; write the arc's next action"
                     (build-path (arc-name arc) start-file)))
       null)
   (for/list : (Listof String) ([path (in-list (arc-missing-references arc))])
     (format "~a: ~a is not a regular file"
             (build-path (arc-name arc) start-file)
             (scope-path->string path)))
   (for/list : (Listof String)
             ([check (in-list (arc-checks arc))]
              #:when (equal? (check-id check) placeholder))
     (format "~a: check ~a is the placeholder from handoff new; replace it with the arc's checks"
             (build-path (arc-name arc) checks-file)
             placeholder))
   (for*/list : (Listof String)
              ([document (in-list (arc-documents arc))]
               [issue (in-value (let ([text (cdr document)])
                                  (and text
                                       (not (verbatim-copy? text))
                                       (summary-issue text))))]
               #:when issue)
     (define path (car document))
     (format "~a: ~a"
             (if (eq? (scope-path-scope path) 'arc)
                 (build-path (arc-name arc) (scope-path-path path))
                 (scope-path-path path))
             issue))
   (append*
    (for/list : (Listof (Listof String)) ([entry (in-list (arc-log arc))])
      (define path (build-path (arc-name arc)
                               log-directory
                               (number->string (log-entry-number entry))))
      (define pending? (eq? entry pending))
      (define text (log-entry-text entry))
      (define run (log-entry-run entry))
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
         [(string? run) (list run)]
         [(or run pending?) null]
         [else (list (format "~a: expected a regular file"
                             (build-path path results-file)))]))))
   (for/list : (Listof String) ([stray (in-list (arc-invalid-log-entries arc))])
     (format (if (parse-log-entry-name stray)
                 "~a: expected a directory"
                 "~a: expected a log entry number, a positive integer without leading zeros")
             (build-path (arc-name arc) log-directory stray)))
   (budget-diagnostics arc)))

(: budget-diagnostics (-> arc (Listof String)))
(define (budget-diagnostics arc)
  (define sections
    (inlined-sections arc
                      (or (find-pending-log-entry (arc-log arc))
                          (find-last-committed-log-entry (arc-log arc)))))
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
                    (arc-name arc)
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

(struct project-status ([path : Path]
                        [branch : (U String #f)]
                        [head : (U String #f)]
                        [history : (U 'unresolvable history)])
  #:transparent)

(struct history ([only-recorded : (Listof String)]
                 [only-current : (Listof String)])
  #:transparent)

(: read-project-status (-> Path (U check-run #f) project-status))
(define (read-project-status project-repository baseline)
  (define-values (branch head) (read-project-head project-repository))
  (define recorded (and baseline (check-run-head baseline)))
  (: commits (-> (U String #f) (U String #f) (Listof String)))
  (define (commits from to)
    (if to
        (string-split (git project-repository
                           "log"
                           "--abbrev=12"
                           "--format=%h %s"
                           (if from (string-append from ".." to) to))
                      "\n")
        null))
  (project-status project-repository
                  branch
                  head
                  (cond
                    [(not baseline) (history null null)]
                    [(and recorded
                          (not (git* project-repository
                                     "cat-file"
                                     "-e"
                                     (string-append recorded "^{commit}"))))
                     'unresolvable]
                    [else (history (commits head recorded)
                                   (commits recorded head))])))

(: read-project-head (-> Path (Values (U String #f) (U String #f))))
(define (read-project-head project-repository)
  (values (git* project-repository "symbolic-ref" "--quiet" "--short" "HEAD")
          (git* project-repository "rev-parse" "--verify" "--quiet" "HEAD")))

(: read-dirty (-> Path (Listof String)))
(define (read-dirty handoff-repository)
  (string-split
   (git handoff-repository "--no-optional-locks" "status" "--porcelain")
   "\n"))

(: inlined-sections (-> arc (U log-entry #f) (Listof (Pairof String String))))
(define (inlined-sections arc entry)
  (: text (-> String String))
  (define (text file)
    (hash-ref (arc-state-texts arc) file))
  (define-values (tasks count) (omit-finished-tasks (text "tasks.md")))
  (filter-map
   (λ ([section : (Pairof String (U String #f))])
     (match section
       [(cons header (? string? text*)) (cons header text*)]
       [_ #f]))
   (list (cons "arc:start.toml" (text start-file))
         (cons "arc:arc.md" (text "arc.md"))
         (cons (scope-path->string (start-workflow (arc-start arc)))
               (arc-workflow-text arc))
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

(struct start-report ([handoff-repository : Path]
                      [sections : (Listof (Pairof String String))]
                      [other-documents : (Listof (Pairof scope-path String))]
                      [project-status : project-status]
                      [baseline : (U (Pairof Positive-Integer check-run) #f)]
                      [diagnostics : (Listof String)]
                      [pending : (U log-entry #f)]
                      [dirty : (Listof String)]
                      [checks : Report-Checks])
  #:transparent)

(define-type Report-Checks (U checks-compared checks-uncompared))
(struct checks-compared ([log-entry-number : Positive-Integer]
                         [host : String]
                         [started : Moment]
                         [comparisons : (Listof Check-Comparison)])
  #:transparent)
(struct checks-uncompared ([results : (Listof check-result)]) #:transparent)

(: build-start-report (-> Path
                          arc
                          (U (Pairof Positive-Integer check-run) #f)
                          (Listof check-result)
                          project-status
                          (Listof String)
                          start-report))
(define (build-start-report handoff-repository
                            arc
                            baseline
                            check-results
                            project-status
                            dirty)
  (start-report
   handoff-repository
   (inlined-sections arc (find-last-committed-log-entry (arc-log arc)))
   (other-documents arc)
   project-status
   baseline
   (lint arc)
   (find-pending-log-entry (arc-log arc))
   dirty
   (compare-with-baseline baseline check-results)))

(: compare-with-baseline (-> (U (Pairof Positive-Integer check-run) #f)
                             (Listof check-result)
                             Report-Checks))
(define (compare-with-baseline baseline check-results)
  (match baseline
    [(cons log-entry-number run)
     (checks-compared log-entry-number
                      (check-run-host run)
                      (check-run-started run)
                      (compare-check-results (check-run-results run)
                                             check-results))]
    [#f (checks-uncompared check-results)]))

(: other-documents (-> arc (Listof (Pairof scope-path String))))
(define (other-documents arc)
  (define workflow (start-workflow (arc-start arc)))
  (for/list : (Listof (Pairof scope-path String))
            ([document (in-list (arc-documents arc))]
             #:unless (equal? (car document) workflow)
             #:unless (let ([text (cdr document)])
                        (and text (verbatim-copy? text))))
    (define text (cdr document))
    (cons (car document) (if text (one-line-summary text) ""))))

(: print-start (-> start-report Void))
(define (print-start report)
  (printf "project: ~a\n"
          (project-status-path (start-report-project-status report)))
  (printf "handoff repository: ~a\n" (start-report-handoff-repository report))
  (print-sections (start-report-sections report))
  (print-header "other documents")
  (print-other-documents (start-report-other-documents report))
  (print-header "project repository")
  (print-project-repository (start-report-project-status report)
                            (start-report-baseline report))
  (print-header "handoff repository")
  (print-handoff-repository (start-report-diagnostics report)
                            (start-report-pending report)
                            (start-report-dirty report))
  (print-header "checks")
  (print-checks (start-report-checks report)))

(: print-checks (-> Report-Checks Void))
(define (print-checks checks)
  (match checks
    [(checks-compared log-entry-number host started comparisons)
     (print-comparisons comparisons log-entry-number host started)]
    [(checks-uncompared check-results)
     (printf "no committed log entry to compare against\n\n")
     (print-results check-results)]))

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

(: print-other-documents (-> (Listof (Pairof scope-path String)) Void))
(define (print-other-documents documents)
  (when (empty? documents)
    (displayln "nothing"))
  (for ([document (in-list documents)])
    (define summary (cdr document))
    (printf "- ~a~a\n"
            (scope-path->string (car document))
            (if (string=? summary "") "" (string-append ": " summary)))))

(: print-project-repository (-> project-status
                                (U (Pairof Positive-Integer check-run) #f)
                                Void))
(define (print-project-repository project-status baseline)
  (printf "now: ~a\n" (describe-head (project-status-branch project-status)
                                     (project-status-head project-status)))
  (when baseline
    (match-define (cons log-entry-number run) baseline)
    (: commits (-> String (Listof String) Void))
    (define (commits heading lines)
      (displayln heading)
      (for ([line (in-list (take lines (min commit-limit (length lines))))])
        (printf "  ~a\n" line))
      (when (> (length lines) commit-limit)
        (printf "  and ~a more\n" (- (length lines) commit-limit))))
    (printf "at log/~a: ~a\n"
            log-entry-number
            (describe-head (check-run-branch run) (check-run-head run)))
    (unless (equal? (check-run-branch run)
                    (project-status-branch project-status))
      (printf "the branch differs from log/~a\n" log-entry-number))
    (match (project-status-history project-status)
      ['unresolvable
       (displayln "recorded head does not resolve here: its history was rewritten and pruned, or it was committed on another machine")]
      [(history '() '()) (displayln "no new commits")]
      [(history '() only-current)
       (commits (format "commits since log/~a:" log-entry-number) only-current)]
      [(history only-recorded only-current)
       (commits (format "history rewritten since log/~a; < marks commits only in the recorded history, > only in the current one:"
                        log-entry-number)
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

(: print-handoff-repository (-> (Listof String) (U log-entry #f) (Listof String)
                                Void))
(define (print-handoff-repository diagnostics pending dirty)
  (printf "lint: ~a\n" (count-errors diagnostics))
  (for ([diagnostic (in-list diagnostics)])
    (printf "  ~a\n" diagnostic))
  (cond
    [pending
     (define run (log-entry-run pending))
     (printf "pending log entry: log/~a was begun but not committed, so it is not the baseline\n"
             (log-entry-number pending))
     (printf "  results: ~a\n"
             (cond
               [(check-run? run)
                (format "recorded on ~a at ~a"
                        (check-run-host run)
                        (moment->iso8601 (check-run-started run)))]
               [run "does not parse; see lint"]
               [else "none"]))
     (printf "  entry.md: ~a\n"
             (if (log-entry-text pending) "written" "not written"))]
    [else (displayln "pending log entry: none")])
  (print-dirty dirty))

(: print-dirty (-> (Listof String) Void))
(define (print-dirty dirty)
  (cond
    [(empty? dirty) (displayln "uncommitted: none")]
    [else
     (displayln "uncommitted:")
     (for ([line (in-list dirty)])
       (printf "  ~a\n" line))]))

;;; Updates

(: read-arc-and-lint (-> Path Path String arc))
(define (read-arc-and-lint handoff-repository project-repository arc-name)
  (match (read-arc handoff-repository project-repository arc-name)
    [(? arc? arc)
     (define diagnostics (lint arc))
     (if (empty? diagnostics) arc (refuse-arc-errors arc-name diagnostics))]
    [(? list? errors) (refuse-arc-errors arc-name errors)]))

(: commit-subject-or-refuse (-> arc (U String #f) (U String #f) String))
(define (commit-subject-or-refuse arc branch head)
  (define pending
    (or (find-pending-log-entry (arc-log arc))
        (raise-handoff-error "~a has no pending log entry; run handoff update --begin first"
                             (arc-name arc))))
  (define path (build-path (arc-name arc)
                           log-directory
                           (number->string (log-entry-number pending))))
  (define text (log-entry-text pending))
  (define run (log-entry-run pending))
  (define headline (and text (entry-headline text)))
  (cond
    [(not text)
     (raise-handoff-error "~a: write the entry first" (build-path path entry-file))]
    [(not headline)
     (raise-handoff-error "~a: expected a heading on its first nonblank line"
                          (build-path path entry-file))]
    [(not (check-run? run))
     (raise-handoff-error "~a: expected a regular file; run handoff update --begin"
                          (build-path path results-file))]
    [(not (equal? (arc-checks arc)
                  (map check-result-check (check-run-results run))))
     (raise-handoff-error "~a: the checks changed since the results were recorded; run handoff update --begin again"
                          (build-path (arc-name arc) checks-file))]
    [(not (and (equal? branch (check-run-branch run))
               (equal? head (check-run-head run))))
     (raise-handoff-error "the project moved from ~a to ~a since ~a was recorded; run handoff update --begin again"
                          (describe-head (check-run-branch run) (check-run-head run))
                          (describe-head branch head)
                          (build-path path results-file))]
    [else (format "~a: ~a" (arc-name arc) headline)]))

;;; Reusable workflows

(define guide-file "HANDOFF.md")

(struct reusable-workflow ([name : String] [path : Path]) #:transparent)

(define program-name "handoff")

(: handoff-data-directories (-> (Listof Path)))
(define (handoff-data-directories)
  (list-data-dirs #:program program-name #:only-existing? #f))

(: workflow-directories (-> (Listof Path)))
(define (workflow-directories)
  (for/list ([directory (in-list (handoff-data-directories))])
    (build-path directory "workflows")))

(: read-reusable-workflows (-> (Listof Path) (Listof reusable-workflow)))
(define (read-reusable-workflows directories)
  (define-values (workflows _seen)
    (for*/fold ([workflows : (Listof reusable-workflow) null]
                [seen : (Listof Path) null])
               ([directory (in-list directories)]
                #:when (directory-exists? directory)
                [file (in-list (directory-list directory))])
      (define path (build-path directory file))
      (define name (path->string (path-replace-extension file #"")))
      (cond
        [(and (path-has-extension? file #".md")
              (workflow-name? name)
              (file-exists? path)
              (not (member (normalize-path path) seen)))
         (values (cons (reusable-workflow name path) workflows)
                 (cons (normalize-path path) seen))]
        [else (values workflows seen)])))
  (reverse workflows))

(: find-reusable-workflow (-> String reusable-workflow))
(define (find-reusable-workflow name)
  (or (findf (λ ([workflow : reusable-workflow])
               (equal? (reusable-workflow-name workflow) name))
             (read-reusable-workflows (workflow-directories)))
      (raise-handoff-error "no reusable workflow named ~a; see handoff workflow list"
                           name)))

(: workflow-name? (-> String Boolean))
(define (workflow-name? name)
  (regexp-match? #px"^[a-z0-9][a-z0-9-]*$" name))

(: workflow-fork-name (-> (U reusable-workflow scope-path) String))
(define (workflow-fork-name source)
  (match source
    [(reusable-workflow name _) name]
    [(scope-path _ path)
     (define file (file-name-from-path path))
     (if file
         (regexp-replace #px"^workflow-"
                         (regexp-replace #px"\\.md$" (path->string file) "")
                         "")
         "")]))

(: workflow-copy-name (-> String String))
(define (workflow-copy-name name)
  (string-append "workflow-" name ".md"))

(define-type Workflow-Install-Outcome (U 'installed 'unchanged 'changed 'not-verbatim))

(: install-workflow (-> Path reusable-workflow Workflow-Install-Outcome))
(define (install-workflow handoff-repository workflow)
  (define name (reusable-workflow-name workflow))
  (define path (build-path handoff-repository (workflow-copy-name name)))
  (define contents (file->bytes (reusable-workflow-path workflow)))
  (cond
    [(file-exists? path) (workflow-install-status (file->string path) contents)]
    [(or (directory-exists? path) (link-exists? path))
     (raise-handoff-error "handoff:~a exists and is not a regular file"
                          (workflow-copy-name name))]
    [else
     (call-with-output-file path
       (λ ([out : Output-Port])
         (write-string (format-workflow-provenance-line 'copied
                                                        (format "workflow ~a" name)
                                                        contents
                                                        (posix->moment (current-seconds) 0))
                       out)
         (write-bytes contents out)
         (void))
       #:exists 'error)
     'installed]))

(: workflow-install-status (-> String Bytes (U 'unchanged 'changed 'not-verbatim)))
(define (workflow-install-status copy contents)
  (define-values (provenance _text) (split-workflow-provenance copy))
  (cond
    [(not (and provenance (eq? (workflow-provenance-how provenance) 'copied)))
     'not-verbatim]
    [(equal? (workflow-provenance-hash provenance)
             (bytes->hex-string (sha256-bytes contents)))
     'unchanged]
    [else 'changed]))

(: print-workflow-install (-> reusable-workflow Workflow-Install-Outcome Void))
(define (print-workflow-install workflow outcome)
  (define reference
    (string-append "handoff:"
                   (workflow-copy-name (reusable-workflow-name workflow))))
  (case outcome
    [(installed)
     (printf "installed: ~a\n" reference)
     (printf "from: ~a\n" (reusable-workflow-path workflow))]
    [else
     (printf "exists: ~a, left unchanged\n" reference)
     (displayln
      (case outcome
        [(unchanged) "the reusable workflow is unchanged since it was copied"]
        [(changed) "the reusable workflow has changed since it was copied"]
        [else "it is not a verbatim copy"]))]))

;;; Workflow provenance lines

(struct workflow-provenance ([how : (U 'copied 'forked 'created)]
                             [hash : (U String #f)]) ; #f for created
  #:transparent)

(: format-workflow-provenance-line (-> (U 'copied 'forked) String Bytes Moment String))
(define (format-workflow-provenance-line how source contents now)
  (format "<!-- ~a from ~a (sha256:~a) on ~a~a -->\n"
          how
          source
          (bytes->hex-string (sha256-bytes contents))
          (~t (adjust-timezone now 0) "yyyy-MM-dd")
          (if (eq? how 'copied) "; verbatim" "")))

(: split-workflow-provenance (-> String (Values (U workflow-provenance #f) String)))
(define (split-workflow-provenance text)
  (match (regexp-match #px"^<!-- (copied|forked|created) ([^\n]*)-->(?:\n|$)" text)
    [(list line (? string? how) (? string? details))
     (define hash (regexp-match #px"\\(sha256:([0-9a-f]{64})\\)" details))
     (values (workflow-provenance (case how
                                    [("copied") 'copied]
                                    [("forked") 'forked]
                                    [else 'created])
                                  (and hash (second hash)))
             (substring text (string-length line)))]
    [_ (values #f text)]))

;;; New arcs

(define placeholder "replace-me")

(define placeholder-workflow
  (scope-path 'arc (string->path (workflow-copy-name placeholder))))

(define placeholder-checks
  (string-append
   "# Each check is a claim about the project and a command that acts as a\n"
   "# mechanical decision procedure for the claim. Checks run in parallel and\n"
   "# are reported in file order.\n"
   "# The `timeout` field is measured in seconds; it is optional and defaults to\n"
   "# " (toml-number default-timeout) ".\n"
   "[[check]]\n"
   "id = " (toml-string placeholder) "\n"
   "claim = " (toml-string "A natural language claim that is either true or false") "\n"
   "command = " (toml-string
                 (string-append
                  "echo \"Replace this placeholder from handoff new with the arc's checks. "
                  "command is a shell command, run from the project root, that exits 0 when "
                  "the claim is true, 1 when it is false, and anything else when unknown.\"; exit 3"))
   "\n"
   "timeout = " (toml-number default-timeout) "\n"))

(: new-arc-files (-> String scope-path (Listof (Pairof String String))))
(define (new-arc-files arc-name workflow)
  (list (cons start-file
              (format "next = ~a\nworkflow = ~a\nread-first = []\n"
                      (toml-string placeholder)
                      (toml-string (scope-path->string workflow))))
        (cons checks-file placeholder-checks)
        (cons "arc.md" (format "# ~a\n" arc-name))
        (cons "tasks.md" "# Tasks\n")
        (cons "do-not-reopen.md" "# Do not reopen\n")
        (cons "open-questions.md" "# Open questions\n")
        (cons "review.md" "# Review\n")))

;;; Markdown

;; The lines of the first paragraph after a document's provenance line and
;; opening headings.
(: first-paragraph (-> String (Listof String)))
(define (first-paragraph text)
  (define (skip? [line : String])
    (or (string=? (string-trim line) "") (regexp-match? #px"^ {0,3}#" line)))
  (define lines (string-split (strip-workflow-provenance text) "\n" #:trim? #f))
  (takef (dropf lines skip?) (negate skip?)))

(: one-line-summary (-> String String))
(define (one-line-summary text)
  (string-join (map string-trim (first-paragraph text))))

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
  (define-values (provenance _text) (split-workflow-provenance text))
  (and provenance (eq? (workflow-provenance-how provenance) 'copied)))

(: strip-workflow-provenance (-> String String))
(define (strip-workflow-provenance text)
  (define-values (_provenance rest) (split-workflow-provenance text))
  rest)

;; The text of the heading on an entry's first nonblank line.
(: entry-headline (-> String (U String #f)))
(define (entry-headline text)
  (define line
    (findf (compose1 non-empty-string? string-trim) (string-split text "\n")))
  (define heading (and line
                       (regexp-match #px"^ {0,3}#{1,6}[ \t]+(.*\\S)" line)))
  (and heading (second heading)))

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


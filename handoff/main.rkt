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

(module signal racket/base
  (require ffi/unsafe)
  (provide kill-group)
  (define kill (get-ffi-obj "kill" #f (_fun _int _int -> _int)))
  (define (kill-group pid)
    (kill (- pid) 9)
    (void)))

(require/typed (submod "." signal)
  [kill-group (-> Integer Void)])

(provide (struct-out check)
         (struct-out result)
         parse-checks
         result-outcome
         print-results)

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
(struct check-subcommand ([arc : (U String #f)]) #:transparent)
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
      "<workflow> is a name from handoff workflow list"
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
          [else (raise-handoff-error "unknown command workflow ~a; expected list, install, or fork" verb)])))]
    [("guide") (command-line #:program program #:argv arguments #:args () (guide-subcommand))]
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

(: handle-check (-> Path String Status))
(define (handle-check repository arc)
  (define checks (read-checks (build-path repository arc checks-file)))
  (define results (run-checks (find-project-repository repository) checks))
  (print-results results)
  (if (andmap (λ ([result : result]) (eq? (result-outcome result) 'true))
              results)
      'success
      'failure))

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

(: arc? (-> Path Boolean))
(define (arc? path)
  (and (directory-exists? path) (file-exists? (build-path path start-file))))

(: list-arcs (-> Path (Listof String)))
(define (list-arcs repository)
  ;; Note: `directory-list` sorts by `path<?`.
  (for/list : (Listof String) ([entry (in-list (directory-list repository))]
                               #:when (arc? (build-path repository entry)))
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
(define default-timeout : Positive-Real 60)
(define capture-limit : Natural (* 1024 1024)) ;; 1 MiB
(define output-lines 20)

;; We store `timeout` in milliseconds, which is the unit of the monotonic
;; clock. However, the checks.toml file specifies it in seconds.
(struct check ([id : String]
               [claim : String]
               [command : String]
               [timeout : Positive-Real])
  #:transparent)

;; `exit` is #f when the check timed out, `duration` is in milliseconds, and
;; `truncated` counts the bytes dropped from the start of `output` to keep it
;; under `capture-limit`.
(struct result ([check : check]
                [exit : (U Integer #f)]
                [duration : Nonnegative-Real]
                [output : String]
                [truncated : Natural])
  #:transparent)

(define-type Outcome (U 'true 'false 'unknown 'timeout))

(: exit-outcome (-> (U Integer #f) Outcome))
(define (exit-outcome code)
  (match code
    [#f 'timeout]
    [0 'true]
    [1 'false]
    [_ 'unknown]))

(: result-outcome (-> result Outcome))
(define result-outcome (compose exit-outcome result-exit))

(: read-checks (-> Path (Listof check)))
(define (read-checks path)
  (unless (file-exists? path)
    (raise-handoff-error "~a does not exist" path))
  (parse-checks (file->string path) path))

(: parse-checks (-> String Path (Listof check)))
(define (parse-checks text source)
  (define source-string (path->string source))
  (define table
    (with-handlers ([exn:fail? (λ ([error : exn])
                                 (raise-handoff-error "~a: ~a"
                                                      source-string
                                                      (exn-message error)))])
      (parse-toml text)))
  (expect-fields table null (list 'check) source-string)
  (define entries (hash-ref table 'check (const null)))
  (unless (list? entries)
    (raise-handoff-error "~a: check must be an array of tables" source-string))
  (define checks
    (for/list : (Listof check) ([entry (in-list entries)]
                                [index (in-naturals)])
      (parse-check entry (format "~a: check[~a]" source-string index))))
  (define duplicate ((inst check-duplicates check String) checks #:key check-id))
  (when duplicate
    (raise-handoff-error "~a: check ~a appears more than once"
                         source-string
                         (check-id duplicate)))
  checks)

(: parse-check (-> Any String check))
(define (parse-check entry context)
  (unless (table? entry)
    (raise-handoff-error "~a must be a table" context))
  (expect-fields entry (list 'id 'claim 'command) (list 'timeout) context)
  (define timeout (hash-ref entry 'timeout (const default-timeout)))
  (define milliseconds (and (real? timeout) (* 1000 timeout)))
  (check (expect-string entry 'id context)
         (expect-string entry 'claim context)
         (expect-string entry 'command context)
         (if (and milliseconds (positive? milliseconds) (not (infinite? milliseconds)))
             milliseconds
             (raise-handoff-error "~a: timeout must be a positive number of seconds"
                                  context))))

(: run-checks (-> Path (Listof check) (Listof result)))
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
       (: start-check (-> check Path (Evtof result)))
       (define (start-check check output)
         (define started (current-inexact-monotonic-milliseconds))
         (define process
           (call-with-output-file output
             (λ ([out : Output-Port])
               (define-values (process _stdout stdin _stderr)
                 (subprocess out #f out bash "-c" (check-command check)))
               (close-output-port (assert stdin))
               process)))
         (define deadline (+ started (check-timeout check)))
         (handle-evt
          (choice-evt process (monotonic-alarm-evt deadline #t))
          ;; `thunk` and `thunk*` cause type checking problems here
          (λ (_) (finish-check check process output started))))
       ;; A check is finished when its shell exits or its timeout deadline
       ;; lapses. Either way we kill its process group, which also stops
       ;; anything that the shell left running in the background.
       (: finish-check (-> check Subprocess Path Flonum result))
       (define (finish-check check process output started)
         (define duration
           (- (current-inexact-monotonic-milliseconds) started))
         (define status (subprocess-status process))
         (kill-group (subprocess-pid process))
         (subprocess-wait process)
         (define truncated
           (max 0 (- (file-size output) capture-limit)))
         (result check
                 (if (eq? status 'running) #f status)
                 (max 0.0 duration)
                 (call-with-input-file output
                   (λ ([in : Input-Port])
                     (file-position in truncated)
                     (port->string in)))
                 truncated))
       (define events
         (for/hasheq : (Immutable-HashTable check (Evtof result))
                     ([check (in-list checks)]
                      [index (in-naturals)])
           (values check
                   (start-check check
                                (build-path directory (number->string index))))))
       (let loop : (Listof result)
            ([pending : (Immutable-HashTable check (Evtof result)) events]
             [finished : (Immutable-HashTable check result) (hasheq)])
         (cond
           [(hash-empty? pending)
            (for/list : (Listof result) ([check (in-list checks)])
              (hash-ref finished check))]
           [else
            (define result (apply sync (hash-values pending)))
            (loop (hash-remove pending (result-check result))
                  (hash-set finished (result-check result) result))]))))
   (thunk
     (custodian-shutdown-all custodian)
     (delete-directory/files directory))))

(: print-results (-> (Listof result) Void))
(define (print-results results)
  (for ([result (in-list results)])
    (print-result result)
    (newline))
  (define counts
    (for*/list : (Listof String)
               ([outcome (in-list '(true false unknown timeout))]
                [count (in-value (count (λ ([result : result])
                                          (eq? (result-outcome result) outcome))
                                        results))]
                #:unless (zero? count))
      (format "~a ~a" count outcome)))
  (printf "summary: ~a\n"
          (if (empty? counts)
              "no checks"
              (string-join counts ", "))))

(: print-result (-> result Void))
(define (print-result result)
  (define check (result-check result))
  (define outcome (result-outcome result))
  (printf "id: ~a\n" (check-id check))
  (printf "claim: ~a\n" (check-claim check))
  (printf "outcome: ~a\n" outcome)
  (case outcome
    [(true) (void)]
    [(timeout) (printf "timeout: ~as\n"
                       (~r (/ (check-timeout check) 1000) #:precision '(= 1)))]
    [else (printf "exit: ~a\n" (result-exit result))])
  (printf "duration: ~as\n"
          (~r (/ (result-duration result) 1000) #:precision '(= 1)))
  (unless (eq? outcome 'true)
    (unless (zero? (result-truncated result))
      (printf "truncated: ~a bytes before the output were not kept\n"
              (result-truncated result)))
    (define lines (string-split (result-output result) "\n"))
    (unless (empty? lines)
      (define tail (take-right lines (min output-lines (length lines))))
      (printf "output~a:\n"
              (if (< (length tail) (length lines))
                  (format " (last ~a of ~a lines)" (length tail) (length lines))
                  ""))
      (for ([line (in-list tail)])
        (printf "  ~a\n" line)))))

;;; TOML

(define-type Table (Immutable-HashTable Symbol Any))
(define-predicate table? Table)

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

(: expect-string (-> Table Symbol String String))
(define (expect-string table key context)
  (define value (hash-ref table key #f))
  (if (and (string? value) (non-empty-string? (string-trim value)))
      value
      (raise-handoff-error "~a: ~a must be a nonempty string" context key)))

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


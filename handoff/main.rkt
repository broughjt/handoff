#lang typed/racket/base
;; Automation for the handoff skills.

(require racket/cmdline
         racket/file
         racket/format
         racket/function
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         typed/racket/os
         "error.rkt"
         "model.rkt"
         "toml.rkt")
(require/typed racket/base
  ;; Typed Racket has no type for this.
  [current-inexact-monotonic-milliseconds (-> Flonum)]
  ;; Typed Racket's type for subprocess has no 'stdout for stderr.
  [(subprocess subprocess/merged)
   (-> False False 'stdout Path-String String *
       (Values Subprocess Input-Port Output-Port False))])

(provide main
         Exit-Code
         (struct-out diagnostic)
         lint-arc
         capture-limit
         run-check
         git
         git*
         git-repository?
         project-branch-head)

(define output-lines 20)

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
     (execute (parse-arguments argv)))))

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
  (U list-subcommand start-subcommand check-subcommand lint-subcommand update-subcommand))
(struct list-subcommand ([archived? : Boolean]) #:transparent)
(struct start-subcommand ([arc : (U String #f)]) #:transparent)
(struct check-subcommand ([arc : (U String #f)]) #:transparent)
(struct lint-subcommand ([arc : (U String #f)]) #:transparent)
(struct update-subcommand ([step : (U 'begin 'commit)] [arc : (U String #f)]) #:transparent)

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
   "  list   list handoff arcs"
   "  start  get the brief for starting work on a handoff arc"
   "  check  run the commands in checks.toml and report their results"
   "  lint   validate expected structure of a handoff arc"
   "  update record a log entry: --begin runs the checks and records them, --commit commits"
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
    [else (raise-handoff-error "unknown command ~a; expected list, start, check, lint, or update" subcommand)]))

(: execute (-> invocation Status))
(define (execute invocation)
  (define option (invocation-repository invocation))
  (when (and option (not (git-repository? option)))
    (raise-handoff-error "~a is not a git repository" option))
  (define repository (or option (find-handoff-repository (current-directory))))
  (define subcommand (invocation-subcommand invocation))
  (cond
    [(list-subcommand? subcommand)
     (handle-list repository (list-subcommand-archived? subcommand))]
    [(start-subcommand? subcommand)
     (handle-start repository (select-arc repository (start-subcommand-arc subcommand)))]
    [(check-subcommand? subcommand)
     (handle-check repository (select-arc repository (check-subcommand-arc subcommand)))]
    [(lint-subcommand? subcommand)
     (handle-lint repository (select-arc repository (lint-subcommand-arc subcommand)))]
    [(update-subcommand? subcommand)
     (define arc (select-arc repository (update-subcommand-arc subcommand)))
     (case (update-subcommand-step subcommand)
       [(begin) (handle-update-begin repository arc)]
       [(commit) (handle-update-commit repository arc)])]))

;;; Commands

(: handle-list (-> Path Boolean Status))
(define (handle-list repository archived?)
  (for ([name (in-list ((if archived? list-archived-arcs list-arcs) repository))])
    (displayln name))
  'success)

(: handle-check (-> Path String Status))
(define (handle-check repository arc)
  (define checks-path (build-path repository arc checks-file))
  (define checks
    (with-handlers ([exn:fail:handoff?
                     (λ ([error : exn])
                       (raise-handoff-error "~a: ~a" checks-path (exn-message error)))])
      (parse-checks (read-toml checks-path))))
  (define run (run-checks checks (project-branch-head (find-project-repository repository))))
  (report run)
  (if (andmap (λ ([result : check-result]) (eq? (check-result-outcome result) 'true))
              (check-run-results run))
      'success 'failure))

(: handle-lint (-> Path String Status))
(define (handle-lint repository arc)
  (define diagnostics (lint-arc (build-path repository arc) repository))
  (for ([diagnostic (in-list diagnostics)])
    (displayln (diagnostic->string diagnostic)))
  (printf "~a: ~a lint errors\n" arc (length diagnostics))
  (if (empty? diagnostics) 'success 'failure))

;; Brief an agent to start work on a handoff arc. First check for abnormalities
;; like a failing lint diagnostics or a pending log entry. Next, run the checks
;; output the diff between the last check run. Finally output start metadata and
;; state files inline.
(: handle-start (-> Path String Status))
(define (handle-start repository* arc)
  (define repository (normalize-path repository*))
  (define arc-path (build-path repository arc))
  (define project (find-project-repository repository))
  (define (show-path [path : Path]) (some-system-path->string (find-relative-path project path)))
  (define first-section? #t)
  (define (section [name : String])
    (unless first-section? (newline))
    (set! first-section? #f)
    (printf "==> ~a <==\n" name))
  (define (inline [path : Path])
    (when (file-exists? path)
      (section (show-path path))
      (define text (file->string path))
      (write-string text)
      (unless (or (string=? text "") (string-suffix? text "\n"))
        (newline))))

  (section "handoff start")
  (printf (string-append "arc: ~a\n"
                         "project: ~a\n"
                         "paths are relative to the project\n")
          arc project)

  (define diagnostics (lint-arc arc-path repository))
  (unless (empty? diagnostics)
    (section "lint")
    (for ([diagnostic (in-list diagnostics)])
      (printf "~a: ~a\n" (show-path (diagnostic-path diagnostic)) (diagnostic-message diagnostic))))

  (define pending (pending-log-entry arc-path))
  (when pending
    (section "pending log entry")
    (define entry-path (log-entry-path arc-path pending))
    (define run (with-handlers ([exn:fail:handoff? (const #f)])
                  (parse-check-run (read-toml (build-path entry-path results-file)))))
    (printf "log/~a was begun with handoff update --begin but not committed, so it is not the baseline\n"
            pending)
    (printf "results: ~a\n"
            (if run
                (format "recorded on ~a at ~a"
                        (check-run-host run) (timestamp->string (check-run-started run)))
                "none readable"))
    (printf "entry.md: ~a\n" (if (file-exists? (build-path entry-path entry-file)) "written" "not written")))

  (define baseline (find-baseline arc-path))
  (define previous (and baseline (baseline-results baseline)))

  (section "project")
  (report-project project baseline)

  (section "handoff repository")
  (define dirty (string-split (git repository "status" "--porcelain") "\n"))
  (cond
    [(empty? dirty) (printf "no uncommitted changes\n")]
    [else
     (printf "uncommitted, relative to ~a:\n" (show-path repository))
     (for ([line (in-list dirty)])
       (printf "  ~a\n" line))])

  (section "checks")
  (cond
    [(not baseline) (printf "no earlier log entry to compare against\n")]
    [previous (printf "compared against log/~a, recorded on ~a at ~a\n"
                     (baseline-number baseline) (check-run-host previous)
                     (timestamp->string (check-run-started previous)))]
    [else (printf "not compared: log/~a/~a: ~a\n"
                  (baseline-number baseline) results-file (baseline-run baseline))])
  (define checks
    (with-handlers ([exn:fail:handoff? exn-message])
      (parse-checks (read-toml (build-path arc-path checks-file)))))
  (cond
    [(string? checks) (printf "not run: ~a: ~a\n" checks-file checks)]
    [else
     (newline)
     (report (run-checks checks (project-branch-head project)) #:baseline baseline)])

  (define start*
    (with-handlers ([exn:fail:handoff? (λ (_) #f)])
      (parse-start (read-toml (build-path arc-path start-file)))))
  (when start*
    (define (resolve [scope-path : scope-path])
      (resolve-scope-path scope-path #:arc arc-path #:repository repository #:project project))
    (define (describe [path : Path])
      (if (file-exists? path) (show-path path) (format "~a (missing)" (show-path path))))
    (define workflow (resolve (start-workflow start*)))
    (section "start")
    (printf "Next:\n")
    (for ([line (in-list (string-split (start-next start*) "\n" #:trim? #f))])
      (printf "  ~a\n" line))
    (printf "Workflow: ~a\n" (describe workflow))
    (printf "Read first:\n")
    (when (empty? (start-read-first start*))
      (printf "  nothing\n"))
    (for ([read (in-list (start-read-first start*))])
      (printf "- ~a: ~a\n" (describe (resolve (read-first-path read))) (read-first-reason read)))
    (inline workflow))

  (for ([name (in-list state-files)])
    (inline (build-path arc-path name)))
  (when baseline
    (inline (build-path (log-entry-path arc-path (baseline-number baseline)) entry-file)))
  'success)

;; The most commits listed when reporting what changed in the project.
(define commit-limit 20)

;; Compare the project's branch and head with those recorded in the baseline.
(: report-project (-> Path (U baseline #f) Void))
(define (report-project project baseline)
  (define current (project-branch-head project))
  (define earlier (and baseline (baseline-results baseline)))
  (printf "now: ~a\n" (describe-branch-head current))
  (when (and baseline earlier)
    (define number (baseline-number baseline))
    (define recorded (check-run-project-head earlier))
    (printf "at log/~a: ~a\n" number (describe-branch-head recorded))
    (unless (equal? (branch-head-branch recorded) (branch-head-branch current))
      (printf "the branch differs\n"))
    (define old (branch-head-head recorded))
    (define new (branch-head-head current))
    (define (commits [heading : String] . [arguments : String *])
      (define lines
        (string-split (apply git project "log" "--abbrev=12" "--format=%m %h %s" arguments) "\n"))
      (printf "~a\n" heading)
      (for ([line (in-list (take lines (min commit-limit (length lines))))])
        (printf "  ~a\n" line))
      (when (> (length lines) commit-limit)
        (printf "  and ~a more\n" (- (length lines) commit-limit))))
    (cond
      [(equal? old new) (printf "no new commits\n")]
      [(not new) (printf "the project has no commits now\n")]
      [(not old) (commits "commits, all new since:" new)]
      [(not (git* project "cat-file" "-e" (string-append old "^{commit}")))
       (printf "the recorded head does not resolve here: its history was rewritten and pruned, or it was committed on another machine\n")]
      [(git* project "merge-base" "--is-ancestor" old new)
       (commits "commits since:" (format "~a..~a" old new))]
      [else
       (commits "the recorded head is no longer an ancestor of HEAD, so history was rewritten; < marks commits only in the recorded history, > only in the current one:"
                "--left-right" (format "~a...~a" old new))])))

;; Recording a log entry happens in two command-line steps: `update --begin`
;; records check results in a results.toml file for the pending log entry, the
;; agent writes entry.md, and then `update --commit` commits them.

;; Run the checks in checks.toml and record their results into the results.toml
;; file of the pending log entry, creating the next log entry when none is
;; pending. Rerunning replaces results.toml but leaves entry.md alone.
(: handle-update-begin (-> Path String Status))
(define (handle-update-begin repository arc)
  (define arc-path (build-path repository arc))
  (refuse-lint-errors repository arc)
  (define checks (parse-checks (read-toml (build-path arc-path checks-file))))
  (define run (run-checks checks (project-branch-head (find-project-repository repository))))

  (define-values (numbers _invalid) (list-log-entries arc-path))
  (define number (or (pending-log-entry arc-path) (add1 (apply max 0 numbers))))
  (define entry-path (log-entry-path arc-path number))
  (unless (directory-exists? entry-path)
    (make-directory* (build-path arc-path log-directory))
    ;; Fails rather than reusing an entry that appeared since we listed the log.
    (make-directory entry-path))
  (define results-path (build-path entry-path results-file))
  (define temporary (path-add-extension results-path #".tmp"))
  (display-to-file (check-run->toml run) temporary #:exists 'truncate/replace)
  (rename-file-or-directory temporary results-path #t)

  (report run)
  (printf "\nlog/~a results recorded. Write the entry to ~a, then run handoff update --commit ~a\n"
          number (build-path entry-path entry-file) arc)
  'success)

;; Commit the pending log entry with the arc's changes and anything already
;; staged in the handoff repository. Refuse if the results.toml no longer
;; matches the current state.
(: handle-update-commit (-> Path String Status))
(define (handle-update-commit repository arc)
  (define arc-path (build-path repository arc))
  (define number
    (or (pending-log-entry arc-path)
        (raise-handoff-error "~a has no update in progress; run handoff update --begin first" arc)))
  (define entry-path (log-entry-path arc-path number))
  (define entry (build-path entry-path entry-file))
  (unless (file-exists? entry)
    (raise-handoff-error "write the entry to ~a first" entry))
  (define subject (format "~a: ~a" arc (entry-headline (file->string entry))))
  (refuse-lint-errors repository arc)

  (define results-path (build-path entry-path results-file))
  (unless (file-exists? results-path)
    (raise-handoff-error "log/~a has no results; run handoff update --begin again" number))
  (define recorded (parse-check-run (read-toml results-path)))
  (define checks (parse-checks (read-toml (build-path arc-path checks-file))))
  (unless (equal? checks (map check-result-check (check-run-results recorded)))
    (raise-handoff-error "~a changed since log/~a's results were recorded; run handoff update --begin again"
                         checks-file number))
  (define recorded-head (check-run-project-head recorded))
  (define head (project-branch-head (find-project-repository repository)))
  (unless (equal? head recorded-head)
    (raise-handoff-error "the project moved from ~a to ~a since log/~a's results were recorded; run handoff update --begin again"
                         (describe-branch-head recorded-head) (describe-branch-head head) number))

  ;; Fails on an unmerged index, which could not be committed anyway.
  (define index (string-trim (git repository "write-tree")))
  (with-handlers ([exn:fail?
                   (λ ([error : exn])
                     (git* repository "read-tree" index)
                     (raise error))])
    (git repository "add" "--" arc)
    (git repository "commit" "--quiet" "--message" subject))

  (printf "log/~a committed as ~a ~a\n"
          number (substring (string-trim (git repository "rev-parse" "HEAD")) 0 7) subject)
  ;; Git's short status lines for what the commit left out.
  (define dirty (string-split (git repository "status" "--porcelain") "\n"))
  (unless (empty? dirty)
    (printf "not committed:\n")
    (for ([line (in-list dirty)])
      (printf "  ~a\n" line)))
  'success)

(: refuse-lint-errors (-> Path String Void))
(define (refuse-lint-errors repository arc)
  (define diagnostics (lint-arc (build-path repository arc) repository))
  (unless (empty? diagnostics)
    (raise-handoff-error "~a has ~a lint errors:\n~a"
                         arc (length diagnostics)
                         (string-join (map diagnostic->string diagnostics) "\n"))))

(: report (->* (check-run) (#:baseline (U baseline #f)) Void))
(define (report run #:baseline [baseline #f])
  (define results (check-run-results run))
  (define earlier (and baseline (baseline-results baseline)))
  (define-values (changes removed)
    (if earlier
        (compare-results results (check-run-results earlier))
        (values null null)))
  (define (seconds [duration : Real]) (string-append (~r duration #:precision '(= 1)) "s"))
  (define (width [heading : String] [cells : (Listof String)])
    (apply max (string-length heading) (map string-length cells)))
  (define id-width (width "ID" (map (λ ([r : check-result]) (check-id (check-result-check r))) results)))
  (define claim-width (width "CLAIM" (map (λ ([r : check-result]) (check-claim (check-result-check r))) results)))
  (define duration-width (width "DURATION" (map (λ ([r : check-result]) (seconds (check-result-duration r))) results)))
  (define (row [id : String] [claim : String] [outcome : String] [duration : String])
    (printf "~a  ~a  ~a  ~a\n"
            (~a id #:min-width id-width)
            (~a claim #:min-width claim-width)
            (~a outcome #:min-width 7)
            (~a duration #:min-width duration-width #:align 'right)))
  (define (note [template : String] . [arguments : Any *])
    (printf "         ~a\n" (apply format template arguments)))
  (define (print-output [label : String] [result : check-result])
    (unless (string=? (check-result-output result) "")
      (note "~a:" label)
      (unless (zero? (check-result-truncated result))
        (note "  [~a bytes before this were not kept]" (check-result-truncated result)))
      (define lines (string-split (check-result-output result) "\n"))
      (for ([line (in-list (drop lines (max 0 (- (length lines) output-lines))))])
        (note "  ~a" line))))
  (row "ID" "CLAIM" "RESULT" "DURATION")
  (for ([result (in-list results)])
    (define check (check-result-check result))
    (define outcome (check-result-outcome result))
    (row (check-id check) (check-claim check) (symbol->string outcome) (seconds (check-result-duration result)))
    (define change (findf (λ ([change : check-change]) (eq? (check-change-after change) result)) changes))
    (define before (and change (check-change-before change)))
    (define number (and baseline (baseline-number baseline)))
    (case (and change (check-change-change change))
      [(flipped) (note "was ~a at log/~a" (check-result-outcome (assert before)) number)]
      [(redefined) (note "definition changed since log/~a, where it was ~a"
                         number (check-result-outcome (assert before)))]
      [(new) (note "new since log/~a" number)]
      [else (void)])
    (unless (eq? outcome 'true)
      (if (eq? outcome 'timeout)
          (note "after ~as" (check-timeout check))
          (note "exit ~a" (check-result-exit result))))
    (define flipped? (and change (eq? (check-change-change change) 'flipped)))
    (when (or flipped? (not (eq? outcome 'true)))
      (print-output "output" result))
    (when (and flipped? before)
      (print-output (format "output at log/~a" number) before)))
  (for ([check (in-list removed)])
    (printf "removed since log/~a: ~a (~a)\n"
            (and baseline (baseline-number baseline)) (check-id check) (check-claim check)))
  (define counts
    (for*/list : (Listof String) ([outcome (in-list '(true false unknown timeout))]
                                  [count (in-value (count (λ ([r : check-result]) (eq? (check-result-outcome r) outcome)) results))]
                                  #:when (positive? count))
      (format "~a ~a" count outcome)))
  (printf "\nhost: ~a, duration: ~a, summary: ~a\n"
          (check-run-host run)
          (seconds (check-run-duration run))
          (if (empty? counts) "no checks" (string-join counts ", "))))

;;; Lint

(struct diagnostic ([path : Path] [message : String]) #:transparent)

(: diagnostic->string (-> diagnostic String))
(define (diagnostic->string diagnostic)
  (format "~a: ~a"
          (diagnostic-path diagnostic)
          (diagnostic-message diagnostic)))

(: lint-arc (-> Path Path (Listof diagnostic)))
(define (lint-arc arc repository)
  (define diagnostics : (Listof diagnostic) null)
  (define (report! [path : Path] [message : String])
    (set! diagnostics (cons (diagnostic path message) diagnostics)))
  (define ((report-error [path : Path]) [error : exn])
    (report! path (exn-message error)))

  ;; Each of the state files exists
  (for ([name (in-list state-files)])
    (define path (build-path arc name))
    (unless (file-exists? path)
      (report! path "expected a regular file")))

  ;; The start.toml file exists, parses in the expected format, and each of the
  ;; scoped path references point to files that exist
  (define start-path (build-path arc start-file))
  (define project (find-project-repository repository))
  (define start*
    (with-handlers ([exn:fail:handoff? (report-error start-path)])
      (parse-start (read-toml start-path))))
  (when (start? start*)
    (for ([scope-path (in-list (cons (start-workflow start*) (map read-first-path (start-read-first start*))))])
      (define path (resolve-scope-path scope-path #:arc arc #:repository repository #:project project))
      (unless (file-exists? path)
        (report! start-path (format "~a: expected a regular file at ~a" (scope-path->string scope-path) path)))))

  ;; The checks.toml exists and parses in the expected format.
  (define checks-path (build-path arc checks-file))
  (with-handlers ([exn:fail:handoff? (report-error checks-path)])
    (parse-checks (read-toml checks-path)))

  ;; - The log path either does not exist or is a directory
  ;; - Every entry is a directory named a positive integer with no leading
  ;;   zeros, such that:
  ;;   - It contains an entry.md file and a results.toml file, except that the
  ;;     pending entry (whose number is the output of `pending-log-entry`) may
  ;;     lack either.
  ;;   - Its results.toml, when present, is in the format written by
  ;;     `check-run->toml`.
  (define log (build-path arc log-directory))
  (cond
    [(not (or (file-exists? log) (directory-exists? log) (link-exists? log))) (void)]
    [(not (directory-exists? log)) (report! log "expected a directory")]
    [else
     (define-values (entries invalid) (list-log-entries arc))
     (define pending (pending-log-entry arc))
     (for ([number (in-list entries)])
       (define entry (log-entry-path arc number))
       (define pending? (eqv? number pending))
       (cond
         [(not (directory-exists? entry)) (report! entry "expected a directory")]
         [else
          (define entry-path (build-path entry entry-file))
          (unless (or pending? (file-exists? entry-path))
            (report! entry-path "expected a regular file"))
          (define results-path (build-path entry results-file))
          (unless (and pending? (not (file-exists? results-path)))
            (with-handlers ([exn:fail:handoff? (report-error results-path)])
              (parse-check-run (read-toml results-path))))]))
     (for ([name (in-list invalid)])
       (report! (build-path log name)
                "expected a log entry number, a positive integer without leading zeros"))])

  (reverse diagnostics))

;;; Running checks

;; The most output kept per check, in bytes. Past it, the start of the output
;; is dropped: a failure is usually explained at the end.
(define capture-limit : Exact-Positive-Integer (* 1024 1024))

(: run-checks (-> (Listof check) branch-head check-run))
(define (run-checks checks project)
  (define wall-clock (current-seconds))
  (define started (current-inexact-monotonic-milliseconds))
  (define results (map run-check checks))
  (check-run (gethostname) wall-clock (seconds-since started) project results))

(: run-check (-> check check-result))
(define (run-check check)
  (define started (current-inexact-monotonic-milliseconds))
  (define-values (process stdout stdin _stderr)
    ;; Use a new process group, so that a timeout kills the shell's children
    ;; too. `system*` can neither do that nor time out, so we use `subprocess`.
    (parameterize ([subprocess-group-enabled #t])
      (subprocess/merged #f #f 'stdout
                         (or (find-executable-path "bash")
                             (raise-handoff-error "bash not found on PATH"))
                         "-c" (check-command check))))
  (close-output-port stdin)
  ;; Output is held whole until the check ends, so a check that floods it until
  ;; its timeout holds all of it in memory.
  (define buffer (open-output-bytes))
  (define reader (thread (λ () (copy-port stdout buffer))))
  ;; Finished means the output reached EOF and the shell exited, both within
  ;; the timeout. A background child holding stdout open counts against it.
  (define (remaining) (max 0 (- (check-timeout check) (seconds-since started))))
  (define finished?
    (and (sync/timeout (remaining) (thread-dead-evt reader))
         (sync/timeout (remaining) process)
         #t))
  (unless finished?
    (subprocess-kill process #t)
    (kill-thread reader))
  (close-input-port stdout)
  (subprocess-wait process)
  (define captured (get-output-bytes buffer))
  (define excess (max 0 (- (bytes-length captured) capture-limit)))
  (define output (bytes->string/utf-8 (subbytes captured excess) (integer->char #xFFFD)))
  (define duration (seconds-since started))
  (cond
    [finished?
     (define code (assert (subprocess-status process) exact-integer?))
     (check-result check (case code [(0) 'true] [(1) 'false] [else 'unknown]) duration output excess code)]
    [else (check-result check 'timeout duration output excess #f)]))

(: seconds-since (-> Flonum Nonnegative-Real))
(define (seconds-since started)
  (max 0 (/ (- (current-inexact-monotonic-milliseconds) started) 1000)))

;;; Repositories

;; Whether `path` is the top level of a Git repository or worktree, rather
;; than a directory inside one.
(: git-repository? (-> Path-String Boolean))
(define (git-repository? path)
  (and (directory-exists? path)
       (let ([output (git* path "rev-parse" "--show-toplevel")])
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
    (or (git* (build-path (normalize-path repository) 'up) "rev-parse" "--show-toplevel")
        (raise-handoff-error "no project repository contains ~a" repository)))
  (string->path (string-trim output "\n" #:left? #f)))

(: project-branch-head (-> Path branch-head))
(define (project-branch-head project)
  (define (line [output : (U String #f)])
    (and output (string-trim output "\n" #:left? #f)))
  (branch-head (line (git* project "symbolic-ref" "--quiet" "--short" "HEAD"))
               (line (git* project "rev-parse" "--verify" "--quiet" "HEAD"))))

;; An arc is a top-level directory containing a start file.
(: arc? (-> Path Boolean))
(define (arc? path)
  (and (directory-exists? path) (file-exists? (build-path path start-file))))

(: list-arcs (-> Path (Listof String)))
(define (list-arcs repository)
  (sort (for/list : (Listof String) ([name (in-list (directory-list repository))]
                                     #:when (arc? (build-path repository name)))
          (path->string name))
        string<?))

(: list-archived-arcs (-> Path (Listof String)))
(define (list-archived-arcs repository)
  (define output (git repository "tag" "--list" (string-append archive-tag-prefix "*")))
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

;; Some entries in the log directory might not pass `parse-log-entry-name`, so
;; partition into successfully parsed entries and entries which are invalid.
(: list-log-entries (-> Path (Values (Listof Exact-Positive-Integer) (Listof String))))
(define (list-log-entries arc)
  (define log (build-path arc log-directory))
  (define names (if (directory-exists? log) (map path->string (directory-list log)) null))
  (define-values (numbered invalid) (partition parse-log-entry-name names))
  (values (sort (map (λ ([name : String]) (assert (parse-log-entry-name name))) numbered) <)
          (sort invalid string<?)))

;; The baseline is the log entry that `handoff start` compares against, that is,
;; the highest-numbered entry that is not pending.
(struct baseline ([number : Exact-Positive-Integer] [run : (U check-run String)]) #:transparent)

(: baseline-results (-> baseline (U check-run #f)))
(define (baseline-results baseline)
  (define run (baseline-run baseline))
  (and (check-run? run) run))

;; A latest complete entry without readable results is still the baseline,
;; so that a broken entry is reported rather than skipped for an older one.
(: find-baseline (-> Path (U baseline #f)))
(define (find-baseline arc)
  (define-values (numbers _invalid) (list-log-entries arc))
  (define pending (pending-log-entry arc))
  (define complete (filter (λ ([number : Exact-Positive-Integer]) (not (eqv? number pending))) numbers))
  (and (pair? complete)
       (let ([number (last complete)])
         (baseline
          number
          (with-handlers ([exn:fail:handoff? exn-message])
            (define results-path (build-path (log-entry-path arc number) results-file))
            (parse-check-run (read-toml results-path)))))))

;; Get the number of the update in progress, which is the highest-numbered log
;; entry not yet in the handoff repository's HEAD (#f if this does not exist).
(: pending-log-entry (-> Path (U Exact-Positive-Integer #f)))
(define (pending-log-entry arc)
  (define-values (numbers _invalid) (list-log-entries arc))
  (and (pair? numbers)
       (let* ([number (last numbers)]
              [entry (log-entry-path arc number)])
         (and (directory-exists? entry)
              ;; `HEAD:./` specifies the tree at the current directory.
              (not (git* entry "cat-file" "-e" "HEAD:./"))
              number))))

;;; Git

;; Obtain Git's stdout, run in `directory`. Stdin is empty. Raises a handoff
;; error that records Git's stderr when it exits non-zero.
(: git (-> Path-String String * String))
(define (git directory . arguments)
  (define-values (succeeded? output error) (apply run-git directory arguments))
  (unless succeeded?
    (raise-handoff-error "git ~a failed in ~a~a"
                         (string-join arguments) directory
                         (if (string=? error "") "" (string-append ":\n" (string-trim error)))))
  output)

;; Like `git`, but #f when Git exits non-zero. Stderr is discarded.
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

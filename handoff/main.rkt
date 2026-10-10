#lang typed/racket/base
;; Automation for the handoff skills.

(require racket/cmdline
         racket/format
         racket/list
         racket/string
         "checks.rkt"
         "error.rkt"
         "lint.rkt"
         "repository.rkt"
         "results.rkt"
         "toml.rkt"
         "update.rkt")

(provide main Exit-Status)

(define output-lines 20)

(module+ main
  (exit (main (current-command-line-arguments))))

(: main (-> (Vectorof String) Exit-Status))
(define (main argv)
  (with-handlers ([exn:fail:handoff?
                   (λ ([error : exn]) : Exit-Status
                     (eprintf "handoff: ~a\n" (exn-message error))
                     2)]
                  [exn:fail:user?
                   (λ ([error : exn]) : Exit-Status
                     (eprintf "~a\n" (exn-message error))
                     2)])
    (execute (parse-arguments argv))))

;; 0 for success, 1 when a command's exit code is negative (a check that is not
;; true, a lint error), 2 for usage and input errors.
(define-type Exit-Status (U 0 1 2))

(define-type Command (U list-command check-command lint-command update-command))
(struct list-command ([archived? : Boolean]) #:transparent)
(struct check-command ([arc : (U String #f)]) #:transparent)
(struct lint-command ([arc : (U String #f)]) #:transparent)
(struct update-command ([step : (U 'begin 'commit)] [arc : (U String #f)]) #:transparent)

(struct invocation ([repository : (U Path #f)] [command : Command]) #:transparent)

(: parse-arguments (-> (Vectorof String) invocation))
(define (parse-arguments argv)
  (define repository : (U Path #f) #f)
  (command-line
   #:program "handoff"
   #:argv argv
   #:usage-help
   "Commands:"
   "  list   list handoff arcs"
   "  check  run the commands in checks.toml and report their results"
   "  lint   validate expected structure of a handoff arc"
   "  update record a checkpoint: --begin records results, --commit commits the entry"
   #:once-each
   [("--repository") path
                     "Handoff repository to use instead of the nearest .scratch/handoff/"
                     (set! repository (string->path (assert path string?)))]
   #:args (command . arguments)
   ;; Typed Racket types the formal arguments of `#:args` as `Any`, so we do a
   ;; runtime check explicitly (that we expect will never fail).
   (invocation repository
               (parse-command (assert command string?)
                              (map (λ (a) (assert a string?)) arguments)))))

(: parse-command (-> String (Listof String) Command))
(define (parse-command command arguments)
  (define program (string-append "handoff " command))
  (case command
    [("list")
     (define archived? : Boolean #f)
     (command-line
      #:program program
      #:argv arguments
      #:once-each
      [("--archived") "List archived arcs instead" (set! archived? #t)]
      #:args ()
      (list-command archived?))]
    [("check") (check-command (parse-arc-argument program arguments))]
    [("lint") (lint-command (parse-arc-argument program arguments))]
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
      (update-command (or step (raise-handoff-error "update requires --begin or --commit"))
                      (and arc (assert arc string?))))]
    [else (raise-handoff-error "unknown command ~a; expected list, check, lint, or update" command)]))

(: parse-arc-argument (-> String (Listof String) (U String #f)))
(define (parse-arc-argument program arguments)
  (command-line
   #:program program
   #:argv arguments
   #:args ([arc #f])
   (and arc (assert arc string?))))

(: execute (-> invocation Exit-Status))
(define (execute invocation)
  (define option (invocation-repository invocation))
  (when (and option (not (git-repository? option)))
    (raise-handoff-error "~a is not a git repository" option))
  (define repository (or option (find-handoff-repository (current-directory))))
  (define command (invocation-command invocation))
  (cond
    [(list-command? command)
     (handle-list repository (list-command-archived? command))]
    [(check-command? command)
     (handle-check repository (select-arc repository (check-command-arc command)))]
    [(lint-command? command)
     (handle-lint repository (select-arc repository (lint-command-arc command)))]
    [(update-command? command)
     (define arc (select-arc repository (update-command-arc command)))
     (case (update-command-step command)
       [(begin) (handle-update-begin repository arc)]
       [(commit) (handle-update-commit repository arc)])]))

(: handle-list (-> Path Boolean Exit-Status))
(define (handle-list repository archived?)
  (for ([name (in-list ((if archived? list-archived-arcs list-arcs) repository))])
    (displayln name))
  0)

(: handle-check (-> Path String Exit-Status))
(define (handle-check repository arc)
  (define checks-path (build-path repository arc checks-file))
  (define checks
    (with-handlers ([exn:fail:handoff?
                     (λ ([error : exn])
                       (raise-handoff-error "~a: ~a" checks-path (exn-message error)))])
      (parse-checks (read-toml checks-path))))
  (define run (run-checks checks))
  (report run)
  (if (andmap (λ ([result : result]) (eq? (result-outcome result) 'true)) (run-results run)) 0 1))

(: handle-lint (-> Path String Exit-Status))
(define (handle-lint repository arc)
  (define diagnostics (lint-arc (build-path repository arc) repository))
  (for ([diagnostic (in-list diagnostics)])
    (printf "~a: ~a\n" (diagnostic-path diagnostic) (diagnostic-message diagnostic)))
  (printf "~a: ~a lint errors\n" arc (length diagnostics))
  (if (empty? diagnostics) 0 1))

(: handle-update-begin (-> Path String Exit-Status))
(define (handle-update-begin repository arc)
  (define pending* (begin-update repository arc))
  (report (results-run (pending-results pending*)))
  (printf "\nlog/~a results recorded. Write the entry to ~a, then run handoff update --commit ~a\n"
          (pending-number pending*) (pending-entry pending*) arc)
  0)

(: handle-update-commit (-> Path String Exit-Status))
(define (handle-update-commit repository arc)
  (define checkpoint (commit-update repository arc))
  (printf "log/~a committed as ~a ~a\n"
          (checkpoint-number checkpoint)
          (substring (checkpoint-commit checkpoint) 0 7)
          (checkpoint-subject checkpoint))
  (define dirty (checkpoint-dirty checkpoint))
  (unless (empty? dirty)
    (printf "not committed:\n")
    (for ([line (in-list dirty)])
      (printf "  ~a\n" line)))
  0)

;; TODO: Move this when it becomes clear where to put it.
(: report (-> run Void))
(define (report run)
  (define results (run-results run))
  (define (seconds [duration : Real]) (string-append (~r duration #:precision '(= 1)) "s"))
  (define (width [heading : String] [cells : (Listof String)])
    (apply max (string-length heading) (map string-length cells)))
  (define id-width (width "ID" (map (λ ([r : result]) (check-id (result-check r))) results)))
  (define claim-width (width "CLAIM" (map (λ ([r : result]) (check-claim (result-check r))) results)))
  (define duration-width (width "DURATION" (map (λ ([r : result]) (seconds (result-duration r))) results)))
  (define (row [id : String] [claim : String] [outcome : String] [duration : String])
    (printf "~a  ~a  ~a  ~a\n"
            (~a id #:min-width id-width)
            (~a claim #:min-width claim-width)
            (~a outcome #:min-width 7)
            (~a duration #:min-width duration-width #:align 'right)))
  (row "ID" "CLAIM" "RESULT" "DURATION")
  (for ([result (in-list results)])
    (define check (result-check result))
    (define outcome (result-outcome result))
    (row (check-id check) (check-claim check) (symbol->string outcome) (seconds (result-duration result)))
    (unless (eq? outcome 'true)
      (if (eq? outcome 'timeout)
          (printf "         after ~as\n" (check-timeout check))
          (printf "         exit ~a\n" (result-exit result)))
      (unless (string=? (result-output result) "")
        (printf "         output:\n")
        (unless (zero? (result-truncated result))
          (printf "           [~a bytes before this were not kept]\n" (result-truncated result)))
        (define lines (string-split (result-output result) "\n"))
        (for ([line (in-list (drop lines (max 0 (- (length lines) output-lines))))])
          (printf "           ~a\n" line)))))
  (define counts
    (for*/list : (Listof String) ([outcome (in-list '(true false unknown timeout))]
                                  [count (in-value (count (λ ([r : result]) (eq? (result-outcome r) outcome)) results))]
                                  #:when (positive? count))
      (format "~a ~a" count outcome)))
  (printf "\nhost: ~a, duration: ~a, summary: ~a\n"
          (run-host run)
          (seconds (run-duration run))
          (if (empty? counts) "no checks" (string-join counts ", "))))

#lang typed/racket/base

(require racket/function
         racket/math
         racket/port
         racket/list
         "error.rkt"
         "toml.rkt")

(require/typed racket/base
  [current-inexact-monotonic-milliseconds (-> Flonum)]
  ;; Typed Racket's type for subprocess has no 'stdout for stderr.
  [(subprocess subprocess/merged)
   (-> False False 'stdout Path-String String *
       (Values Subprocess Input-Port Output-Port False))])

(require/typed racket/os
  [gethostname (-> String)])

(provide (struct-out check)
         (struct-out result)
         (struct-out run)
         Outcome
         default-timeout
         parse-checks
         run-check
         run-checks)

(define default-timeout : Positive-Real 60)

(struct check ([id : String] [claim : String] [command : String] [timeout : Positive-Real]) #:transparent)

;; Exit 0 is `true`, exit 1 is `false`, a timeout is `timeout`, and any other
;; exit code is `unknown`.
(define-type Outcome (U 'true 'false 'unknown 'timeout))

;; `exit` is #f exactly when the outcome is timeout. Durations are seconds.
(struct result ([check : check] [outcome : Outcome] [duration : Nonnegative-Real] [output : String] [exit : (U Integer #f)])
  #:transparent)
(struct run ([host : String] [duration : Nonnegative-Real] [results : (Listof result)]) #:transparent)

;; Checks are an array of tables so that they run in file order.
(: parse-checks (-> Table (Listof check)))
(define (parse-checks table)
  (expect-fields table null (list 'check) "checks")
  (define entries (hash-ref table 'check (const null)))
  (unless (list? entries)
    (raise-handoff-error "check must be an array of tables"))
  (define checks
    (for/list : (Listof check) ([entry (in-list entries)] [index : Natural (in-naturals)])
      (parse-check entry index)))
  (define duplicate (check-duplicates (map check-id checks)))
  (when duplicate
    (raise-handoff-error "check ~a appears more than once" duplicate))
  checks)

(: parse-check (-> Any Natural check))
(define (parse-check entry index)
  (define table (expect-table entry (format "check[~a]" index)))
  (define id (hash-ref table 'id #f))
  (define context (if (string? id) (format "check ~a" id) (format "check[~a]" index)))
  (expect-fields table '(id claim run) '(timeout) context)
  (define timeout (hash-ref table 'timeout (λ () default-timeout)))
  (check (expect-non-empty-string table 'id context)
         (expect-non-empty-string table 'claim context)
         (expect-non-empty-string table 'run context)
         (if (and (real? timeout) (positive? timeout) (not (infinite? timeout)))
             timeout
             (raise-handoff-error "~a: timeout must be a positive number of seconds" context))))

(: run-checks (-> (Listof check) run))
(define (run-checks checks)
  (define started (current-inexact-monotonic-milliseconds))
  (define results (map run-check checks))
  (run (gethostname) (seconds-since started) results))

(: run-check (-> check result))
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
  (define output (bytes->string/utf-8 (get-output-bytes buffer) (integer->char #xFFFD)))
  (define duration (seconds-since started))
  (cond
    [finished?
     (define code (assert (subprocess-status process) exact-integer?))
     (result check (case code [(0) 'true] [(1) 'false] [else 'unknown]) duration output code)]
    [else (result check 'timeout duration output #f)]))

(: seconds-since (-> Flonum Nonnegative-Real))
(define (seconds-since started)
  (max 0 (/ (- (current-inexact-monotonic-milliseconds) started) 1000)))

#lang typed/racket/base

(require racket/function
         racket/list
         racket/math
         "checks.rkt"
         "error.rkt"
         "repository.rkt"
         "toml.rkt")

(require/typed gregor
  [#:opaque Moment moment?]
  [->posix (-> Moment Real)]
  [posix->moment (-> Real Integer Moment)]
  [moment->iso8601 (-> Moment String)])

(provide (struct-out results)
         results->toml
         parse-results)

;; TODO: Should branch-head just go in run and then we're literally recording a run?
(struct results ([run : run] [project : branch-head]) #:transparent)

;;; Writing

(: results->toml (-> results String))
(define (results->toml results)
  (define out (open-output-string))
  (define (field [key : String] [value : String])
    (fprintf out "~a = ~a\n" key value))
  (define run (results-run results))
  (define branch-head (results-project results))
  (fprintf out "[run]\n")
  (field "host" (toml-string (run-host run)))
  (field "started" (moment->iso8601 (posix->moment (run-started run) 0)))
  (field "duration" (toml-float (run-duration run) 3))
  (fprintf out "\n[project]\n")
  (let ([head (branch-head-head branch-head)])
    (when head
      (field "head" (toml-string head))))
  (let ([branch (branch-head-branch branch-head)])
    (when branch
      (field "branch" (toml-string branch))))
  (for ([result (in-list (run-results run))])
    (define check (result-check result))
    (define timeout (check-timeout check))
    (define exit (result-exit result))
    (fprintf out "\n[[check]]\n")
    (field "id" (toml-string (check-id check)))
    (field "claim" (toml-string (check-claim check)))
    (field "run" (toml-string (check-command check)))
    (field "timeout" (number->string (if (exact-integer? timeout)
                                         timeout
                                         (exact->inexact timeout))))
    (field "outcome" (toml-string (symbol->string (result-outcome result))))
    (when exit (field "exit" (number->string exit)))
    (field "duration" (toml-float (result-duration result) 3))
    (unless (zero? (result-truncated result))
      (field "truncated" (number->string (result-truncated result))))
    ;; Last, since it is usually the only multiline value.
    (field "output" (toml-string (result-output result))))
  (get-output-string out))

;;; Reading

(: parse-results (-> Table results))
(define (parse-results table)
  (expect-fields table (list 'run 'project) (list 'check) "results")
  (define run-table (expect-table (hash-ref table 'run) "run"))
  (expect-fields run-table (list 'host 'started 'duration) null "run")
  (define project-table (expect-table (hash-ref table 'project) "project"))
  (expect-fields project-table null (list 'head 'branch) "project")
  (define entries (hash-ref table 'check (const null)))
  (unless (list? entries)
    (raise-handoff-error "check must be an array of tables"))
  (define parsed
    (for/list : (Listof result) ([entry (in-list entries)]
                                 [index : Natural (in-naturals)])
      (parse-result entry index)))
  (define duplicate (check-duplicates (map (compose check-id result-check) parsed)))
  (when duplicate
    (raise-handoff-error "check ~a appears more than once" duplicate))
  (define started (hash-ref run-table 'started))
  (results (run (expect-non-empty-string run-table 'host "run")
                (if (moment? started)
                    (exact-floor (->posix started))
                    (raise-handoff-error "run.started must be a datetime with an offset"))
                (expect-nonnegative-real run-table 'duration "run")
                parsed)
           (branch-head
            (and (hash-has-key? project-table 'branch)
                 (expect-non-empty-string project-table 'branch "project"))
            (and (hash-has-key? project-table 'head)
                 (expect-non-empty-string project-table 'head "project")))))

(: parse-result (-> Any Natural result))
(define (parse-result entry index)
  (define table (expect-table entry (format "check[~a]" index)))
  (define id (hash-ref table 'id #f))
  (define context (if (string? id) (format "check ~a" id) (format "check[~a]" index)))
  (expect-fields table
                 (list 'id 'claim 'run 'timeout 'outcome 'duration 'output)
                 (list 'exit 'truncated)
                 context)
  (define outcome
    (case (hash-ref table 'outcome)
      [("true") 'true]
      [("false") 'false]
      [("unknown") 'unknown]
      [("timeout") 'timeout]
      [else (raise-handoff-error "~a: outcome must be true, false, unknown or timeout" context)]))
  (define exit (hash-ref table 'exit #f))
  (unless (or (exact-integer? exit) (not exit))
    (raise-handoff-error "~a: exit must be an integer" context))
  (unless (eq? (eq? outcome 'timeout) (not exit))
    (raise-handoff-error "~a: exit must be present exactly when the outcome is not timeout" context))
  (define output (hash-ref table 'output))
  (unless (string? output)
    (raise-handoff-error "~a: output must be a string" context))
  (define truncated (hash-ref table 'truncated (const 0)))
  (unless (exact-nonnegative-integer? truncated)
    (raise-handoff-error "~a: truncated must be a nonnegative integer" context))
  (result (parse-check-definition table context)
          outcome
          (expect-nonnegative-real table 'duration context)
          output
          truncated
          exit))

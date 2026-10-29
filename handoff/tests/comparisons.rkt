#lang typed/racket/base
;; Comparing results with those of the last handoff.

(require racket/function
         racket/port
         (except-in typed/rackunit check)
         "../main.rkt")

(: result (-> String String (U Integer #f) check-result))
(define (result id command exit)
  (check-result (check id "claim" command 60000)
                (check-observation exit 0 "" 0)))

(test-case "comparisons follow the current order, then the removed checks"
  (define comparisons
    (compare-check-results (list (result "removed" "true" 0)
                                 (result "redefined" "old" 0)
                                 (result "unchanged" "true" 0))
                           (list (result "unchanged" "true" 0)
                                 (result "new" "true" 0)
                                 (result "redefined" "new" 0))))
  (check-equal? comparisons
                (list (check-matched (result "unchanged" "true" 0) (result "unchanged" "true" 0))
                      (check-added (result "new" "true" 0))
                      (check-matched (result "redefined" "old" 0) (result "redefined" "new" 0))
                      (check-removed (result "removed" "true" 0)))))

(test-case "rewording a claim does not redefine a check, but changing its timeout does"
  (define before (check "a" "claim" "true" 60000))
  (define (change [after : check])
    (check-matched-change
     (check-matched (check-result before (check-observation 0 0 "" 0))
                    (check-result after (check-observation 0 0 "" 0)))))
  (check-equal? (change (check "a" "reworded" "true" 60000)) 'unchanged)
  (check-equal? (change (check "a" "claim" "true" 120000)) 'redefined))

(test-case "a change of outcome is classified by what each outcome says about the claim"
  (define exits (list 0 1 3 #f))
  (check-equal? (for*/list : (Listof Symbol) ([before (in-list exits)]
                                              [after (in-list exits)])
                  (check-matched-change
                   (check-matched (result "a" "true" before)
                                  (result "a" "true" after))))
                ;; before: true, false, unknown, timeout; after in the same order
                (list 'unchanged 'contradicted 'unverified 'unverified
                      'contradicted 'unchanged 'unverified 'unverified
                      'verified 'verified 'unchanged 'still-unverified
                      'verified 'verified 'still-unverified 'unchanged)))

(test-case "the baseline is the arc's highest-numbered log entry"
  (check-equal? (find-latest-log-entry "v4" (list "v4/log/2"
                                             "v4/log/10"
                                             "v4/log/9"
                                             "v4/log/notes.md"
                                             "v40/log/11"))
                10))

(test-case "an arc without log entries has no baseline"
  (check-false (find-latest-log-entry "v4" (list "v4/log/notes.md"))))

(test-case "a comparison shows changes, the baseline's output and removed checks"
  (define baseline
    (list (check-result (check "flips" "it holds" "test -e x" 60000)
                        (check-observation 1 0 "missing\n" 0))
          (check-result (check "gone" "it is gone" "true" 60000)
                        (check-observation 0 0 "" 0))))
  (define current
    (list (check-result (check "flips" "it holds" "test -e x" 60000)
                        (check-observation 0 1200 "" 0))
          (check-result (check "fresh" "it is new" "false" 60000)
                        (check-observation 1 0 "no\n" 0))))
  (check-equal? (with-output-to-string
                  (thunk (print-comparisons
                          (compare-check-results baseline current)
                          7
                          "murph"
                          1793231741)))
                (string-append "compared against log/7, recorded on murph at 2026-10-28T23:55:41Z\n"
                               "\n"
                               "id: flips\n"
                               "claim: it holds\n"
                               "outcome: true\n"
                               "change: contradicted, was false at log/7\n"
                               "duration: 1.2s\n"
                               "output at log/7:\n"
                               "  missing\n"
                               "\n"
                               "id: fresh\n"
                               "claim: it is new\n"
                               "outcome: false\n"
                               "change: new since log/7\n"
                               "exit: 1\n"
                               "duration: 0.0s\n"
                               "output:\n"
                               "  no\n"
                               "\n"
                               "removed since log/7: gone (it is gone)\n"
                               "\n"
                               "summary: 1 true, 1 false\n")))

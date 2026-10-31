#lang typed/racket/base
;; Refusing to commit an update.

(require racket/function
         (except-in typed/rackunit check)
         "../main.rkt"
         "fixtures.rkt")

(define recorded
  (struct-copy check-run run
               [results (list (check-result (check "tests" "The tests pass" "true" 60)
                                            (check-observation 0 1 "" 0)))]))

;; `clean` with a pending log/3.
(: pending (-> (U String #f) (U check-run String #f) arc))
(define (pending text run*)
  (struct-copy arc clean [log (append (arc-log clean) (list (log-entry 3 #f text run*)))]))

(: with-checks (-> arc (Listof check) arc))
(define (with-checks arc* checks)
  (struct-copy arc arc* [checks checks]))

(test-case "the subject is the arc's name and the entry's headline"
  (check-equal? (commit-subject-or-refuse (pending "# Third\n" recorded) "main" "abc123")
                "a: Third"))

(test-case "an arc without a pending entry is refused"
  (check-exn #rx"^a has no pending log entry; run handoff update --begin first$"
             (thunk (commit-subject-or-refuse clean "main" "abc123"))))

(test-case "a pending entry without entry.md is refused"
  (check-exn #rx"^a/log/3/entry.md: write the entry first$"
             (thunk (commit-subject-or-refuse (pending #f recorded) "main" "abc123"))))

(test-case "an entry.md that does not open with a heading is refused"
  (check-exn #rx"^a/log/3/entry.md: expected a heading on its first nonblank line$"
             (thunk (commit-subject-or-refuse (pending "Third\n" recorded) "main" "abc123"))))

(test-case "a pending entry without results.toml is refused"
  (check-exn #rx"^a/log/3/results.toml: expected a regular file; run handoff update --begin$"
             (thunk (commit-subject-or-refuse (pending "# Third\n" #f) "main" "abc123"))))

(test-case "a check redefined since the results were recorded is refused"
  (check-exn #rx"^a/checks.toml: the checks changed since the results were recorded"
             (thunk (commit-subject-or-refuse (with-checks (pending "# Third\n" recorded)
                                                             (list (check "tests" "The tests pass" "true" 30)))
                                                "main"
                                                "abc123"))))

(test-case "a claim reworded since the results were recorded is refused"
  (check-exn #rx"^a/checks.toml: the checks changed since the results were recorded"
             (thunk (commit-subject-or-refuse (with-checks (pending "# Third\n" recorded)
                                                             (list (check "tests" "The suite passes" "true" 60)))
                                                "main"
                                                "abc123"))))

(test-case "a check added since the results were recorded is refused"
  (check-exn #rx"^a/checks.toml: the checks changed since the results were recorded"
             (thunk (commit-subject-or-refuse (with-checks (pending "# Third\n" recorded)
                                                             (list (check "tests" "The tests pass" "true" 60)
                                                                   (check "builds" "It builds" "true" 60)))
                                                "main"
                                                "abc123"))))

(test-case "a project head that moved since the results were recorded is refused"
  (check-exn #rx"^the project moved from main at abc123 to main at def456 since a/log/3/results.toml was recorded; run handoff update --begin again$"
             (thunk (commit-subject-or-refuse (pending "# Third\n" recorded) "main" "def456"))))

(test-case "a project branch that changed since the results were recorded is refused"
  (check-exn #rx"^the project moved from main at abc123 to other at abc123 since"
             (thunk (commit-subject-or-refuse (pending "# Third\n" recorded) "other" "abc123"))))

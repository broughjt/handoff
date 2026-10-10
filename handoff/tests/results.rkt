#lang typed/racket/base

(require racket/list
         racket/string
         typed/rackunit
         (rename-in "../checks.rkt" [check make-check])
         "../error.rkt"
         "../repository.rkt"
         "../results.rkt"
         "../toml.rkt")

(define (round-trip [results : results]) (parse-results (parse-toml (results->toml results))))

(define sample
  (results (run "murph" 1791504000 1.5
                (list (result (make-check "yes" "holds" "true" 60) 'true 0.25 "" 0 0)
                      (result (make-check "slow" "times out" "sleep 5" 0.5) 'timeout 0.5 "waiting\n" 0 #f)
                      (result (make-check "big" "fails" "seq 1 3" 60) 'false 0.125 "2\n3\n" 1234 1)))
           (branch-head "main" "714bc77")))

(test-case "results survive a round trip"
  (check-equal? (round-trip sample) sample)
  (define detached (struct-copy results sample [project (branch-head #f #f)]))
  (check-equal? (round-trip detached) detached)
  (define empty (struct-copy results sample [run (run "murph" 0 0.0 '())]))
  (check-equal? (round-trip empty) empty))

(test-case "output that toml-lib's writer cannot represent reads back intact"
  (define everything
    (list->string (append (for/list : (Listof Char) ([n (in-range 0 128)]) (integer->char n))
                          (string->list "é 😀 \"\"\" \\\n trailing \\"))))
  (for ([output (in-list (list everything "\e[31mred\e[0m" "\r\n" "\n\nleading newlines" "ends with a quote\""
                               "no newline" ""))])
    (define with-output
      (struct-copy results sample
                   [run (run "murph" 0 0.0
                             (list (result (make-check "x" "c" "r" 60) 'false 0.0 output 0 1)))]))
    (check-equal? (round-trip with-output) with-output (format "~s" output))))

(test-case "durations are written to the millisecond and timeouts keep their type"
  (define text (results->toml sample))
  (check-true (string-contains? text "duration = 1.500\n"))
  (check-true (string-contains? text "timeout = 60\n"))
  (check-true (string-contains? text "timeout = 0.5\n"))
  (check-true (string-contains? text "started = 2026-10-09T00:00:00Z\n")))

(test-case "malformed results are rejected"
  (define valid (results->toml sample))
  (define (variant [from : String] [to : String])
    (check-true (string-contains? valid from) from)
    (string-replace valid from to #:all? #f))
  (for ([text (in-list (list ""
                             (variant "[project]\n" "")
                             (variant "host = \"murph\"\n" "")
                             (variant "started = 2026-10-09T00:00:00Z" "started = 2026-10-09T00:00:00")
                             (variant "started = 2026-10-09T00:00:00Z" "started = \"yesterday\"")
                             (variant "duration = 1.500" "duration = -1.0")
                             (variant "duration = 1.500" "duration = nan")
                             (variant "outcome = \"true\"" "outcome = \"maybe\"")
                             (variant "outcome = \"true\"\nexit = 0\n" "outcome = \"true\"\n")
                             (variant "outcome = \"timeout\"\n" "outcome = \"timeout\"\nexit = 0\n")
                             (variant "truncated = 1234" "truncated = -1")
                             (variant "id = \"slow\"" "id = \"yes\"")
                             (variant "id = \"yes\"\n" "id = \"yes\"\nexpect = 0\n")
                             (variant "branch = \"main\"" "branch = \"\"")))])
    (check-exn exn:fail:handoff? (λ () (parse-results (parse-toml text))) text)))

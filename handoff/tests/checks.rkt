#lang typed/racket/base
;; Parsing checks.toml and printing results.

(require racket/function
         racket/port
         racket/string
         (except-in typed/rackunit check)
         "../main.rkt"
         "../error.rkt")

(define source (string->path "checks.toml"))

(define echo (check "echo" "it echoes" "echo hi" 60000))

(test-case "a check's timeout defaults to 60 seconds"
  (check-equal? (parse-checks "[[check]]
                               id = 'echo'
                               claim = 'it echoes'
                               command = 'echo hi'"
                              source)
                (list echo)))

(test-case "checks keep their file order and their timeouts"
  (check-equal? (map (λ ([check : check]) (cons (check-id check) (check-timeout check)))
                     (parse-checks "[[check]]
                                    id = 'b'
                                    claim = 'c'
                                    command = 'd'
                                    timeout = 1.5

                                    [[check]]
                                    id = 'a'
                                    claim = 'c'
                                    command = 'd'"
                                   source))
                (list (cons "b" 1500) (cons "a" 60000))))

(test-case "a file without checks parses to the empty list"
  (check-equal? (parse-checks "" source) null))

(test-case "a check with an unknown field is rejected"
  (check-exn #rx"checks.toml: check\\[0\\] has unknown fields run"
             (thunk (parse-checks "[[check]]
                                   id = 'a'
                                   claim = 'c'
                                   command = 'd'
                                   run = 'd'"
                                  source))))

(test-case "a check missing a field is rejected"
  (check-exn #rx"check\\[0\\] is missing claim"
             (thunk (parse-checks "[[check]]
                                   id = 'a'
                                   command = 'd'"
                                  source))))

(test-case "a blank field is rejected"
  (check-exn #rx"check\\[0\\]: command must be a nonempty string"
             (thunk (parse-checks "[[check]]
                                   id = 'a'
                                   claim = 'c'
                                   command = '  '"
                                  source))))

(test-case "a timeout must be positive"
  (check-exn #rx"timeout must be a positive number of seconds"
             (thunk (parse-checks "[[check]]
                                   id = 'a'
                                   claim = 'c'
                                   command = 'd'
                                   timeout = 0"
                                  source))))

(test-case "ids must be unique"
  (check-exn #rx"checks.toml: check a appears more than once"
             (thunk (parse-checks "[[check]]
                                   id = 'a'
                                   claim = 'c'
                                   command = 'd'

                                   [[check]]
                                   id = 'a'
                                   claim = 'c'
                                   command = 'd'"
                                  source))))

(test-case "a timeout is kept in whole milliseconds"
  (check-equal? (map check-timeout
                     (parse-checks "[[check]]
                                    id = 'a'
                                    claim = 'c'
                                    command = 'd'
                                    timeout = 0.0016"
                                   source))
                (list 2)))

(test-case "a timeout that rounds to zero milliseconds is rejected"
  (check-exn #rx"timeout must be a positive number of seconds"
             (thunk (parse-checks "[[check]]
                                   id = 'a'
                                   claim = 'c'
                                   command = 'd'
                                   timeout = 0.0004"
                                  source))))

(test-case "invalid TOML is reported against its source"
  (check-exn (λ (error) (and (exn:fail:handoff? error)
                             (string-prefix? (exn-message error) "checks.toml: ")))
             (thunk (parse-checks "[[check]" source))))

(test-case "outcomes follow the exit code"
  (check-equal? (for/list : (Listof Symbol) ([exit (in-list (list 0 1 3 #f))])
                  (check-result-outcome
                   (check-result echo (check-observation exit 0 "" 0))))
                (list 'true 'false 'unknown 'timeout)))

(test-case "a true result is printed without its output"
  (check-equal? (with-output-to-string
                  (thunk (print-results
                          (list (check-result echo (check-observation 0 1240 "hi\n" 0))))))
                (string-append "id: echo\n"
                               "claim: it echoes\n"
                               "outcome: true\n"
                               "duration: 1.2s\n"
                               "\n"
                               "summary: 1 true\n")))

(test-case "a failing result is printed with its exit code and the last 20 lines of output"
  (define output (string-join (for/list : (Listof String) ([n (in-range 1 23)]) (number->string n)) "\n"))
  (check-equal? (with-output-to-string
                  (thunk (print-results
                          (list (check-result echo (check-observation 1 0 output 0))))))
                (string-append "id: echo\n"
                               "claim: it echoes\n"
                               "outcome: false\n"
                               "exit: 1\n"
                               "duration: 0.0s\n"
                               "output (last 20 of 22 lines):\n"
                               (apply string-append
                                      (for/list : (Listof String) ([n (in-range 3 23)])
                                        (format "  ~a\n" n)))
                               "\n"
                               "summary: 1 false\n")))

(test-case "a timed-out result is printed with its timeout"
  (check-regexp-match #rx"outcome: timeout\ntimeout: 60.0s\n"
                      (with-output-to-string
                        (thunk (print-results
                                (list (check-result echo (check-observation #f 60000 "" 0))))))))

(test-case "truncated output says how much was dropped"
  (check-regexp-match #rx"truncated: 7 bytes before the output were not kept\noutput:\n  tail\n"
                      (with-output-to-string
                        (thunk (print-results
                                (list (check-result echo (check-observation 1 0 "tail\n" 7))))))))

(test-case "the summary counts outcomes in a fixed order"
  (check-regexp-match #rx"summary: 1 true, 2 false, 1 timeout\n$"
                      (with-output-to-string
                        (thunk (print-results
                                (list (check-result echo (check-observation #f 0 "" 0))
                                      (check-result echo (check-observation 1 0 "" 0))
                                      (check-result echo (check-observation 0 0 "" 0))
                                      (check-result echo (check-observation 1 0 "" 0))))))))

(test-case "a run without checks says \"no checks\" in the summary"
  (check-equal? (with-output-to-string (thunk (print-results null)))
                "summary: no checks\n"))

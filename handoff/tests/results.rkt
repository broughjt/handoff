#lang typed/racket/base
;; Reading and writing results.toml.

(require racket/function
         racket/port
         racket/string
         (except-in typed/rackunit check)
         "../main.rkt")

(require/typed gregor
  [iso8601->moment (-> String Moment)])

(define source (string->path "results.toml"))

(define echo (check "echo" "it echoes" "echo hi" 60))

(define started (iso8601->moment "2026-10-28T23:55:41Z"))

(test-case "a run is written with durations and timeouts in seconds"
  (define run
    (check-run "murph" started 5/2 "main" "abc123"
               (list (check-result (check "slow" "it is slow" "sleep 9" 3/2)
                                   (check-observation #f 751/500 "" 0))
                     (check-result echo
                                   (check-observation 0 3/250 "hi\n" 0)))))
  (check-equal? (with-output-to-string (thunk (write-check-run run)))
                (string-append "[run]\n"
                               "host = \"murph\"\n"
                               "started = 2026-10-28T23:55:41Z\n"
                               "duration = 2.5\n"
                               "\n"
                               "[project]\n"
                               "branch = \"main\"\n"
                               "head = \"abc123\"\n"
                               "\n"
                               "[[check]]\n"
                               "id = \"slow\"\n"
                               "claim = \"it is slow\"\n"
                               "command = \"sleep 9\"\n"
                               "timeout = 1.5\n"
                               "duration = 1.502\n"
                               "output = \"\"\n"
                               "\n"
                               "[[check]]\n"
                               "id = \"echo\"\n"
                               "claim = \"it echoes\"\n"
                               "command = \"echo hi\"\n"
                               "timeout = 60\n"
                               "exit = 0\n"
                               "duration = 0.012\n"
                               "output = \"\"\"\n"
                               "hi\n"
                               "\"\"\"\n")))

(test-case "a run with hostile output reads back as written"
  (define output
    (string-append "\n\"quoted\" \\back\\slash \"\"\" three quotes\n"
                   "tab\tcarriage\rescape\ebell\anul\u0000delete\u007F\n"
                   "ünïcödé 🙂\n"
                   "ends with a quote\""))
  (define run
    (check-run "murph" started 5/2 "main" "abc123"
               (list (check-result (check "c" "claim \"quoted\"" "printf '\\a'" 1/1000)
                                   (check-observation 3 7/1000 output 1024)))))
  (check-true (equal? (parse-check-run (with-output-to-string (thunk (write-check-run run)))
                                       source)
                      run)))

(test-case "a run on a detached head without commits reads back as written"
  (define run (check-run "murph" started 0 #f #f null))
  (check-true (equal? (parse-check-run (with-output-to-string (thunk (write-check-run run)))
                                       source)
                      run)))

(test-case "a check's outcome is not a field of results.toml"
  (check-exn #rx"results.toml: check\\[0\\] has unknown fields outcome"
             (thunk (parse-check-run "[run]
                                      host = 'murph'
                                      started = 2026-10-28T23:55:41Z
                                      duration = 1

                                      [project]

                                      [[check]]
                                      id = 'a'
                                      claim = 'c'
                                      command = 'd'
                                      outcome = 'true'
                                      exit = 0
                                      duration = 1
                                      output = ''"
                                     source))))

(test-case "a start time without an offset is rejected"
  (check-exn #rx"results.toml: run: started must be a datetime with an offset"
             (thunk (parse-check-run "[run]
                                      host = 'murph'
                                      started = 2026-10-28T23:55:41
                                      duration = 1

                                      [project]"
                                     source))))

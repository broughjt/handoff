#lang typed/racket/base

(require racket/file
         racket/list
         racket/string
         typed/rackunit
         "common.rkt"
         (rename-in "../checks.rkt" [check make-check])
         "../error.rkt"
         "../toml.rkt")

(define (load [text : String]) (parse-checks (parse-toml text)))

(test-case "checks keep file order and default their timeout"
  (define checks (load "[[check]]\nid = \"second\"\nclaim = \"two\"\nrun = \"true\"\ntimeout = 5\n\n[[check]]\nid = \"first\"\nclaim = \"one\"\nrun = \"false\"\n"))
  (check-equal? (map check-id checks) '("second" "first"))
  (check-equal? (first checks) (make-check "second" "two" "true" 5))
  (check-equal? (check-timeout (second checks)) default-timeout))

(test-case "an empty checks file has no checks"
  (check-equal? (load "") '()))

(test-case "malformed checks are rejected"
  (for ([text (in-list (list "[[check]]\nid = \"x\"\nrun = \"true\"\n"
                             "[[check]]\nid = \"x\"\nclaim = \"c\"\n"
                             "[[check]]\nclaim = \"c\"\nrun = \"true\"\n"
                             "[[check]]\nid = \"x\"\nclaim = \"c\"\nrun = \"true\"\nexpect = 0\n"
                             "[[check]]\nid = \"x\"\nclaim = \" \"\nrun = \"true\"\n"
                             "[[check]]\nid = \"x\"\nclaim = \"c\"\nrun = \"true\"\ntimeout = 0\n"
                             "[[check]]\nid = \"x\"\nclaim = \"c\"\nrun = \"true\"\n[[check]]\nid = \"x\"\nclaim = \"d\"\nrun = \"true\"\n"
                             "[check]\nid = \"x\"\nclaim = \"c\"\nrun = \"true\"\n"
                             "check = 1\n"
                             "other = 1\n"))])
    (check-exn exn:fail:handoff? (λ () (load text)) text)))

(test-case "errors name the check"
  (check-exn #rx"check x is missing claim"
             (λ () (load "[[check]]\nid = \"x\"\nrun = \"true\"\n"))))

(define (run-shell [command : String] #:timeout [timeout : Positive-Real default-timeout])
  (run-check (make-check "x" "claim" command timeout)))

(test-case "exit codes map to outcomes"
  (check-equal? (result-outcome (run-shell "exit 0")) 'true)
  (check-equal? (result-outcome (run-shell "exit 1")) 'false)
  (for ([code (in-list '(2 3 4 255))])
    (define result (run-shell (format "exit ~a" code)))
    (check-equal? (result-outcome result) 'unknown)
    (check-equal? (result-exit result) code)))

(test-case "output is interleaved and kept whole"
  (define output (result-output (run-shell "echo out; echo err >&2; seq 1 100")))
  (check-equal? (string-split output "\n")
                (list* "out" "err" (for/list : (Listof String) ([n (in-range 1 101)]) (number->string n))))
  (check-true (string-suffix? output "100\n")))

(test-case "invalid UTF-8 is replaced rather than fatal"
  (check-equal? (result-output (run-shell "printf 'a\\377b'")) "a�b"))

(test-case "stdin is closed"
  (check-equal? (result-outcome (run-shell "read line")) 'false))

;; Both shapes leave a child that outlives `bash -c` unless the whole process
;; group is killed: one where the shell is still running at the timeout, and
;; one where it has exited but its child holds stdout open.
(test-case "a timeout kills the process group"
  (for ([ending (in-list '("sleep 30" "exit 0"))])
    (define marker (make-temporary-file))
    (define result
      (run-shell (format "echo started; (sleep 1; echo leaked > ~a) & ~a" marker ending)
                 #:timeout 0.5))
    (check-equal? (result-outcome result) 'timeout ending)
    (check-false (result-exit result))
    (check-equal? (result-output result) "started\n")
    (sleep 1.5)
    (check-equal? (file->string marker) "" ending)
    (delete-file marker)))

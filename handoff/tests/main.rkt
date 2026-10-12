#lang typed/racket/base
;; Running checks and inspecting repositories.

(require racket/file
         racket/string
         typed/rackunit
         "common.rkt"
         (rename-in "../model.rkt" [check make-check])
         "../main.rkt")

(test-case "repository detection accepts worktrees and rejects subdirectories"
  (call-with-repository
   (λ ([repository : Path])
     (check-true (git-repository? repository))
     (check-false (git-repository? (build-path repository "arc")))
     (run-git repository "commit" "--allow-empty" "-qm" "Initialize")
     (define worktree (build-path repository "linked"))
     (run-git repository "worktree" "add" "-q" "--detach" (path->string worktree))
     (check-true (file-exists? (build-path worktree ".git")))
     (check-true (git-repository? worktree)))))

(test-case "the project branch head has HEAD and its branch, when there are any"
  (call-with-repository
   (λ ([repository : Path])
     (define unborn (project-branch-head repository))
     (check-false (branch-head-head unborn))
     (check-true (string? (branch-head-branch unborn)))
     (run-git repository "commit" "--allow-empty" "-qm" "Initialize")
     (run-git repository "checkout" "-q" "-b" "work")
     (define on-branch (project-branch-head repository))
     (check-regexp-match #px"^[0-9a-f]{40}$" (assert (branch-head-head on-branch)))
     (check-equal? (branch-head-branch on-branch) "work")
     (run-git repository "checkout" "-q" "--detach")
     (check-equal? (project-branch-head repository) (branch-head #f (branch-head-head on-branch))))))

(define (run-shell [command : String] #:timeout [timeout : Positive-Real default-timeout])
  (run-check (make-check "x" "claim" command timeout)))

(test-case "exit codes map to outcomes"
  (check-equal? (check-result-outcome (run-shell "exit 0")) 'true)
  (check-equal? (check-result-outcome (run-shell "exit 1")) 'false)
  (for ([code (in-list '(2 3 4 255))])
    (define result (run-shell (format "exit ~a" code)))
    (check-equal? (check-result-outcome result) 'unknown)
    (check-equal? (check-result-exit result) code)))

(test-case "output is interleaved and kept whole"
  (define output (check-result-output (run-shell "echo out; echo err >&2; seq 1 100")))
  (check-equal? (string-split output "\n")
                (list* "out" "err" (for/list : (Listof String) ([n (in-range 1 101)]) (number->string n))))
  (check-true (string-suffix? output "100\n")))

(test-case "invalid UTF-8 is replaced rather than fatal"
  (check-equal? (check-result-output (run-shell "printf 'a\\377b'")) "a�b"))

(test-case "stdin is closed"
  (check-equal? (check-result-outcome (run-shell "read line")) 'false))

;; Both shapes leave a child that outlives `bash -c` unless the whole process
;; group is killed: one where the shell is still running at the timeout, and
;; one where it has exited but its child holds stdout open.
(test-case "a timeout kills the process group"
  (for ([ending (in-list '("sleep 30" "exit 0"))])
    (define marker (make-temporary-file))
    (define result
      (run-shell (format "echo started; (sleep 1; echo leaked > ~a) & ~a" marker ending)
                 #:timeout 0.5))
    (check-equal? (check-result-outcome result) 'timeout ending)
    (check-false (check-result-exit result))
    (check-equal? (check-result-output result) "started\n")
    (sleep 1.5)
    (check-equal? (file->string marker) "" ending)
    (delete-file marker)))

(test-case "output past the capture limit keeps its end and counts the rest"
  (define size (+ (* 3 capture-limit) 17))
  (define result (run-shell (format "head -c ~a /dev/zero | tr '\\0' a; echo end" size)))
  (check-equal? (check-result-outcome result) 'true)
  (check-equal? (string-length (check-result-output result)) capture-limit)
  (check-true (string-suffix? (check-result-output result) "aaaend\n"))
  (check-equal? (check-result-truncated result) (- (+ size 4) capture-limit))
  (check-equal? (check-result-truncated (run-shell "echo small")) 0))

(test-case "a timeout keeps the count of dropped output"
  (define size (* 3 capture-limit))
  (define result (run-shell (format "head -c ~a /dev/zero | tr '\\0' a; sleep 30" size) #:timeout 2))
  (check-equal? (check-result-outcome result) 'timeout)
  (check-equal? (string-length (check-result-output result)) capture-limit)
  (check-equal? (check-result-truncated result) (- size capture-limit)))

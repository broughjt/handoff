#lang typed/racket/base

(require typed/rackunit
         "common.rkt"
         "../repository.rkt")

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

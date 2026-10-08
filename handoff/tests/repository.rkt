#lang typed/racket/base

(require typed/rackunit
         "common.rkt"
         "../repository.rkt")

(test-case "repository detection accepts worktrees and rejects subdirectories"
  (call-with-repository
   (λ ([repository : Path])
     (check-true (git-repository? repository))
     (check-false (git-repository? (build-path repository "arc")))
     (run-git repository "-c" "user.name=Test" "-c" "user.email=test@example.com" "-c" "commit.gpgSign=false"
              "commit" "--allow-empty" "-qm" "Initialize")
     (define worktree (build-path repository "linked"))
     (run-git repository "worktree" "add" "-q" "--detach" (path->string worktree))
     (check-true (file-exists? (build-path worktree ".git")))
     (check-true (git-repository? worktree)))))

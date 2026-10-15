#lang typed/racket/base
;; Recording a log entry with update --begin and --commit.

(require racket/file
         racket/list
         racket/string
         typed/rackunit
         "common.rkt"
         (rename-in "../model.rkt" [check make-check])
         "../main.rkt"
         "../toml.rkt")

(define entry "# Measured the thing\n\nIt was 3.\n")

(define checks-text #<<TOML
[[check]]
id = "yes"
claim = "holds"
command = "true"

[[check]]
id = "no"
claim = "does not hold"
command = "echo failed; exit 1"
TOML
  )

(define (committed-paths [repository : Path])
  (sort (string-split (git repository "show" "--name-only" "--format=" "HEAD") "\n") string<?))

;;; Beginning and committing

(define (begin-update* [repository : Path] . [arguments : String *])
  (apply handoff* repository "update" "--begin" arguments))

(define (commit-update* [repository : Path] . [arguments : String *])
  (apply handoff* repository "update" "--commit" arguments))

(define (head-subject [repository : Path])
  (git* repository "log" "-1" "--format=%s"))

(test-case "begin records results in a pending entry, and commit commits it with the arc"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "checks.toml") checks-text)
     (run-git repository "add" "--all")
     (run-git repository "commit" "-qm" "Initialize")
     ;; A state edit, a staged root file, an unstaged root file and a sibling arc.
     (write-file (build-path arc "tasks.md") "- [ ] Measure the thing\n")
     (write-file (build-path repository "topic.md") "A fact.\n")
     (run-git repository "add" "topic.md")
     (write-file (build-path repository "shared.md") "Edited, but not staged.\n")
     (write-file (build-path repository "other" "start.toml") "")

     (define begun (begin-update* repository "arc"))
     (check-equal? (outcome-code begun) 0 (outcome-stderr begun))
     (check-equal? (outcome-stderr begun) "")
     (define entry-path (build-path arc "log" "1"))
     (check-true (string-contains? (outcome-stdout begun) "id: no\nclaim: does not hold\noutcome: false\nexit: 1\n"))
     (check-true (string-suffix?
                  (outcome-stdout begun)
                  (format "\nlog/1 results recorded. Write the entry to ~a, then run handoff update --commit arc\n"
                          (build-path entry-path "entry.md")))
                 (outcome-stdout begun))
     (define recorded (parse-check-run (read-toml (build-path entry-path "results.toml"))))
     (check-equal? (map (λ ([r : check-result]) (cons (check-id (check-result-check r)) (check-result-outcome r)))
                        (check-run-results recorded))
                   '(("yes" . true) ("no" . false)))
     (check-equal? (check-run-project-head recorded) (project-branch-head (build-path repository 'up 'up)))
     (check-equal? (directory-list entry-path) (list (string->path "results.toml")))
     (check-equal? (head-subject repository) "Initialize\n")

     (write-file (build-path entry-path "entry.md") entry)
     (define committed (commit-update* repository "arc"))
     (check-equal? (outcome-code committed) 0 (outcome-stderr committed))
     (check-equal? (outcome-stderr committed) "")
     (define stdout (outcome-stdout committed))
     (check-regexp-match #px"^log/1 committed as [0-9a-f]{7} arc: Measured the thing\n" stdout)
     (check-regexp-match #px"not committed:\n   M shared.md\n  \\?\\? other/\n$" stdout)
     (check-equal? (head-subject repository) "arc: Measured the thing\n")
     (check-equal? (committed-paths repository)
                   '("arc/log/1/entry.md" "arc/log/1/results.toml" "arc/tasks.md" "topic.md")))))

(test-case "rerunning begin replaces the pending results and keeps the entry"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "checks.toml") checks-text)
     (check-equal? (outcome-code (begin-update* repository)) 0)
     (define entry-path (build-path arc "log" "1"))
     (write-file (build-path entry-path "entry.md") entry)
     (write-file (build-path arc "checks.toml") "[[check]]\nid = \"fixed\"\nclaim = \"holds now\"\ncommand = \"true\"\n")
     (check-equal? (outcome-code (begin-update* repository)) 0)
     (check-equal? (sort (map path->string (directory-list entry-path)) string<?)
                   '("entry.md" "results.toml"))
     (check-equal? (file->string (build-path entry-path "entry.md")) entry)
     (define recorded (parse-check-run (read-toml (build-path entry-path "results.toml"))))
     (check-equal? (map (λ ([r : check-result]) (check-id (check-result-check r))) (check-run-results recorded))
                   '("fixed"))
     (check-equal? (directory-list (build-path arc "log")) (list (string->path "1")))
     (check-equal? (outcome-code (commit-update* repository)) 0))))

(test-case "each update takes the next number, and the first needs no commit before it"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "checks.toml") "")
     (for ([number (in-range 1 4)])
       (define begun (begin-update* repository))
       (check-equal? (outcome-code begun) 0 (outcome-stderr begun))
       (check-true (string-contains? (outcome-stdout begun) (format "log/~a results recorded" number)))
       (write-file (build-path arc "log" (number->string number) "entry.md") (format "# Update ~a\n" number))
       (define committed (commit-update* repository))
       (check-equal? (outcome-code committed) 0 (outcome-stderr committed)))
     (check-equal? (git repository "log" "--format=%s")
                   "arc: Update 3\narc: Update 2\narc: Update 1\n")
     (check-equal? (git repository "status" "--porcelain") "?? shared.md\n"))))

(test-case "update needs exactly one of --begin and --commit"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (for ([arguments (in-list '(("update") ("update" "--begin" "--commit")))])
       (define result (apply handoff* repository arguments))
       (check-equal? (outcome-code result) 2 (format "~a" arguments))))))

;;; Refusing

;; Run `arguments`, expecting a refusal mentioning `message` that leaves
;; every file, the index included, as it was.
(define (check-refused [repository : Path] [message : String] . [arguments : String *])
  (define before (snapshot repository))
  (define result (apply handoff* repository arguments))
  (check-equal? (outcome-code result) 2 message)
  (check-true (string-contains? (outcome-stderr result) message) (outcome-stderr result))
  (check-equal? (snapshot repository) before message))

(test-case "begin refuses on lint errors without running checks or writing"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (run-git repository "add" "shared.md")
     (delete-file (build-path arc "review.md"))
     (check-refused repository "review.md" "update" "--begin")
     (write-file (build-path arc "review.md") "")
     (write-file (build-path arc "log" "01" "entry.md") "")
     (check-refused repository "log entry number" "update" "--begin")
     (delete-directory/files (build-path arc "log"))
     (write-file (build-path arc "checks.toml") (cdr (assert (assoc "checks.toml" (new-arc-files "arc" #f)))))
     (check-refused repository "check replace-me is the placeholder" "update" "--begin"))))

(test-case "commit refuses without a pending entry, a headline or a clean lint"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "checks.toml") "")
     (run-git repository "add" "shared.md")
     (check-refused repository "no update in progress" "update" "--commit")
     (check-equal? (outcome-code (begin-update* repository)) 0)
     (define entry-path (build-path arc "log" "1" "entry.md"))
     (check-refused repository "write the entry" "update" "--commit")
     (write-file entry-path "")
     (check-refused repository "empty" "update" "--commit")
     (write-file entry-path "No heading\n")
     (check-refused repository "headline" "update" "--commit")
     (write-file entry-path entry)
     (delete-file (build-path arc "review.md"))
     (check-refused repository "review.md" "update" "--commit")
     (check-false (head-subject repository)))))

(test-case "commit refuses results that no longer describe the handoff"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "checks.toml") checks-text)
     (check-equal? (outcome-code (begin-update* repository)) 0)
     (write-file (build-path arc "log" "1" "entry.md") entry)
     (write-file (build-path arc "checks.toml") (string-replace checks-text "exit 1" "exit 3"))
     (check-refused repository "checks.toml changed" "update" "--commit")
     (write-file (build-path arc "checks.toml") checks-text)
     (run-git (build-path repository 'up 'up) "commit" "--allow-empty" "-qm" "Project work")
     (check-refused repository "the project moved" "update" "--commit")
     (check-equal? (outcome-code (begin-update* repository)) 0)
     (check-equal? (outcome-code (commit-update* repository)) 0))))

(test-case "a failed commit restores the index and keeps the pending entry"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "checks.toml") "")
     (define hook (build-path repository ".git" "hooks" "pre-commit"))
     (write-file hook "#!/bin/sh\necho refused by hook >&2\nexit 1\n")
     (file-or-directory-permissions hook #o755)
     (run-git repository "add" "shared.md")
     (check-equal? (outcome-code (begin-update* repository)) 0)
     (define entry-path (build-path arc "log" "1"))
     (write-file (build-path entry-path "entry.md") entry)
     (define index (git repository "write-tree"))
     (define result (commit-update* repository))
     (check-equal? (outcome-code result) 2)
     (check-true (string-contains? (outcome-stderr result) "refused by hook") (outcome-stderr result))
     (check-equal? (sort (map path->string (directory-list entry-path)) string<?)
                   '("entry.md" "results.toml"))
     (check-equal? (git repository "write-tree") index)
     (check-false (head-subject repository))
     (delete-file hook)
     (check-equal? (outcome-code (commit-update* repository)) 0))))

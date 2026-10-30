#lang typed/racket/base
;; A clean arc, for tests to vary.

(require "../main.rkt")

(provide run
         workflow
         clean
         with-text)

(require/typed gregor
  [iso8601->moment (-> String Moment)])

(define run
  (check-run "murph" (iso8601->moment "2026-10-29T18:00:00Z") 1 "main" "abc123" null))

(define summarized "# Title\n\nWhat this covers.\n")

(define workflow "<!-- created for this arc on 2026-10-29 -->\n# Workflow\n\nHow we work.\n")

(define clean
  (arc "a"
       (start "n"
              (scope-path 'arc (string->path "workflow.md"))
              (list (read-first (scope-path 'project (string->path "src/main.rkt"))
                                "the code")))
       (list (check "tests" "The tests pass" "true" 60))
       (for/fold ([texts : (Immutable-HashTable String String)
                         (hash "start.toml" "next = 'n'\n")])
                 ([file (in-list (list "arc.md" "tasks.md" "do-not-reopen.md"
                                       "open-questions.md" "review.md"))])
         (hash-set texts file summarized))
       workflow
       null
       (list (cons (scope-path 'arc (string->path "workflow.md")) workflow)
             (cons (scope-path 'arc (string->path "notes/")) #f)
             (cons (scope-path 'handoff (string->path "topic.md")) summarized))
       (list (log-entry 1 #t "# First\n" run)
             (log-entry 2 #t "# Second\n" run))
       null))

(: with-text (-> arc String String arc))
(define (with-text parsed file text)
  (struct-copy arc parsed [state-texts (hash-set (arc-state-texts parsed) file text)]))

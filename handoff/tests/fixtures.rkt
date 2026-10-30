#lang typed/racket/base
;; A clean arc's files, for tests to vary.

(require racket/function
         racket/port
         "../main.rkt")

(provide run
         results
         workflow
         clean
         with-text)

(require/typed gregor
  [iso8601->moment (-> String Moment)])

(define run
  (check-run "murph" (iso8601->moment "2026-10-29T18:00:00Z") 1 "main" "abc123" null))

(define results (with-output-to-string (thunk (write-check-run run))))

(define summarized "# Title\n\nWhat this covers.\n")

(define workflow "<!-- created for this arc on 2026-10-29 -->\n# Workflow\n\nHow we work.\n")

(define clean
  (arc-files "a"
             (list* (cons "start.toml"
                          "next = 'n'
                           workflow = 'arc:workflow.md'

                           [[read-first]]
                           path = 'project:src/main.rkt'
                           reason = 'the code'")
                    (cons "checks.toml"
                          "[[check]]
                           id = 'tests'
                           claim = 'The tests pass'
                           command = 'true'")
                    (for/list : (Listof (Pairof String (U String #f)))
                              ([file (in-list (list "arc.md" "tasks.md" "do-not-reopen.md"
                                                    "open-questions.md" "review.md"))])
                      (cons file summarized)))
             workflow
             (list (cons (scope-path 'arc (string->path "workflow.md")) workflow)
                   (cons (scope-path 'arc (string->path "notes/")) #f)
                   (cons (scope-path 'handoff (string->path "topic.md")) summarized))
             (list (log-entry-files "1" #t "# First\n" results)
                   (log-entry-files "2" #t "# Second\n" results))
             (list "1" "2")
             (baseline-files 2 results)
             (list (cons (scope-path 'arc (string->path "workflow.md")) #t)
                   (cons (scope-path 'project (string->path "src/main.rkt")) #t))))

(: with-text (-> arc-files String (U String #f) arc-files))
(define (with-text files file text)
  (struct-copy arc-files files
               [texts (for/list : (Listof (Pairof String (U String #f)))
                                ([entry (in-list (arc-files-texts files))])
                        (if (equal? (car entry) file) (cons file text) entry))]))

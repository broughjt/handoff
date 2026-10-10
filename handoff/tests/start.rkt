#lang typed/racket/base

(require typed/rackunit
         "../error.rkt"
         "../start.rkt"
         "../toml.rkt")

(define base : Table
  (hasheq 'next "Continue" 'workflow "arc:workflow.md" 'read_first '()))

(test-case "a multiline next and read-first entries"
  (define start*
    (parse-start (hash-set* base
                            'next "Discuss the plan.\nThen implement."
                            'workflow "handoff:shared.md"
                            'read_first (list (hasheq 'path "project:handoff/main.rkt" 'reason "Entry point")))))
  (check-equal? (start-next start*) "Discuss the plan.\nThen implement.")
  (check-equal? (start-workflow start*) (scope-path 'handoff (string->path "shared.md")))
  (check-equal? (start-read-first start*)
                (list (read-first (scope-path 'project (string->path "handoff/main.rkt")) "Entry point"))))

(test-case "invalid fields and references are rejected"
  (for ([changes (in-list (ann (list '((next . " ")) '((next . #f)) '((extra . "unknown"))
                                '((workflow . "workflow.md")) '((workflow . "arc:/absolute"))
                                '((workflow . "handoff:../escape")) '((workflow . "handoff:a/../../b"))
                                '((workflow . "arc:")) '((workflow . "other:file"))
                                (list (cons 'read_first (hasheq))) '((read_first . ("file")))
                                (list (cons 'read_first (list (hasheq 'path "arc:context.md"))))
                                (list (cons 'read_first (list (hasheq 'path "arc:context.md" 'reason "")))))
                               (Listof (Listof (Pairof Symbol Any)))))])
    (define table (for/fold ([table : Table base]) ([change (in-list changes)])
                    (hash-set table (car change) (cdr change))))
    (check-exn exn:fail:handoff? (λ () (parse-start table)) (format "~s" changes)))
  (for ([field (in-list '(next workflow read_first))])
    (check-exn exn:fail:handoff? (λ () (parse-start (hash-remove base field))) (symbol->string field))))

(test-case "references resolve against their scope"
  (define arc (string->path "/repository/arc"))
  (define repository (string->path "/repository"))
  (define project (string->path "/project"))
  (check-equal? (resolve-scope-path (parse-scope-path "arc:notes/a.md") #:arc arc #:repository repository #:project project)
                (string->path "/repository/arc/notes/a.md"))
  (check-equal? (resolve-scope-path (parse-scope-path "handoff:shared.md") #:arc arc #:repository repository #:project project)
                (string->path "/repository/shared.md")))

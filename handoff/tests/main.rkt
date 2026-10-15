#lang typed/racket/base
;; Parsing the command line.

(require typed/rackunit
         "../main.rkt")

(define (parse . [arguments : String *])
  (parse-arguments (list->vector arguments)))

(test-case "a command without an arc leaves it to be selected"
  (check-equal? (parse "start") (invocation #f (start-subcommand #f))))

(test-case "a command takes an arc"
  (check-equal? (parse "check" "v4") (invocation #f (check-subcommand "v4"))))

(test-case "the repository option comes before the command"
  (check-equal? (parse "--repository" "/elsewhere" "lint")
                (invocation (string->path "/elsewhere") (lint-subcommand #f))))

(test-case "list takes --archived"
  (check-equal? (parse "list" "--archived") (invocation #f (list-subcommand #t))))

(test-case "update takes exactly one of --begin and --commit"
  (check-equal? (parse "update" "--begin" "v4") (invocation #f (update-subcommand 'begin "v4")))
  (check-equal? (parse "update" "--commit") (invocation #f (update-subcommand 'commit #f)))
  (check-exn #rx"update requires --begin or --commit" (λ () (parse "update")))
  (check-exn exn:fail:user? (λ () (parse "update" "--begin" "--commit"))))

(test-case "new takes an arc and an optional workflow"
  (check-equal? (parse "new" "v5") (invocation #f (new-subcommand "v5" #f)))
  (check-equal? (parse "new" "v5" "spike") (invocation #f (new-subcommand "v5" "spike"))))

(test-case "workflow has its own verbs"
  (check-equal? (parse "workflow" "list") (invocation #f (workflow-list-subcommand)))
  (check-equal? (parse "workflow" "install" "spike") (invocation #f (workflow-install-subcommand "spike")))
  (check-equal? (parse "workflow" "fork" "v4" "mine" "handoff:workflow-spike.md")
                (invocation #f (workflow-fork-subcommand "v4" "mine" "handoff:workflow-spike.md")))
  (check-exn #rx"unknown command workflow remove" (λ () (parse "workflow" "remove"))))

(test-case "guide takes no arguments"
  (check-equal? (parse "guide") (invocation #f (guide-subcommand)))
  (check-exn exn:fail:user? (λ () (parse "guide" "extra"))))

(test-case "an unknown command is an error"
  (check-exn #rx"unknown command archive" (λ () (parse "archive"))))

(test-case "extra arguments are an error"
  (check-exn exn:fail:user? (λ () (parse "start" "v4" "v5"))))

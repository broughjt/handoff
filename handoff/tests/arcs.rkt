#lang typed/racket/base
;; Parsing and linting arcs.

(require racket/function
         racket/port
         (except-in typed/rackunit check)
         "../error.rkt"
         "../main.rkt")

(require/typed gregor
  [iso8601->moment (-> String Moment)])

(define source (string->path "a/start.toml"))

(define results
  (with-output-to-string
    (thunk (write-check-run (check-run "murph"
                                       (iso8601->moment "2026-10-29T18:00:00Z")
                                       1
                                       "main"
                                       "abc123"
                                       null)))))

(define summarized "# Title\n\nWhat this covers.\n")

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
             (list (cons (scope-path 'arc (string->path "workflow.md"))
                         "<!-- created for this arc on 2026-10-29 -->\n# Workflow\n\nHow we work.\n")
                   (cons (scope-path 'arc (string->path "notes/")) #f)
                   (cons (scope-path 'handoff (string->path "topic.md")) summarized))
             (list (log-entry-files "1" #t "# First\n" results)
                   (log-entry-files "2" #t "# Second\n" results))
             (list "1" "2")
             (list (cons (scope-path 'arc (string->path "workflow.md")) #t)
                   (cons (scope-path 'project (string->path "src/main.rkt")) #t))))

(: with-text (-> arc-files String (U String #f) arc-files))
(define (with-text files file text)
  (struct-copy arc-files files
               [texts (for/list : (Listof (Pairof String (U String #f)))
                                ([entry (in-list (arc-files-texts files))])
                        (if (equal? (car entry) file) (cons file text) entry))]))

(: parse-lint (-> arc-files (Listof String)))
(define (parse-lint files)
  (lint files (parse-arc files)))

(test-case "start.toml parses into next, the workflow, and the read-first entries"
  (check-equal? (parse-start "next = 'Do the thing'
                              workflow = 'handoff:workflow-spike.md'

                              [[read-first]]
                              path = 'project:src/main.rkt'
                              reason = 'the code'"
                             source)
                (start "Do the thing"
                       (scope-path 'handoff (string->path "workflow-spike.md"))
                       (list (read-first (scope-path 'project (string->path "src/main.rkt"))
                                         "the code")))))

(test-case "start.toml may have no read-first entries"
  (check-equal? (start-read-first (parse-start "next = 'n'
                                                workflow = 'arc:w.md'
                                                read-first = []"
                                               source))
                null))

(test-case "start.toml without a workflow is rejected"
  (check-exn #rx"a/start.toml is missing workflow"
             (thunk (parse-start "next = 'n'
                                  read-first = []"
                                 source))))

(test-case "start.toml with an unknown field is rejected"
  (check-exn #rx"a/start.toml has unknown fields read_first"
             (thunk (parse-start "next = 'n'
                                  workflow = 'arc:w.md'
                                  read-first = []
                                  read_first = []"
                                 source))))

(test-case "a read-first entry without a reason is rejected"
  (check-exn #rx"a/start.toml: read-first\\[0\\] is missing reason"
             (thunk (parse-start "next = 'n'
                                  workflow = 'arc:w.md'

                                  [[read-first]]
                                  path = 'arc:plan.md'"
                                 source))))

(test-case "a scope path is a scope and a relative path"
  (check-equal? (map parse-scope-path (list "arc:plan.md" "handoff:notes/a.md" "project:src/main.rkt"))
                (list (scope-path 'arc (string->path "plan.md"))
                      (scope-path 'handoff (string->path "notes/a.md"))
                      (scope-path 'project (string->path "src/main.rkt")))))

(test-case "a scope path needs a known scope, a nonempty relative path and no .."
  (check-equal? (map parse-scope-path (list "home:a.md" "arc:" "arc:/etc/passwd" "project:src/../../a.md"))
                (list #f #f #f #f)))

(test-case "a log entry's name is a positive integer without leading zeros"
  (check-equal? (map parse-log-entry-name (list "1" "10" "01" "0" "x"))
                (list 1 10 #f #f #f)))

(test-case "an arc parses with its log entries in ascending order"
  (define parsed
    (parse-arc (struct-copy arc-files clean
                            [log (list (log-entry-files "10" #t "# Tenth\n" results)
                                       (log-entry-files "9" #t "# Ninth\n" results))]
                            [committed-log (list "9" "10")])))
  (check-equal? (arc-log-entries parsed) (list 9 10))
  (check-false (arc-pending parsed)))

(test-case "the highest log entry is pending when it is not committed"
  (check-equal? (arc-pending (parse-arc (struct-copy arc-files clean [committed-log (list "1")])))
                2))

(test-case "an arc missing a state file does not parse"
  (check-exn #rx"a/review.md: expected a regular file"
             (thunk (parse-arc (with-text clean "review.md" #f)))))

(test-case "an arc with two broken files raises with both messages"
  (check-equal? (with-handlers ([exn:fail:handoff:arc? exn:fail:handoff:arc-messages])
                  (parse-arc (with-text (with-text clean "start.toml" #f) "review.md" #f))
                  null)
                (list "a/start.toml: expected a regular file"
                      "a/review.md: expected a regular file")))

(test-case "a clean arc has no lint errors"
  (check-equal? (parse-lint clean) null))

(test-case "lint reports a reference that is not a regular file"
  (check-equal? (parse-lint (struct-copy arc-files clean
                                         [references (list (cons (scope-path 'arc (string->path "gone.md")) #f))]))
                (list "a/start.toml: arc:gone.md is not a regular file")))

(test-case "lint reports the placeholder check"
  (check-equal? (parse-lint (with-text clean
                                       "checks.toml"
                                       "[[check]]
                                        id = 'replace-me'
                                        claim = 'c'
                                        command = 'exit 3'"))
                (list "a/checks.toml: check replace-me is the placeholder from handoff new; replace it with the arc's checks")))

(test-case "lint reports a document whose summary is a list"
  (check-equal? (parse-lint (struct-copy arc-files clean
                                         [documents (list (cons (scope-path 'arc (string->path "plan.md"))
                                                                "# Plan\n\n- a list\n"))]))
                (list "a/plan.md: expected a paragraph of prose after the opening headings, not a list, code block, table, or HTML")))

(test-case "lint does not require a summary of a verbatim workflow copy"
  (check-equal? (parse-lint (struct-copy arc-files clean
                                         [documents (list (cons (scope-path 'handoff (string->path "workflow-spike.md"))
                                                                "<!-- copied from workflow spike (sha256:00) on 2026-10-29; verbatim -->\n# Spike\n"))]))
                null))

(test-case "lint reports a name in the log that is not an entry number"
  (check-equal? (parse-lint (struct-copy arc-files clean
                                         [log (list (log-entry-files "notes.md" #f #f #f))]
                                         [committed-log null]))
                (list "a/log/notes.md: expected a log entry number, a positive integer without leading zeros")))

(test-case "lint reports a committed entry without entry.md"
  (check-equal? (parse-lint (struct-copy arc-files clean
                                         [log (list (log-entry-files "1" #t #f results))]
                                         [committed-log (list "1")]))
                (list "a/log/1/entry.md: expected a regular file")))

(test-case "lint reports an entry.md that does not open with a heading"
  (check-equal? (parse-lint (struct-copy arc-files clean
                                         [log (list (log-entry-files "1" #t "Text first\n# Heading\n" results))]
                                         [committed-log (list "1")]))
                (list "a/log/1/entry.md: expected a heading on its first nonblank line")))

(test-case "lint reports an older entry whose results.toml does not parse"
  (check-equal? (parse-lint (struct-copy arc-files clean
                                         [log (list (log-entry-files "1" #t "# First\n" "[run]")
                                                    (log-entry-files "2" #t "# Second\n" results))]))
                (list "a/log/1/results.toml is missing project")))

(test-case "lint reports log entries in numeric order"
  (check-equal? (parse-lint (struct-copy arc-files clean
                                         [log (list (log-entry-files "10" #t "# Tenth\n" "[run]")
                                                    (log-entry-files "2" #t "# Second\n" "[run]"))]
                                         [committed-log (list "2" "10")]))
                (list "a/log/2/results.toml is missing project"
                      "a/log/10/results.toml is missing project")))

(test-case "a pending entry may lack entry.md and results.toml"
  (check-equal? (parse-lint (struct-copy arc-files clean
                                         [log (list (log-entry-files "1" #t "# First\n" results)
                                                    (log-entry-files "2" #t #f #f))]
                                         [committed-log (list "1")]))
                null))

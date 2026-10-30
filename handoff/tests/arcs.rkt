#lang typed/racket/base
;; Parsing and linting arcs.

(require racket/function
         racket/list
         (except-in typed/rackunit check)
         "../error.rkt"
         "../main.rkt"
         "fixtures.rkt")

(define source (string->path "a/start.toml"))

(: parse-lint (-> arc-files (Listof String)))
(define (parse-lint files)
  (lint files (parse-arc files)))

(: omit (-> String (Pairof String Natural)))
(define (omit text)
  (call-with-values (thunk (omit-finished-tasks text)) (inst cons String Natural)))

(: sections (-> arc-files (U Positive-Integer #f) (Listof (Pairof String String))))
(define (sections files entry)
  (inlined-sections files (parse-arc files) entry))

;; `clean` with tasks.md padded so that the report inlines `total` bytes.
(: padded (-> Natural arc-files))
(define (padded total)
  (define others
    (for/sum : Integer ([section (in-list (sections clean 2))]
                        #:unless (equal? (car section) "arc:tasks.md"))
      (bytes-length (string->bytes/utf-8 (cdr section)))))
  (with-text clean "tasks.md" (make-string (assert (- total others) exact-nonnegative-integer?) #\a)))

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

(test-case "the latest log entry is the highest entry number, ignoring other names"
  (check-equal? (latest-log-entry (list "2" "10" "9" "notes.md" "011")) 10))

(test-case "a log without entry numbers has no latest entry"
  (check-false (latest-log-entry (list "notes.md"))))

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

(test-case "lint reports nothing when the report inlines exactly the budget"
  (check-equal? (parse-lint (padded 24000)) null))

(test-case "lint reports a report over the budget"
  (check-equal? (length (parse-lint (padded 24001))) 1))

(test-case "the budget diagnostic lists the three largest sections, largest first"
  (check-regexp-match
   #rx"^a: the report inlines 25\\.[0-9] KB, over the 24 KB budget; largest: arc:tasks.md 20\\.0 KB, arc:do-not-reopen.md 3\\.0 KB, arc:review.md 2\\.0 KB$"
   (first (parse-lint (with-text (with-text (with-text clean "tasks.md" (make-string 20000 #\a))
                                            "review.md" (make-string 2000 #\a))
                                 "do-not-reopen.md" (make-string 3000 #\a))))))

(test-case "finished tasks are left out with the lines that continue them, and counted"
  (check-equal? (omit "- [x] Done\n  detail\n- [ ] Open\n  detail\n- [x] Also done\n")
                (cons "- [ ] Open\n  detail\n" 2)))

(test-case "unfinished tasks and headings are kept"
  (check-equal? (omit "# Tasks\n\n- [ ] Open\n")
                (cons "# Tasks\n\n- [ ] Open\n" 0)))

(test-case "a task checked with a capital X is finished"
  (check-equal? (omit "- [X] Done\n") (cons "" 1)))

(test-case "the report inlines the arc's files in order, with the given entry last"
  (check-equal? (map (inst car String String) (sections clean 2))
                (list "arc:start.toml" "arc:arc.md" "arc:workflow.md" "arc:tasks.md"
                      "arc:do-not-reopen.md" "arc:open-questions.md" "arc:review.md"
                      "arc:log/2/entry.md")))

(test-case "the workflow is inlined under the reference start.toml gives"
  (check-equal? (assoc "handoff:workflow-x.md"
                       (sections (struct-copy arc-files
                                              (with-text clean "start.toml" "next = 'n'
                                                                             workflow = 'handoff:workflow-x.md'
                                                                             read-first = []")
                                              [workflow "W"])
                                 2))
                (cons "handoff:workflow-x.md" "W")))

(test-case "a missing file is left out of the report"
  (check-false (assoc "arc:workflow.md" (sections (struct-copy arc-files clean [workflow #f]) 2))))

(test-case "the report inlines the entry it is given"
  (check-equal? (assoc "arc:log/1/entry.md" (sections clean 1))
                (cons "arc:log/1/entry.md" "# First\n")))

(test-case "the report says how many finished tasks it left out"
  (check-equal? (assoc "arc:tasks.md" (sections (with-text clean "tasks.md" "- [x] Done\n- [ ] Open\n") 2))
                (cons "arc:tasks.md" "- [ ] Open\n(1 finished task left out; they are in arc:tasks.md)\n")))

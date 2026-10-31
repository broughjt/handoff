#lang typed/racket/base
;; Parsing and linting arcs.

(require racket/function
         racket/list
         (except-in typed/rackunit check)
         "../main.rkt"
         "fixtures.rkt")

(define source (string->path "a/start.toml"))

;; A new arc as `handoff new` writes it without a workflow, whose placeholder
;; workflow does not exist.
(define new-arc
  (let ([files (new-arc-files "a" placeholder-workflow)])
    (: text (-> String String))
    (define (text file)
      (cdr (assert (assoc file files))))
    (arc "a"
         (parse-start (text "start.toml") source)
         (parse-checks (text "checks.toml") (string->path "a/checks.toml"))
         (for/hash : (Immutable-HashTable String String)
                   ([file (in-list (list "start.toml" "arc.md" "tasks.md" "do-not-reopen.md"
                                         "open-questions.md" "review.md"))])
           (values file (text file)))
         #f
         (list placeholder-workflow)
         null
         null
         null)))

(: omit (-> String (Pairof String Natural)))
(define (omit text)
  (call-with-values (thunk (omit-finished-tasks text)) (inst cons String Natural)))

(: sections (-> arc (U Positive-Integer #f) (Listof (Pairof String String))))
(define (sections arc log-entry-number*)
  (inlined-sections arc
                    (findf (λ ([entry : log-entry])
                             (eqv? (log-entry-number entry) log-entry-number*))
                           (arc-log arc))))

;; `clean` with tasks.md padded so that the report inlines `total` bytes.
(: padded (-> Natural arc))
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

(test-case "the highest log entry is pending when it is not committed"
  (check-equal? (find-pending-log-entry (list (log-entry 1 #t "# First\n" run) (log-entry 2 #f #f #f)))
                (log-entry 2 #f #f #f)))

(test-case "no log entry is pending when the highest is committed"
  (check-false (find-pending-log-entry (arc-log clean))))

(test-case "the first log entry is 1"
  (check-equal? (next-log-entry-number null) 1))

(test-case "the next log entry is one past the highest when none is pending"
  (check-equal? (next-log-entry-number (arc-log clean)) 3))

(test-case "the next log entry is the pending one"
  (check-equal? (next-log-entry-number (list (log-entry 1 #t "# First\n" run) (log-entry 2 #f #f #f)))
                2))

(test-case "the baseline is the highest committed log entry"
  (check-true (equal? (find-baseline "a" (list (log-entry 1 #t "# First\n" run) (log-entry 2 #f #f #f)))
                      (cons 1 run))))

(test-case "a baseline whose results.toml does not parse is rejected"
  (check-exn #rx"^a/log/1/results.toml is missing project$"
             (thunk (find-baseline "a" (list (log-entry 1 #t "# First\n" "a/log/1/results.toml is missing project"))))))

(test-case "a baseline without results.toml is rejected"
  (check-exn #rx"^a/log/1/results.toml: expected a regular file$"
             (thunk (find-baseline "a" (list (log-entry 1 #t "# First\n" #f))))))

(test-case "a clean arc has no lint errors"
  (check-equal? (lint clean) null))

(test-case "lint reports a reference that is not a regular file"
  (check-equal? (lint (struct-copy arc clean [missing-references (list (scope-path 'arc (string->path "gone.md")))]))
                (list "a/start.toml: arc:gone.md is not a regular file")))

(test-case "lint reports the placeholder next"
  (check-equal? (lint (struct-copy arc clean [start (struct-copy start (arc-start clean) [next "replace-me"])]))
                (list "a/start.toml: next is the placeholder from handoff new; write the arc's next action")))

(test-case "lint reports each placeholder of a new arc"
  (check-equal? (lint new-arc)
                (list "a/start.toml: next is the placeholder from handoff new; write the arc's next action"
                      "a/start.toml: arc:workflow-replace-me.md is not a regular file"
                      "a/checks.toml: check replace-me is the placeholder from handoff new; replace it with the arc's checks")))

(test-case "lint reports the placeholder check"
  (check-equal? (lint (struct-copy arc clean [checks (list (check "replace-me" "c" "exit 3" 60))]))
                (list "a/checks.toml: check replace-me is the placeholder from handoff new; replace it with the arc's checks")))

(test-case "lint reports a document whose summary is a list"
  (check-equal? (lint (struct-copy arc clean
                                   [documents (list (cons (scope-path 'arc (string->path "plan.md"))
                                                    "# Plan\n\n- a list\n"))]))
                (list "a/plan.md: expected a paragraph of prose after the opening headings, not a list, code block, table, or HTML")))

(test-case "lint does not require a summary of a verbatim workflow copy"
  (check-equal? (lint (struct-copy arc clean
                                   [documents (list (cons (scope-path 'handoff (string->path "workflow-spike.md"))
                                                    "<!-- copied from workflow spike (sha256:00) on 2026-10-29; verbatim -->\n# Spike\n"))]))
                null))

(test-case "lint reports a name in the log that is not an entry number"
  (check-equal? (lint (struct-copy arc clean [invalid-log-entries (list "notes.md")]))
                (list "a/log/notes.md: expected a log entry number, a positive integer without leading zeros")))

(test-case "lint reports an entry number in the log that is not a directory"
  (check-equal? (lint (struct-copy arc clean [invalid-log-entries (list "3")]))
                (list "a/log/3: expected a directory")))

(test-case "lint reports a committed entry without entry.md"
  (check-equal? (lint (struct-copy arc clean [log (list (log-entry 1 #t #f run))]))
                (list "a/log/1/entry.md: expected a regular file")))

(test-case "lint reports an entry.md that does not open with a heading"
  (check-equal? (lint (struct-copy arc clean [log (list (log-entry 1 #t "Text first\n# Heading\n" run))]))
                (list "a/log/1/entry.md: expected a heading on its first nonblank line")))

(test-case "lint reports an entry whose results.toml does not parse"
  (check-equal? (lint (struct-copy arc clean
                                   [log (list (log-entry 1 #t "# First\n" "a/log/1/results.toml is missing project")
                                              (log-entry 2 #t "# Second\n" run))]))
                (list "a/log/1/results.toml is missing project")))

(test-case "lint reports a committed entry without results.toml"
  (check-equal? (lint (struct-copy arc clean [log (list (log-entry 1 #t "# First\n" #f))]))
                (list "a/log/1/results.toml: expected a regular file")))

(test-case "a pending entry may lack entry.md and results.toml"
  (check-equal? (lint (struct-copy arc clean
                                   [log (list (log-entry 1 #t "# First\n" run)
                                              (log-entry 2 #f #f #f))]))
                null))

(test-case "lint reports nothing when the report inlines exactly the budget"
  (check-equal? (lint (padded 24000)) null))

(test-case "lint reports a report over the budget"
  (check-equal? (length (lint (padded 24001))) 1))

(test-case "the budget diagnostic lists the three largest sections, largest first"
  (check-regexp-match
   #rx"^a: the report inlines 25\\.[0-9] KB, over the 24 KB budget; largest: arc:tasks.md 20\\.0 KB, arc:do-not-reopen.md 3\\.0 KB, arc:review.md 2\\.0 KB$"
   (first (lint (with-text (with-text (with-text clean "tasks.md" (make-string 20000 #\a))
                                            "review.md" (make-string 2000 #\a))
                                 "do-not-reopen.md" (make-string 3000 #\a))))))

(test-case "finished tasks are left out with the lines that continue them, and counted"
  (check-equal? (omit "- [x] Done\n  detail\n- [ ] Open\n  detail\n- [x] Also done\n")
                (cons "- [ ] Open\n  detail\n" 2)))

(test-case "unfinished tasks and headings are kept"
  (check-equal? (omit "# Tasks\n\n- [ ] Open\n")
                (cons "# Tasks\n\n- [ ] Open\n" 0)))

(test-case "a blank line left beside another where finished tasks were left out is dropped"
  (check-equal? (omit "### Tests\n\n- [x] Done\n\n### Rewrite\n")
                (cons "### Tests\n\n### Rewrite\n" 1)))

(test-case "blank lines not left beside each other by leaving out tasks are kept"
  (check-equal? (omit "# Tasks\n\n\n- [ ] Open\n- [x] Done\n\n## Next\n")
                (cons "# Tasks\n\n\n- [ ] Open\n\n## Next\n" 1)))

(test-case "a task checked with a capital X is finished"
  (check-equal? (omit "- [X] Done\n") (cons "" 1)))

(test-case "the report inlines the arc's files in order, with the given entry last"
  (check-equal? (map (inst car String String) (sections clean 2))
                (list "arc:start.toml" "arc:arc.md" "arc:workflow.md" "arc:tasks.md"
                      "arc:do-not-reopen.md" "arc:open-questions.md" "arc:review.md"
                      "arc:log/2/entry.md")))

(test-case "the workflow is inlined under the reference start.toml gives"
  (check-equal? (assoc "handoff:workflow-x.md"
                       (sections (struct-copy arc clean
                                              [start (start "n" (scope-path 'handoff (string->path "workflow-x.md")) null)]
                                              [workflow-text "W"])
                                 2))
                (cons "handoff:workflow-x.md" "W")))

(test-case "a missing file is left out of the report"
  (check-false (assoc "arc:workflow.md" (sections (struct-copy arc clean [workflow-text #f]) 2))))

(test-case "the report inlines the entry it is given"
  (check-equal? (assoc "arc:log/1/entry.md" (sections clean 1))
                (cons "arc:log/1/entry.md" "# First\n")))

(test-case "the report says how many finished tasks it left out"
  (check-equal? (assoc "arc:tasks.md" (sections (with-text clean "tasks.md" "- [x] Done\n- [ ] Open\n") 2))
                (cons "arc:tasks.md" "- [ ] Open\n(1 finished task left out; they are in arc:tasks.md)\n")))

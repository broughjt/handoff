#lang typed/racket/base
;; Printing the start report.

(require racket/function
         racket/list
         racket/port
         racket/string
         (except-in typed/rackunit check)
         "../main.rkt"
         "fixtures.rkt")

(define head "0123456789abcdef")

(define recorded (struct-copy check-run run [head head]))

(define here (project-status (string->path "/p") "main" head (history null null)))

(: project-output (-> project-status (U (Pairof Positive-Integer check-run) #f) String))
(define (project-output project-status baseline)
  (with-output-to-string
    (thunk (print-project-repository project-status baseline))))

(: with-history (-> (Listof String) (Listof String) project-status))
(define (with-history only-recorded only-current)
  (struct-copy project-status here [history (history only-recorded only-current)]))

(: repository-output (-> (Listof String) (U log-entry #f) (Listof String) String))
(define (repository-output diagnostics pending dirty)
  (with-output-to-string
    (thunk (print-handoff-repository diagnostics pending dirty))))

(: with-pending (-> log-entry arc))
(define (with-pending entry)
  (struct-copy arc clean [log (list (log-entry 1 #t "# First\n" run) entry)]))

(: other-documents-output (-> (Listof (Pairof scope-path String)) String))
(define (other-documents-output documents)
  (with-output-to-string (thunk (print-other-documents documents))))

(: report (-> arc (U (Pairof Positive-Integer check-run) #f) start-report))
(define (report arc baseline)
  (build-start-report (string->path "/p/.scratch/handoff")
                      arc
                      baseline
                      null
                      here
                      null))

(test-case "the project section says when nothing was committed since the baseline"
  (check-equal? (project-output here (cons 1 recorded))
                (string-append "now: main at 0123456789ab\n"
                               "at log/1: main at 0123456789ab\n"
                               "no new commits\n")))

(test-case "the project section lists commits since the baseline"
  (check-equal? (project-output (with-history null (list "aaa One" "bbb Two")) (cons 1 recorded))
                (string-append "now: main at 0123456789ab\n"
                               "at log/1: main at 0123456789ab\n"
                               "commits since log/1:\n"
                               "  aaa One\n"
                               "  bbb Two\n")))

(test-case "the project section marks each side of a rewritten history"
  (check-equal? (project-output (with-history (list "aaa Old") (list "bbb New")) (cons 1 recorded))
                (string-append "now: main at 0123456789ab\n"
                               "at log/1: main at 0123456789ab\n"
                               "history rewritten since log/1; < marks commits only in the recorded history, > only in the current one:\n"
                               "  < aaa Old\n"
                               "  > bbb New\n")))

(test-case "the project section says when the recorded head does not resolve"
  (check-equal? (project-output (struct-copy project-status here [history 'unresolvable]) (cons 1 recorded))
                (string-append "now: main at 0123456789ab\n"
                               "at log/1: main at 0123456789ab\n"
                               "recorded head does not resolve here: its history was rewritten and pruned, or it was committed on another machine\n")))

(test-case "the project section says when the branch differs from the baseline's"
  (check-equal? (project-output (struct-copy project-status here [branch "topic"]) (cons 1 recorded))
                (string-append "now: topic at 0123456789ab\n"
                               "at log/1: main at 0123456789ab\n"
                               "the branch differs from log/1\n"
                               "no new commits\n")))

(test-case "without a baseline the project section gives only the head"
  (check-equal? (project-output here #f) "now: main at 0123456789ab\n"))

(test-case "a detached project without commits"
  (check-equal? (project-output (project-status (string->path "/p") #f #f (history null null)) #f)
                "now: detached with no commits\n"))

(test-case "the project section lists at most 20 commits"
  (define lines (for/list : (Listof String) ([n (in-range 25)]) (format "c~a" n)))
  (check-equal? (take-right (string-split (project-output (with-history null lines) (cons 1 recorded)) "\n") 2)
                (list "  c19" "  and 5 more")))

(test-case "the handoff repository section of a clean repository"
  (check-equal? (repository-output null #f null)
                (string-append "lint: no errors\n"
                               "pending log entry: none\n"
                               "uncommitted: none\n")))

(test-case "the handoff repository section lists the diagnostics"
  (check-equal? (repository-output (list "a/x.md: bad" "a/y.md: worse") #f null)
                (string-append "lint: 2 errors\n"
                               "  a/x.md: bad\n"
                               "  a/y.md: worse\n"
                               "pending log entry: none\n"
                               "uncommitted: none\n")))

(test-case "the handoff repository section describes a pending entry's results and entry.md"
  (check-equal? (repository-output null (log-entry 2 #f "# Second\n" run) null)
                (string-append "lint: no errors\n"
                               "pending log entry: log/2 was begun but not committed, so it is not the baseline\n"
                               "  results: recorded on murph at 2026-10-29T18:00:00Z\n"
                               "  entry.md: written\n"
                               "uncommitted: none\n")))

(test-case "the handoff repository section says when a pending entry's results do not parse"
  (check-regexp-match #rx"\n  results: does not parse; see lint\n"
                      (repository-output null (log-entry 2 #f "# Second\n" "a/log/2/results.toml is missing project") null)))

(test-case "the handoff repository section says when a pending entry has neither results nor entry.md"
  (check-regexp-match #rx"\n  results: none\n  entry.md: not written\n"
                      (repository-output null (log-entry 2 #f #f #f) null)))

(test-case "the handoff repository section lists uncommitted changes"
  (check-equal? (repository-output null #f (list " M a/tasks.md" "?? a/plan.md"))
                (string-append "lint: no errors\n"
                               "pending log entry: none\n"
                               "uncommitted:\n"
                               "   M a/tasks.md\n"
                               "  ?? a/plan.md\n")))

(test-case "other documents gives each document's summary, when it has one"
  (check-equal? (other-documents-output (list (cons (scope-path 'arc (string->path "notes/")) "")
                                              (cons (scope-path 'handoff (string->path "topic.md")) "What this covers.")))
                (string-append "- arc:notes/\n"
                               "- handoff:topic.md: What this covers.\n")))

(test-case "other documents says when there are none"
  (check-equal? (other-documents-output null) "nothing\n"))

(test-case "the report lists the documents with their summaries, leaving out the workflow and verbatim copies"
  (check-equal? (start-report-other-documents
                 (report (struct-copy arc clean
                                      [documents (append (arc-documents clean)
                                                         (list (cons (scope-path 'handoff (string->path "workflow-spike.md"))
                                                                     "<!-- copied from workflow spike (sha256:00) on 2026-10-29; verbatim -->\n# Spike\n\nSpike.\n")))])
                         #f))
                (list (cons (scope-path 'arc (string->path "notes/")) "")
                      (cons (scope-path 'handoff (string->path "topic.md")) "What this covers."))))

(test-case "the report inlines the last committed log entry, not the pending one"
  (check-equal? (car (last (start-report-sections (report (with-pending (log-entry 2 #f "# Second\n" run)) #f))))
                "arc:log/1/entry.md"))

(test-case "the report's pending entry is the uncommitted last entry"
  (define pending (log-entry 2 #f "# Second\n" run))
  (check-equal? (start-report-pending (report (with-pending pending) #f)) pending))

(test-case "without a baseline the report's checks are not compared"
  (check-equal? (start-report-checks (report clean #f)) (checks-uncompared null)))

(test-case "with a baseline the report's checks are compared against it"
  (check-true (equal? (start-report-checks (report clean (cons 2 recorded)))
                      (checks-compared 2 "murph" (check-run-started recorded) null))))

(test-case "the report gives the inlined files, then the other documents, the project, the handoff repository and the checks"
  (define output
    (with-output-to-string
      (thunk (print-start (report clean (cons 2 recorded))))))
  (check-equal? (filter (λ ([line : String]) (string-prefix? line "==> "))
                        (string-split output "\n"))
                (list "==> arc:start.toml <=="
                      "==> arc:arc.md <=="
                      "==> arc:workflow.md <=="
                      "==> arc:tasks.md <=="
                      "==> arc:do-not-reopen.md <=="
                      "==> arc:open-questions.md <=="
                      "==> arc:review.md <=="
                      "==> arc:log/2/entry.md <=="
                      "==> other documents <=="
                      "==> project repository <=="
                      "==> handoff repository <=="
                      "==> checks <==")))

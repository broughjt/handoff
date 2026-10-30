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

(define here (project (string->path "/p") "main" head null))

(: project-output (-> project (U (Pairof Positive-Integer check-run) #f) String))
(define (project-output project baseline)
  (with-output-to-string (thunk (print-project project baseline))))

(: with-history (-> (Listof String) project))
(define (with-history history)
  (struct-copy project here [history history]))

(: repository-output (-> (Listof String) arc-files (Listof String) String))
(define (repository-output diagnostics files dirty)
  (with-output-to-string
    (thunk (print-handoff-repository diagnostics files (parse-arc files) dirty))))

(: with-pending (-> log-entry-files arc-files))
(define (with-pending entry)
  (struct-copy arc-files clean
               [log (list (log-entry-files "1" #t "# First\n" results) entry)]
               [committed-log (list "1")]
               [baseline (baseline-files 1 results)]))

(: documents-output (-> arc-files String))
(define (documents-output files)
  (with-output-to-string (thunk (print-documents files (parse-arc files)))))

(test-case "the project section says when nothing was committed since the baseline"
  (check-equal? (project-output here (cons 1 recorded))
                (string-append "now: main at 0123456789ab\n"
                               "at log/1: main at 0123456789ab\n"
                               "no new commits\n")))

(test-case "the project section lists commits since the baseline"
  (check-equal? (project-output (with-history (list "> aaa One" "> bbb Two")) (cons 1 recorded))
                (string-append "now: main at 0123456789ab\n"
                               "at log/1: main at 0123456789ab\n"
                               "commits since log/1:\n"
                               "  aaa One\n"
                               "  bbb Two\n")))

(test-case "the project section marks each side of a rewritten history"
  (check-equal? (project-output (with-history (list "< aaa Old" "> bbb New")) (cons 1 recorded))
                (string-append "now: main at 0123456789ab\n"
                               "at log/1: main at 0123456789ab\n"
                               "history rewritten since log/1; < marks commits only in the recorded history, > only in the current one:\n"
                               "  < aaa Old\n"
                               "  > bbb New\n")))

(test-case "the project section says when the recorded head does not resolve"
  (check-equal? (project-output (struct-copy project here [history 'unresolvable]) (cons 1 recorded))
                (string-append "now: main at 0123456789ab\n"
                               "at log/1: main at 0123456789ab\n"
                               "recorded head does not resolve here: its history was rewritten and pruned, or it was committed on another machine\n")))

(test-case "the project section says when the branch differs from the baseline's"
  (check-equal? (project-output (struct-copy project here [branch "topic"]) (cons 1 recorded))
                (string-append "now: topic at 0123456789ab\n"
                               "at log/1: main at 0123456789ab\n"
                               "the branch differs from log/1\n"
                               "no new commits\n")))

(test-case "without a baseline the project section gives only the head"
  (check-equal? (project-output here #f) "now: main at 0123456789ab\n"))

(test-case "a detached project without commits"
  (check-equal? (project-output (project (string->path "/p") #f #f null) #f)
                "now: detached with no commits\n"))

(test-case "the project section lists at most 20 commits"
  (define lines (for/list : (Listof String) ([n (in-range 25)]) (format "> c~a" n)))
  (check-equal? (take-right (string-split (project-output (with-history lines) (cons 1 recorded)) "\n") 2)
                (list "  c19" "  and 5 more")))

(test-case "the handoff repository section of a clean repository"
  (check-equal? (repository-output null clean null)
                (string-append "lint: no errors\n"
                               "pending log entry: none\n"
                               "uncommitted: none\n")))

(test-case "the handoff repository section lists the diagnostics"
  (check-equal? (repository-output (list "a/x.md: bad" "a/y.md: worse") clean null)
                (string-append "lint: 2 errors\n"
                               "  a/x.md: bad\n"
                               "  a/y.md: worse\n"
                               "pending log entry: none\n"
                               "uncommitted: none\n")))

(test-case "the handoff repository section describes a pending entry's results and entry.md"
  (check-equal? (repository-output null (with-pending (log-entry-files "2" #t "# Second\n" results)) null)
                (string-append "lint: no errors\n"
                               "pending log entry: log/2 was begun but not committed, so it is not the baseline\n"
                               "  results: recorded on murph at 2026-10-29T18:00:00Z\n"
                               "  entry.md: written\n"
                               "uncommitted: none\n")))

(test-case "the handoff repository section says when a pending entry's results do not parse"
  (check-regexp-match #rx"\n  results: does not parse; see lint\n"
                      (repository-output null (with-pending (log-entry-files "2" #t "# Second\n" "[run]")) null)))

(test-case "the handoff repository section says when a pending entry has neither results nor entry.md"
  (check-regexp-match #rx"\n  results: none\n  entry.md: not written\n"
                      (repository-output null (with-pending (log-entry-files "2" #t #f #f)) null)))

(test-case "the handoff repository section lists uncommitted changes"
  (check-equal? (repository-output null clean (list " M a/tasks.md" "?? a/plan.md"))
                (string-append "lint: no errors\n"
                               "pending log entry: none\n"
                               "uncommitted:\n"
                               "   M a/tasks.md\n"
                               "  ?? a/plan.md\n")))

(test-case "other documents leaves out the workflow and verbatim copies"
  (check-equal? (documents-output
                 (struct-copy arc-files clean
                              [documents (append (arc-files-documents clean)
                                                 (list (cons (scope-path 'handoff (string->path "workflow-spike.md"))
                                                             "<!-- copied from workflow spike (sha256:00) on 2026-10-29; verbatim -->\n# Spike\n\nSpike.\n")))]))
                (string-append "- arc:notes/\n"
                               "- handoff:topic.md: What this covers.\n")))

(test-case "other documents says when there are none"
  (check-equal? (documents-output (struct-copy arc-files clean [documents null])) "nothing\n"))

(test-case "the report gives the inlined files, then the other documents, the project, the handoff repository and the checks"
  (define report
    (with-output-to-string
      (thunk (print-start (string->path "/p/.scratch/handoff")
                          clean
                          (parse-arc clean)
                          (cons 2 recorded)
                          null
                          here
                          null))))
  (check-equal? (filter (λ ([line : String]) (string-prefix? line "==> "))
                        (string-split report "\n"))
                (list "==> arc:start.toml <=="
                      "==> arc:arc.md <=="
                      "==> arc:workflow.md <=="
                      "==> arc:tasks.md <=="
                      "==> arc:do-not-reopen.md <=="
                      "==> arc:open-questions.md <=="
                      "==> arc:review.md <=="
                      "==> arc:log/2/entry.md <=="
                      "==> other documents <=="
                      "==> project <=="
                      "==> handoff repository <=="
                      "==> checks <==")))

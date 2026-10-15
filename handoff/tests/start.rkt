#lang typed/racket/base
;; Briefing an arc with start.

(require racket/file
         racket/string
         typed/rackunit
         "common.rkt"
         "../main.rkt")

;; Checks that read a marker file in the project, so a test can flip them.
(: checks-text (-> Path String))
(define (checks-text project)
  (format "[[check]]\nid = \"marker\"\nclaim = \"the marker exists\"\ncommand = \"test -e ~a || { echo no marker; exit 1; }\"\n\n[[check]]\nid = \"steady\"\nclaim = \"always holds\"\ncommand = \"true\"\n"
          (build-path project "marker")))

(define (project-of [repository : Path]) (simplify-path (build-path repository 'up 'up)))

;; An arc with the marker checks, all committed.
(define (call-with-start-arc [body : (-> Path Path Path Any)])
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (define project (project-of repository))
     (write-file (build-path arc "checks.toml") (checks-text project))
     (write-file (build-path arc "arc.md") "# The arc\n\nWhy it exists.\n")
     (write-file (build-path arc "tasks.md") "- [ ] Measure\n")
     (run-git repository "add" "--all")
     (run-git repository "commit" "-qm" "Initialize")
     (run-git project "commit" "--allow-empty" "-qm" "Project start")
     (body repository arc project))))

;; Record a log entry with update --begin and --commit.
(define (record! [repository : Path] [arc : Path] [headline : String])
  (define begun (handoff* repository "update" "--begin"))
  (check-equal? (outcome-code begun) 0 (outcome-stderr begun))
  (define log (build-path arc "log"))
  (define number (length (directory-list log)))
  (write-file (build-path log (number->string number) "entry.md") (format "# ~a\n\nNarrative ~a.\n" headline number))
  (define committed (handoff* repository "update" "--commit"))
  (check-equal? (outcome-code committed) 0 (outcome-stderr committed)))

;; Run start, check that it succeeds without writing anything, and return its
;; output.
(define (start! [repository : Path])
  (define project (project-of repository))
  (define before (snapshot project))
  (define result (handoff* repository "start"))
  (check-equal? (outcome-code result) 0 (outcome-stderr result))
  (check-equal? (outcome-stderr result) "")
  (check-equal? (snapshot project) before)
  (outcome-stdout result))

(define (check-contains [output : String] . [needles : String *])
  (for ([needle (in-list needles)])
    (check-true (string-contains? output needle) (format "~s not in:\n~a" needle output))))

(define (check-lacks [output : String] . [needles : String *])
  (for ([needle (in-list needles)])
    (check-false (string-contains? output needle) (format "~s in:\n~a" needle output))))

(define (check-order [output : String] . [needles : String *])
  (define positions
    (for/list : (Listof (U Natural #f)) ([needle (in-list needles)])
      (define match (regexp-match-positions (regexp-quote needle) output))
      (and match (car (car match)))))
  (check-true (andmap exact-nonnegative-integer? positions) (format "~s missing from:\n~a" needles output))
  (check-equal? (sort (cast positions (Listof Natural)) <) positions output))

(test-case "with no log, start inlines the arc, then reports status and checks"
  (call-with-start-arc
   (λ ([repository : Path] [arc : Path] [project : Path])
     (define output (start! repository))
     (check-true (string-prefix? output "==> start, generated from .scratch/handoff/arc/start.toml <==\nNext:\n  Continue\nWorkflow: .scratch/handoff/shared.md\nRead first:\n  nothing\n\n")
                 output)
     (check-order output
                  "==> start, generated from "
                  "==> .scratch/handoff/arc/arc.md <==\n# The arc\n\nWhy it exists.\n\n"
                  "==> .scratch/handoff/shared.md <==\nworkflow\n\n"
                  "==> .scratch/handoff/arc/tasks.md <==\n- [ ] Measure\n\n"
                  "==> .scratch/handoff/arc/do-not-reopen.md <==\n\n"
                  "==> .scratch/handoff/arc/open-questions.md <==\n\n"
                  "==> .scratch/handoff/arc/review.md <==\n\n"
                  "==> other documents <==\nnothing\n\n"
                  (format "==> project repository status <==\nrepository: ~a\nnow: "
                          (string-trim (path->string project) "/" #:left? #f))
                  "==> handoff repository status <==\nrepository: .scratch/handoff\nlint: no errors\npending log entry: none\nuncommitted: none\n\n"
                  "==> checks <==\nno earlier log entry to compare against\n\nid: marker\n"
                  "outcome: false\nexit: 1\n"
                  "output:\n  no marker\n"
                  "summary: 1 true, 1 false\n")
     (check-lacks output "at log/" "entry.md"))))

(test-case "documents are listed by reference with their summaries, not inlined"
  (call-with-start-arc
   (λ ([repository : Path] [arc : Path] [project : Path])
     (write-file (build-path arc "failures.md") "# Failures\n\nWhat failed\nand why.\n\nSecret contents.\n")
     (write-file (build-path arc "notes" "a.md") "more")
     (write-file (build-path arc "script.sh") "echo")
     (write-file (build-path repository "topic.md") "# Topic\n\nShared facts.\n")
     (write-file (build-path repository "workflow-solo.md")
                 "<!-- copied from workflow pattern solo (sha256:00) on 2026-10-15; verbatim -->\n# Solo\n\nCopied.\n")
     (define output (start! repository))
     (check-contains output (string-append "==> other documents <==\n"
                                           "- arc:failures.md: What failed and why.\n"
                                           "- arc:notes/\n"
                                           "- arc:script.sh\n"
                                           "- handoff:topic.md: Shared facts.\n\n"))
     (check-lacks output "Secret contents" "handoff:workflow-solo" "handoff:shared"))))

(test-case "a multiline next renders without a trailing blank line"
  (call-with-start-arc
   (λ ([repository : Path] [arc : Path] [project : Path])
     (write-file (build-path arc "start.toml")
                 "next = \"\"\"\nFirst line.\n\nSecond paragraph.\n\"\"\"\nworkflow = \"handoff:shared.md\"\nread_first = []\n")
     (check-contains (start! repository) "Next:\n  First line.\n  \n  Second paragraph.\nWorkflow: "))))

(test-case "lint diagnostics are reported, and whatever parses is still rendered"
  (call-with-start-arc
   (λ ([repository : Path] [arc : Path] [project : Path])
     (delete-file (build-path arc "review.md"))
     (write-file (build-path arc "start.toml")
                 "next = \"Go\"\nworkflow = \"arc:missing.md\"\nread_first = [{ path = \"project:gone.rkt\", reason = \"Entry\" }]\n")
     (define output (start! repository))
     (check-contains output
                     "lint: 3 errors\n  .scratch/handoff/arc/review.md: expected a regular file\n"
                     "Workflow: .scratch/handoff/arc/missing.md (missing)\n"
                     "- gone.rkt (missing): Entry\n"
                     "==> .scratch/handoff/arc/tasks.md <==")
     (check-lacks output "review.md <==")
     (write-file (build-path arc "start.toml") "next = \n")
     (write-file (build-path arc "checks.toml") "[[check]]\n")
     (define broken (start! repository))
     (check-contains broken
                     "==> start, generated from .scratch/handoff/arc/start.toml <==\nstart.toml did not parse"
                     "  .scratch/handoff/arc/start.toml: "
                     "not run: checks.toml: "
                     "==> .scratch/handoff/arc/arc.md <==")
     (check-lacks broken "Next:" "id: " "shared.md <=="))))

(test-case "start compares the checks against the latest log entry"
  (call-with-start-arc
   (λ ([repository : Path] [arc : Path] [project : Path])
     (write-file (build-path project "marker") "")
     (write-file (build-path arc "checks.toml")
                 (string-append (checks-text project) "\n[[check]]\nid = \"gone\"\nclaim = \"will be removed\"\ncommand = \"true\"\n"))
     (record! repository arc "First")
     (delete-file (build-path project "marker"))
     (write-file (build-path arc "checks.toml")
                 (string-append (string-replace (checks-text project) "command = \"true\"" "command = \"exit 0\"")
                                "\n[[check]]\nid = \"fresh\"\nclaim = \"is new\"\ncommand = \"true\"\n"))
     (define output (start! repository))
     (check-regexp-match #px"compared against log/1, recorded on \\S+ at \\d{4}-\\d\\d-\\d\\dT" output)
     (check-contains output
                     "id: marker\nclaim: the marker exists\noutcome: false\nchange: was true at log/1\nexit: 1\nduration: "
                     "output:\n  no marker\n"
                     "id: steady\nclaim: always holds\noutcome: true\nchange: definition changed since log/1, where it was true\n"
                     "id: fresh\nclaim: is new\noutcome: true\nchange: new since log/1\n"
                     "\n\nremoved since log/1: gone (will be removed)\n\nhost: "
                     "==> .scratch/handoff/arc/log/1/entry.md <==\n# First\n\nNarrative 1.\n"
                     "   M arc/checks.toml\n"))))

(test-case "a flip to true shows the output from before"
  (call-with-start-arc
   (λ ([repository : Path] [arc : Path] [project : Path])
     (record! repository arc "Without the marker")
     (write-file (build-path project "marker") "")
     (check-contains (start! repository)
                     "outcome: true\nchange: was false at log/1\nduration: "
                     "output at log/1:\n  no marker\n"))))

(test-case "a pending entry is reported and is not the baseline"
  (call-with-start-arc
   (λ ([repository : Path] [arc : Path] [project : Path])
     (record! repository arc "First")
     (check-equal? (outcome-code (handoff* repository "update" "--begin")) 0)
     (define output (start! repository))
     (check-regexp-match #px"\npending log entry: log/2 [^\n]*not the baseline\n  results: recorded on \\S+ at [^\n]+\n  entry.md: not written\n"
                         output)
     (check-contains output "compared against log/1" "==> .scratch/handoff/arc/log/1/entry.md <==")
     (check-lacks output "log/2/entry.md"))))

(test-case "a latest entry without readable results is flagged, not skipped"
  (call-with-start-arc
   (λ ([repository : Path] [arc : Path] [project : Path])
     (record! repository arc "First")
     (write-file (build-path arc "log" "2" "entry.md") "# Second\n")
     (write-file (build-path arc "log" "2" "results.toml") "not toml")
     (run-git repository "add" "--all")
     (run-git repository "commit" "-qm" "By hand")
     (define output (start! repository))
     (check-contains output
                     "not compared: log/2/results.toml: "
                     "==> .scratch/handoff/arc/log/2/entry.md <==\n# Second\n")
     (check-lacks output "compared against log/1" "log/1/entry.md" "at log/")
     (check-regexp-match #px"==> project repository status <==\n[^\n]+\nnow: [^\n]+\n\n" output))))

(test-case "start reports what happened to the project since the latest entry"
  (call-with-start-arc
   (λ ([repository : Path] [arc : Path] [project : Path])
     (record! repository arc "First")
     (check-contains (start! repository) "no new commits\n")
     (run-git project "commit" "--allow-empty" "-qm" "Later work")
     (check-regexp-match #px"commits since:\n  [0-9a-f]{12} Later work\n" (start! repository))
     (run-git project "commit" "--amend" "--allow-empty" "-qm" "Rewritten work")
     (record! repository arc "Second")
     (run-git project "reset" "-q" "--hard" "HEAD~1")
     (run-git project "commit" "--allow-empty" "-qm" "Replacement")
     ;; Both commits share a second, so Git may list either side first.
     (define rewritten (start! repository))
     (check-regexp-match #px"no longer an ancestor[^\n]*\n(  [<>] [^\n]+\n){2}\n" rewritten)
     (check-regexp-match #px"\n  > [0-9a-f]{12} Replacement\n" rewritten)
     (check-regexp-match #px"\n  < [0-9a-f]{12} Rewritten work\n" rewritten)
     (run-git project "checkout" "-q" "-b" "other")
     (check-contains (start! repository) "the branch differs\n")
     (define results (build-path arc "log" "2" "results.toml"))
     (write-file results (regexp-replace #px"head = \"[0-9a-f]+\"" (file->string results)
                                         "head = \"0123456789abcdef0123456789abcdef01234567\""))
     (check-contains (start! repository) "recorded head does not resolve here"))))

#lang typed/racket/base
;; The command line, run in-process.

(require racket/file
         racket/list
         racket/string
         typed/rackunit
         "common.rkt"
         "../main.rkt")

(define minimal-results "[run]\nhost = \"h\"\nstarted = 2026-10-09T00:00:00Z\nduration = 0.0\n\n[project]\n")

(define (write-log-entry [arc : Path] [name : String] #:results [results : String minimal-results])
  (define entry (build-path arc "log" name))
  (write-file (build-path entry "entry.md") "entry")
  (write-file (build-path entry "results.toml") results)
  entry)

(define (commit-all [repository : Path])
  (run-git repository "add" "--all")
  (run-git repository "commit" "-qm" "Commit"))

(define (lines [text : String]) (string-split text "\n"))

;;; check

(test-case "check reports every outcome without writing or comparing"
  (call-with-repository
   (λ ([repository : Path])
     (define arc (build-path repository "arc"))
     (write-file (build-path arc "start.toml") "")
     (write-file (build-path arc "checks.toml") #<<TOML
[[check]]
id = "yes"
claim = "holds"
command = "echo hidden"

[[check]]
id = "no"
claim = "does not hold"
command = "echo failed; exit 1"

[[check]]
id = "unknown"
claim = "cannot observe"
command = "echo unavailable; exit 3"

[[check]]
id = "slow"
claim = "times out"
command = "echo waiting; sleep 5"
timeout = 0.1
TOML
                 )
     (for ([existing-results? (in-list '(#f #t))])
       (when existing-results?
         (write-file (build-path arc "results.toml") "deliberately invalid TOML")
         (write-file (build-path arc "log" "1" "results.toml") "also invalid"))
       (define before (snapshot repository))
       (for ([_ (in-range 2)])
         (define result (handoff* repository "check"))
         (define stdout (outcome-stdout result))
         (check-equal? (outcome-code result) 1 (outcome-stderr result))
         (check-regexp-match #px"^id: yes\nclaim: holds\noutcome: true\nduration: \\d+\\.\\ds\n\nid: no\n" stdout)
         (check-regexp-match
          #px"^host: .+, duration: \\d+\\.\\ds, summary: 1 true, 1 false, 1 unknown, 1 timeout$"
          (last (lines stdout)))
         (for ([expected (in-list '("output:\n  failed\n" "exit: 1\n" "exit: 3\n" "timeout: 0.1s\n"
                                    "output:\n  unavailable\n" "output:\n  waiting\n"))])
           (check-true (string-contains? stdout expected) expected))
         (for ([absent (in-list '("hidden" "before:" "previous"))])
           (check-false (string-contains? stdout absent) absent))
         (check-equal? (outcome-stderr result) "")
         (check-equal? (snapshot repository) before))))))

(test-case "check succeeds with true or no checks"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (for ([contents (in-list '("[[check]]\nid = \"yes\"\nclaim = \"holds\"\ncommand = \"true\"\n" ""))])
       (write-file (build-path arc "checks.toml") contents)
       (define result (handoff* repository "check" "arc"))
       (check-equal? (outcome-code result) 0 (outcome-stderr result))
       (when (string=? contents "")
         (check-true (string-contains? (outcome-stdout result) "no checks")))))))

(test-case "checks run in the project root wherever handoff runs from"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "checks.toml")
                 "[[check]]\nid = \"root\"\nclaim = \"runs in the project root\"\ncommand = \"test -d .scratch/handoff\"\n")
     (for ([directory (in-list (list repository arc))])
       (define result (handoff directory "check"))
       (check-equal? (outcome-code result) 0 (outcome-stdout result))))))

(test-case "check shows only the tail of the output"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "checks.toml") "[[check]]\nid = \"x\"\nclaim = \"fails\"\ncommand = \"seq 1 100; exit 1\"\n")
     (define result (handoff* repository "check"))
     (check-equal? (outcome-code result) 1)
     (define stdout (outcome-stdout result))
     (check-true (string-contains? stdout "output (last 20 of 100 lines):\n") stdout)
     (check-equal? (for/list : (Listof String) ([line (in-list (lines stdout))]
                                                #:when (string-prefix? line "  "))
                     (string-trim line))
                   (for/list : (Listof String) ([n (in-range 81 101)]) (number->string n))))))

(test-case "arc selection"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "checks.toml") "")
     (write-file (build-path repository "other" "start.toml") "")
     (check-equal? (outcome-code (handoff* repository "check")) 2)
     (check-equal? (outcome-code (handoff* repository "check" "arc")) 0)
     (check-equal? (outcome-code (handoff* repository "check" "missing")) 2)
     (check-equal? (outcome-code (handoff* repository "check" "arc" "other")) 2))))

(test-case "an explicit arc does not need a start file"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "checks.toml") "")
     (delete-file (build-path arc "start.toml"))
     (check-equal? (outcome-code (handoff* repository "check" "arc")) 0)
     (check-equal? (outcome-code (handoff* repository "check")) 2))))

(test-case "a missing or invalid checks file is an input error"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (delete-file (build-path arc "checks.toml"))
     (define missing (handoff* repository "check"))
     (check-equal? (outcome-code missing) 2)
     (check-true (string-contains? (outcome-stderr missing) "checks.toml"))
     (write-file (build-path arc "checks.toml") "[[broken")
     (define broken (handoff* repository "check"))
     (check-equal? (outcome-code broken) 2)
     (check-regexp-match #rx"^handoff: .*checks.toml: " (outcome-stderr broken)))))

(test-case "an invalid timeout is an input error"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (for ([timeout (in-list '("true" "nan" "inf" "-inf" "0" "-1"))])
       (write-file (build-path arc "checks.toml")
                   (format "[[check]]\nid = \"x\"\nclaim = \"holds\"\ncommand = \"true\"\ntimeout = ~a\n" timeout))
       (define result (handoff* repository "check"))
       (check-equal? (outcome-code result) 2 timeout)
       (check-true (string-contains? (outcome-stderr result) "timeout must be") timeout)))))

;;; lint

(test-case "a new arc is valid, and lint is read-only"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (define before (snapshot repository))
     (define result (handoff arc "--repository" (path->string repository) "lint"))
     (check-equal? (outcome-code result) 0 (outcome-stdout result))
     (check-equal? (snapshot repository) before))))

(test-case "lint reports every defect, including a missing start file"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (delete-file (build-path arc "start.toml"))
     (delete-file (build-path arc "arc.md"))
     (make-directory (build-path arc "arc.md"))
     (write-file (build-path arc "checks.toml") "[[check]]\nid = \"x\"\ncommand = \"true\"\n")
     (define entry (write-log-entry arc "1" #:results "[broken"))
     (commit-all repository)
     (delete-file (build-path entry "entry.md"))
     (define result (handoff* repository "lint" "arc"))
     (check-equal? (outcome-code result) 1)
     (for ([text (in-list '("start.toml" "arc.md" "checks.toml" "entry.md" "results.toml" "5 lint errors"))])
       (check-true (string-contains? (outcome-stdout result) text) text)))))

(test-case "lint flags documents without a prose summary, except verbatim copies"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "notes.md") "# Notes\n\n- a list\n")
     (write-file (build-path arc "workflow-mine.md") "<!-- created for this arc on 2026-10-15 -->\n# Mine\n\nHow we work.\n")
     (write-file (build-path arc "data.txt") "")
     (write-file (build-path repository "topic.md") "# Only a heading\n")
     (write-file (build-path repository "workflow-solo.md")
                 "<!-- copied from workflow pattern solo (sha256:00) on 2026-10-15; verbatim -->\n- not checked\n")
     (check-equal? (map diagnostic-path (lint-arc arc repository))
                   (list (build-path arc "notes.md") (build-path repository "topic.md"))))))

(test-case "an incomplete committed latest log entry is reported"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-log-entry arc "1")
     (define latest (write-log-entry arc "2"))
     (commit-all repository)
     (delete-file (build-path latest "results.toml"))
     (check-equal? (map diagnostic-path (lint-arc arc repository))
                   (list (build-path latest "results.toml"))))))

(test-case "only the pending entry, the latest and uncommitted, may lack its files"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-log-entry arc "1")
     (commit-all repository)
     (define pending (build-path arc "log" "2"))
     (make-directory* pending)
     (check-equal? (lint-arc arc repository) null)
     (write-file (build-path pending "results.toml") minimal-results)
     (check-equal? (lint-arc arc repository) null)
     (write-file (build-path pending "results.toml") "[broken")
     (check-equal? (map diagnostic-path (lint-arc arc repository))
                   (list (build-path pending "results.toml")))
     (write-file (build-path pending "results.toml") minimal-results)
     (write-log-entry arc "3")
     (check-equal? (map diagnostic-path (lint-arc arc repository))
                   (list (build-path pending "entry.md"))))))

(test-case "log entry names are canonical and reported in numeric order"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-log-entry arc "1")
     (write-log-entry arc "10")
     (define misnamed '("01" "0001" "0" "yesterday" "١" "²" "-1"))
     (for ([name (in-list misnamed)])
       (write-log-entry arc name))
     (write-file (build-path arc "log" "2") "")
     (write-file (build-path arc "log" "9" "entry.md") "")
     (define names
       (for/list : (Listof String) ([diagnostic (in-list (lint-arc arc repository))])
         (define-values (_base name _directory?) (split-path (diagnostic-path diagnostic)))
         (path->string (assert name path?))))
     (check-equal? names (append '("2" "results.toml") (sort misnamed string<?))))))

(test-case "the log must be a directory"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "log") "")
     (check-equal? (outcome-code (handoff* repository "lint")) 1))))

(test-case "missing references are reported together"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "start.toml")
                 "next = \"Continue\"\nworkflow = \"arc:missing.md\"\n[[read-first]]\npath = \"handoff:also-missing.md\"\nreason = \"Context\"\n")
     (define diagnostics (lint-arc arc repository))
     (check-equal? (length diagnostics) 2)
     (check-equal? (remove-duplicates (map diagnostic-path diagnostics)) (list (build-path arc "start.toml"))))))

(test-case "project references resolve against the enclosing repository"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "start.toml")
                 "next = \"Continue\"\nworkflow = \"handoff:shared.md\"\n[[read-first]]\npath = \"project:source.rkt\"\nreason = \"Context\"\n")
     (check-equal? (length (lint-arc arc repository)) 1)
     (write-file (build-path repository 'up 'up "source.rkt") "")
     (check-equal? (lint-arc arc repository) '())
     ;; Found by walking up from inside the project.
     (check-equal? (outcome-code (handoff arc "lint")) 0))))

(test-case "lint requires an enclosing project"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (delete-directory/files (build-path repository 'up 'up ".git"))
     (check-equal? (outcome-code (handoff* repository "lint")) 2))))

(test-case "a malformed start file is a lint error"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "start.toml") "[broken")
     (check-equal? (outcome-code (handoff* repository "lint" "arc")) 1))))

(test-case "an explicit arc cannot escape the repository"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (for ([name (in-list (list ".." ".git" "../arc" (path->string arc) ""))])
       (check-equal? (outcome-code (handoff arc "--repository" (path->string repository) "lint" name)) 2 name)))))

;;; list and usage

(test-case "list names active arcs and archive tags"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path repository "notes" "readme.md") "")
     (check-equal? (outcome-stdout (handoff* repository "list")) "arc\n")
     (run-git repository "commit" "--allow-empty" "-qm" "Initialize")
     (run-git repository "tag" "archive/old")
     (check-equal? (outcome-stdout (handoff* repository "list" "--archived")) "old\n"))))

(test-case "usage errors exit 2"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (check-equal? (outcome-code (handoff* repository "bogus")) 2)
     (check-equal? (outcome-code (handoff* repository "list" "extra")) 2)
     (check-equal? (outcome-code (handoff* repository "--bogus" "list")) 2))))

(test-case "help text calls reusable workflows workflows, not patterns"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (for ([arguments (in-list '(("--help") ("new" "--help") ("workflow" "--help")
                                 ("workflow" "fork" "--help")))])
       (define stdout (open-output-string))
       (let/ec escape : Void
         (parameterize ([current-directory repository]
                        [current-output-port stdout]
                        [exit-handler (λ (_code) (escape (void)))])
           (main (list->vector arguments))
           (void)))
       (check-true (string-contains? (get-output-string stdout) "workflow") (format "~a" arguments))
       (check-false (string-contains? (get-output-string stdout) "pattern") (format "~a" arguments))))))

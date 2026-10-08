#lang typed/racket/base
;; The command line, run in-process.

(require racket/file
         racket/list
         racket/string
         typed/rackunit
         "common.rkt"
         "../lint.rkt")

(define start-text "next = \"Continue\"\nworkflow = \"handoff:shared.md\"\nread_first = []\n")
(define state-files '("arc.md" "tasks.md" "do-not-reopen.md" "open-questions.md" "review.md"))

;; A repository whose `arc` passes lint. Its one check must never run during
;; lint.
(: call-with-arc (All (A) (-> (-> Path Path A) A)))
(define (call-with-arc body)
  (call-with-repository
   (λ ([repository : Path])
     (define arc (build-path repository "arc"))
     (for ([name (in-list state-files)])
       (write-file (build-path arc name) ""))
     (write-file (build-path arc "start.toml") start-text)
     (write-file (build-path repository "shared.md") "workflow")
     (write-file (build-path arc "checks.toml")
                 "[[check]]\nid = \"x\"\nclaim = \"example\"\nrun = \"touch SHOULD_NOT_EXIST\"\n")
     (body repository arc))))

(define (write-log-entry [arc : Path] [name : String] #:results [results : String ""])
  (define entry (build-path arc "log" name))
  (write-file (build-path entry "entry.md") "entry")
  (write-file (build-path entry "results.toml") results)
  entry)

(define (lines [text : String]) (string-split text "\n"))

;; Run from the repository, naming it explicitly.
(define (handoff* [repository : Path] . [arguments : String *])
  (apply handoff repository "--repository" (path->string repository) arguments))

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
run = "echo hidden"

[[check]]
id = "no"
claim = "does not hold"
run = "echo failed; exit 1"

[[check]]
id = "unknown"
claim = "cannot observe"
run = "echo unavailable; exit 3"

[[check]]
id = "slow"
claim = "times out"
run = "echo waiting; sleep 5"
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
         (check-equal? (string-split (first (lines stdout))) '("ID" "CLAIM" "RESULT" "DURATION"))
         (check-regexp-match #px"yes\\s+holds\\s+true\\s+\\d+\\.\\ds" stdout)
         (check-regexp-match
          #px"^host: .+, duration: \\d+\\.\\ds, summary: 1 true, 1 false, 1 unknown, 1 timeout$"
          (last (lines stdout)))
         (for ([expected (in-list '("failed" "unavailable" "waiting" "exit 1" "exit 3" "after 0.1s"))])
           (check-true (string-contains? stdout expected) expected))
         (for ([absent (in-list '("hidden" "before:" "previous"))])
           (check-false (string-contains? stdout absent) absent))
         (check-equal? (outcome-stderr result) "")
         (check-equal? (snapshot repository) before))))))

(test-case "check succeeds with true or no checks"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (for ([contents (in-list '("[[check]]\nid = \"yes\"\nclaim = \"holds\"\nrun = \"true\"\n" ""))])
       (write-file (build-path arc "checks.toml") contents)
       (define result (handoff* repository "check" "arc"))
       (check-equal? (outcome-code result) 0 (outcome-stderr result))
       (when (string=? contents "")
         (check-true (string-contains? (outcome-stdout result) "no checks")))))))

(test-case "check shows only the tail of the output"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "checks.toml") "[[check]]\nid = \"x\"\nclaim = \"fails\"\nrun = \"seq 1 100; exit 1\"\n")
     (define result (handoff* repository "check"))
     (check-equal? (outcome-code result) 1)
     (check-equal? (for/list : (Listof String) ([line (in-list (lines (outcome-stdout result)))]
                                                #:when (string-prefix? line "           "))
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
                   (format "[[check]]\nid = \"x\"\nclaim = \"holds\"\nrun = \"true\"\ntimeout = ~a\n" timeout))
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
     (write-file (build-path arc "checks.toml") "[[check]]\nid = \"x\"\nrun = \"true\"\n")
     (delete-file (build-path (write-log-entry arc "1" #:results "[broken") "entry.md"))
     (define result (handoff* repository "lint" "arc"))
     (check-equal? (outcome-code result) 1)
     (for ([text (in-list '("start.toml" "arc.md" "checks.toml" "entry.md" "results.toml" "5 lint errors"))])
       (check-true (string-contains? (outcome-stdout result) text) text)))))

(test-case "an incomplete latest log entry is reported"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-log-entry arc "1")
     (define latest (write-log-entry arc "2"))
     (delete-file (build-path latest "results.toml"))
     (check-equal? (map diagnostic-path (lint-arc arc repository))
                   (list (build-path latest "results.toml"))))))

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
                 "next = \"Continue\"\nworkflow = \"arc:missing.md\"\n[[read_first]]\npath = \"handoff:also-missing.md\"\nreason = \"Context\"\n")
     (define diagnostics (lint-arc arc repository))
     (check-equal? (length diagnostics) 2)
     (check-equal? (remove-duplicates (map diagnostic-path diagnostics)) (list (build-path arc "start.toml"))))))

(test-case "project references resolve against the enclosing repository"
  (call-with-arc
   (λ ([project : Path] [_arc : Path])
     (define repository (build-path project ".scratch" "handoff"))
     (make-directory* repository)
     (run-git repository "init" "-q")
     (define arc (build-path repository "arc"))
     (for ([name (in-list state-files)])
       (write-file (build-path arc name) ""))
     (write-file (build-path arc "checks.toml") "")
     (write-file (build-path repository "shared.md") "")
     (write-file (build-path arc "start.toml")
                 "next = \"Continue\"\nworkflow = \"handoff:shared.md\"\n[[read_first]]\npath = \"project:source.rkt\"\nreason = \"Context\"\n")
     (check-equal? (length (lint-arc arc repository)) 1)
     (write-file (build-path project "source.rkt") "")
     (check-equal? (lint-arc arc repository) '())
     ;; Found by walking up from inside the project.
     (check-equal? (outcome-code (handoff arc "lint")) 0))))

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
     (run-git repository "-c" "user.name=Test" "-c" "user.email=test@example.com" "-c" "commit.gpgSign=false"
              "commit" "--allow-empty" "-qm" "Initialize")
     (run-git repository "-c" "tag.gpgSign=false" "tag" "archive/old")
     (check-equal? (outcome-stdout (handoff* repository "list" "--archived")) "old\n"))))

(test-case "usage errors exit 2"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (check-equal? (outcome-code (handoff* repository "bogus")) 2)
     (check-equal? (outcome-code (handoff* repository "list" "extra")) 2)
     (check-equal? (outcome-code (handoff* repository "--bogus" "list")) 2))))

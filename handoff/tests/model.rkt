#lang typed/racket/base
;; The file formats, parsed and written without running anything.

(require racket/list
         racket/path
         racket/string
         typed/rackunit
         (rename-in "../model.rkt" [check make-check])
         "../error.rkt"
         "../toml.rkt")

;;; start.toml
(define base : Table
  (hasheq 'next "Continue" 'workflow "arc:workflow.md" 'read-first '()))

(test-case "a multiline next and read-first entries"
  (define start*
    (parse-start (hash-set* base
                            'next "Discuss the plan.\nThen implement."
                            'workflow "handoff:shared.md"
                            'read-first (list (hasheq 'path "project:handoff/main.rkt" 'reason "Entry point")))))
  (check-equal? (start-next start*) "Discuss the plan.\nThen implement.")
  (check-equal? (start-workflow start*) (scope-path 'handoff (string->path "shared.md")))
  (check-equal? (start-read-first start*)
                (list (read-first (scope-path 'project (string->path "handoff/main.rkt")) "Entry point"))))

(test-case "invalid fields and references are rejected"
  (for ([changes (in-list (ann (list '((next . " ")) '((next . #f)) '((extra . "unknown")) '((read_first . ()))
                                '((workflow . "workflow.md")) '((workflow . "arc:/absolute"))
                                '((workflow . "handoff:../escape")) '((workflow . "handoff:a/../../b"))
                                '((workflow . "arc:")) '((workflow . "other:file"))
                                (list (cons 'read-first (hasheq))) '((read-first . ("file")))
                                (list (cons 'read-first (list (hasheq 'path "arc:context.md"))))
                                (list (cons 'read-first (list (hasheq 'path "arc:context.md" 'reason "")))))
                               (Listof (Listof (Pairof Symbol Any)))))])
    (define table (for/fold ([table : Table base]) ([change (in-list changes)])
                    (hash-set table (car change) (cdr change))))
    (check-exn exn:fail:handoff? (λ () (parse-start table)) (format "~s" changes)))
  (for ([field (in-list '(next workflow read-first))])
    (check-exn exn:fail:handoff? (λ () (parse-start (hash-remove base field))) (symbol->string field))))

(test-case "references resolve against their scope"
  (define arc (string->path "/repository/arc"))
  (define repository (string->path "/repository"))
  (define project (string->path "/project"))
  (check-equal? (resolve-scope-path (parse-scope-path "arc:notes/a.md") #:arc arc #:repository repository #:project project)
                (string->path "/repository/arc/notes/a.md"))
  (check-equal? (resolve-scope-path (parse-scope-path "handoff:shared.md") #:arc arc #:repository repository #:project project)
                (string->path "/repository/shared.md")))

;;; checks.toml

(define (load [text : String]) (parse-checks (parse-toml text)))

(test-case "checks keep file order and default their timeout"
  (define checks (load "[[check]]\nid = \"second\"\nclaim = \"two\"\ncommand = \"true\"\ntimeout = 5\n\n[[check]]\nid = \"first\"\nclaim = \"one\"\ncommand = \"false\"\n"))
  (check-equal? (map check-id checks) '("second" "first"))
  (check-equal? (first checks) (make-check "second" "two" "true" 5))
  (check-equal? (check-timeout (second checks)) default-timeout))

(test-case "an empty checks file has no checks"
  (check-equal? (load "") '()))

(test-case "malformed checks are rejected"
  (for ([text (in-list (list "[[check]]\nid = \"x\"\ncommand = \"true\"\n"
                             "[[check]]\nid = \"x\"\nclaim = \"c\"\n"
                             "[[check]]\nclaim = \"c\"\ncommand = \"true\"\n"
                             "[[check]]\nid = \"x\"\nclaim = \"c\"\ncommand = \"true\"\nexpect = 0\n"
                             "[[check]]\nid = \"x\"\nclaim = \" \"\ncommand = \"true\"\n"
                             "[[check]]\nid = \"x\"\nclaim = \"c\"\ncommand = \"true\"\ntimeout = 0\n"
                             "[[check]]\nid = \"x\"\nclaim = \"c\"\ncommand = \"true\"\n[[check]]\nid = \"x\"\nclaim = \"d\"\ncommand = \"true\"\n"
                             "[check]\nid = \"x\"\nclaim = \"c\"\ncommand = \"true\"\n"
                             "[[check]]\nid = \"x\"\nclaim = \"c\"\nrun = \"true\"\n"
                             "check = 1\n"
                             "other = 1\n"))])
    (check-exn exn:fail:handoff? (λ () (load text)) text)))

(test-case "errors name the check"
  (check-exn #rx"check x is missing claim"
             (λ () (load "[[check]]\nid = \"x\"\ncommand = \"true\"\n"))))

;;; results.toml

(define (round-trip [run : check-run]) (parse-check-run (parse-toml (check-run->toml run))))

(define sample
  (check-run "murph" 1791504000 1.5
       (branch-head "main" "714bc77")
       (list (check-result (make-check "yes" "holds" "true" 60) 'true 0.25 "" 0 0)
             (check-result (make-check "slow" "times out" "sleep 5" 0.5) 'timeout 0.5 "waiting\n" 0 #f)
             (check-result (make-check "big" "fails" "seq 1 3" 60) 'false 0.125 "2\n3\n" 1234 1))))

(test-case "results survive a round trip"
  (check-equal? (round-trip sample) sample)
  (define detached (struct-copy check-run sample [project-head (branch-head #f #f)]))
  (check-equal? (round-trip detached) detached)
  (define empty (struct-copy check-run sample [started 0] [duration 0.0] [results '()]))
  (check-equal? (round-trip empty) empty))

(test-case "output that toml-lib's writer cannot represent reads back intact"
  (define everything
    (list->string (append (for/list : (Listof Char) ([n (in-range 0 128)]) (integer->char n))
                          (string->list "é 😀 \"\"\" \\\n trailing \\"))))
  (for ([output (in-list (list everything "\e[31mred\e[0m" "\r\n" "\n\nleading newlines" "ends with a quote\""
                               "no newline" ""))])
    (define with-output
      (struct-copy check-run sample
                   [results (list (check-result (make-check "x" "c" "r" 60) 'false 0.0 output 0 1))]))
    (check-equal? (round-trip with-output) with-output (format "~s" output))))

(test-case "durations are written to the millisecond and timeouts keep their type"
  (define text (check-run->toml sample))
  (check-true (string-contains? text "duration = 1.500\n"))
  (check-true (string-contains? text "timeout = 60\n"))
  (check-true (string-contains? text "timeout = 0.5\n"))
  (check-true (string-contains? text "started = 2026-10-09T00:00:00Z\n")))

(test-case "malformed results are rejected"
  (define valid (check-run->toml sample))
  (define (variant [from : String] [to : String])
    (check-true (string-contains? valid from) from)
    (string-replace valid from to #:all? #f))
  (for ([text (in-list (list ""
                             (variant "[project]\n" "")
                             (variant "host = \"murph\"\n" "")
                             (variant "started = 2026-10-09T00:00:00Z" "started = 2026-10-09T00:00:00")
                             (variant "started = 2026-10-09T00:00:00Z" "started = \"yesterday\"")
                             (variant "duration = 1.500" "duration = -1.0")
                             (variant "duration = 1.500" "duration = nan")
                             (variant "outcome = \"true\"" "outcome = \"maybe\"")
                             (variant "outcome = \"true\"\nexit = 0\n" "outcome = \"true\"\n")
                             (variant "outcome = \"timeout\"\n" "outcome = \"timeout\"\nexit = 0\n")
                             (variant "truncated = 1234" "truncated = -1")
                             (variant "id = \"slow\"" "id = \"yes\"")
                             (variant "id = \"yes\"\n" "id = \"yes\"\nexpect = 0\n")
                             (variant "branch = \"main\"" "branch = \"\"")))])
    (check-exn exn:fail:handoff? (λ () (parse-check-run (parse-toml text))) text)))

;;; Log entries

(test-case "the headline is the text of the opening heading"
  (for ([text+headline (in-list '(("# Title\n\nBody" . "Title")
                                  ("\n  \n## Title ##\n" . "Title")
                                  ("   ###### Deep\tspace  " . "Deep\tspace")
                                  ("# C# and F#\n" . "C# and F#")
                                  ("# Title\r\n\r\nBody" . "Title")))])
    (check-equal? (entry-headline (car text+headline)) (cdr text+headline) (car text+headline)))
  (for ([text (in-list '("" "  \n\t\n" "Body\n# Later heading" "#Title" "# " "# ##"
                         "    # Indented code" "####### Seven" "Title\n=====\n"))])
    (check-exn exn:fail:handoff? (λ () (entry-headline text)) text)))


;;; Comparing runs

(test-case "results compare by id, definition and outcome"
  (define (result [id : String] [run : String] [outcome : Outcome])
    (check-result (make-check id "claim" run 60) outcome 0.0 "" 0 (if (eq? outcome 'true) 0 1)))
  (define earlier (list (result "same" "true" 'true)
                        (result "flips" "test -e x" 'true)
                        (result "redefined" "old" 'true)
                        (result "gone" "true" 'true)))
  (define current (list (result "new" "true" 'true)
                        (result "redefined" "new" 'true)
                        (result "flips" "test -e x" 'false)
                        (result "same" "true" 'true)))
  (define changes (map (λ ([after : check-result]) (compare-result after earlier)) current))
  (define removed (removed-checks (map check-result-check current) earlier))
  (check-equal? (map check-change-after changes) current)
  (check-equal? (map check-change-change changes) '(new redefined flipped unchanged))
  (check-equal? (map check-change-before changes)
                (list #f (third earlier) (second earlier) (first earlier)))
  (check-equal? removed (list (check-result-check (fourth earlier)))))

;;; Workflow patterns

(test-case "pattern names are lowercase file names"
  (for ([name (in-list '("spike" "agent-implements" "v2"))])
    (check-true (pattern-name? name) name))
  (for ([name (in-list '("README" "-x" "" "a.b" "a b"))])
    (check-false (pattern-name? name) name)))

(test-case "a document's summary is its first paragraph after its provenance line and headings"
  (check-equal? (document-summary "# `spike`\n\nAn investigation\n  with a budget.\n\nMore.\n") "An investigation with a budget.")
  (check-equal? (document-summary "\nNo heading.\n## Review\nNone.\n") "No heading.")
  (check-equal? (document-summary "# Only a heading\n") "")
  (check-equal? (document-summary "<!-- created for this arc on 2026-10-15 -->\n# W\n\nHow.\n") "How."))

(test-case "a summary must be a paragraph of prose"
  (check-false (summary-issue "# T\n\nProse.\n"))
  (for ([text (in-list '("# T\n" "" "# T\n\n- item\n" "1. item\n" "```\ncode\n```\n" "| a | b |\n" "<!-- note -->\n"))])
    (check-true (string? (summary-issue text)) text)))

(test-case "only a copied provenance line marks a verbatim copy"
  (check-true (verbatim-copy? (workflow-provenance 'copied "workflow solo" "abc" "2026-10-13")))
  ;; The wording before 2026-10-15, which existing copies keep.
  (check-true (verbatim-copy? "<!-- copied from workflow pattern solo (sha256:00) on 2026-10-13; verbatim -->\n"))
  (check-false (verbatim-copy? (workflow-provenance 'forked "arc:workflow-a.md" "abc" "2026-10-13")))
  (check-false (verbatim-copy? "# Topic\n")))

(test-case "a verbatim copy's provenance records the SHA-256 of the pattern in lowercase hexadecimal"
  (check-equal? (workflow-provenance 'copied "workflow pattern solo" "abc" "2026-10-13")
                "<!-- copied from workflow pattern solo (sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad) on 2026-10-13; verbatim -->\n"))

(test-case "a fork's provenance names its source reference"
  (check-equal? (workflow-provenance 'forked "arc:workflow-a.md" "abc" "2026-10-13")
                "<!-- forked from arc:workflow-a.md (sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad) on 2026-10-13 -->\n"))

(test-case "only an opening provenance line is stripped"
  (for ([how (in-list '("copied from x" "forked from y" "created for this arc on z"))])
    (check-equal? (strip-provenance (format "<!-- ~a -->\n# W\n" how)) "# W\n" how))
  (for ([text (in-list '("# W\n" "<!-- a note -->\n# W\n" "# W\n<!-- copied from x -->\n"))])
    (check-equal? (strip-provenance text) text text)))

;;; Arcs

(define directory (string->path "/repository/arc"))

(define (run-with [ids : (Listof String)])
  (check-run "murph" 0 1.0 (branch-head "main" "abc")
             (for/list ([id (in-list ids)])
               (check-result (make-check id "holds" "true" 60) 'true 0.5 "" 0 0))))

(define (entry [number : Exact-Positive-Integer]
               #:entry [text : (U String #f) "# Did a thing\n"]
               #:results [results : (U check-run failure #f) (run-with '("x"))])
  (log-entry number #t text results))

(define workflow-reference
  (reference (scope-path 'handoff (string->path "workflow.md")) (string->path "/repository/workflow.md") #t))

;; An arc that lint accepts, for tests to change one part of.
(define valid
  (arc "arc"
       directory
       (string->path "/repository")
       (string->path "/project")
       (start "Continue" (reference-scope-path workflow-reference) '())
       (list workflow-reference)
       "# Workflow\n\nHow we work.\n"
       (for/list : (Listof (Pairof String (U String #f))) ([name (in-list state-files)])
         (cons name ""))
       (list (make-check "x" "claim" "true" 60))
       (list (document "arc:notes.md" (build-path directory "notes.md") "# Notes\n\nWhat we found.\n"))
       (entry-log 'directory (list (entry 1)) '() #f)))

(define (with-log #:shape [shape : (U 'absent 'directory 'not-a-directory) 'directory]
                  #:pending [pending : (U Exact-Positive-Integer #f) #f]
                  #:invalid [invalid : (Listof String) '()]
                  . [entries : log-entry *])
  (struct-copy arc valid [log (entry-log shape entries invalid pending)]))

(define (lint-messages [arc* : arc])
  (for/list : (Listof (Pairof String String)) ([diagnostic (in-list (lint arc*))])
    (cons (some-system-path->string (find-relative-path directory (diagnostic-path diagnostic)))
          (diagnostic-message diagnostic))))

(test-case "a reference resolves to the arc's resolution of it"
  (check-equal? (resolve-reference valid (reference-scope-path workflow-reference)) workflow-reference))

(test-case "the baseline is the highest-numbered entry that is not pending"
  (check-equal? (arc-baseline (with-log (entry 1) (entry 2) (entry 3) #:pending 3)) (entry 2)))

(test-case "there is no baseline when the only entry is pending"
  (check-false (arc-baseline (with-log (entry 1) #:pending 1))))

(test-case "an entry with unreadable results is still the baseline"
  (check-equal? (arc-baseline (with-log (entry 1) (entry 2 #:results (failure "bad"))))
                (entry 2 #:results (failure "bad"))))

(test-case "a missing results file reads as a failure"
  (check-equal? (log-entry-run (entry 1 #:results #f)) (failure "expected a regular file")))

;;; Lint

(test-case "a valid arc has no diagnostics"
  (check-equal? (lint valid) '()))

(test-case "a missing state file is reported"
  (define state (list* (cons "arc.md" #f) (rest (arc-state-files valid))))
  (check-equal? (lint-messages (struct-copy arc valid [state-files state]))
                '(("arc.md" . "expected a regular file"))))

(test-case "a start file that did not read is reported at start.toml"
  (check-equal? (lint-messages (struct-copy arc valid [start (failure "start is missing next")] [references '()]))
                '(("start.toml" . "start is missing next"))))

(test-case "a reference to a missing file is reported at start.toml"
  (define missing (struct-copy reference workflow-reference [exists? #f]))
  (check-equal? (lint-messages (struct-copy arc valid [references (list missing)]))
                '(("start.toml" . "handoff:workflow.md: expected a regular file at /repository/workflow.md"))))

(test-case "a checks file that did not read is reported at checks.toml"
  (check-equal? (lint-messages (struct-copy arc valid [checks (failure "check x is missing claim")]))
                '(("checks.toml" . "check x is missing claim"))))

(test-case "the placeholder check is reported"
  (check-equal? (lint-messages (struct-copy arc valid [checks (list (make-check "replace-me" "c" "exit 3" 60))]))
                '(("checks.toml" . "check replace-me is the placeholder from handoff new; replace it with the arc's checks"))))

(test-case "a Markdown document without a summary is reported, and other documents are not read"
  (define documents (list (document "arc:notes.md" (build-path directory "notes.md") "# Notes\n\n- a list\n")
                          (document "arc:data/" (build-path directory "data") #f)))
  (check-equal? (lint-messages (struct-copy arc valid [documents documents]))
                '(("notes.md" . "expected a summary, a paragraph of prose after the opening headings, not a list, code block, table or HTML"))))

(test-case "an absent log is accepted"
  (check-equal? (lint (with-log #:shape 'absent)) '()))

(test-case "a log that is not a directory is reported"
  (check-equal? (lint-messages (with-log #:shape 'not-a-directory)) '(("log" . "expected a directory"))))

(test-case "an entry that is not a directory is reported"
  (check-equal? (lint-messages (with-log (log-entry 1 #f #f #f))) '(("log/1" . "expected a directory"))))

(test-case "a complete entry needs entry.md and results.toml"
  (check-equal? (lint-messages (with-log (entry 1 #:entry #f #:results #f)))
                '(("log/1/entry.md" . "expected a regular file")
                  ("log/1/results.toml" . "expected a regular file"))))

(test-case "the pending entry may lack entry.md and results.toml"
  (check-equal? (lint (with-log (entry 1) (entry 2 #:entry #f #:results #f) #:pending 2)) '()))

(test-case "unreadable results are reported, even in the pending entry"
  (check-equal? (lint-messages (with-log (entry 1 #:results (failure "run is missing host")) #:pending 1))
                '(("log/1/results.toml" . "run is missing host"))))

(test-case "a log entry name that is not an entry number is reported"
  (check-equal? (lint-messages (with-log (entry 1) #:invalid '("01")))
                '(("log/01" . "expected a log entry number, a positive integer without leading zeros"))))

;;; Rendering

(define (result [outcome : Outcome]
                #:id [id : String "x"]
                #:command [command : String "true"]
                #:output [output : String ""]
                #:truncated [truncated : Natural 0])
  (check-result (make-check id "holds" command 60) outcome 0.5 output truncated
                (case outcome [(true) 0] [(false) 1] [(unknown) 2] [else #f])))

(test-case "a true result shows no exit or output"
  (check-equal? (render-result (result 'true #:output "noise\n") #f #f)
                "id: x\nclaim: holds\noutcome: true\nduration: 0.5s\n"))

(test-case "a false result shows its exit and output"
  (check-equal? (render-result (result 'false #:output "a\nb\n") #f #f)
                "id: x\nclaim: holds\noutcome: false\nexit: 1\nduration: 0.5s\noutput:\n  a\n  b\n"))

(test-case "a timeout shows the check's timeout instead of an exit"
  (check-equal? (render-result (result 'timeout) #f #f)
                "id: x\nclaim: holds\noutcome: timeout\ntimeout: 60.0s\nduration: 0.5s\n"))

(test-case "only the last lines of long output are shown"
  (define output (string-join (for/list : (Listof String) ([n (in-range 1 26)]) (number->string n)) "\n"))
  (define text (render-result (result 'false #:output output) #f #f))
  (check-true (string-contains? text "output (last 20 of 25 lines):\n  6\n"))
  (check-true (string-suffix? text "  25\n")))

(test-case "output dropped at capture is counted"
  (check-true (string-contains? (render-result (result 'false #:output "end" #:truncated 9) #f #f)
                                "truncated: 9 bytes before the output were not kept\n")))

(test-case "a flipped result shows its output now and at the earlier entry"
  (define now (result 'true #:output "fine"))
  (define before (result 'false #:output "broken"))
  (check-equal? (render-result now (check-change now before 'flipped) 3)
                "id: x\nclaim: holds\noutcome: true\nchange: was false at log/3\nduration: 0.5s\noutput:\n  fine\noutput at log/3:\n  broken\n"))

(test-case "a redefined result names its earlier outcome"
  (define now (result 'true))
  (check-true (string-contains? (render-result now (check-change now (result 'false) 'redefined) 3)
                                "change: definition changed since log/3, where it was false\n")))

(test-case "a new result says so"
  (define now (result 'true))
  (check-true (string-contains? (render-result now (check-change now #f 'new) 3) "change: new since log/3\n")))

(test-case "an unchanged result has no change line"
  (define now (result 'true))
  (check-equal? (render-result now (check-change now now 'unchanged) 3) (render-result now #f #f)))

(test-case "a result is compared against the baseline's results"
  (define baseline (entry 2 #:results (run-with '("x"))))
  (check-true (string-contains? (render-compared-result (result 'false) baseline) "change: was true at log/2\n")))

(test-case "a result is not compared without a baseline with results"
  (check-equal? (render-compared-result (result 'false) (entry 2 #:results (failure "bad")))
                (render-result (result 'false) #f #f)))

(test-case "a run opens with the number of checks"
  (check-equal? (render-run-start (list (make-check "x" "c" "true" 60))) "running 1 check\n")
  (check-equal? (render-run-start '()) "running 0 checks\n"))

(test-case "a run closes with its host, duration and counts of outcomes"
  (define run (check-run "murph" 0 71.46 (branch-head "main" "abc")
                         (list (result 'true) (result 'false #:id "y") (result 'true #:id "z"))))
  (check-equal? (render-run-end run #f) "\nhost: murph, duration: 71.5s, summary: 2 true, 1 false\n"))

(test-case "a run without checks says so"
  (check-equal? (render-run-end (run-with '()) #f) "\nhost: murph, duration: 1.0s, summary: no checks\n"))

(test-case "checks removed since the baseline are listed before the summary"
  (check-equal? (render-run-end (run-with '("x")) (entry 2 #:results (run-with '("x" "gone"))))
                "\nremoved since log/2: gone (holds)\n\nhost: murph, duration: 1.0s, summary: 1 true\n"))

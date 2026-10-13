#lang typed/racket/base
;; The file formats, parsed and written without running anything.

(require racket/list
         racket/string
         typed/rackunit
         (rename-in "../model.rkt" [check make-check])
         "../error.rkt"
         "../toml.rkt")

;;; start.toml
(define base : Table
  (hasheq 'next "Continue" 'workflow "arc:workflow.md" 'read_first '()))

(test-case "a multiline next and read-first entries"
  (define start*
    (parse-start (hash-set* base
                            'next "Discuss the plan.\nThen implement."
                            'workflow "handoff:shared.md"
                            'read_first (list (hasheq 'path "project:handoff/main.rkt" 'reason "Entry point")))))
  (check-equal? (start-next start*) "Discuss the plan.\nThen implement.")
  (check-equal? (start-workflow start*) (scope-path 'handoff (string->path "shared.md")))
  (check-equal? (start-read-first start*)
                (list (read-first (scope-path 'project (string->path "handoff/main.rkt")) "Entry point"))))

(test-case "invalid fields and references are rejected"
  (for ([changes (in-list (ann (list '((next . " ")) '((next . #f)) '((extra . "unknown"))
                                '((workflow . "workflow.md")) '((workflow . "arc:/absolute"))
                                '((workflow . "handoff:../escape")) '((workflow . "handoff:a/../../b"))
                                '((workflow . "arc:")) '((workflow . "other:file"))
                                (list (cons 'read_first (hasheq))) '((read_first . ("file")))
                                (list (cons 'read_first (list (hasheq 'path "arc:context.md"))))
                                (list (cons 'read_first (list (hasheq 'path "arc:context.md" 'reason "")))))
                               (Listof (Listof (Pairof Symbol Any)))))])
    (define table (for/fold ([table : Table base]) ([change (in-list changes)])
                    (hash-set table (car change) (cdr change))))
    (check-exn exn:fail:handoff? (λ () (parse-start table)) (format "~s" changes)))
  (for ([field (in-list '(next workflow read_first))])
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
  (define checks (load "[[check]]\nid = \"second\"\nclaim = \"two\"\nrun = \"true\"\ntimeout = 5\n\n[[check]]\nid = \"first\"\nclaim = \"one\"\nrun = \"false\"\n"))
  (check-equal? (map check-id checks) '("second" "first"))
  (check-equal? (first checks) (make-check "second" "two" "true" 5))
  (check-equal? (check-timeout (second checks)) default-timeout))

(test-case "an empty checks file has no checks"
  (check-equal? (load "") '()))

(test-case "malformed checks are rejected"
  (for ([text (in-list (list "[[check]]\nid = \"x\"\nrun = \"true\"\n"
                             "[[check]]\nid = \"x\"\nclaim = \"c\"\n"
                             "[[check]]\nclaim = \"c\"\nrun = \"true\"\n"
                             "[[check]]\nid = \"x\"\nclaim = \"c\"\nrun = \"true\"\nexpect = 0\n"
                             "[[check]]\nid = \"x\"\nclaim = \" \"\nrun = \"true\"\n"
                             "[[check]]\nid = \"x\"\nclaim = \"c\"\nrun = \"true\"\ntimeout = 0\n"
                             "[[check]]\nid = \"x\"\nclaim = \"c\"\nrun = \"true\"\n[[check]]\nid = \"x\"\nclaim = \"d\"\nrun = \"true\"\n"
                             "[check]\nid = \"x\"\nclaim = \"c\"\nrun = \"true\"\n"
                             "check = 1\n"
                             "other = 1\n"))])
    (check-exn exn:fail:handoff? (λ () (load text)) text)))

(test-case "errors name the check"
  (check-exn #rx"check x is missing claim"
             (λ () (load "[[check]]\nid = \"x\"\nrun = \"true\"\n"))))

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
  (define-values (changes removed) (compare-results current earlier))
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

(test-case "a pattern's summary is its first paragraph after the headings"
  (check-equal? (pattern-summary "# `spike`\n\nAn investigation\n  with a budget.\n\nMore.\n") "An investigation with a budget.")
  (check-equal? (pattern-summary "\nNo heading.\n## Review\nNone.\n") "No heading.")
  (check-equal? (pattern-summary "# Only a heading\n") ""))

(test-case "a verbatim copy's provenance records the SHA-256 of the pattern in lowercase hexadecimal"
  (check-equal? (workflow-provenance "solo" "abc" "2026-10-13")
                "<!-- copied from workflow pattern solo (sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad) on 2026-10-13; verbatim -->\n"))

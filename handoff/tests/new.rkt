#lang typed/racket/base
;; Creating arcs with new, listing, installing and forking workflows, and
;; finding the guide.

(require racket/file
         racket/list
         racket/string
         typed/file/sha1
         typed/rackunit
         "common.rkt"
         (rename-in "../model.rkt" [check make-check])
         "../main.rkt"
         "../toml.rkt")

;; Point XDG_CONFIG_HOME and XDG_DATA_DIRS at fresh directories, returning the
;; pattern directory under each. Only for use inside `call-with-repository`,
;; whose environment is a copy.
(define (use-pattern-directories [base : Path])
  (define config (build-path base "config"))
  (define data (build-path base "data"))
  (define environment (current-environment-variables))
  (environment-variables-set! environment #"XDG_CONFIG_HOME" (string->bytes/utf-8 (path->string config)))
  (environment-variables-set! environment #"XDG_DATA_DIRS"
                              (string->bytes/utf-8 (string-append "relative:" (path->string data))))
  (values (build-path config "handoff" "workflows") (build-path data "handoff" "workflows")))

(define pattern-text "# `solo`\n\nThe agent works\nalone.\n\n## Review\n\nNone.\n")

;;; workflow list

(test-case "workflow list lists the user's patterns before installed ones, which they shadow"
  (call-with-repository
   (λ ([repository : Path])
     (define-values (config data) (use-pattern-directories (build-path repository 'up 'up)))
     (write-file (build-path data "solo.md") pattern-text)
     (write-file (build-path data "pair.md") "# Pair\n\nTwo agents.\n")
     (write-file (build-path config "solo.md") "# Mine\n\nMy own.\n")
     (write-file (build-path config "README.md") "# Not a pattern\n\nText.\n")
     (write-file (build-path config "notes.txt") "not a pattern")
     ;; Patterns belong to the user, so no repository is needed.
     (define elsewhere (make-temporary-file "handoff-test-~a" 'directory))
     (define result (handoff elsewhere "workflow" "list"))
     (delete-directory/files elsewhere)
     (check-equal? (outcome-code result) 0 (outcome-stderr result))
     (check-equal? (outcome-stdout result)
                   (format "name: pair\npath: ~a\nsummary: Two agents.\n\nname: solo\npath: ~a\nshadows: ~a\nsummary: My own.\n"
                           (build-path data "pair.md") (build-path config "solo.md") (build-path data "solo.md"))))))

(test-case "workflow list with no patterns says where it looked"
  (call-with-repository
   (λ ([repository : Path])
     (define-values (config _data) (use-pattern-directories (build-path repository 'up 'up)))
     (define result (handoff* repository "workflow" "list"))
     (check-equal? (outcome-code result) 0)
     (check-true (string-contains? (outcome-stdout result) (path->string config))))))

;;; new

(test-case "new copies the pattern with its provenance and writes headings only"
  (call-with-repository
   (λ ([repository : Path])
     (define-values (_config data) (use-pattern-directories (build-path repository 'up 'up)))
     (write-file (build-path data "solo.md") pattern-text)
     (define result (handoff* repository "new" "offsite" "solo"))
     (check-equal? (outcome-code result) 0 (outcome-stderr result))
     (define arc (build-path repository "offsite"))
     (check-equal? (file->string (build-path arc "start.toml"))
                   "next = \"\"\nworkflow = \"handoff:workflow-solo.md\"\nread_first = []\n")
     (check-equal? (map check-id (parse-checks (read-toml (build-path arc "checks.toml")))) (list placeholder-check-id))
     (for ([name+text (in-list '(("arc.md" . "# offsite\n") ("tasks.md" . "# Tasks\n")
                                 ("do-not-reopen.md" . "# Do not reopen\n")
                                 ("open-questions.md" . "# Open questions\n") ("review.md" . "# Review\n")))])
       (check-equal? (file->string (build-path arc (car name+text))) (cdr name+text)))
     (define copy (file->string (build-path repository "workflow-solo.md")))
     (check-regexp-match
      #px"^<!-- copied from workflow pattern solo \\(sha256:([0-9a-f]{64})\\) on \\d{4}-\\d{2}-\\d{2}; verbatim -->\n"
      copy)
     (check-equal? (second (string-split copy "\n" #:trim? #f)) "# `solo`")
     (check-true (string-suffix? copy pattern-text))
     (check-true (string-contains? copy (bytes->hex-string (sha256-bytes (string->bytes/utf-8 pattern-text)))))
     ;; Lint reports only the missing next and the placeholder check, and
     ;; nothing is committed.
     (check-regexp-match (pregexp (string-append
                                   "lint, until the arc is written:\n"
                                   "  [^\n]*start\\.toml: start\\.next must be a nonempty string\n"
                                   "  [^\n]*checks\\.toml: check replace-me is the placeholder from handoff new; [^\n]*\n$"))
                         (outcome-stdout result))
     (check-false (git* repository "rev-parse" "--verify" "--quiet" "HEAD"))
     (write-file (build-path arc "start.toml")
                 "next = \"Begin\"\nworkflow = \"handoff:workflow-solo.md\"\nread_first = []\n")
     (write-file (build-path arc "checks.toml") "[[check]]\nid = \"x\"\nclaim = \"holds\"\nrun = \"true\"\n")
     (check-equal? (outcome-code (handoff* repository "lint" "offsite")) 0))))

(test-case "the placeholder check fails lint, but check runs it as unknown"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (write-file (build-path arc "checks.toml") (cdr (assert (assoc "checks.toml" (new-arc-files "arc" #f)))))
     (define lint (handoff* repository "lint" "arc"))
     (check-equal? (outcome-code lint) 1)
     (check-true (string-contains? (outcome-stdout lint) "check replace-me is the placeholder from handoff new")
                 (outcome-stdout lint))
     (define result (handoff* repository "check" "arc"))
     (check-equal? (outcome-code result) 1 (outcome-stderr result))
     (check-true (string-contains? (outcome-stdout result) "outcome: unknown\nexit: 3\n"))
     (check-true (string-contains? (outcome-stdout result) "Replace this placeholder")))))

(test-case "new uses an existing copy of the pattern as it stands"
  (call-with-repository
   (λ ([repository : Path])
     (define-values (_config data) (use-pattern-directories (build-path repository 'up 'up)))
     (write-file (build-path data "solo.md") pattern-text)
     (write-file (build-path repository "workflow-solo.md") "amended")
     (define result (handoff* repository "new" "offsite" "solo"))
     (check-equal? (outcome-code result) 0 (outcome-stderr result))
     (check-true (string-contains? (outcome-stdout result) "handoff:workflow-solo.md (existing)"))
     (check-equal? (file->string (build-path repository "workflow-solo.md")) "amended"))))

(test-case "new takes a reference to an existing workflow, or none"
  (call-with-repository
   (λ ([repository : Path])
     (write-file (build-path repository "coined.md") "coined")
     (check-equal? (outcome-code (handoff* repository "new" "one" "handoff:coined.md")) 0)
     (check-equal? (file->string (build-path repository "one" "start.toml"))
                   "next = \"\"\nworkflow = \"handoff:coined.md\"\nread_first = []\n")
     (check-equal? (outcome-code (handoff* repository "new" "two")) 0)
     (check-equal? (file->string (build-path repository "two" "start.toml"))
                   "next = \"\"\nworkflow = \"\"\nread_first = []\n"))))

(test-case "new refuses, writing nothing"
  (call-with-repository
   (λ ([repository : Path])
     (use-pattern-directories (build-path repository 'up 'up))
     (define before (snapshot repository))
     (for ([arguments (in-list '(("arc") ("arc/x") (".git") ("fresh" "absent")
                                 ("fresh" "handoff:missing.md") ("fresh" "handoff:../escape.md")))])
       (define result (apply handoff* repository "new" arguments))
       (check-equal? (outcome-code result) 2 (format "~a" arguments))
       (check-equal? (outcome-stdout result) "")
       (check-equal? (snapshot repository) before)
       (check-false (directory-exists? (build-path repository "fresh")))))))

;;; workflow install

(test-case "workflow install copies a pattern once, as new does"
  (call-with-repository
   (λ ([repository : Path])
     (define-values (_config data) (use-pattern-directories (build-path repository 'up 'up)))
     (write-file (build-path data "solo.md") pattern-text)
     (define first (handoff* repository "workflow" "install" "solo"))
     (check-equal? (outcome-code first) 0 (outcome-stderr first))
     (check-equal? (outcome-stdout first)
                   (format "installed handoff:workflow-solo.md, copied from ~a\n" (build-path data "solo.md")))
     (define copy (file->string (build-path repository "workflow-solo.md")))
     (check-true (string-prefix? copy "<!-- copied from workflow pattern solo (sha256:"))
     (check-true (string-suffix? copy pattern-text))
     (write-file (build-path data "solo.md") "changed")
     (define second (handoff* repository "workflow" "install" "solo"))
     (check-equal? (outcome-code second) 0)
     (check-equal? (outcome-stdout second) "handoff:workflow-solo.md already exists and was not modified\n")
     (check-equal? (file->string (build-path repository "workflow-solo.md")) copy)
     (check-equal? (outcome-code (handoff* repository "workflow" "install" "absent")) 2))))

;;; workflow fork

(define (fork! [repository : Path] [arc : String] [name : String] [source : String])
  (define result (handoff* repository "workflow" "fork" arc name source))
  (check-equal? (outcome-code result) 0 (outcome-stderr result))
  (outcome-stdout result))

(define (sha256-hex [text : String])
  (bytes->hex-string (sha256-bytes (string->bytes/utf-8 text))))

(define (fork-pattern [source : String] [hashed : String] [body : String])
  (pregexp (format "^<!-- forked from ~a \\(sha256:~a\\) on \\d{4}-\\d{2}-\\d{2} -->\n~a$"
                   (regexp-quote source) hashed (regexp-quote body))))

(test-case "workflow fork copies a reference into the arc, recording only the immediate source"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (define shared "<!-- copied from workflow pattern shared (sha256:00) on 2026-10-01; verbatim -->\n# Shared\n\nRules.\n")
     (write-file (build-path repository "shared.md") shared)
     (define start (file->string (build-path arc "start.toml")))
     (check-equal? (fork! repository "arc" "mine" "handoff:shared.md")
                   "forked arc:workflow-mine.md from handoff:shared.md\nto use it, set workflow in start.toml to \"arc:workflow-mine.md\"\n")
     (define fork (file->string (build-path arc "workflow-mine.md")))
     (check-regexp-match (fork-pattern "handoff:shared.md" (sha256-hex shared) "# Shared\n\nRules.\n") fork)
     ;; start.toml is the agent's to change.
     (check-equal? (file->string (build-path arc "start.toml")) start)
     ;; A fork of a fork names the fork it came from; arc: means the arc forked into.
     (fork! repository "arc" "again" "arc:workflow-mine.md")
     (check-regexp-match (fork-pattern "arc:workflow-mine.md" (sha256-hex fork) "# Shared\n\nRules.\n")
                         (file->string (build-path arc "workflow-again.md"))))))

(test-case "workflow fork copies a pattern, whatever the arc runs under"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (define-values (_config data) (use-pattern-directories (build-path repository 'up 'up)))
     (write-file (build-path data "solo.md") pattern-text)
     (check-equal? (fork! repository "arc" "solo-amended" "solo")
                   "forked arc:workflow-solo-amended.md from workflow pattern solo\nto use it, set workflow in start.toml to \"arc:workflow-solo-amended.md\"\n")
     (check-regexp-match (fork-pattern "workflow pattern solo" (sha256-hex pattern-text) pattern-text)
                         (file->string (build-path arc "workflow-solo-amended.md")))
     ;; Forking installs nothing at the root.
     (check-false (file-exists? (build-path repository "workflow-solo.md"))))))

(test-case "workflow fork refuses, writing nothing"
  (call-with-arc
   (λ ([repository : Path] [arc : Path])
     (use-pattern-directories (build-path repository 'up 'up))
     (write-file (build-path arc "workflow-taken.md") "taken")
     (define before (snapshot repository))
     (for ([arguments (in-list '(("arc" "Bad Name" "handoff:shared.md") ("arc" "taken" "handoff:shared.md")
                                 ("absent" "fine" "handoff:shared.md") ("arc" "fine" "handoff:missing.md")
                                 ("arc" "fine" "absent-pattern") ("arc" "fine" "other:shared.md")
                                 ("arc" "fine")))])
       (define result (apply handoff* repository "workflow" "fork" arguments))
       (check-equal? (outcome-code result) 2 (format "~a" arguments))
       (check-equal? (snapshot repository) before)))))

;;; guide

(test-case "guide prints the first HANDOFF.md under XDG_DATA_DIRS, outside any repository"
  (define base (make-temporary-file "handoff-test-~a" 'directory))
  (define-values (first second third)
    (values (build-path base "first") (build-path base "second") (build-path base "third")))
  (write-file (build-path second "handoff" "HANDOFF.md") "second")
  (write-file (build-path third "handoff" "HANDOFF.md") "third")
  ;; Relative entries are ignored, even when they hold a match.
  (write-file (build-path base "relative" "handoff" "HANDOFF.md") "relative")
  (define (guide [data-dirs : String])
    (define environment (environment-variables-copy (current-environment-variables)))
    (environment-variables-set! environment #"XDG_DATA_DIRS" (string->bytes/utf-8 data-dirs))
    (parameterize ([current-environment-variables environment])
      (handoff base "guide")))
  (define found (guide (string-join (list "relative" (path->string first) (path->string second)
                                          (path->string third))
                                    ":")))
  (check-equal? (outcome-code found) 0 (outcome-stderr found))
  (check-equal? (outcome-stdout found) (format "~a\n" (build-path second "handoff" "HANDOFF.md")))
  (define missing (guide (string-append "relative:" (path->string first))))
  (check-equal? (outcome-code missing) 2)
  (check-equal? (outcome-stdout missing) "")
  (check-true (string-contains? (outcome-stderr missing) (path->string first)) (outcome-stderr missing))
  (check-equal? (outcome-code (handoff base "guide" "extra")) 2)
  (delete-directory/files base))

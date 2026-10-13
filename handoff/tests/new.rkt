#lang typed/racket/base
;; Listing workflow patterns and creating arcs with new.

(require racket/file
         racket/list
         racket/string
         typed/file/sha1
         typed/rackunit
         "common.rkt"
         (rename-in "../model.rkt" [check make-check])
         "../main.rkt")

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

;;; workflows

(test-case "workflows lists the user's patterns before installed ones, which they shadow"
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
     (define result (handoff elsewhere "workflows"))
     (delete-directory/files elsewhere)
     (check-equal? (outcome-code result) 0 (outcome-stderr result))
     (check-equal? (outcome-stdout result)
                   (format "name: pair\npath: ~a\nsummary: Two agents.\n\nname: solo\npath: ~a\nshadows: ~a\nsummary: My own.\n"
                           (build-path data "pair.md") (build-path config "solo.md") (build-path data "solo.md"))))))

(test-case "workflows with no patterns says where it looked"
  (call-with-repository
   (λ ([repository : Path])
     (define-values (config _data) (use-pattern-directories (build-path repository 'up 'up)))
     (define result (handoff* repository "workflows"))
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
     (check-equal? (file->string (build-path arc "checks.toml")) "")
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
     ;; Lint reports only the missing next, and nothing is committed.
     (check-regexp-match #px"lint, until the arc is written:\n  [^\n]*start\\.toml: start\\.next must be a nonempty string\n$"
                         (outcome-stdout result))
     (check-false (git* repository "rev-parse" "--verify" "--quiet" "HEAD"))
     (write-file (build-path arc "start.toml")
                 "next = \"Begin\"\nworkflow = \"handoff:workflow-solo.md\"\nread_first = []\n")
     (check-equal? (outcome-code (handoff* repository "lint" "offsite")) 0))))

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

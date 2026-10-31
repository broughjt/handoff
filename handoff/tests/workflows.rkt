#lang typed/racket/base
;; Reusable workflows.

(require racket/function
         (except-in typed/rackunit check)
         "../main.rkt")

(require/typed gregor
  [iso8601->moment (-> String Moment)])

;; The SHA-256 of #"x", as sha256sum prints it.
(define x-hash "2d711642b726b04401627ca9fbac32f5c8530fb1903cc4db02258717921a4881")

(define now (iso8601->moment "2026-10-31T12:00:00Z"))

(: split (-> String (Pairof (U workflow-provenance #f) String)))
(define (split text)
  (call-with-values (thunk (split-workflow-provenance text))
                    (inst cons (U workflow-provenance #f) String)))

(define spike-copy (scope-path 'handoff (string->path "workflow-spike.md")))

(: new-arc-text (-> String scope-path String))
(define (new-arc-text file workflow)
  (cdr (assert (assoc file (new-arc-files "a" workflow)))))

(test-case "a workflow name is lowercase letters, digits and hyphens"
  (check-true (workflow-name? "design-first-tdd2")))

(test-case "a workflow name does not start with a hyphen"
  (check-false (workflow-name? "-x")))

(test-case "a workflow name has no capitals or underscores"
  (check-equal? (map workflow-name? (list "README" "x_y" ""))
                (list #f #f #f)))

(test-case "a fork of a reusable workflow takes its name"
  (check-equal? (workflow-fork-name (reusable-workflow "spike" (string->path "/w/spike.md")))
                "spike"))

(test-case "a fork of a reference takes its file name without workflow- and .md"
  (check-equal? (workflow-fork-name (scope-path 'arc (string->path "workflow-review.md")))
                "review"))

(test-case "a fork of a reference without the workflow- prefix takes its file name without .md"
  (check-equal? (workflow-fork-name (scope-path 'project (string->path "docs/review.md")))
                "review"))

(test-case "a fork of a reference whose file name is not a workflow name takes it anyway"
  (check-equal? (workflow-fork-name (scope-path 'handoff (string->path "Review_Notes.md")))
                "Review_Notes"))

(test-case "a workflow's copy is named workflow-<name>.md"
  (check-equal? (workflow-copy-name "spike") "workflow-spike.md"))

(test-case "a verbatim copy's provenance line names the workflow, its hash and the date"
  (check-equal? (format-workflow-provenance-line 'copied "workflow spike" #"x" now)
                (format "<!-- copied from workflow spike (sha256:~a) on 2026-10-31; verbatim -->\n"
                        x-hash)))

(test-case "a fork's provenance line names its source and is not verbatim"
  (check-equal? (format-workflow-provenance-line 'forked "handoff:workflow-spike.md" #"x" now)
                (format "<!-- forked from handoff:workflow-spike.md (sha256:~a) on 2026-10-31 -->\n"
                        x-hash)))

(test-case "a provenance line's date is the UTC date"
  (check-regexp-match #rx" on 2026-11-01; verbatim -->\n$"
                      (format-workflow-provenance-line 'copied
                                                       "workflow spike"
                                                       #"x"
                                                       (iso8601->moment "2026-10-31T22:00:00-05:00"))))

(test-case "a verbatim copy's provenance is split from its text, with the hash"
  (check-equal? (split (string-append (format-workflow-provenance-line 'copied "workflow spike" #"x" now)
                                      "# Spike\n"))
                (cons (workflow-provenance 'copied x-hash) "# Spike\n")))

(test-case "a fork's provenance is split from its text, with the hash"
  (check-equal? (split (string-append (format-workflow-provenance-line 'forked "workflow spike" #"x" now)
                                      "# Spike\n"))
                (cons (workflow-provenance 'forked x-hash) "# Spike\n")))

(test-case "a created workflow's provenance has no hash"
  (check-equal? (split "<!-- created for this arc on 2026-10-31 -->\n# W\n")
                (cons (workflow-provenance 'created #f) "# W\n")))

(test-case "a text without a provenance line is left whole"
  (check-equal? (split "# W\n") (cons #f "# W\n")))

(test-case "a provenance line that is not the first line is not provenance"
  (check-equal? (split "# W\n<!-- created for this arc on 2026-10-31 -->\n")
                (cons #f "# W\n<!-- created for this arc on 2026-10-31 -->\n")))

(test-case "a copy is unchanged when its hash is the reusable workflow's"
  (check-equal? (workflow-install-status (format-workflow-provenance-line 'copied "workflow spike" #"x" now) #"x")
                'unchanged))

(test-case "a copy is changed when its hash is not the reusable workflow's"
  (check-equal? (workflow-install-status (format-workflow-provenance-line 'copied "workflow spike" #"x" now) #"y")
                'changed))

(test-case "a fork is not a verbatim copy"
  (check-equal? (workflow-install-status (format-workflow-provenance-line 'forked "workflow spike" #"x" now) #"x")
                'not-verbatim))

(test-case "a created workflow is not a verbatim copy"
  (check-equal? (workflow-install-status "<!-- created for this arc on 2026-10-31 -->\n" #"x")
                'not-verbatim))

(test-case "a file without a provenance line is not a verbatim copy"
  (check-equal? (workflow-install-status "x" #"x") 'not-verbatim))

(test-case "a new arc's files are its state files, in order"
  (check-equal? (map (inst car String String) (new-arc-files "a" placeholder-workflow))
                (list "start.toml" "checks.toml" "arc.md" "tasks.md" "do-not-reopen.md"
                      "open-questions.md" "review.md")))

(test-case "a new arc's start.toml holds the placeholders"
  (check-equal? (parse-start (new-arc-text "start.toml" placeholder-workflow)
                             (string->path "a/start.toml"))
                (start "replace-me" (scope-path 'arc (string->path "workflow-replace-me.md")) null)))

(test-case "a new arc's start.toml names the workflow it is given"
  (check-equal? (start-workflow (parse-start (new-arc-text "start.toml" spike-copy)
                                             (string->path "a/start.toml")))
                spike-copy))

(test-case "a new arc's checks.toml holds the placeholder check"
  (check-equal? (map check-id (parse-checks (new-arc-text "checks.toml" placeholder-workflow)
                                            (string->path "a/checks.toml")))
                (list "replace-me")))

(test-case "a new arc's arc.md is headed by its name"
  (check-equal? (new-arc-text "arc.md" placeholder-workflow) "# a\n"))

#lang typed/racket/base

(require racket/format
         racket/function
         racket/list
         racket/match
         racket/math
         racket/path
         racket/string
         typed/file/sha1
         "error.rkt"
         "toml.rkt")

(require/typed gregor
  [#:opaque Moment moment?]
  [->posix (-> Moment Real)]
  [posix->moment (-> Real Integer Moment)]
  [moment->iso8601 (-> Moment String)])

(provide repository-path
         archive-tag-prefix
         start-file
         checks-file
         state-files
         log-directory
         entry-file
         results-file
         parse-log-entry-name
         log-entry-path
         entry-headline
         (struct-out start)
         (struct-out scope-path)
         (struct-out read-first)
         Scope
         parse-start
         parse-scope-path
         scope-path->string
         resolve-scope-path
         (struct-out check)
         (struct-out check-result)
         (struct-out branch-head)
         (struct-out check-run)
         Outcome
         default-timeout
         parse-checks
         parse-check-result
         parse-check-run
         check-run->toml
         timestamp->string
         date->string
         describe-branch-head
         Change
         (struct-out check-change)
         compare-result
         removed-checks
         (struct-out pattern)
         pattern-name?
         document-summary
         summary-issue
         verbatim-copy?
         workflow-copy-name
         workflow-provenance
         strip-provenance
         placeholder-check-id
         new-arc-files
         (struct-out failure)
         Read
         (struct-out arc)
         (struct-out reference)
         (struct-out document)
         (struct-out entry-log)
         (struct-out log-entry)
         resolve-reference
         log-entry-run
         arc-baseline
         (struct-out diagnostic)
         diagnostic->string
         lint
         output-lines
         seconds->string
         render-run-start
         render-compared-result
         render-result
         render-run-end
         (struct-out project-status)
         (struct-out project-change)
         History
         render-project
         render-run
         render-start
         arc-pending-entry
         next-entry-number
         lint-refusal
         commit-refusal)

;;; Layout

(define repository-path (build-path ".scratch" "handoff"))
(define archive-tag-prefix "archive/")

;; The expected layout of a handoff arc directory
(define start-file "start.toml")
(define checks-file "checks.toml")
;; TODO: I think this would be better as a locally defined map in `new-arc-files`
(define state-headings
  (list (cons "arc.md" #f)
        (cons "tasks.md" "Tasks")
        (cons "do-not-reopen.md" "Do not reopen")
        (cons "open-questions.md" "Open questions")
        (cons "review.md" "Review")))
(define state-files (map (inst car String (U String #f)) state-headings))
(define log-directory "log")
(define entry-file "entry.md")
(define results-file "results.toml")

;; TODO: Do the next three functions belong under the "Layout" section?

;; Ah, a typical canonicity problem of string representations of data
;; types: "1" and "01" both represent 1. To avoid this, we require the
;; absence of leading zeros.
(: parse-log-entry-name (-> String (U Exact-Positive-Integer #f)))
(define (parse-log-entry-name name)
  (and (regexp-match? #px"^[1-9][0-9]*$" name)
       (let ([number (string->number name)])
         (and (exact-positive-integer? number) number))))

(: log-entry-path (-> Path Exact-Positive-Integer Path))
(define (log-entry-path arc number)
  (build-path arc log-directory (number->string number)))

;; TODO: Do we parse markdown headlines anywhere else?

;; Parse the text of a log entry's opening markdown heading, which must occur on
;; its first nonblank line.
(: entry-headline (-> String String))
(define (entry-headline entry)
  (define lines (filter (compose non-empty-string? string-trim)
                        (string-split entry "\n")))
  (when (empty? lines)
    (raise-handoff-error "the entry is empty"))
  (define heading (regexp-match #px"^ {0,3}#{1,6}[ \t]+(.*)$" (first lines)))
  (define headline
    (and heading
         ;; Drop an optional closing sequence of hastags.
         (string-trim (regexp-replace #px"(^|[ \t]+)#+[ \t]*$" (string-trim (assert (second heading))) ""))))
  (unless (and headline (non-empty-string? headline))
    (raise-handoff-error "the entry must open with a headline"))
  headline)

;;; start.toml

(struct start
  ([next : String]
   [workflow : scope-path]
   [read-first : (Listof read-first)])
  #:transparent)
;; Maybe put in its own section since other things use scope paths besides the start.toml struct
(struct scope-path ([scope : Scope] [path : Path]) #:transparent)
(define-type Scope (U 'arc 'handoff 'project))
(struct read-first ([path : scope-path] [reason : String]) #:transparent)

(: parse-start (-> Table start))
(define (parse-start table)
  (expect-fields table (list 'next 'workflow 'read-first) null "start")
  (define reads (hash-ref table 'read-first))
  (unless (list? reads)
    (raise-handoff-error "read-first must be an array of tables"))
  (start (expect-non-empty-string table 'next "start")
         (parse-scope-path (expect-non-empty-string table 'workflow "start"))
         (for/list ([entry (in-list reads)] [index (in-naturals)])
           (define context (format "read-first[~a]" index))
           (define read (expect-table entry context))
           (expect-fields read (list 'path 'reason) null context)
           (read-first (parse-scope-path (expect-non-empty-string read 'path context))
                       (expect-non-empty-string read 'reason context)))))

(: parse-scope-path (-> String scope-path))
(define (parse-scope-path text)
  (define fail
    (thunk
     (raise-handoff-error "~s: expected arc:, handoff:, or project: followed by a relative file path without '..'" text)))
  ;; Is this something we could lift into the regex and define globally so other functions could use it? I'm thinking something like greping for the scope-path regex in the future
  (match (regexp-match #rx"^([^:]*):(.*)$" text)
    [(list _ (? string? scope-name) (? string? relative))
     (define scope : Scope
       (match scope-name
         ["arc" 'arc]
         ["handoff" 'handoff]
         ["project" 'project]
         [_ (fail)]))
     (when (or (string=? relative "") (regexp-match? #rx"\0" relative))
       (fail))
     (define path (string->path relative))
     (unless (and (relative-path? path) (not (memq 'up (explode-path path))))
       (fail))
     (scope-path scope path)]
    [_ (fail)]))

(: scope-path->string (-> scope-path String))
(define (scope-path->string scope-path)
  (format "~a:~a" (scope-path-scope scope-path) (path->string (scope-path-path scope-path))))

(: resolve-scope-path (-> scope-path #:arc Path #:repository Path #:project Path Path))
(define (resolve-scope-path scope-path #:arc arc #:repository repository #:project project)
  (build-path (case (scope-path-scope scope-path)
                [(arc) arc]
                [(handoff) repository]
                [(project) project])
              (scope-path-path scope-path)))

;;; checks.toml and results.toml

;; TODO: Put all global constants at the top of the file?
(define default-timeout : Positive-Real 60)

(struct check ([id : String]
               [claim : String]
               [command : String]
               [timeout : Positive-Real])
  #:transparent)

;; Exit 0 is `true`, exit 1 is `false`, a timeout is `timeout`, and any other
;; exit code is `unknown`.
(define-type Outcome (U 'true 'false 'unknown 'timeout))
(define-predicate outcome? Outcome)

;; The `exit` field is `#f` when the outcome is a timeout, the `duration` field
;; measures seconds, and the `truncated` field counts the bytes dropped from the
;; start of `output` if it went over the capture limit.
(struct check-result ([check : check]
                      [outcome : Outcome]
                      [duration : Nonnegative-Real]
                      [output : String]
                      [truncated : Natural]
                      [exit : (U Integer #f)])
  #:transparent)

;; The results and associated metadata of running each of the checks in a
;; checks.toml. This is recorded to the results.toml file of log entry as a part
;; of the handoff update process.
;;
;; `started` and the durations come from different clocks. `started` is
;; wall-clock time in whole seconds since the epoch: it says when, and seconds
;; match the resolution of Git's commit times. Durations come from the monotonic
;; clock, which does not jump when the system clock is adjusted but has no fixed
;; origin, so it can only say how long.
(struct check-run ([host : String]
                   [started : Integer]
                   [duration : Nonnegative-Real]
                   [project-head : branch-head]
                   [results : (Listof check-result)])
  #:transparent)

;; The branch head, consisting of the branch name (`#f` when detached) and its
;; latest commit (`#f` before first commit).
(struct branch-head ([branch : (U String #f)] [head : (U String #f)]) #:transparent)

;; Checks are an array of tables so that they are reported in file order.
(: parse-checks (-> Table (Listof check)))
(define (parse-checks table)
  (expect-fields table null (list 'check) "checks")
  (parse-check-entries table null null (λ ([_table : Table] [_context : String] [check : check]) check)))

(: parse-check-result (-> Table String check check-result))
(define (parse-check-result table context check)
  (define outcome (let ([value (hash-ref table 'outcome)])
                    (and (string? value) (string->symbol value))))
  (unless (outcome? outcome)
    (raise-handoff-error "~a: outcome must be true, false, unknown or timeout" context))
  (define exit (hash-ref table 'exit #f))
  (unless (or (exact-integer? exit) (not exit))
    (raise-handoff-error "~a: exit must be an integer" context))
  (unless (eq? (eq? outcome 'timeout) (not exit))
    (raise-handoff-error "~a: exit must be present exactly when the outcome is not timeout" context))
  (define output (hash-ref table 'output))
  (unless (string? output)
    (raise-handoff-error "~a: output must be a string" context))
  (define truncated (hash-ref table 'truncated (const 0)))
  (unless (exact-nonnegative-integer? truncated)
    (raise-handoff-error "~a: truncated must be a nonnegative integer" context))
  (check-result check
                outcome
                (expect-nonnegative-real table 'duration context)
                output
                truncated
                exit))

(: parse-check-run (-> Table check-run))
(define (parse-check-run table)
  (expect-fields table (list 'run 'project) (list 'check) "results")
  (define run-table (expect-table (hash-ref table 'run) "run"))
  (expect-fields run-table (list 'host 'started 'duration) null "run")
  (define project-table (expect-table (hash-ref table 'project) "project"))
  (expect-fields project-table null (list 'head 'branch) "project")
  (define started (hash-ref run-table 'started))
  (check-run
   (expect-non-empty-string run-table 'host "run")
   (if (moment? started)
       (exact-floor (->posix started))
       (raise-handoff-error "started must be a datetime with an offset"))
   (expect-nonnegative-real run-table 'duration "run")
   (branch-head
    (and (hash-has-key? project-table 'branch)
         (expect-non-empty-string project-table 'branch "project"))
    (and (hash-has-key? project-table 'head)
         (expect-non-empty-string project-table 'head "project")))
   (parse-check-entries table
                        (list 'outcome 'duration 'output)
                        (list 'exit 'truncated)
                        parse-check-result)))

;; The `check` array of tables in `table`, in file order. Each entry is a check
;; definition plus the `required` and `optional` fields that `parse` reads.
(: parse-check-entries
   (All (A) (-> Table (Listof Symbol) (Listof Symbol) (-> Table String check A) (Listof A))))
(define (parse-check-entries table required optional parse)
  (define entries (hash-ref table 'check (const null)))
  (unless (list? entries)
    (raise-handoff-error "check must be an array of tables"))
  (define-values (checks parsed)
    (for/lists ([checks : (Listof check)] [parsed : (Listof A)])
               ([entry (in-list entries)] [index : Natural (in-naturals)])
      (define table (expect-table entry (format "check[~a]" index)))
      (define id (hash-ref table 'id #f))
      (define context (if (string? id)
                          (format "check ~a" id)
                          (format "check[~a]" index)))
      (expect-fields table (list* 'id 'claim 'command required) (cons 'timeout optional) context)
      (define timeout (hash-ref table 'timeout (const default-timeout)))
      (define check*
        (check (expect-non-empty-string table 'id context)
               (expect-non-empty-string table 'claim context)
               (expect-non-empty-string table 'command context)
               (if (and (real? timeout) (positive? timeout) (not (infinite? timeout)))
                   timeout
                   (raise-handoff-error "~a: timeout must be a positive number of seconds" context))))
      (values check* (parse table context check*))))
  (define duplicate (check-duplicates (map check-id checks)))
  (when duplicate
    (raise-handoff-error "check ~a appears more than once" duplicate))
  parsed)

(: check-run->toml (-> check-run String))
(define (check-run->toml run)
  (define out (open-output-string))
  (define (field [key : String] [value : String])
    (fprintf out "~a = ~a\n" key value))
  (define project (check-run-project-head run))
  (fprintf out "[run]\n")
  (field "host" (toml-string (check-run-host run)))
  (field "started" (timestamp->string (check-run-started run)))
  (field "duration" (toml-float (check-run-duration run) 3))
  (fprintf out "\n[project]\n")
  (let ([head (branch-head-head project)])
    (when head
      (field "head" (toml-string head))))
  (let ([branch (branch-head-branch project)])
    (when branch
      (field "branch" (toml-string branch))))
  (for ([result (in-list (check-run-results run))])
    (define check (check-result-check result))
    (define timeout (check-timeout check))
    (define exit (check-result-exit result))
    (fprintf out "\n[[check]]\n")
    (field "id" (toml-string (check-id check)))
    (field "claim" (toml-string (check-claim check)))
    (field "command" (toml-string (check-command check)))
    (field "timeout" (number->string (if (exact-integer? timeout)
                                         timeout
                                         (exact->inexact timeout))))
    (field "outcome" (toml-string (symbol->string (check-result-outcome result))))
    (when exit (field "exit" (number->string exit)))
    (field "duration" (toml-float (check-result-duration result) 3))
    (unless (zero? (check-result-truncated result))
      (field "truncated" (number->string (check-result-truncated result))))
    ;; Last, since it is usually the only multiline value.
    (field "output" (toml-string (check-result-output result))))
  (get-output-string out))

;; TODO: rename to format-branch-head?

(: describe-branch-head (-> branch-head String))
(define (describe-branch-head branch-head*)
  (define head (branch-head-head branch-head*))
  (format "~a at ~a"
          (or (branch-head-branch branch-head*) "a detached HEAD")
          (if head (substring head 0 (min 12 (string-length head))) "no commit")))

;; TODO: Really want to get rid of these two

;; Seconds since the epoch as an ISO 8601 datetime in UTC.
(: timestamp->string (-> Integer String))
(define (timestamp->string seconds)
  (moment->iso8601 (posix->moment seconds 0)))

;; Seconds since the epoch as an ISO 8601 date in UTC.
(: date->string (-> Integer String))
(define (date->string seconds)
  (substring (timestamp->string seconds) 0 10))

;;; Comparing runs

;; Is this comparison code systematic? Should false or unknown with different output count as a diff?

(define-type Change (U 'unchanged 'flipped 'redefined 'new))

(struct check-change ([after : check-result]
                      [before : (U check-result #f)] ;; `#f` when the `after` check is new
                      [change : Change])
  #:transparent)

;; How `after` changed since the result with its id in `previous`.
(: compare-result (-> check-result (Listof check-result) check-change))
(define (compare-result after previous)
  (define id (check-id (check-result-check after)))
  (define before
    (findf (λ ([result : check-result]) (equal? (check-id (check-result-check result)) id)) previous))
  (define change
    (cond
      [(not before) 'new]
      [(not (equal? (check-result-check before) (check-result-check after))) 'redefined]
      [(eq? (check-result-outcome before) (check-result-outcome after)) 'unchanged]
      [else 'flipped]))
  (check-change after before change))

;; The checks of `previous` whose ids none of `current` has.
(: removed-checks (-> (Listof check) (Listof check-result) (Listof check)))
(define (removed-checks current previous)
  (define ids (map check-id current))
  (for/list ([result (in-list previous)]
             #:unless (member (check-id (check-result-check result)) ids))
    (check-result-check result)))

;; TODO: This should be renamed

;; TODO: Why do we care if there are multiple copies? Should we just list one instead of taking care to find and list shadows?

;;; Workflow patterns and new arcs

;; A workflow pattern's file, with the files of the same name in lower
;; precedence directories that it shadows.
(struct pattern ([name : String] [path : Path] [shadowed : (Listof Path)]) #:transparent)

;; TODO: When we define string predicates that are literally just regexp-match? the-regex, we should just export the regex globally

;; A pattern's name is its file name without `.md`. We use lowercase only so
;; that a README.md sitting in the same directory as the patterns does not
;; count.
(: pattern-name? (-> String Boolean))
(define (pattern-name? name)
  (regexp-match? #px"^[a-z0-9][a-z0-9-]*$" name))

;; TODO: Move this to a section for doing text wrangling on markdown files

;; The lines of the first paragraph after a document's provenance line and
;; opening headings. Workflows and topic documents share this layout.
(: first-paragraph (-> String (Listof String)))
(define (first-paragraph text)
  ;; TODO: Export this as a regex constant
  (define (heading? [line : String]) (regexp-match? #px"^ {0,3}#" line))
  (define (blank? [line : String]) (string=? (string-trim line) ""))
  (define (skip? [line : String]) (or (blank? line) (heading? line)))
  (takef (dropf (string-split (strip-provenance text) "\n" #:trim? #f) skip?) (negate skip?)))

;; A document's summary: its first paragraph, on one line.
(: document-summary (-> String String))
(define (document-summary text)
  (string-join (map string-trim (first-paragraph text))))

;; Openings that mark a paragraph as something other than prose
(define non-prose-openings
  (list #px"^[-*+]\\s"       ; bullet list
        #px"^[0-9]+[.)]\\s"  ; ordered list
        #px"^(```|~~~)"      ; code fence
        #px"^\\|"            ; table
        #px"^<"))            ; HTML

;; Why a document has no usable summary, or #f when it has one.
(: summary-issue (-> String (U String #f)))
(define (summary-issue text)
  (define paragraph (first-paragraph text))
  (cond
    [(empty? paragraph) "expected a summary, a paragraph of prose after the opening headings"]
    [(let ([line (string-trim (first paragraph) #:right? #f)])
       (ormap (λ ([opening : Regexp]) (regexp-match? opening line)) non-prose-openings))
     "expected a summary, a paragraph of prose after the opening headings, not a list, code block, table or HTML"]
    [else #f]))

;; TODO: Make the workflow heading systematic based on what is now described in HANDOFF.md and the skills

;; Whether a workflow file is a verbatim copy
(: verbatim-copy? (-> String Boolean))
(define (verbatim-copy? text)
  (regexp-match? #px"^<!-- copied [^\n]*; verbatim -->\n" text))

;; Where a verbatim copy of the pattern lives, relative to the handoff
;; repository, which shares it between arcs.
(: workflow-copy-name (-> String String))
(define (workflow-copy-name name)
  (string-append "workflow-" name ".md"))

;; This is part of the "make it systematic" TODO a few lines up

;; The line opening a workflow copied verbatim or forked from `source`, which
;; describes a pattern ("workflow spike") or is a reference to a file,
;; when the source's contents were `text`. The hash says whether the source has
;; changed since. For a verbatim copy, also include whether the copy was
;; amended.

;; TODO: Isn't it the case that verbatim copies shouldn't be amended?
(: workflow-provenance (-> (U 'copied 'forked) String String String String))
(define (workflow-provenance how source text date)
  (format "<!-- ~a from ~a (sha256:~a) on ~a~a -->\n"
          how
          source
          (bytes->hex-string (sha256-bytes (string->bytes/utf-8 text)))
          date
          (if (eq? how 'copied) "; verbatim" "")))

;; A workflow's text without its opening provenance line, if it has one.
(: strip-provenance (-> String String))
(define (strip-provenance text)
  (regexp-replace #px"^<!-- (copied|forked|created) [^\n]*-->\n" text ""))

;; TODO: Now we're talking about checks again, why is it here?

(define placeholder-check-id "replace-me")

(define placeholder-checks
  (string-append
   "# Each check is a claim about the project and a command that acts as a\n"
   "# mechanical decision procedure for the claim. Checks run in parallel and\n"
   "# are reported in file order.\n"
   "# The `timeout` field is measured in seconds; it is optional and defaults to\n"
   "# " (number->string default-timeout) ".\n"
   "[[check]]\n"
   "id = " (toml-string placeholder-check-id) "\n"
   "claim = " (toml-string "A natural language claim that is either true or false") "\n"
   "command = " (toml-string (string-append
                              "echo \"Replace this placeholder from handoff new with the arc's checks. "
                              "command is a shell command, run from the project root, that exits 0 when "
                              "the claim is true, 1 when it is false, and anything else when unknown.\"; exit 3"))
   "\n"
   "timeout = " (number->string default-timeout) "\n"))

;; TODO: This feels like it should live below the definition of the arc struct

(: new-arc-files (-> String (U scope-path #f) (Listof (Pairof String String))))
(define (new-arc-files name workflow)
  (list*
   (cons start-file
         (format "next = ~a\nworkflow = ~a\nread-first = []\n"
                 (toml-string "")
                 (toml-string (if workflow (scope-path->string workflow) ""))))
   (cons checks-file placeholder-checks)
   (for/list : (Listof (Pairof String String)) ([file+heading (in-list state-headings)])
     (cons (car file+heading) (format "# ~a\n" (or (cdr file+heading) name))))))

;;; Arcs

;; Why part of an arc could not be read, kept as a value so that lint can
;; report it and start can render around it.
(struct failure ([message : String]) #:transparent)
(define-type (Read A) (U A failure))

;; An arc as the script sees it: everything lint, start and update decide
;; from, read once by `read-arc` in main.rkt. `references` resolves the
;; workflow and then each read-first path of `start`, and is empty when
;; `start` did not read; `workflow-text` is #f when the workflow is missing.
;; Each state file is paired with its contents, #f when it is missing.
(struct arc ([name : String]
             [directory : Path] ;; TODO: What does `directory` store?
             [repository : Path]
             [project : Path]
             [start : (Read start)]
             [references : (Listof reference)]
             [workflow-text : (U String #f)]
             [state-files : (Listof (Pairof String (U String #f)))]
             [checks : (Read (Listof check))]
             [documents : (Listof document)]
             [log : entry-log])
  #:transparent)

;; TODO: At least choose a different name, or maybe remov

;; A scope path from start.toml, resolved on this machine.
(struct reference ([scope-path : scope-path] [path : Path] [exists? : Boolean]) #:transparent)

;; TODO: This feels like it should be split into Markdown files and not markdown files

;; A file or directory beside the arc's layout, or a Markdown file at the
;; handoff repository's root other than a verbatim workflow copy. `label` is
;; its scoped name, with a trailing slash for a directory, and `text` is #f for
;; anything but a Markdown file.
(struct document ([label : String] [path : Path] [text : (U String #f)]) #:transparent)

;; TODO: Bad name

;; The arc's log directory. `entries` are ascending by number, `invalid` are
;; the names that are not entry numbers, and `pending` is the number of the
;; pending entry, which only Git can decide.
(struct entry-log ([shape : (U 'absent 'directory 'not-a-directory)]
                   [entries : (Listof log-entry)]
                   [invalid : (Listof String)]
                   [pending : (U Exact-Positive-Integer #f)])
  #:transparent)

;; `entry` is the text of entry.md, and `results` is #f when results.toml is
;; missing.
(struct log-entry ([number : Exact-Positive-Integer]
                   [directory? : Boolean]
                   [entry : (U String #f)]
                   [results : (U check-run failure #f)])
  #:transparent)

;; Where `scope-path` resolves in `arc*`. Every scope path in the arc's start
;; is among its references.
(: resolve-reference (-> arc scope-path reference))
(define (resolve-reference arc* scope-path)
  (or (findf (λ ([reference : reference]) (equal? (reference-scope-path reference) scope-path))
             (arc-references arc*))
      (error 'resolve-reference "~a is not a reference of ~a" (scope-path->string scope-path) (arc-name arc*))))

;; An entry's results, with a missing file as a failure.
(: log-entry-run (-> log-entry (Read check-run)))
(define (log-entry-run entry)
  (or (log-entry-results entry) (failure "expected a regular file")))

;; The log entry start compares against: the highest-numbered entry that is
;; not pending. An entry without readable results is still the baseline, so
;; that a broken entry is reported rather than skipped for an older one.
(: arc-baseline (-> arc (U log-entry #f)))
(define (arc-baseline arc*)
  (define log (arc-log arc*))
  (define complete
    (filter (λ ([entry : log-entry]) (not (eqv? (log-entry-number entry) (entry-log-pending log))))
            (entry-log-entries log)))
  (and (pair? complete) (last complete)))

;; The pending log entry, if there is one.
(: arc-pending-entry (-> arc (U log-entry #f)))
(define (arc-pending-entry arc*)
  (define log (arc-log arc*))
  (findf (λ ([entry : log-entry]) (eqv? (log-entry-number entry) (entry-log-pending log)))
         (entry-log-entries log)))

;; The number of the entry `update --begin` records into: the pending entry,
;; or one past the highest-numbered entry.
(: next-entry-number (-> entry-log Exact-Positive-Integer))
(define (next-entry-number log)
  (or (entry-log-pending log)
      (add1 (apply max 0 (map log-entry-number (entry-log-entries log))))))

;; Why an update must not proceed: the arc has lint errors.
(: lint-refusal (-> arc (U String #f)))
(define (lint-refusal arc*)
  (define diagnostics (lint arc*))
  (and (pair? diagnostics)
       (format "~a has ~a lint errors:\n~a"
               (arc-name arc*) (length diagnostics) (string-join (map diagnostic->string diagnostics) "\n"))))

;; Why `update --commit` must refuse, given the project's head now: there is
;; no pending entry or it has no headline, the arc has lint errors, or the
;; pending entry's results no longer match the checks or the project.
(: commit-refusal (-> arc branch-head (U String #f)))
(define (commit-refusal arc* head)
  (define entry (arc-pending-entry arc*))
  (define checks (arc-checks arc*))
  (cond
    [(not entry)
     (format "~a has no update in progress; run handoff update --begin first" (arc-name arc*))]
    [(not (log-entry-entry entry))
     (format "write the entry to ~a first"
             (build-path (log-entry-path (arc-directory arc*) (log-entry-number entry)) entry-file))]
    [(with-handlers ([exn:fail:handoff? exn-message])
       (entry-headline (assert (log-entry-entry entry)))
       #f)]
    [(lint-refusal arc*)]
    [else
     (define number (log-entry-number entry))
     (define recorded (log-entry-results entry))
     (cond
       [(not (check-run? recorded))
        (format "log/~a has no results; run handoff update --begin again" number)]
       [(not (equal? checks (map check-result-check (check-run-results recorded))))
        (format "~a changed since log/~a's results were recorded; run handoff update --begin again"
                checks-file number)]
       [(not (equal? head (check-run-project-head recorded)))
        (format "the project moved from ~a to ~a since log/~a's results were recorded; run handoff update --begin again"
                (describe-branch-head (check-run-project-head recorded)) (describe-branch-head head) number)]
       [else #f])]))

;;; Lint

(struct diagnostic ([path : Path] [message : String]) #:transparent)

(: diagnostic->string (-> diagnostic String))
(define (diagnostic->string diagnostic)
  (format "~a: ~a" (diagnostic-path diagnostic) (diagnostic-message diagnostic)))

;; What is wrong with the arc, in the order of its layout.
(: lint (-> arc (Listof diagnostic)))
(define (lint arc*)
  (define directory (arc-directory arc*))
  (define start-path (build-path directory start-file))
  (define checks-path (build-path directory checks-file))
  (define checks (arc-checks arc*))
  (append
   ;; Each of the state files exists
   (for/list : (Listof diagnostic) ([file (in-list (arc-state-files arc*))]
                                    #:unless (cdr file))
     (diagnostic (build-path directory (car file)) "expected a regular file"))
   ;; start.toml reads, and each of its references names a file that exists
   (failure-diagnostics start-path (arc-start arc*))
   (for/list : (Listof diagnostic) ([reference (in-list (arc-references arc*))]
                                    #:unless (reference-exists? reference))
     (diagnostic start-path (format "~a: expected a regular file at ~a"
                                    (scope-path->string (reference-scope-path reference))
                                    (reference-path reference))))
   ;; checks.toml reads, and no longer holds the placeholder from `handoff new`
   (failure-diagnostics checks-path checks)
   (if (and (list? checks)
            (memf (λ ([check : check]) (equal? (check-id check) placeholder-check-id)) checks))
       (list (diagnostic checks-path
                         (format "check ~a is the placeholder from handoff new; replace it with the arc's checks"
                                 placeholder-check-id)))
       null)
   ;; Every document start lists with a summary has one
   (for*/list : (Listof diagnostic) ([document (in-list (arc-documents arc*))]
                                     [text (in-value (document-text document))]
                                     #:when text
                                     [issue (in-value (summary-issue text))]
                                     #:when issue)
     (diagnostic (document-path document) issue))
   (lint-log directory (arc-log arc*))))

(: failure-diagnostics (-> Path Any (Listof diagnostic)))
(define (failure-diagnostics path read)
  (if (failure? read) (list (diagnostic path (failure-message read))) null))

;; The log path either does not exist or is a directory, and every entry in it
;; is a directory named a positive integer with no leading zeros, holding an
;; entry.md file and a results.toml file in the format `check-run->toml`
;; writes. The pending entry may lack either file.
(: lint-log (-> Path entry-log (Listof diagnostic)))
(define (lint-log directory log)
  (define log-path (build-path directory log-directory))
  (case (entry-log-shape log)
    [(absent) null]
    [(not-a-directory) (list (diagnostic log-path "expected a directory"))]
    [else
     (append
      (append*
       (for/list : (Listof (Listof diagnostic)) ([entry (in-list (entry-log-entries log))])
         (define path (log-entry-path directory (log-entry-number entry)))
         (define pending? (eqv? (log-entry-number entry) (entry-log-pending log)))
         (define results (log-entry-results entry))
         (cond
           [(not (log-entry-directory? entry)) (list (diagnostic path "expected a directory"))]
           [else
            (append
             (if (or pending? (log-entry-entry entry))
                 null
                 (list (diagnostic (build-path path entry-file) "expected a regular file")))
             (if (and pending? (not results))
                 null
                 (failure-diagnostics (build-path path results-file) (log-entry-run entry))))])))
      (for/list : (Listof diagnostic) ([name (in-list (entry-log-invalid log))])
        (diagnostic (build-path log-path name)
                    "expected a log entry number, a positive integer without leading zeros")))]))

;;; Rendering

;; The most lines of a check's output shown in a report.
(define output-lines 20)

(: seconds->string (-> Real String))
(define (seconds->string duration)
  (string-append (~r duration #:precision '(= 1)) "s"))

;; The line that opens a run's report.
(: render-run-start (-> (Listof check) String))
(define (render-run-start checks)
  (format "running ~a ~a\n" (length checks) (if (= (length checks) 1) "check" "checks")))

;; One check's result, with its change since `baseline` when that has results.
(: render-compared-result (-> check-result (U log-entry #f) String))
(define (render-compared-result result baseline)
  (define previous (baseline-results baseline))
  (if previous
      (render-result result (compare-result result previous) (log-entry-number (assert baseline)))
      (render-result result #f #f)))

;; One check's result as a record of labeled fields, with its change since
;; log/`number` when compared.
(: render-result (-> check-result (U check-change #f) (U Exact-Positive-Integer #f) String))
(define (render-result result change number)
  (define out (open-output-string))
  (define (field [name : String] [template : String] . [arguments : Any *])
    (fprintf out "~a: ~a\n" name (apply format template arguments)))
  (define (print-output [label : String] [result : check-result])
    (define lines (string-split (check-result-output result) "\n"))
    (unless (empty? lines)
      (define tail (drop lines (max 0 (- (length lines) output-lines))))
      (fprintf out "~a~a:\n" label (if (< (length tail) (length lines))
                                       (format " (last ~a of ~a lines)" (length tail) (length lines))
                                       ""))
      (for ([line (in-list tail)])
        (fprintf out "  ~a\n" line))))
  (define check (check-result-check result))
  (define outcome (check-result-outcome result))
  (define before (and change (check-change-before change)))
  (define flipped? (and change (eq? (check-change-change change) 'flipped)))
  (field "id" "~a" (check-id check))
  (field "claim" "~a" (check-claim check))
  (field "outcome" "~a" outcome)
  (case (and change (check-change-change change))
    [(flipped) (field "change" "was ~a at log/~a" (check-result-outcome (assert before)) number)]
    [(redefined) (field "change" "definition changed since log/~a, where it was ~a"
                        number (check-result-outcome (assert before)))]
    [(new) (field "change" "new since log/~a" number)]
    [else (void)])
  (case outcome
    [(true) (void)]
    [(timeout) (field "timeout" "~a" (seconds->string (check-timeout check)))]
    [else (field "exit" "~a" (check-result-exit result))])
  (field "duration" "~a" (seconds->string (check-result-duration result)))
  (when (or flipped? (not (eq? outcome 'true)))
    (unless (zero? (check-result-truncated result))
      (field "truncated" "~a bytes before the output were not kept" (check-result-truncated result)))
    (print-output "output" result))
  (when (and flipped? before)
    (print-output (format "output at log/~a" number) before))
  (get-output-string out))

;; What closes a run's report: the checks removed since `baseline` when that
;; has results, then the run's host, duration and counts of outcomes.
(: render-run-end (-> check-run (U log-entry #f) String))
(define (render-run-end run baseline)
  (define out (open-output-string))
  (define results (check-run-results run))
  (define previous (baseline-results baseline))
  (define removed (if previous (removed-checks (map check-result-check results) previous) null))
  (unless (empty? removed)
    (newline out)
    (for ([check (in-list removed)])
      (fprintf out "removed since log/~a: ~a (~a)\n"
               (log-entry-number (assert baseline)) (check-id check) (check-claim check))))
  (define counts
    (for*/list : (Listof String) ([outcome (in-list '(true false unknown timeout))]
                                  [count (in-value (count (λ ([result : check-result])
                                                            (eq? (check-result-outcome result) outcome))
                                                          results))]
                                  #:unless (zero? count))
      (format "~a ~a" count outcome)))
  (fprintf out "\nhost: ~a, duration: ~a, summary: ~a\n"
           (check-run-host run)
           (seconds->string (check-run-duration run))
           (if (empty? counts) "no checks" (string-join counts ", ")))
  (get-output-string out))

;; A whole run's report, as `run-and-report` in main.rkt prints it while the
;; checks run.
(: render-run (-> check-run (U log-entry #f) String))
(define (render-run run baseline)
  (define results (check-run-results run))
  (string-append (render-run-start (map check-result-check results))
                 (apply string-append
                        (for/list : (Listof String) ([result (in-list results)])
                          (string-append "\n" (render-compared-result result baseline))))
                 (render-run-end run baseline)))

;; The brief for starting work on an arc. First the arc's state: its start
;; metadata, the files it names and the latest log entry, inlined, and the
;; other documents with their summaries. Then what has happened since that
;; entry: the project's commits, the handoff repository's lint diagnostics,
;; pending entry and uncommitted changes (`dirty`, Git's short status lines),
;; and the checks compared against the entry's results. `run` is #f when the
;; checks did not read. Paths are shown relative to the project.
(: render-start (-> arc project-status (Listof String) (U check-run #f) String))
(define (render-start arc* status dirty run)
  (define out (open-output-string))
  (define directory (arc-directory arc*))
  (define (show-path [path : Path])
    (some-system-path->string (find-relative-path (arc-project arc*) path)))
  (define first-section? #t)
  (define (section [name : String])
    (unless first-section? (newline out))
    (set! first-section? #f)
    (fprintf out "==> ~a <==\n" name))
  (define (inline [path : Path] [text : (U String #f)])
    (when text
      (section (show-path path))
      (write-string text out)
      (unless (or (string=? text "") (string-suffix? text "\n"))
        (newline out))))
  (define baseline (arc-baseline arc*))
  (define start* (let ([start* (arc-start arc*)]) (and (start? start*) start*)))
  (define workflow (and start* (resolve-reference arc* (start-workflow start*))))

  (section (format "start, generated from ~a" (show-path (build-path directory start-file))))
  (cond
    [start*
     (define (describe [scope-path : scope-path])
       (define reference (resolve-reference arc* scope-path))
       (define path (show-path (reference-path reference)))
       (if (reference-exists? reference) path (format "~a (missing)" path)))
     (fprintf out "Next:\n")
     ;; A TOML multiline string usually ends in a newline, which would render
     ;; as a blank line.
     (for ([line (in-list (string-split (string-trim (start-next start*) #:left? #f) "\n" #:trim? #f))])
       (fprintf out "  ~a\n" line))
     (fprintf out "Workflow: ~a\n" (describe (start-workflow start*)))
     (fprintf out "Read first:\n")
     (when (empty? (start-read-first start*))
       (fprintf out "  nothing\n"))
     (for ([read (in-list (start-read-first start*))])
       (fprintf out "- ~a: ~a\n" (describe (read-first-path read)) (read-first-reason read)))]
    [else (fprintf out "~a did not parse; see the lint diagnostics under handoff repository status\n" start-file)])

  (define state (arc-state-files arc*))
  (inline (build-path directory (car (first state))) (cdr (first state)))
  (when workflow
    (inline (reference-path workflow) (arc-workflow-text arc*)))
  (for ([file (in-list (rest state))])
    (inline (build-path directory (car file)) (cdr file)))
  (when baseline
    (inline (build-path (log-entry-path directory (log-entry-number baseline)) entry-file)
            (log-entry-entry baseline)))

  ;; Each with its summary, so an agent can choose what to open, except the
  ;; workflow, which is inlined above.
  (section "other documents")
  (define workflow-path (and workflow (simplify-path (reference-path workflow) #f)))
  (define documents
    (filter-not (λ ([document : document]) (equal? (simplify-path (document-path document) #f) workflow-path))
                (arc-documents arc*)))
  (when (empty? documents)
    (fprintf out "nothing\n"))
  (for ([document (in-list documents)])
    (define text (document-text document))
    (define summary (if text (document-summary text) ""))
    (fprintf out "- ~a~a\n" (document-label document) (if (string=? summary "") "" (string-append ": " summary))))

  (section "project repository status")
  (write-string (render-project status) out)

  (section "handoff repository status")
  (fprintf out "repository: ~a\n" (show-path (arc-repository arc*)))
  (define diagnostics (lint arc*))
  (cond
    [(empty? diagnostics) (fprintf out "lint: no errors\n")]
    [else
     (fprintf out "lint: ~a error~a\n" (length diagnostics) (if (= (length diagnostics) 1) "" "s"))
     (for ([diagnostic (in-list diagnostics)])
       (fprintf out "  ~a: ~a\n" (show-path (diagnostic-path diagnostic)) (diagnostic-message diagnostic)))])
  (define pending (arc-pending-entry arc*))
  (cond
    [pending
     (define results (log-entry-results pending))
     (fprintf out "pending log entry: log/~a was begun with handoff update --begin but not committed, so it is not the baseline\n"
              (log-entry-number pending))
     (fprintf out "  results: ~a\n"
              (if (check-run? results)
                  (format "recorded on ~a at ~a"
                          (check-run-host results) (timestamp->string (check-run-started results)))
                  "none readable"))
     (fprintf out "  entry.md: ~a\n" (if (log-entry-entry pending) "written" "not written"))]
    [else (fprintf out "pending log entry: none\n")])
  (cond
    [(empty? dirty) (fprintf out "uncommitted: none\n")]
    [else
     (fprintf out "uncommitted, relative to the repository:\n")
     (for ([line (in-list dirty)])
       (fprintf out "  ~a\n" line))])

  (section "checks")
  (define previous (and baseline (log-entry-run baseline)))
  (cond
    [(not baseline) (fprintf out "no earlier log entry to compare against\n")]
    [(check-run? previous) (fprintf out "compared against log/~a, recorded on ~a at ~a\n"
                                    (log-entry-number baseline) (check-run-host previous)
                                    (timestamp->string (check-run-started previous)))]
    [else (fprintf out "not compared: log/~a/~a: ~a\n"
                   (log-entry-number baseline) results-file (failure-message (assert previous failure?)))])
  (define checks (arc-checks arc*))
  (cond
    [(failure? checks) (fprintf out "not run: ~a: ~a\n" checks-file (failure-message checks))]
    [run (write-string (render-run run baseline) out)]
    [else (error 'render-start "no run for checks that read")])
  (get-output-string out))

;; The project's branch and head now, and what happened since the baseline,
;; when it has results.
(struct project-status ([path : Path] [now : branch-head] [since : (U project-change #f)])
  #:transparent)

;; The head recorded in log/`number`'s results, and the commits between it and
;; the project's head now: Git's log lines, untruncated.
(struct project-change ([number : Exact-Positive-Integer] [recorded : branch-head] [history : History])
  #:transparent)

(define-type History
  (U 'unchanged                ; the head is the recorded one
     'no-head                  ; the project has no commits now
     'unresolvable             ; the recorded head is not in the repository
     (Pairof (U 'all           ; nothing was recorded, so every commit is new
                'since         ; the recorded head is an ancestor of the head
                'rewritten)    ; it is not, so lines are marked < or > by side
             (Listof String))))

;; The most commits listed when reporting what changed in the project.
(define commit-limit 20)

(: render-project (-> project-status String))
(define (render-project status)
  (define out (open-output-string))
  (define now (project-status-now status))
  (define since (project-status-since status))
  (fprintf out "repository: ~a\n" (project-status-path status))
  (fprintf out "now: ~a\n" (describe-branch-head now))
  (when since
    (define recorded (project-change-recorded since))
    (fprintf out "at log/~a: ~a\n" (project-change-number since) (describe-branch-head recorded))
    (unless (equal? (branch-head-branch recorded) (branch-head-branch now))
      (fprintf out "the branch differs\n"))
    (define (commits [heading : String] [lines : (Listof String)])
      (fprintf out "~a\n" heading)
      (for ([line (in-list (take lines (min commit-limit (length lines))))])
        (fprintf out "  ~a\n" line))
      (when (> (length lines) commit-limit)
        (fprintf out "  and ~a more\n" (- (length lines) commit-limit))))
    (define history (project-change-history since))
    (match history
      ['unchanged (fprintf out "no new commits\n")]
      ['no-head (fprintf out "project has no commits now\n")]
      ['unresolvable
       (fprintf out "recorded head does not resolve here: its history was rewritten and pruned, or it was committed on another machine\n")]
      [(cons 'all lines) (commits "commits, all new since:" lines)]
      [(cons 'since lines) (commits "commits since:" lines)]
      [(cons 'rewritten lines)
       (commits "recorded head is no longer an ancestor of HEAD, so history was rewritten; < marks commits only in the recorded history, > only in the current one:"
                lines)]))
  (get-output-string out))

;; The results of the baseline's run, or #f without a baseline with results.
(: baseline-results (-> (U log-entry #f) (U (Listof check-result) #f)))
(define (baseline-results baseline)
  (define run (and baseline (log-entry-results baseline)))
  (and (check-run? run) (check-run-results run)))

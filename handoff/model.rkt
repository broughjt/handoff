#lang typed/racket/base

(require racket/function
         racket/list
         racket/match
         racket/math
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
         (struct-out baseline)
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
         compare-results
         (struct-out pattern)
         pattern-name?
         pattern-summary
         workflow-copy-name
         workflow-provenance
         strip-provenance
         placeholder-check-id
         new-arc-files)

;;; Layout

(define repository-path (build-path ".scratch" "handoff"))
(define archive-tag-prefix "archive/")

;; The expected layout of a handoff arc directory
(define start-file "start.toml")
(define checks-file "checks.toml")
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
(struct scope-path ([scope : Scope] [path : Path]) #:transparent)
(define-type Scope (U 'arc 'handoff 'project))
(struct read-first ([path : scope-path] [reason : String]) #:transparent)

(: parse-start (-> Table start))
(define (parse-start table)
  (expect-fields table (list 'next 'workflow 'read_first) null "start")
  (define reads (hash-ref table 'read_first))
  (unless (list? reads)
    (raise-handoff-error "read_first must be an array of tables"))
  (start (expect-non-empty-string table 'next "start")
         (parse-scope-path (expect-non-empty-string table 'workflow "start"))
         (for/list ([entry (in-list reads)] [index (in-naturals)])
           (define context (format "read_first[~a]" index))
           (define read (expect-table entry context))
           (expect-fields read (list 'path 'reason) null context)
           (read-first (parse-scope-path (expect-non-empty-string read 'path context))
                       (expect-non-empty-string read 'reason context)))))

(: parse-scope-path (-> String scope-path))
(define (parse-scope-path text)
  (define fail
    (thunk
     (raise-handoff-error "~s: expected arc:, handoff:, or project: followed by a relative file path without '..'" text)))
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

;; The baseline is the log entry that `handoff start` compares against, that is,
;; the highest-numbered entry that is not pending.
(struct baseline ([number : Exact-Positive-Integer] [run : (U check-run String)]) #:transparent)

;; Checks are an array of tables so that they run in file order.
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
      (expect-fields table (list* 'id 'claim 'run required) (cons 'timeout optional) context)
      (define timeout (hash-ref table 'timeout (const default-timeout)))
      (define check*
        (check (expect-non-empty-string table 'id context)
               (expect-non-empty-string table 'claim context)
               (expect-non-empty-string table 'run context)
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
    (field "run" (toml-string (check-command check)))
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

(: describe-branch-head (-> branch-head String))
(define (describe-branch-head branch-head*)
  (define head (branch-head-head branch-head*))
  (format "~a at ~a"
          (or (branch-head-branch branch-head*) "a detached HEAD")
          (if head (substring head 0 (min 12 (string-length head))) "no commit")))

;; Seconds since the epoch as an ISO 8601 datetime in UTC.
(: timestamp->string (-> Integer String))
(define (timestamp->string seconds)
  (moment->iso8601 (posix->moment seconds 0)))

;; Seconds since the epoch as an ISO 8601 date in UTC.
(: date->string (-> Integer String))
(define (date->string seconds)
  (substring (timestamp->string seconds) 0 10))

;;; Comparing runs

(define-type Change (U 'unchanged 'flipped 'redefined 'new))

(struct check-change ([after : check-result]
                      [before : (U check-result #f)] ;; `#f` when the `after` check is new
                      [change : Change])
  #:transparent)

;; Obtain a list of `check-changes` for overlapping or new ids and a list of
;; removed ids.
(: compare-results (-> (Listof check-result) (Listof check-result)
                       (Values (Listof check-change) (Listof check))))
(define (compare-results current previous)
  (define (id [result : check-result]) (check-id (check-result-check result)))
  (define by-id
    (for/hash : (Immutable-HashTable String check-result) ([result (in-list previous)])
      (values (id result) result)))
  (define changes
    (for/list : (Listof check-change) ([after (in-list current)])
      (define before (hash-ref by-id (id after) #f))
      (define change
        (cond
          [(not before) 'new]
          [(not (equal? (check-result-check before) (check-result-check after))) 'redefined]
          [(eq? (check-result-outcome before) (check-result-outcome after)) 'unchanged]
          [else 'flipped]))
      (check-change after before change)))
  (define ids (map id current))
  (values changes
          (filter-map
           (lambda ([result : check-result])
             (and (not (member (id result) ids)) (check-result-check result)))
           previous)))

;;; Workflow patterns and new arcs

;; A workflow pattern's file, with the files of the same name in lower
;; precedence directories that it shadows.
(struct pattern ([name : String] [path : Path] [shadowed : (Listof Path)]) #:transparent)

;; A pattern's name is its file name without `.md`. We use lowercase only so
;; that a README.md sitting in the same directory as the patterns does not
;; count.
(: pattern-name? (-> String Boolean))
(define (pattern-name? name)
  (regexp-match? #px"^[a-z0-9][a-z0-9-]*$" name))

;; The first paragraph after a pattern's opening headings.
(: pattern-summary (-> String String))
(define (pattern-summary text)
  (define (heading? [line : String]) (regexp-match? #px"^ {0,3}#" line))
  (define (blank? [line : String]) (string=? (string-trim line) ""))
  (define body (dropf (string-split text "\n" #:trim? #f)
                      (λ ([line : String]) (or (blank? line) (heading? line)))))
  (string-join (map string-trim (takef body (λ ([line : String]) (not (or (blank? line) (heading? line))))))))

;; Where a verbatim copy of the pattern lives, relative to the handoff
;; repository, which shares it between arcs.
(: workflow-copy-name (-> String String))
(define (workflow-copy-name name)
  (string-append "workflow-" name ".md"))

;; The line opening a workflow copied verbatim or forked from `source`, which
;; describes a pattern ("workflow pattern spike") or is a reference to a file,
;; when the source's contents were `text`. The hash says whether the source has
;; changed since. For a verbatim copy, also include whether the copy was
;; amended.
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
  (regexp-replace #px"^<!-- (copied|forked|coined) [^\n]*-->\n" text ""))

(define placeholder-check-id "replace-me")

(define placeholder-checks
  (string-append
   "# Each check is a claim about the project and a command that acts as a\n"
   "# mechanical decision procedure for the claim. Checks run in file order.\n"
   "# The `timeout` field is measured in seconds; it is optional and defaults to\n"
   "# " (number->string default-timeout) ".\n"
   "[[check]]\n"
   "id = " (toml-string placeholder-check-id) "\n"
   "claim = " (toml-string "A natural language claim that is either true or false") "\n"
   "run = " (toml-string (string-append
                          "echo \"Replace this placeholder from handoff new with the arc's checks. "
                          "run is a shell command, run from the project root, that exits 0 when "
                          "the claim is true, 1 when it is false, and anything else when unknown.\"; exit 3"))
   "\n"
   "timeout = " (number->string default-timeout) "\n"))

(: new-arc-files (-> String (U scope-path #f) (Listof (Pairof String String))))
(define (new-arc-files name workflow)
  (list*
   (cons start-file
         (format "next = ~a\nworkflow = ~a\nread_first = []\n"
                 (toml-string "")
                 (toml-string (if workflow (scope-path->string workflow) ""))))
   (cons checks-file placeholder-checks)
   (for/list : (Listof (Pairof String String)) ([file+heading (in-list state-headings)])
     (cons (car file+heading) (format "# ~a\n" (or (cdr file+heading) name))))))

#lang typed/racket/base
;; Automation for the handoff skills.

(require racket/cmdline
         racket/file
         racket/format
         racket/function
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         typed/racket/os
         "error.rkt"
         "model.rkt"
         "toml.rkt")
(require/typed racket/base
  ;; Typed Racket has no type for this.
  [current-inexact-monotonic-milliseconds (-> Flonum)]
  ;; Typed Racket's type for subprocess has no 'stdout for stderr.
  [(subprocess subprocess/merged)
   (-> False False 'stdout Path-String String *
       (Values Subprocess Input-Port Output-Port False))]
  ;; Typed Racket has no type for this either.
  [path<? (-> Path Path Boolean)])

(provide main
         Exit-Code
         (struct-out diagnostic)
         lint-arc
         capture-limit
         run-check
         git
         git*
         git-repository?
         project-branch-head)

(module+ main
  (exit (main (current-command-line-arguments))))

(: main (-> (Vectorof String) Exit-Code))
(define (main argv)
  (status->exit-code
   (with-handlers ([exn:fail:handoff?
                    (λ ([error : exn]) : Status
                      (eprintf "handoff: ~a\n" (exn-message error))
                      'usage)]
                   [exn:fail:user?
                    (λ ([error : exn]) : Status
                      (eprintf "~a\n" (exn-message error))
                      'usage)])
     (execute (parse-arguments argv)))))

(define-type Status (U 'success 'failure 'usage))
(define-type Exit-Code (U 0 1 2))

(: status->exit-code (-> Status Exit-Code))
(define (status->exit-code status)
  (case status
    [(success) 0]
    [(failure) 1]
    [(usage) 2]))

;;; Command line

(define-type Subcommand
  (U list-subcommand start-subcommand check-subcommand lint-subcommand update-subcommand
     new-subcommand workflow-list-subcommand workflow-install-subcommand workflow-fork-subcommand
     guide-subcommand))
(struct list-subcommand ([archived? : Boolean]) #:transparent)
(struct new-subcommand ([arc : String] [workflow : (U String #f)]) #:transparent)
(struct workflow-list-subcommand () #:transparent)
(struct workflow-install-subcommand ([pattern : String]) #:transparent)
(struct workflow-fork-subcommand ([arc : String] [name : String] [source : String]) #:transparent)
(struct start-subcommand ([arc : (U String #f)]) #:transparent)
(struct check-subcommand ([arc : (U String #f)]) #:transparent)
(struct lint-subcommand ([arc : (U String #f)]) #:transparent)
(struct update-subcommand ([step : (U 'begin 'commit)] [arc : (U String #f)]) #:transparent)
(struct guide-subcommand () #:transparent)

;; "Envisioning the hereafter, listenin' to Steve Wonder"
(struct invocation ([repository : (U Path #f)] [subcommand : Subcommand]) #:transparent)

(: parse-arguments (-> (Vectorof String) invocation))
(define (parse-arguments argv)
  (define repository : (U Path #f) #f)
  (command-line
   #:program "handoff"
   #:argv argv
   #:usage-help
   "Commands:"
   "  list      list handoff arcs"
   "  start     get the brief for starting work on a handoff arc"
   "  check     run the commands in checks.toml and report their results"
   "  lint      validate expected structure of a handoff arc"
   "  update    record a log entry: --begin runs the checks and records them, --commit commits"
   "  new       create an arc's files, copying its workflow"
   "  workflow  list, install, or fork reusable workflows"
   "  guide     print the path of HANDOFF.md, which documents concepts shared by the handoff skills"
   #:once-each
   [("--repository") path
                     "Handoff repository to use instead of the nearest .scratch/handoff/"
                     (set! repository (string->path (assert path string?)))]
   #:args (subcommand . arguments)
   ;; Typed Racket types the formal arguments of `#:args` as `Any`, so we do a
   ;; runtime check explicitly (that we expect will never fail).
   (invocation repository
               (parse-subcommand (assert subcommand string?)
                                 (map (λ (a) (assert a string?)) arguments)))))

(: parse-subcommand (-> String (Listof String) Subcommand))
(define (parse-subcommand subcommand arguments)
  (define program (string-append "handoff " subcommand))
  (: parse-arc-argument (-> String (Listof String) (U String #f)))
  (define (parse-arc-argument program arguments)
    (command-line
     #:program program
     #:argv arguments
     #:args ([arc #f])
     (and arc (assert arc string?))))
  (case subcommand
    [("list")
     (define archived? : Boolean #f)
     (command-line
      #:program program
      #:argv arguments
      #:once-each
      [("--archived") "List archived arcs instead" (set! archived? #t)]
      #:args ()
      (list-subcommand archived?))]
    [("start") (start-subcommand (parse-arc-argument program arguments))]
    [("check") (check-subcommand (parse-arc-argument program arguments))]
    [("lint") (lint-subcommand (parse-arc-argument program arguments))]
    [("update")
     (define step : (U 'begin 'commit #f) #f)
     (command-line
      #:program program
      #:argv arguments
      #:once-any
      [("--begin") "Run the checks and record their results in a pending log entry"
                   (set! step 'begin)]
      [("--commit") "Commit the pending log entry once its entry.md is written"
                    (set! step 'commit)]
      #:args ([arc #f])
      (update-subcommand (or step (raise-handoff-error "update requires --begin or --commit"))
                      (and arc (assert arc string?))))]
    [("new")
     (command-line
      #:program program
      #:argv arguments
      #:help-labels
      "<workflow> is a name from handoff workflow list, or a reference such as handoff:workflow-x.md"
      #:args (arc [workflow #f])
      (new-subcommand (assert arc string?) (and workflow (assert workflow string?))))]
    [("workflow")
     (command-line
      #:program program
      #:argv arguments
      #:usage-help
      "Commands:"
      "  list                 list the reusable workflows"
      "  install <name>       copy a reusable workflow verbatim to the handoff repository's root"
      "  fork <arc> <name> <source>  copy a reusable workflow or workflow file into the arc as workflow-<name>.md"
      #:args (verb . verb-arguments)
      (parse-workflow-subcommand (assert verb string?)
                                 (map (λ (a) (assert a string?)) verb-arguments)))]
    [("guide") (command-line #:program program #:argv arguments #:args () (guide-subcommand))]
    [else (raise-handoff-error "unknown command ~a; expected list, start, check, lint, update, new, workflow, or guide"
                               subcommand)]))

(: parse-workflow-subcommand (-> String (Listof String) Subcommand))
(define (parse-workflow-subcommand verb arguments)
  (define program (string-append "handoff workflow " verb))
  (case verb
    [("list") (command-line #:program program #:argv arguments #:args () (workflow-list-subcommand))]
    [("install")
     (command-line #:program program #:argv arguments #:args (pattern)
                   (workflow-install-subcommand (assert pattern string?)))]
    [("fork")
     (command-line #:program program #:argv arguments
                   #:help-labels
                   "<source> is a name from handoff workflow list, or a reference such as handoff:workflow-x.md"
                   #:args (arc name source)
                   (workflow-fork-subcommand (assert arc string?) (assert name string?) (assert source string?)))]
    [else (raise-handoff-error "unknown command workflow ~a; expected list, install, or fork" verb)]))

(: execute (-> invocation Status))
(define (execute invocation)
  (define subcommand (invocation-subcommand invocation))
  ;; Commands reading only user-scope or installed files should not execute
  ;; from the project repository.
  (cond
    [(workflow-list-subcommand? subcommand) (handle-workflow-list)]
    [(guide-subcommand? subcommand) (handle-guide)]
    [else (execute-in-repository invocation)]))

(: execute-in-repository (-> invocation Status))
(define (execute-in-repository invocation)
  (define option (invocation-repository invocation))
  (when (and option (not (git-repository? option)))
    (raise-handoff-error "~a is not a git repository" option))
  (define repository (or option (find-handoff-repository (current-directory))))
  (define subcommand (invocation-subcommand invocation))
  (cond
    [(list-subcommand? subcommand)
     (handle-list repository (list-subcommand-archived? subcommand))]
    [(start-subcommand? subcommand)
     (handle-start repository (select-arc repository (start-subcommand-arc subcommand)))]
    [(check-subcommand? subcommand)
     (handle-check repository (select-arc repository (check-subcommand-arc subcommand)))]
    [(lint-subcommand? subcommand)
     (handle-lint repository (select-arc repository (lint-subcommand-arc subcommand)))]
    [(update-subcommand? subcommand)
     (define arc (select-arc repository (update-subcommand-arc subcommand)))
     (case (update-subcommand-step subcommand)
       [(begin) (handle-update-begin repository arc)]
       [(commit) (handle-update-commit repository arc)])]
    [(new-subcommand? subcommand)
     (handle-new repository (new-subcommand-arc subcommand) (new-subcommand-workflow subcommand))]
    [(workflow-install-subcommand? subcommand)
     (handle-workflow-install repository (workflow-install-subcommand-pattern subcommand))]
    [(workflow-fork-subcommand? subcommand)
     (handle-workflow-fork repository
                           (select-arc repository (workflow-fork-subcommand-arc subcommand))
                           (workflow-fork-subcommand-name subcommand)
                           (workflow-fork-subcommand-source subcommand))]
    [else (error 'execute "unhandled subcommand ~a" subcommand)]))

;;; Commands

(: handle-list (-> Path Boolean Status))
(define (handle-list repository archived?)
  (for ([name (in-list ((if archived? list-archived-arcs list-arcs) repository))])
    (displayln name))
  'success)

(: handle-check (-> Path String Status))
(define (handle-check repository arc)
  (define checks-path (build-path repository arc checks-file))
  (define checks
    (with-handlers ([exn:fail:handoff?
                     (λ ([error : exn])
                       (raise-handoff-error "~a: ~a" checks-path (exn-message error)))])
      (parse-checks (read-toml checks-path))))
  (define run (run-and-report checks (find-project-repository repository)))
  (if (andmap (λ ([result : check-result]) (eq? (check-result-outcome result) 'true))
              (check-run-results run))
      'success 'failure))

(: handle-lint (-> Path String Status))
(define (handle-lint repository arc)
  (define diagnostics (lint (read-arc (build-path repository arc) repository)))
  (for ([diagnostic (in-list diagnostics)])
    (displayln (diagnostic->string diagnostic)))
  (printf "~a: ~a lint errors\n" arc (length diagnostics))
  (if (empty? diagnostics) 'success 'failure))

;; Brief an agent to start work on a handoff arc (see `render-start`). Reads
;; and runs everything first, then prints the brief, and writes nothing.
(: handle-start (-> Path String Status))
(define (handle-start repository* name)
  (define repository (normalize-path repository*))
  (define arc* (read-arc (build-path repository name) repository))
  (define status (read-project-status (arc-project arc*) (arc-baseline arc*)))
  ;; Without --no-optional-locks, status may refresh and rewrite the index.
  (define dirty (string-split (git repository "--no-optional-locks" "status" "--porcelain") "\n"))
  (define checks (arc-checks arc*))
  (define run (and (list? checks) (run-checks checks (arc-project arc*))))
  (write-string (render-start arc* status dirty run))
  'success)

;; The project's branch and head, and what happened since those recorded in
;; the baseline's results.
(: read-project-status (-> Path (U log-entry #f) project-status))
(define (read-project-status project baseline)
  (define now (project-branch-head project))
  (define earlier (and baseline (log-entry-results baseline)))
  (project-status
   project
   now
   (and baseline
        (check-run? earlier)
        (let* ([recorded (check-run-project-head earlier)]
               [old (branch-head-head recorded)]
               [new (branch-head-head now)])
          (define (commits [log-format : String] . [arguments : String *])
            (string-split (apply git project "log" "--abbrev=12" (string-append "--format=" log-format) arguments)
                          "\n"))
          (project-change
           (log-entry-number baseline)
           recorded
           (cond
             [(equal? old new) 'unchanged]
             [(not new) 'no-head]
             [(not old) (cons 'all (commits "%h %s" new))]
             [(not (git* project "cat-file" "-e" (string-append old "^{commit}"))) 'unresolvable]
             [(git* project "merge-base" "--is-ancestor" old new)
              (cons 'since (commits "%h %s" (format "~a..~a" old new)))]
             [else (cons 'rewritten (commits "%m %h %s" "--left-right" (format "~a...~a" old new)))]))))))

;; Recording a log entry happens in two command-line steps: `update --begin`
;; records check results in a results.toml file for the pending log entry, the
;; agent writes entry.md, and then `update --commit` commits them.

;; Run the checks in checks.toml and record their results into the results.toml
;; file of the pending log entry, creating the next log entry when none is
;; pending. Rerunning replaces results.toml but leaves entry.md alone.
(: handle-update-begin (-> Path String Status))
(define (handle-update-begin repository name)
  (define arc* (read-arc (build-path repository name) repository))
  (refuse (lint-refusal arc*))
  ;; Lint has accepted checks.toml.
  (define run (run-and-report (assert (arc-checks arc*) list?) (arc-project arc*)))

  (define number (next-entry-number (arc-log arc*)))
  (define entry-path (log-entry-path (arc-directory arc*) number))
  (unless (directory-exists? entry-path)
    (make-directory* (build-path (arc-directory arc*) log-directory))
    ;; Fails rather than reusing an entry that appeared since we read the log.
    (make-directory entry-path))
  (define results-path (build-path entry-path results-file))
  (define temporary (path-add-extension results-path #".tmp"))
  (display-to-file (check-run->toml run) temporary #:exists 'truncate/replace)
  (rename-file-or-directory temporary results-path #t)

  (printf "\nlog/~a results recorded. Write the entry to ~a, then run handoff update --commit ~a\n"
          number (build-path entry-path entry-file) name)
  'success)

;; Commit the pending log entry with the arc's changes and anything already
;; staged in the handoff repository, unless `commit-refusal` refuses.
(: handle-update-commit (-> Path String Status))
(define (handle-update-commit repository name)
  (define arc* (read-arc (build-path repository name) repository))
  (refuse (commit-refusal arc* (project-branch-head (arc-project arc*))))
  (define entry (assert (arc-pending-entry arc*)))
  (define number (log-entry-number entry))
  (define subject (format "~a: ~a" name (entry-headline (assert (log-entry-entry entry)))))

  ;; Fails on an unmerged index, which could not be committed anyway.
  (define index (string-trim (git repository "write-tree")))
  (with-handlers ([exn:fail?
                   (λ ([error : exn])
                     (git* repository "read-tree" index)
                     (raise error))])
    (git repository "add" "--" name)
    (git repository "commit" "--quiet" "--message" subject))

  (printf "log/~a committed as ~a ~a\n"
          number (substring (string-trim (git repository "rev-parse" "HEAD")) 0 7) subject)
  ;; Git's short status lines for what the commit left out.
  (define dirty (string-split (git repository "status" "--porcelain") "\n"))
  (unless (empty? dirty)
    (printf "not committed:\n")
    (for ([line (in-list dirty)])
      (printf "  ~a\n" line)))
  'success)

;; Print each pattern as a record of labeled fields: its name, the file that
;; provides it, the files it shadows, and its summary.
(: handle-workflow-list (-> Status))
(define (handle-workflow-list)
  (define patterns (find-patterns))
  (when (empty? patterns)
    (printf "no reusable workflows in ~a\n" (string-join (map path->string (pattern-directories)) ", ")))
  (for ([pattern (in-list patterns)] [index (in-naturals)])
    (unless (zero? index) (newline))
    (printf "name: ~a\n" (pattern-name pattern))
    (printf "path: ~a\n" (pattern-path pattern))
    (for ([shadowed (in-list (pattern-shadowed pattern))])
      (printf "shadows: ~a\n" shadowed))
    (printf "summary: ~a\n" (document-summary (file->string (pattern-path pattern)))))
  'success)

;; Print the path of the first HANDOFF.md installed under $XDG_DATA_DIRS, for
;; the agent to read with its own tools.
(: handle-guide (-> Status))
(define (handle-guide)
  (define directories (data-directories))
  (define found
    (findf file-exists?
           (for/list : (Listof Path) ([base (in-list directories)])
             (build-path base "handoff" "HANDOFF.md"))))
  (unless found
    (raise-handoff-error "no handoff/HANDOFF.md in ~a"
                         (if (empty? directories)
                             "$XDG_DATA_DIRS, which has no absolute directories"
                             (string-join (map path->string directories) ", "))))
  (displayln (path->string found))
  'success)

;; Stubs out a new handoff arc. Creates each of the expected files with minimal
;; content and without committing them. Takes an optional name or reference to a
;; `workflow` and copies it. The generated arc will fail `handoff lint` until
;; `next` in start.toml is written and the placeholder check in checks.toml is
;; replaced (and the workflow pattern is written if one was not passed).
(: handle-new (-> Path String (U String #f) Status))
(define (handle-new repository arc workflow)
  (check-arc-name arc)
  (define arc-path (build-path repository arc))
  (when (or (file-exists? arc-path) (directory-exists? arc-path) (link-exists? arc-path))
    (raise-handoff-error "~a already exists in ~a" arc repository))
  ;; Everything is validated before anything is written.
  (define-values (source _source-path)
    (if workflow (find-workflow repository arc-path workflow) (values #f #f)))
  (define found (and (pattern? source) source))
  (define reference
    (if (pattern? source)
        (scope-path 'handoff (string->path (workflow-copy-name (pattern-name source))))
        source))
  (define copied (and found (install-pattern repository found)))
  (make-directory arc-path)
  (for ([file+contents (in-list (new-arc-files arc reference))])
    (display-to-file (cdr file+contents) (build-path arc-path (car file+contents)) #:exists 'error))

  (printf "created ~a/ with ~a\n" arc (string-join (map (inst car String String) (new-arc-files arc reference)) ", "))
  (when reference
    (printf "workflow: ~a~a\n" (scope-path->string reference)
            (cond [copied (format " (copied from ~a)" copied)]
                  [found " (existing)"]
                  [else ""])))
  (define diagnostics (lint (read-arc arc-path repository)))
  (printf "lint, until the arc is written:\n")
  (for ([diagnostic (in-list diagnostics)])
    (printf "  ~a\n" (diagnostic->string diagnostic)))
  'success)

(: handle-workflow-install (-> Path String Status))
(define (handle-workflow-install repository name)
  (define found (find-pattern name))
  (define copied (install-pattern repository found))
  (define reference (string-append "handoff:" (workflow-copy-name name)))
  (if copied
      (printf "installed ~a, copied from ~a\n" reference copied)
      (printf "~a already exists and was not modified\n" reference))
  'success)

;; Copy a pattern or an existing workflow file into the arc under a new name,
;; for the agent to amend, recording where it was forked from and that file's
;; hash. Leaves start.toml for the agent to point at the fork.
(: handle-workflow-fork (-> Path String String String Status))
(define (handle-workflow-fork repository arc name workflow)
  (unless (pattern-name? name)
    (raise-handoff-error "~a: a workflow name is lowercase letters, digits and hyphens" name))
  (define arc-path (build-path repository arc))
  (define-values (source source-path) (find-workflow repository arc-path workflow))
  (define description
    (if (pattern? source)
        (format "workflow ~a" (pattern-name source))
        (scope-path->string source)))
  (define file (workflow-copy-name name))
  (define path (build-path arc-path file))
  (when (or (file-exists? path) (directory-exists? path) (link-exists? path))
    (raise-handoff-error "~a already exists" path))
  (define text (file->string source-path))
  (display-to-file (string-append (workflow-provenance 'forked description text
                                                       (date->string (current-seconds)))
                                  (strip-provenance text))
                   path
                   #:exists 'error)
  (printf "forked arc:~a from ~a\nto use it, set workflow in ~a to \"arc:~a\"\n"
          file description start-file file)
  'success)

(: refuse (-> (U String #f) Void))
(define (refuse refusal)
  (when refusal
    (raise-handoff-error "~a" refusal)))

;; Run the checks, printing each check's result as soon as it and every check
;; before it in file order have finished, so that the output does not depend
;; on timing. Note each result's change since `baseline` when that has
;; results, then summarize the run.
(: run-and-report (->* ((Listof check) Path) (#:baseline (U log-entry #f)) check-run))
(define (run-and-report checks project #:baseline [baseline #f])
  (write-string (render-run-start checks))
  (flush-output)
  (define run
    (run-checks checks project
                #:on-result (λ ([result : check-result])
                              (newline)
                              (write-string (render-compared-result result baseline))
                              (flush-output))))
  (write-string (render-run-end run baseline))
  run)

;;; Reading an arc

(: lint-arc (-> Path Path (Listof diagnostic)))
(define (lint-arc directory repository)
  (lint (read-arc directory repository)))

;; Read everything about the arc at `directory` that lint, start and update
;; decide from. A file that does not parse is kept as a failure.
(: read-arc (-> Path Path arc))
(define (read-arc directory repository)
  (define project (find-project-repository repository))
  (define-values (_base name _directory?) (split-path directory))
  (define start* (read-part (λ () (parse-start (read-toml (build-path directory start-file))))))
  (define references
    (if (start? start*)
        (for/list : (Listof reference) ([scope-path (in-list (cons (start-workflow start*)
                                                                   (map read-first-path (start-read-first start*))))])
          (define path (resolve-scope-path scope-path #:arc directory #:repository repository #:project project))
          (reference scope-path path (file-exists? path)))
        null))
  (define workflow (and (pair? references) (first references)))
  (arc (path->string (assert name path?))
       directory
       repository
       project
       start*
       references
       (and workflow (reference-exists? workflow) (file->string (reference-path workflow)))
       (for/list : (Listof (Pairof String (U String #f))) ([name (in-list state-files)])
         (define path (build-path directory name))
         (cons name (and (file-exists? path) (file->string path))))
       (read-part (λ () (parse-checks (read-toml (build-path directory checks-file)))))
       (read-documents directory repository)
       (read-log directory)))

(: read-part (All (A) (-> (-> A) (Read A))))
(define (read-part read)
  (with-handlers ([exn:fail:handoff? (λ ([error : exn]) (failure (exn-message error)))])
    (read)))

;; The documents an agent may open beyond the ones start inlines: the arc's
;; files and directories beside its layout, then the Markdown files at the
;; handoff repository's root other than verbatim workflow copies, which are
;; the shared topic documents.
(: read-documents (-> Path Path (Listof document)))
(define (read-documents directory repository)
  (define layout (map string->path (list* start-file checks-file log-directory state-files)))
  (define (names [directory : Path]) (sort (directory-list directory) path<?))
  (define (markdown [path : Path])
    (and (file-exists? path) (path-has-extension? path #".md") (file->string path)))
  (append
   (for/list : (Listof document) ([name (in-list (names directory))]
                                  #:unless (member name layout))
     (define path (build-path directory name))
     (document (format "arc:~a" (if (directory-exists? path) (path->directory-path name) name))
               path
               (markdown path)))
   (for*/list : (Listof document) ([name (in-list (names repository))]
                                   [path (in-value (build-path repository name))]
                                   [text (in-value (markdown path))]
                                   #:when text
                                   #:unless (verbatim-copy? text))
     (document (format "handoff:~a" name) path text))))

(: read-log (-> Path entry-log))
(define (read-log directory)
  (define log (build-path directory log-directory))
  (define-values (numbers invalid) (list-log-entries directory))
  (entry-log
   (cond
     [(directory-exists? log) 'directory]
     [(or (file-exists? log) (link-exists? log)) 'not-a-directory]
     [else 'absent])
   (for/list : (Listof log-entry) ([number (in-list numbers)])
     (define path (log-entry-path directory number))
     (define entry-path (build-path path entry-file))
     (define results-path (build-path path results-file))
     (log-entry number
                (directory-exists? path)
                (and (file-exists? entry-path) (file->string entry-path))
                (and (file-exists? results-path)
                     (read-part (λ () (parse-check-run (read-toml results-path)))))))
   invalid
   (pending-log-entry directory)))

;;; Running checks

;; The most output kept per check, in bytes. Past it, the start of the output
;; is dropped: a failure is usually explained at the end.
(define capture-limit : Exact-Positive-Integer (* 1024 1024))

(: run-checks (->* ((Listof check) Path) (#:on-result (-> check-result Void)) check-run))
(define (run-checks checks project #:on-result [on-result void])
  (define wall-clock (current-seconds))
  (define started (current-inexact-monotonic-milliseconds))
  ;; Every check starts at once, each in its own thread. Checks run in the
  ;; project root, so that a relative path in a check means the same thing
  ;; wherever handoff was run from.
  (define waits
    (parameterize ([current-directory project])
      (for/list : (Listof (-> check-result)) ([check (in-list checks)])
        (define outcome : (Boxof (U check-result exn:fail #f)) (box #f))
        (define worker
          (thread (λ ()
                    (set-box! outcome (with-handlers ([exn:fail? (λ ([error : exn:fail]) error)])
                                        (run-check check))))))
        (λ ()
          (thread-wait worker)
          (define result (unbox outcome))
          (if (check-result? result) result (raise result))))))
  ;; Results are passed on in file order, each once it and those before it
  ;; have finished.
  (define results
    (for/list : (Listof check-result) ([wait (in-list waits)])
      (define result (wait))
      (on-result result)
      result))
  (check-run (gethostname) wall-clock (seconds-since started) (project-branch-head project) results))

(: run-check (-> check check-result))
(define (run-check check)
  (define started (current-inexact-monotonic-milliseconds))
  (define-values (process stdout stdin _stderr)
    ;; Use a new process group, so that a timeout kills the shell's children
    ;; too. `system*` can neither do that nor time out, so we use `subprocess`.
    (parameterize ([subprocess-group-enabled #t])
      (subprocess/merged #f #f 'stdout
                         (or (find-executable-path "bash")
                             (raise-handoff-error "bash not found on PATH"))
                         "-c" (check-command check))))
  (close-output-port stdin)
  ;; Output is held whole until the check ends, so a check that floods it until
  ;; its timeout holds all of it in memory.
  (define buffer (open-output-bytes))
  (define reader (thread (λ () (copy-port stdout buffer))))
  ;; Finished means the output reached EOF and the shell exited, both within
  ;; the timeout. A background child holding stdout open counts against it.
  (define (remaining) (max 0 (- (check-timeout check) (seconds-since started))))
  (define finished?
    (and (sync/timeout (remaining) (thread-dead-evt reader))
         (sync/timeout (remaining) process)
         #t))
  (unless finished?
    (subprocess-kill process #t)
    (kill-thread reader))
  (close-input-port stdout)
  (subprocess-wait process)
  (define captured (get-output-bytes buffer))
  (define excess (max 0 (- (bytes-length captured) capture-limit)))
  (define output (bytes->string/utf-8 (subbytes captured excess) (integer->char #xFFFD)))
  (define duration (seconds-since started))
  (cond
    [finished?
     (define code (assert (subprocess-status process) exact-integer?))
     (check-result check (case code [(0) 'true] [(1) 'false] [else 'unknown]) duration output excess code)]
    [else (check-result check 'timeout duration output excess #f)]))

(: seconds-since (-> Flonum Nonnegative-Real))
(define (seconds-since started)
  (max 0 (/ (- (current-inexact-monotonic-milliseconds) started) 1000)))

;;; Repositories

;; Whether `path` is the top level of a Git repository or worktree, rather
;; than a directory inside one.
(: git-repository? (-> Path-String Boolean))
(define (git-repository? path)
  (and (directory-exists? path)
       (let ([output (git* path "rev-parse" "--show-toplevel")])
         (and output
              (equal? (path->directory-path (normalize-path (string-trim output "\n" #:left? #f)))
                      (path->directory-path (normalize-path path)))))))

;; Like Git finds .git/, walk up from `start` to the nearest .scratch/handoff/
;; repository.
(: find-handoff-repository (-> Path-String Path))
(define (find-handoff-repository start)
  (let loop ([directory (simple-form-path start)])
    (define candidate (build-path directory repository-path))
    (cond
      [(git-repository? candidate) candidate]
      [else
       (define-values (parent _name _directory?) (split-path directory))
       (if (path? parent)
           (loop parent)
           (raise-handoff-error "no ~a/ at or above ~a" repository-path start))])))

;; The project repository enclosing the handoff repository.
(: find-project-repository (-> Path Path))
(define (find-project-repository repository)
  (define output
    (or (git* (build-path (normalize-path repository) 'up) "rev-parse" "--show-toplevel")
        (raise-handoff-error "no project repository contains ~a" repository)))
  (string->path (string-trim output "\n" #:left? #f)))

(: project-branch-head (-> Path branch-head))
(define (project-branch-head project)
  (define (line [output : (U String #f)])
    (and output (string-trim output "\n" #:left? #f)))
  (branch-head (line (git* project "symbolic-ref" "--quiet" "--short" "HEAD"))
               (line (git* project "rev-parse" "--verify" "--quiet" "HEAD"))))

;;; Installed files

;; The XDG Base Directory specification ignores relative paths.
(: absolute-directory (-> (U String #f) (U Path #f)))
(define (absolute-directory text)
  (and text (non-empty-string? text) (absolute-path? text) (string->path text)))

;; The directories of $XDG_DATA_DIRS, in order of preference.
(: data-directories (-> (Listof Path)))
(define (data-directories)
  (filter-map absolute-directory
              (string-split
               (let ([value (getenv "XDG_DATA_DIRS")])
                 (if (and value (non-empty-string? value))
                     value
                     "/usr/local/share:/usr/share"))
               ":")))

;;; Workflow patterns

(: pattern-directories (-> (Listof Path)))
(define (pattern-directories)
  (define config
    (or (absolute-directory (getenv "XDG_CONFIG_HOME"))
        (build-path (find-system-path 'home-dir) ".config")))
  (for/list ([base (in-list (cons config (data-directories)))])
    (build-path base "handoff" "workflows")))

(: find-patterns (-> (Listof pattern)))
(define (find-patterns)
  (define found : (HashTable String pattern) (make-hash))
  (define order : (Listof String) null)
  (for* ([directory (in-list (pattern-directories))]
         #:when (directory-exists? directory)
         [file (in-list (map path->string (directory-list directory)))])
    (define name (regexp-match #px"^(.*)\\.md$" file))
    (define path (build-path directory file))
    (when (and name (pattern-name? (assert (second name))) (file-exists? path))
      (define key (assert (second name)))
      (define earlier (hash-ref found key #f))
      (cond
        ;; Two data directories can hold the same file, as when one is a
        ;; profile linking to the package that another names directly.
        [(and earlier (equal? (normalize-path path) (normalize-path (pattern-path earlier))))
         (void)]
        [earlier
         (hash-set! found key (struct-copy pattern earlier
                                           [shadowed (append (pattern-shadowed earlier) (list path))]))]
        [else
         (hash-set! found key (pattern key path null))
         (set! order (cons key order))])))
  (for/list ([key (in-list (sort order string<?))])
    (hash-ref found key)))

;; A workflow named on the command line: a pattern name, or a reference to an
;; existing file, `arc:` resolving against `arc`. Also the file's path.
(: find-workflow (-> Path Path String (Values (U pattern scope-path) Path)))
(define (find-workflow repository arc workflow)
  (cond
    [(regexp-match? #rx":" workflow)
     (define reference (parse-scope-path workflow))
     (define path (resolve-scope-path reference #:arc arc #:repository repository
                                      #:project (find-project-repository repository)))
     (unless (file-exists? path)
       (raise-handoff-error "~a: expected a regular file at ~a" workflow path))
     (values reference path)]
    [else
     (define found (find-pattern workflow))
     (values found (pattern-path found))]))

(: find-pattern (-> String pattern))
(define (find-pattern name)
  (or (findf (λ ([candidate : pattern]) (equal? (pattern-name candidate) name)) (find-patterns))
      (raise-handoff-error "no reusable workflow named ~a; see handoff workflow list" name)))

(: install-pattern (-> Path pattern (U Path #f)))
(define (install-pattern repository pattern)
  (define path (build-path repository (workflow-copy-name (pattern-name pattern))))
  (cond
    [(or (file-exists? path) (link-exists? path)) #f]
    [else
     (define text (file->string (pattern-path pattern)))
     (display-to-file (string-append (workflow-provenance 'copied
                                                          (format "workflow ~a" (pattern-name pattern))
                                                          text
                                                          (date->string (current-seconds)))
                                     text)
                      path
                      #:exists 'error)
     (pattern-path pattern)]))

;; An arc is a top-level directory containing a start file.
(: arc? (-> Path Boolean))
(define (arc? path)
  (and (directory-exists? path) (file-exists? (build-path path start-file))))

(: list-arcs (-> Path (Listof String)))
(define (list-arcs repository)
  (sort (for/list : (Listof String) ([name (in-list (directory-list repository))]
                                     #:when (arc? (build-path repository name)))
          (path->string name))
        string<?))

(: list-archived-arcs (-> Path (Listof String)))
(define (list-archived-arcs repository)
  (define output (git repository "tag" "--list" (string-append archive-tag-prefix "*")))
  (sort (for/list : (Listof String) ([tag (in-list (string-split output))])
          (string-trim tag archive-tag-prefix #:right? #f))
        string<?))

(: check-arc-name (-> String Void))
(define (check-arc-name name)
  (when (or (member name '("" "." ".." ".git")) (regexp-match? #rx"/" name))
    (raise-handoff-error "specify a top-level arc directory name")))

(: select-arc (-> Path (U String #f) String))
(define (select-arc repository name)
  (cond
    [name
     (check-arc-name name)
     (unless (directory-exists? (build-path repository name))
       (raise-handoff-error "~a is not a directory in ~a" name repository))
     name]
    [else
     (define arcs (list-arcs repository))
     (cond
       [(empty? arcs) (raise-handoff-error "there are no active arcs")]
       [(empty? (rest arcs)) (first arcs)]
       [else (raise-handoff-error "specify the arc; active arcs: ~a" (string-join arcs ", "))])]))

;; Some entries in the log directory might not pass `parse-log-entry-name`, so
;; partition into successfully parsed entries and entries which are invalid.
(: list-log-entries (-> Path (Values (Listof Exact-Positive-Integer) (Listof String))))
(define (list-log-entries arc)
  (define log (build-path arc log-directory))
  (define names (if (directory-exists? log) (map path->string (directory-list log)) null))
  (define-values (numbered invalid) (partition parse-log-entry-name names))
  (values (sort (map (λ ([name : String]) (assert (parse-log-entry-name name))) numbered) <)
          (sort invalid string<?)))

;; Get the number of the update in progress, which is the highest-numbered log
;; entry not yet in the handoff repository's HEAD (#f if this does not exist).
(: pending-log-entry (-> Path (U Exact-Positive-Integer #f)))
(define (pending-log-entry arc)
  (define-values (numbers _invalid) (list-log-entries arc))
  (and (pair? numbers)
       (let* ([number (last numbers)]
              [entry (log-entry-path arc number)])
         (and (directory-exists? entry)
              ;; `HEAD:./` specifies the tree at the current directory.
              (not (git* entry "cat-file" "-e" "HEAD:./"))
              number))))

;;; Git

;; Obtain Git's stdout, run in `directory`. Stdin is empty. Raises a handoff
;; error that records Git's stderr when it exits non-zero.
(: git (-> Path-String String * String))
(define (git directory . arguments)
  (define-values (succeeded? output error) (apply run-git directory arguments))
  (unless succeeded?
    (raise-handoff-error "git ~a failed in ~a~a"
                         (string-join arguments) directory
                         (if (string=? error "") "" (string-append ":\n" (string-trim error)))))
  output)

;; Like `git`, but #f when Git exits non-zero. Stderr is discarded.
(: git* (-> Path-String String * (U String #f)))
(define (git* directory . arguments)
  (define-values (succeeded? output _error) (apply run-git directory arguments))
  (and succeeded? output))

(: run-git (-> Path-String String * (Values Boolean String String)))
(define (run-git directory . arguments)
  (define program
    (or (find-executable-path "git") (raise-handoff-error "git not found on PATH")))
  (define output (open-output-string))
  (define error (open-output-string))
  (define succeeded?
    (parameterize ([current-input-port (open-input-string "")]
                   [current-output-port output]
                   [current-error-port error])
      (apply system* program "-C" directory arguments)))
  (values succeeded? (get-output-string output) (get-output-string error)))

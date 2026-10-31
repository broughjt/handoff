#lang typed/racket/base
;; Parsing handoff's command line.

(require racket/cmdline
         "error.rkt")

(provide (struct-out list-subcommand)
         (struct-out start-subcommand)
         (struct-out check-subcommand)
         (struct-out lint-subcommand)
         (struct-out update-subcommand)
         (struct-out close-subcommand)
         (struct-out new-subcommand)
         (struct-out workflow-list-subcommand)
         (struct-out workflow-install-subcommand)
         (struct-out workflow-fork-subcommand)
         (struct-out guide-subcommand)
         (struct-out invocation)
         parse-arguments)

(define-type Subcommand
  (U list-subcommand
     start-subcommand
     check-subcommand
     lint-subcommand
     update-subcommand
     close-subcommand
     new-subcommand
     workflow-list-subcommand
     workflow-install-subcommand
     workflow-fork-subcommand
     guide-subcommand))
(struct list-subcommand ([closed? : Boolean]) #:transparent)
(struct start-subcommand ([arc : (U String #f)]) #:transparent)
(struct check-subcommand ([arc : (U String #f)]
                          [compare? : Boolean])
  #:transparent)
(struct lint-subcommand ([arc : (U String #f)]) #:transparent)
(struct update-subcommand ([step : (U 'begin 'commit)]
                           [arc : (U String #f)])
  #:transparent)
(struct close-subcommand ([arc : String]) #:transparent)
(struct new-subcommand ([arc : String]
                        [workflow : (U String #f)])
  #:transparent)
(struct workflow-list-subcommand () #:transparent)
(struct workflow-install-subcommand ([name : String]) #:transparent)
(struct workflow-fork-subcommand ([arc : String]
                                  [source : String]
                                  [name : (U String #f)])
  #:transparent)
(struct guide-subcommand () #:transparent)

;; "Envisioning the hereafter, listenin' to Steve Wonder"
(struct invocation ([handoff-repository : (U Path #f)]
                    [subcommand : Subcommand])
  #:transparent)

(: parse-arguments (-> (Vectorof String) invocation))
(define (parse-arguments argv)
  (define handoff-repository : (U Path #f) #f)
  (command-line
   #:program "handoff"
   #:argv argv
   #:usage-help
   "Commands:"
   "  list      list handoff arcs"
   "  start     print the report for resuming an arc"
   "  check     run the commands in checks.toml and report their results"
   "  lint      validate the expected structure of a handoff arc"
   "  update    record a log entry"
   "  close     close an arc"
   "  new       create an arc"
   "  workflow  list, install, or fork reusable workflows"
   "  guide     print the path of the handoff guide"
   #:once-each
   [("--repository")
    path
    "Handoff repository to use instead of the nearest .scratch/handoff/"
    (set! handoff-repository (string->path (assert path string?)))]
   #:args (subcommand . arguments)
   ;; Typed Racket types the formal arguments of `#:args` as `Any`, so we do a
   ;; runtime check explicitly (that we expect will never fail).
   (invocation handoff-repository
               (parse-subcommand (assert subcommand string?)
                                 (map (λ (a) (assert a string?)) arguments)))))

(: parse-subcommand (-> String (Listof String) Subcommand))
(define (parse-subcommand subcommand arguments)
  (define program (string-append "handoff " subcommand))
  (case subcommand
    [("list") (parse-list-subcommand program arguments)]
    [("start") (parse-start-subcommand program arguments)]
    [("check") (parse-check-subcommand program arguments)]
    [("lint") (parse-lint-subcommand program arguments)]
    [("update") (parse-update-subcommand program arguments)]
    [("close") (parse-close-subcommand program arguments)]
    [("new") (parse-new-subcommand program arguments)]
    [("workflow") (parse-workflow-subcommand program arguments)]
    [("guide") (parse-guide-subcommand program arguments)]
    [else
     (raise-handoff-error
      "unknown command ~a; expected list, start, check, lint, update, close, new, workflow, or guide"
      subcommand)]))

(: parse-arc-argument (-> String (Listof String) (U String #f)))
(define (parse-arc-argument program arguments)
  (command-line
   #:program program
   #:argv arguments
   #:args ([arc #f])
   (and arc (assert arc string?))))

(: parse-list-subcommand (-> String (Listof String) list-subcommand))
(define (parse-list-subcommand program arguments)
  (define closed? : Boolean #f)
  (command-line
   #:program program
   #:argv arguments
   #:once-each
   [("--closed") "List closed arcs instead" (set! closed? #t)]
   #:args ()
   (list-subcommand closed?)))

(: parse-start-subcommand (-> String (Listof String) start-subcommand))
(define (parse-start-subcommand program arguments)
  (start-subcommand (parse-arc-argument program arguments)))

(: parse-check-subcommand (-> String (Listof String) check-subcommand))
(define (parse-check-subcommand program arguments)
  (define compare? : Boolean #f)
  (command-line
   #:program program
   #:argv arguments
   #:once-each
   [("--compare") "Compare the results with those of the last handoff"
                  (set! compare? #t)]
   #:args ([arc #f])
   (check-subcommand (and arc (assert arc string?)) compare?)))

(: parse-lint-subcommand (-> String (Listof String) lint-subcommand))
(define (parse-lint-subcommand program arguments)
  (lint-subcommand (parse-arc-argument program arguments)))

(: parse-update-subcommand (-> String (Listof String) update-subcommand))
(define (parse-update-subcommand program arguments)
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
   (update-subcommand
    (or step (raise-handoff-error "update requires --begin or --commit"))
    (and arc (assert arc string?)))))

(: parse-close-subcommand (-> String (Listof String) close-subcommand))
(define (parse-close-subcommand program arguments)
  (command-line #:program program
                #:argv arguments
                #:args (arc)
                (close-subcommand (assert arc string?))))

(: parse-new-subcommand (-> String (Listof String) new-subcommand))
(define (parse-new-subcommand program arguments)
  (command-line
   #:program program
   #:argv arguments
   #:help-labels
   "<workflow> is a name from handoff workflow list"
   #:args (arc [workflow #f])
   (new-subcommand (assert arc string?)
                   (and workflow (assert workflow string?)))))

(: parse-workflow-subcommand (-> String (Listof String) Subcommand))
(define (parse-workflow-subcommand program arguments)
  (command-line
   #:program program
   #:argv arguments
   #:usage-help
   "Commands:"
   "  list                 list the reusable workflows"
   "  install <name>       copy a reusable workflow verbatim to the handoff repository's root"
   "  fork <arc> <source> [<name>]  copy a reusable workflow or workflow file into the arc as workflow-<name>.md"
   #:args (verb . verb-arguments)
   (let* ([verb (assert verb string?)]
          [arguments (map (λ (a) (assert a string?)) verb-arguments)]
          [program (string-append program " " verb)])
     (case verb
       [("list") (parse-workflow-list-subcommand program arguments)]
       [("install") (parse-workflow-install-subcommand program arguments)]
       [("fork") (parse-workflow-fork-subcommand program arguments)]
       [else
        (raise-handoff-error
         "unknown command workflow ~a; expected list, install, or fork"
         verb)]))))

(: parse-workflow-list-subcommand
   (-> String (Listof String) workflow-list-subcommand))
(define (parse-workflow-list-subcommand program arguments)
  (command-line #:program program #:argv arguments #:args ()
                (workflow-list-subcommand)))

(: parse-workflow-install-subcommand
   (-> String (Listof String) workflow-install-subcommand))
(define (parse-workflow-install-subcommand program arguments)
  (command-line #:program program #:argv arguments #:args (name)
                (workflow-install-subcommand (assert name string?))))

(: parse-workflow-fork-subcommand
   (-> String (Listof String) workflow-fork-subcommand))
(define (parse-workflow-fork-subcommand program arguments)
  (command-line #:program program #:argv arguments
                #:help-labels
                "<source> is either a workflow name or a reference like handoff:workflow-x.md"
                "<name> defaults to the source's name"
                #:args (arc source [name #f])
                (workflow-fork-subcommand
                 (assert arc string?)
                 (assert source string?)
                 (and name (assert name string?)))))

(: parse-guide-subcommand (-> String (Listof String) guide-subcommand))
(define (parse-guide-subcommand program arguments)
  (command-line #:program program
                #:argv arguments
                #:args ()
                (guide-subcommand)))

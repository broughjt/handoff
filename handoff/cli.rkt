#lang typed/racket/base
;; Parsing handoff's command line.

(require racket/cmdline
         "error.rkt")

(provide (struct-out list-subcommand)
         (struct-out start-subcommand)
         (struct-out check-subcommand)
         (struct-out new-subcommand)
         (struct-out workflow-list-subcommand)
         (struct-out workflow-install-subcommand)
         (struct-out workflow-fork-subcommand)
         (struct-out lint-subcommand)
         (struct-out update-subcommand)
         (struct-out guide-subcommand)
         (struct-out invocation)
         parse-arguments)

(define-type Subcommand
  (U list-subcommand
     start-subcommand
     check-subcommand
     lint-subcommand
     update-subcommand
     new-subcommand
     workflow-list-subcommand
     workflow-install-subcommand
     workflow-fork-subcommand
     guide-subcommand))
(struct list-subcommand ([archived? : Boolean]) #:transparent)
(struct start-subcommand ([arc : (U String #f)]) #:transparent)
(struct check-subcommand ([arc : (U String #f)]
                          [compare? : Boolean])
  #:transparent)
(struct new-subcommand ([arc : String]
                        [workflow : (U String #f)])
  #:transparent)
(struct workflow-list-subcommand () #:transparent)
(struct workflow-install-subcommand ([name : String]) #:transparent)
(struct workflow-fork-subcommand ([arc : String]
                                  [source : String]
                                  [name : (U String #f)])
  #:transparent)
(struct lint-subcommand ([arc : (U String #f)]) #:transparent)
(struct update-subcommand ([step : (U 'begin 'commit)]
                           [arc : (U String #f)])
  #:transparent)
(struct guide-subcommand () #:transparent)

;; "Envisioning the hereafter, listenin' to Steve Wonder"
(struct invocation ([repository : (U Path #f)]
                    [subcommand : Subcommand])
  #:transparent)

(: parse-arguments (-> (Vectorof String) invocation))
(define (parse-arguments argv)
  (define repository : (U Path #f) #f)
  (command-line
   #:program "handoff"
   #:argv argv
   #:usage-help
   "Commands:"
   "  list      list handoff arcs"
   "  start     get a brief for starting work on a handoff arc"
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

;; TODO: Make parse-*-subcommand helpers per subcommand here
(: parse-subcommand (-> String (Listof String) Subcommand))
(define (parse-subcommand subcommand arguments)
  (define program (string-append "handoff " subcommand))
  (: parse-arc-argument (-> (U String #f)))
  (define (parse-arc-argument)
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
    [("start") (start-subcommand (parse-arc-argument))]
    [("check")
     (define compare? : Boolean #f)
     (command-line
      #:program program
      #:argv arguments
      #:once-each
      [("--compare") "Compare the results with those of the last handoff"
                     (set! compare? #t)]
      #:args ([arc #f])
      (check-subcommand (and arc (assert arc string?)) compare?))]
    [("lint") (lint-subcommand (parse-arc-argument))]
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
      (update-subcommand
       (or step (raise-handoff-error "update requires --begin or --commit"))
       (and arc (assert arc string?))))]
    [("new")
     (command-line
      #:program program
      #:argv arguments
      #:help-labels
      "<workflow> is a name from handoff workflow list"
      #:args (arc [workflow #f])
      (new-subcommand (assert arc string?)
                      (and workflow (assert workflow string?))))]
    [("workflow")
     (command-line
      #:program program
      #:argv arguments
      #:usage-help
      "Commands:"
      ;; TODO: What is a "reusable" workflow as opposed to a "workflow file"?
      ;; TODO: Why not:
      ;; list workflows
      ;; copy a workflow verbatim to the root of the handoff repository
      ;; copy a workflow to the root of the handoff repository so it can be modified
      "  list                 list the reusable workflows"
      "  install <name>       copy a reusable workflow verbatim to the handoff repository's root"
      "  fork <arc> <source> [<name>]  copy a reusable workflow or workflow file into the arc as workflow-<name>.md"
      #:args (verb . verb-arguments)
      (let* ([verb (assert verb string?)]
             [arguments (map (λ (a) (assert a string?)) verb-arguments)]
             [program (string-append program " " verb)])
        (case verb
          [("list")
           (command-line #:program program #:argv arguments #:args ()
                         (workflow-list-subcommand))]
          [("install")
           (command-line #:program program #:argv arguments #:args (name)
                         (workflow-install-subcommand (assert name string?)))]
          [("fork")
           (command-line #:program program #:argv arguments
                         #:help-labels
                         "<source> is either a workflow name or a reference like handoff:workflow-x.md"
                         "<name> defaults to the source's name"
                         #:args (arc source [name #f])
                         (workflow-fork-subcommand
                          (assert arc string?)
                          (assert source string?)
                          (and name (assert name string?))))]
          [else (raise-handoff-error "unknown command workflow ~a; expected list, install, or fork"
                                     verb)])))]
    [("guide") (command-line #:program program
                             #:argv arguments
                             #:args ()
                             (guide-subcommand))]
    [else (raise-handoff-error "unknown command ~a; expected list, start, check, lint, update, new, workflow, or guide"
                               subcommand)]))

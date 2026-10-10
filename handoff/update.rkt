#lang typed/racket/base
;; The checkpoint, in two steps: `begin-update` records fresh results in a
;; pending log entry, the agent writes its entry.md, and `commit-update`
;; commits them.

(require racket/file
         racket/list
         racket/string
         "checks.rkt"
         "error.rkt"
         "lint.rkt"
         "repository.rkt"
         "results.rkt"
         "toml.rkt")

(provide (struct-out pending)
         (struct-out checkpoint)
         entry-headline
         begin-update
         commit-update)

;; `entry` is where the agent writes the entry.
(struct pending ([number : Exact-Positive-Integer]
                 [results : results]
                 [entry : Path])
  #:transparent)

;; `dirty` holds Git's short status lines for what the commit left out.
(struct checkpoint ([number : Exact-Positive-Integer]
                    [commit : String]
                    [subject : String]
                    [dirty : (Listof String)])
  #:transparent)

;; The text of the entry's opening ATX heading, which must be its first
;; nonblank line.
(: entry-headline (-> String String))
(define (entry-headline entry)
  (define lines (filter (λ ([line : String]) (non-empty-string? (string-trim line)))
                        (string-split entry "\n")))
  (when (empty? lines)
    (raise-handoff-error "the entry is empty"))
  (define heading (regexp-match #px"^ {0,3}#{1,6}[ \t]+(.*)$" (first lines)))
  (define headline
    (and heading
         ;; Drop an optional closing sequence of #s.
         (string-trim (regexp-replace #px"(^|[ \t]+)#+[ \t]*$" (string-trim (assert (second heading))) ""))))
  (unless (and headline (non-empty-string? headline))
    (raise-handoff-error "the entry must open with a heading, its headline"))
  headline)

;; Run the checks and record their results in the pending log entry, creating
;; it as the next entry when there is none. Rerunning replaces the results and
;; leaves an entry.md already written alone. Nothing is written until the
;; checks have run, and the results are renamed into place, so they are
;; never half written.
(: begin-update (-> Path String pending))
(define (begin-update repository arc)
  (define arc-path (build-path repository arc))
  (refuse-lint-errors repository arc)
  (define checks (parse-checks (read-toml (build-path arc-path checks-file))))
  (define results* (results (run-checks checks)
                            (project-branch-head (find-project-repository repository))))

  (define-values (numbers _invalid) (list-log-entries arc-path))
  (define number (or (pending-log-entry arc-path) (add1 (apply max 0 numbers))))
  (define entry-path (log-entry-path arc-path number))
  (unless (directory-exists? entry-path)
    (make-directory* (build-path arc-path log-directory))
    ;; Fails rather than reusing an entry that appeared since we listed the log.
    (make-directory entry-path))
  (define results-path (build-path entry-path results-file))
  (define temporary (path-add-extension results-path #".tmp"))
  (call-with-output-file* temporary #:exists 'truncate/replace
    (λ ([port : Output-Port]) (void (write-string (results->toml results*) port))))
  (rename-file-or-directory temporary results-path #t)
  (pending number results* (build-path entry-path entry-file)))

;; Commit the pending log entry, once its entry.md is written, along with the
;; arc's state edits and anything already staged. Refuses, leaving everything
;; in place, when the results no longer describe what is being handed over:
;; the check definitions or the project's branch head changed since
;; `begin-update`. A failed commit restores the index and keeps the entry.
(: commit-update (-> Path String checkpoint))
(define (commit-update repository arc)
  (define arc-path (build-path repository arc))
  (define number
    (or (pending-log-entry arc-path)
        (raise-handoff-error "~a has no update in progress; run handoff update --begin first" arc)))
  (define entry-path (log-entry-path arc-path number))
  (define entry (build-path entry-path entry-file))
  (unless (file-exists? entry)
    (raise-handoff-error "write the entry to ~a first" entry))
  (define subject (format "~a: ~a" arc (entry-headline (file->string entry))))
  (refuse-lint-errors repository arc)

  (define results-path (build-path entry-path results-file))
  (unless (file-exists? results-path)
    (raise-handoff-error "log/~a has no results; run handoff update --begin again" number))
  (define recorded (parse-results (read-toml results-path)))
  (define checks (parse-checks (read-toml (build-path arc-path checks-file))))
  (unless (equal? checks (map result-check (run-results (results-run recorded))))
    (raise-handoff-error "~a changed since log/~a's results were recorded; run handoff update --begin again"
                         checks-file number))
  (define recorded-head (results-project recorded))
  (define head (project-branch-head (find-project-repository repository)))
  (unless (equal? head recorded-head)
    (raise-handoff-error "the project moved from ~a to ~a since log/~a's results were recorded; run handoff update --begin again"
                         (describe-branch-head recorded-head) (describe-branch-head head) number))

  ;; Fails on an unmerged index, which could not be committed anyway.
  (define index (string-trim (git! repository "write-tree")))
  (with-handlers ([exn:fail?
                   (λ ([error : exn])
                     (git repository "read-tree" index)
                     (raise error))])
    (git! repository "add" "--" arc)
    (git! repository "commit" "--quiet" "--message" subject))
  (checkpoint number
              (string-trim (git! repository "rev-parse" "HEAD"))
              subject
              (string-split (git! repository "status" "--porcelain") "\n")))

(: refuse-lint-errors (-> Path String Void))
(define (refuse-lint-errors repository arc)
  (define diagnostics (lint-arc (build-path repository arc) repository))
  (unless (empty? diagnostics)
    (raise-handoff-error
     "~a has ~a lint errors:\n~a"
     arc (length diagnostics)
     (string-join (for/list : (Listof String) ([diagnostic (in-list diagnostics)])
                    (format "~a: ~a" (diagnostic-path diagnostic) (diagnostic-message diagnostic)))
                  "\n"))))

(: describe-branch-head (-> branch-head String))
(define (describe-branch-head branch-head*)
  (define head (branch-head-head branch-head*))
  (format "~a at ~a"
          (or (branch-head-branch branch-head*) "a detached HEAD")
          (if head (substring head 0 (min 12 (string-length head))) "no commit")))

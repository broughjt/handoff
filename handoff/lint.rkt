#lang typed/racket/base

(require "checks.rkt"
         "error.rkt"
         "repository.rkt"
         "results.rkt"
         "start.rkt"
         "toml.rkt")

(provide (struct-out diagnostic)
         lint-arc)

(struct diagnostic ([path : Path] [message : String]) #:transparent)

(: lint-arc (-> Path Path (Listof diagnostic)))
(define (lint-arc arc repository)
  (define diagnostics : (Listof diagnostic) null)
  (define (report! [path : Path] [message : String])
    (set! diagnostics (cons (diagnostic path message) diagnostics)))
  (define ((report-error [path : Path]) [error : exn])
    (report! path (exn-message error)))

  ;; Each of the state files exists
  (for ([name (in-list state-files)])
    (define path (build-path arc name))
    (unless (file-exists? path)
      (report! path "expected a regular file")))

  ;; The start.toml file exists, parses in the expected format, and each of the
  ;; scoped path references point to files that exist
  (define start-path (build-path arc start-file))
  (define project (find-project-repository repository))
  (define start*
    (with-handlers ([exn:fail:handoff? (report-error start-path)])
      (parse-start (read-toml start-path))))
  (when (start? start*)
    (for ([scope-path (in-list (cons (start-workflow start*) (map read-first-path (start-read-first start*))))])
      (define path (resolve-scope-path scope-path #:arc arc #:repository repository #:project project))
      (unless (file-exists? path)
        (report! start-path (format "~a: expected a regular file at ~a" (scope-path->string scope-path) path)))))

  ;; The checks.toml exists and parses in the expected format.
  (define checks-path (build-path arc checks-file))
  (with-handlers ([exn:fail:handoff? (report-error checks-path)])
    (parse-checks (read-toml checks-path)))

  ;; - The log path either does not exist or is a directory
  ;; - Every entry is a directory named a positive integer with no leading zeros, containing:
  ;;   - A entry.md file
  ;;   - A results.toml in the format results.rkt writes
  (define log (build-path arc log-directory))
  (cond
    [(not (or (file-exists? log) (directory-exists? log) (link-exists? log))) (void)]
    [(not (directory-exists? log)) (report! log "expected a directory")]
    [else
     (define-values (entries invalid) (list-log-entries arc))
     (for ([number (in-list entries)])
       (define entry (log-entry-path arc number))
       (cond
         [(not (directory-exists? entry)) (report! entry "expected a directory")]
         [else
          (define entry-path (build-path entry "entry.md"))
          (unless (file-exists? entry-path)
            (report! entry-path "expected a regular file"))
          (define results-path (build-path entry "results.toml"))
          (with-handlers ([exn:fail:handoff? (report-error results-path)])
            (parse-results (read-toml results-path)))]))
     (for ([name (in-list invalid)])
       (report! (build-path log name)
                "expected a log entry number, a positive integer without leading zeros"))])

  (reverse diagnostics))

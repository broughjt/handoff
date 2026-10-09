#lang typed/racket/base

(require racket/promise
         "checks.rkt"
         "error.rkt"
         "repository.rkt"
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
  (define (require-file! [path : Path])
    (unless (file-exists? path)
      (report! path "expected a regular file")))
  ;; Report the first problem in a file.
  (define-syntax-rule (try path body ...)
    (with-handlers ([exn:fail:handoff? (λ ([error : exn]) (report! path (exn-message error)))])
      body ...))

  (for ([name (in-list state-files)])
    (require-file! (build-path arc name)))

  (define start-path (build-path arc start-file))
  (define project : (Promise Path) (delay (find-project-repository repository)))
  (define start* (try start-path (parse-start (read-toml start-path))))
  (when (start? start*)
    (for ([reference (in-list (cons (start-workflow start*) (map read-first-path (start-read-first start*))))])
      (try start-path
           (define path (resolve-reference reference #:arc arc #:repository repository #:project project))
           (unless (file-exists? path)
             (report! start-path (format "~a: expected a regular file at ~a" (reference->string reference) path))))))

  (define checks-path (build-path arc checks-file))
  (try checks-path (parse-checks (read-toml checks-path)))

  (define log (build-path arc log-directory))
  (cond
    ;; A new arc does not have a log yet.
    [(not (or (file-exists? log) (directory-exists? log) (link-exists? log))) (void)]
    [(not (directory-exists? log)) (report! log "expected a directory")]
    [else
     (define-values (entries misnamed) (list-log-entries arc))
     (for ([number (in-list entries)])
       (define entry (log-entry-path arc number))
       (cond
         [(not (directory-exists? entry)) (report! entry "expected a directory")]
         [else
          (require-file! (build-path entry "entry.md"))
          (define results-path (build-path entry "results.toml"))
          (try results-path (read-toml results-path))]))
     (for ([name (in-list misnamed)])
       (report! (build-path log name)
                "expected a log entry number: a positive integer without leading zeros"))])

  (reverse diagnostics))

#lang typed/racket/base

(require racket/list
         racket/match
         racket/promise
         racket/string
         "error.rkt"
         "toml.rkt")

(provide (struct-out reference)
         (struct-out read-first)
         (struct-out start)
         Scope
         parse-start
         parse-reference
         reference->string
         resolve-reference)

(define-type Scope (U 'arc 'handoff 'project))

;; A file named relative to the arc, the handoff repository or the project.
(struct reference ([scope : Scope] [path : Path]) #:transparent)
(struct read-first ([path : reference] [reason : String]) #:transparent)
(struct start ([next : String] [workflow : reference] [read-first : (Listof read-first)]) #:transparent)

(: parse-start (-> Table start))
(define (parse-start table)
  (expect-fields table '(next workflow read_first) '() "start")
  (define reads (hash-ref table 'read_first))
  (unless (list? reads)
    (raise-handoff-error "read_first must be an array of tables"))
  (start (expect-non-empty-string table 'next "start")
         (parse-reference (expect-non-empty-string table 'workflow "start"))
         (for/list ([entry (in-list reads)] [index : Natural (in-naturals)])
           (define context (format "read_first[~a]" index))
           (define read (expect-table entry context))
           (expect-fields read '(path reason) '() context)
           (read-first (parse-reference (expect-non-empty-string read 'path context))
                       (expect-non-empty-string read 'reason context)))))

(: parse-reference (-> String reference))
(define (parse-reference text)
  (define (fail)
    (raise-handoff-error "~s: expected arc:, handoff: or project: followed by a relative file path without '..'" text))
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
     (define elements (explode-path path))
     (unless (and (relative-path? path) (not (memq 'up elements)) (ormap path? elements))
       (fail))
     (reference scope path)]
    [_ (fail)]))

(: reference->string (-> reference String))
(define (reference->string reference)
  (format "~a:~a" (reference-scope reference) (path->string (reference-path reference))))

;; `project` is a promise because finding the project runs Git, and most
;; references do not need it.
(: resolve-reference (-> reference #:arc Path #:repository Path #:project (Promise Path) Path))
(define (resolve-reference reference #:arc arc #:repository repository #:project project)
  (build-path (case (reference-scope reference)
                [(arc) arc]
                [(handoff) repository]
                [(project) (force project)])
              (reference-path reference)))

#lang typed/racket/base

(require racket/function
         racket/match
         racket/string
         racket/list
         "error.rkt"
         "toml.rkt")

(provide (struct-out scope-path)
         (struct-out read-first)
         (struct-out start)
         Scope
         parse-start
         parse-scope-path
         scope-path->string
         resolve-scope-path)

(struct start
  ([next : String]
   [workflow : scope-path]
   [read-first : (Listof read-first)])
  #:transparent)
(define-type Scope (U 'arc 'handoff 'project))
(struct scope-path ([scope : Scope] [path : Path]) #:transparent)
(struct read-first ([path : scope-path] [reason : String]) #:transparent)

(: parse-start (-> Table start))
(define (parse-start table)
  (expect-fields table (list 'next 'workflow 'read_first) null "start")
  (define reads (hash-ref table 'read_first))
  (unless (list? reads)
    (raise-handoff-error "read_first must be an array of tables"))
  (start (expect-non-empty-string table 'next "start")
         (parse-scope-path (expect-non-empty-string table 'workflow "start"))
         (for/list ([entry (in-list reads)] [index : Natural (in-naturals)])
           (define context (format "read_first[~a]" index))
           (define read (expect-table entry context))
           (expect-fields read '(path reason) null context)
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

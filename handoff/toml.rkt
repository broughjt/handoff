#lang typed/racket/base

(require racket/file
         racket/list
         racket/string
         "error.rkt")

(require/typed toml
  [parse-toml (-> String Table)])

(provide Table
         table?
         parse-toml
         read-toml
         expect-table
         expect-non-empty-string
         expect-fields)

(define-type Table (Immutable-HashTable Symbol Any))
(define-predicate table? Table)

(: read-toml (-> Path-String Table))
(define (read-toml path)
  (unless (file-exists? path)
    (raise-handoff-error "expected a regular file"))
  (define text
    (with-handlers ([exn:fail:filesystem?
                     (λ ([error : exn]) (raise-handoff-error "~a" (exn-message error)))])
      (file->string path)))
  (with-handlers ([exn:fail? (λ ([error : exn]) (raise-handoff-error "~a" (exn-message error)))])
    (parse-toml text)))

(: expect-table (-> Any String Table))
(define (expect-table value context)
  (if (table? value) value (raise-handoff-error "~a must be a table" context)))

(: expect-non-empty-string (-> Table Symbol String String))
(define (expect-non-empty-string table key context)
  (define value (hash-ref table key #f))
  (if (and (string? value) (non-empty-string? (string-trim value)))
      value
      (raise-handoff-error "~a.~a must be a nonempty string" context key)))

(: expect-fields (-> Table (Listof Symbol) (Listof Symbol) String Void))
(define (expect-fields table required optional context)
  (define (names [keys : (Listof Symbol)])
    (string-join (sort (map symbol->string keys) string<?) ", "))
  (define missing (filter (λ ([key : Symbol]) (not (hash-has-key? table key))) required))
  (define unknown (remove* (append required optional) (hash-keys table)))
  (unless (empty? missing)
    (raise-handoff-error "~a is missing ~a" context (names missing)))
  (unless (empty? unknown)
    (raise-handoff-error "~a has unknown fields ~a" context (names unknown))))

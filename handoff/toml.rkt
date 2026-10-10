#lang typed/racket/base

(require racket/file
         racket/format
         racket/list
         racket/math
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
         expect-fields
         expect-nonnegative-real
         toml-string
         toml-float)

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

(define-predicate nonnegative-real? Nonnegative-Real)

;; A finite Nonnegative-Real; Typed Racket counts +nan.0 as one.
(: expect-nonnegative-real (-> Table Symbol String Nonnegative-Real))
(define (expect-nonnegative-real table key context)
  (define value (hash-ref table key #f))
  (if (and (nonnegative-real? value) (not (nan? value)) (not (infinite? value)))
      value
      (raise-handoff-error "~a.~a must be a nonnegative number" context key)))

;;; Writing

;; toml-lib's writer escapes strings using Racket's syntax, so output with an
;; ANSI escape (\e) or a bell (\a) does not round trip through the parser.
(: toml-string (-> String String))
(define (toml-string text)
  (define multiline? (string-contains? text "\n"))
  (define out (open-output-string))
  (write-string (if multiline? "\"\"\"\n" "\"") out)
  (for ([c (in-string text)])
    (cond
      [(char=? c #\\) (write-string "\\\\" out)]
      [(char=? c #\") (write-string "\\\"" out)]
      [(and multiline? (char=? c #\newline)) (write-char c out)]
      [(char=? c #\tab) (write-char c out)]
      [(or (char<? c #\space) (char=? c #\rubout))
       (write-string "\\u" out)
       (write-string (~r (char->integer c) #:base '(up 16) #:min-width 4 #:pad-string "0") out)]
      [else (write-char c out)]))
  (write-string (if multiline? "\"\"\"" "\"") out)
  (get-output-string out))

;; We always format with a decimal point so the value reads back as a float.
(: toml-float (-> Real Natural String))
(define toml-float real->decimal-string)

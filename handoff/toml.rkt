#lang typed/racket/base
;; Reading and writing TOML.

(require racket/format
         racket/function
         racket/list
         racket/string
         "error.rkt")

(require/typed toml
  [parse-toml (-> String Table)])

(require/typed gregor
  [#:opaque Moment moment?]
  [moment->iso8601 (-> Moment String)])

(provide Table
         Moment
         moment->iso8601
         read-toml
         expect-fields
         expect-table
         expect-tables
         expect-string
         expect-nonempty-string
         expect-integer
         expect-natural
         expect-datetime
         toml-string
         toml-number)

(define-type Table (Immutable-HashTable Symbol Any))
(define-predicate table? Table)

(: read-toml (-> String String Table))
(define (read-toml text source)
  (with-handlers ([exn:fail? (λ ([error : exn])
                               (raise-handoff-error "~a: ~a"
                                                    source
                                                    (exn-message error)))])
    (parse-toml text)))

(: expect-fields (-> Table (Listof Symbol) (Listof Symbol) String Void))
(define (expect-fields table required optional context)
  (define (names [keys : (Listof Symbol)])
    (string-join (sort (map symbol->string keys) string<?) ", "))
  (define missing (remove* (hash-keys table) required))
  (define unknown (remove* (append required optional) (hash-keys table)))
  (unless (empty? missing)
    (raise-handoff-error "~a is missing ~a" context (names missing)))
  (unless (empty? unknown)
    (raise-handoff-error "~a has unknown fields ~a" context (names unknown))))

(: expect-table (-> Any String Table))
(define (expect-table value context)
  (if (table? value)
      value
      (raise-handoff-error "~a must be a table" context)))

(: expect-tables (All (A) (-> Table Symbol String (-> Table String A)
                              (Listof A))))
(define (expect-tables table key context parse)
  (define entries (hash-ref table key (const null)))
  (unless (list? entries)
    (raise-handoff-error "~a: ~a must be an array of tables" context key))
  (for/list : (Listof A) ([entry (in-list entries)]
                          [index (in-naturals)])
    (define entry-context (format "~a: ~a[~a]" context key index))
    (parse (expect-table entry entry-context) entry-context)))

(: expect-string (-> Table Symbol String String))
(define (expect-string table key context)
  (define value (hash-ref table key #f))
  (if (string? value)
      value
      (raise-handoff-error "~a: ~a must be a string" context key)))

(: expect-nonempty-string (-> Table Symbol String String))
(define (expect-nonempty-string table key context)
  (define value (hash-ref table key #f))
  (if (and (string? value) (non-empty-string? (string-trim value)))
      value
      (raise-handoff-error "~a: ~a must be a nonempty string" context key)))

(: expect-integer (-> Table Symbol String Integer))
(define (expect-integer table key context)
  (define value (hash-ref table key #f))
  (if (exact-integer? value)
      value
      (raise-handoff-error "~a: ~a must be an integer" context key)))

(: expect-natural (-> Table Symbol String Natural))
(define (expect-natural table key context)
  (define value (hash-ref table key #f))
  (if (exact-nonnegative-integer? value)
      value
      (raise-handoff-error "~a: ~a must be a nonnegative integer" context key)))

(: expect-datetime (-> Table Symbol String Moment))
(define (expect-datetime table key context)
  (define value (hash-ref table key #f))
  (if (moment? value)
      value
      (raise-handoff-error "~a: ~a must be a datetime with an offset"
                           context
                           key)))

;; We escape strings ourselves, since toml-lib's writer uses Racket's escapes
;; and so output with an ANSI escape (\e) or a bell (\a) would not read back.
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
       (write-string (~r (char->integer c)
                         #:base (list 'up 16)
                         #:min-width 4
                         #:pad-string "0")
                     out)]
      [else (write-char c out)]))
  (write-string (if multiline? "\"\"\"" "\"") out)
  (get-output-string out))

(: toml-number (-> Exact-Rational String))
(define (toml-number number)
  (number->string (if (integer? number) number (exact->inexact number))))

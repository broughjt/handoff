#lang typed/racket/base
;; The TOML helpers.

(require racket/function
         typed/rackunit
         "../toml.rkt")

(: ids (-> String (Listof String)))
(define (ids text)
  (expect-tables (read-toml text "a.toml")
                 'entry
                 "a.toml"
                 (λ ([table : Table] [context : String])
                   (expect-nonempty-string table 'id context))))

(test-case "an array of tables is parsed in order"
  (check-equal? (ids "[[entry]]
                      id = 'a'
                      [[entry]]
                      id = 'b'")
                (list "a" "b")))

(test-case "a missing array of tables is empty"
  (check-equal? (ids "") null))

(test-case "a value that is not an array is rejected"
  (check-exn #rx"^a.toml: entry must be an array of tables$"
             (thunk (ids "entry = 1"))))

(test-case "an element that is not a table is rejected"
  (check-exn #rx"^a.toml: entry\\[1\\] must be a table$"
             (thunk (ids "entry = [{ id = 'a' }, 1]"))))

(test-case "each table is parsed with its index in the context"
  (check-exn #rx"^a.toml: entry\\[1\\]: id must be a nonempty string$"
             (thunk (ids "[[entry]]
                          id = 'a'
                          [[entry]]
                          id = ''"))))

(test-case "expect-string accepts an empty string"
  (check-equal? (expect-string (read-toml "a = ''" "a.toml") 'a "a.toml") ""))

(test-case "expect-integer rejects a float"
  (check-exn #rx"^a.toml: a must be an integer$"
             (thunk (expect-integer (read-toml "a = 1.5" "a.toml")
                                    'a
                                    "a.toml"))))

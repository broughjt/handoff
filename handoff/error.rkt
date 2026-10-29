#lang typed/racket/base

(require racket/string)

(provide (struct-out exn:fail:handoff)
         (struct-out exn:fail:handoff:arc)
         raise-handoff-error
         raise-arc-errors)

;; An error thrown when the user has passed incorrect arguments or
;; configuration.
(struct exn:fail:handoff exn:fail:user ())

(: raise-handoff-error (-> String Any * Nothing))
(define (raise-handoff-error template . arguments)
  (raise (exn:fail:handoff (apply format template arguments) (current-continuation-marks))))

;; The errors of every file in an arc that does not parse.
(struct exn:fail:handoff:arc exn:fail:handoff ([messages : (Listof String)]))

(: raise-arc-errors (-> (Listof String) Nothing))
(define (raise-arc-errors messages)
  (raise (exn:fail:handoff:arc (string-join messages "\n") (current-continuation-marks) messages)))

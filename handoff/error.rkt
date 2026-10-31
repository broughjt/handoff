#lang typed/racket/base

(provide (struct-out exn:fail:handoff)
         raise-handoff-error)

;; An error thrown when the user has passed incorrect arguments or
;; configuration.
(struct exn:fail:handoff exn:fail:user ())

(: raise-handoff-error (-> String Any * Nothing))
(define (raise-handoff-error template . arguments)
  (raise (exn:fail:handoff (apply format template arguments)
                           (current-continuation-marks))))

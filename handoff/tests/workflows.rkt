#lang typed/racket/base
;; Reusable workflows.

(require (except-in typed/rackunit check)
         "../main.rkt")

(define home (string->path "/home/u"))

(: paths (-> (Listof String) (Listof Path)))
(define (paths strings)
  (map string->path strings))

(test-case "the data home comes before the data directories"
  (check-equal? (data-directories "/data" "/a:/b" home)
                (paths (list "/data" "/a" "/b"))))

(test-case "the data home defaults to ~/.local/share"
  (check-equal? (data-directories #f "/a" home)
                (paths (list "/home/u/.local/share" "/a"))))

(test-case "the data directories default to /usr/local/share and /usr/share"
  (check-equal? (data-directories "/data" #f home)
                (paths (list "/data" "/usr/local/share" "/usr/share"))))

(test-case "an empty variable takes the default"
  (check-equal? (data-directories "" "" home)
                (paths (list "/home/u/.local/share" "/usr/local/share" "/usr/share"))))

(test-case "a relative data home takes the default"
  (check-equal? (data-directories "data" "/a" home)
                (paths (list "/home/u/.local/share" "/a"))))

(test-case "empty and relative data directories are ignored"
  (check-equal? (data-directories "/data" "/a::b:/c" home)
                (paths (list "/data" "/a" "/c"))))

(test-case "a workflow name is lowercase letters, digits and hyphens"
  (check-true (workflow-name? "design-first-tdd2")))

(test-case "a workflow name does not start with a hyphen"
  (check-false (workflow-name? "-x")))

(test-case "a workflow name has no capitals or underscores"
  (check-equal? (map workflow-name? (list "README" "x_y" ""))
                (list #f #f #f)))

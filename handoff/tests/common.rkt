#lang typed/racket/base
;; Fixtures shared by the tests.

(require racket/file
         racket/port
         "../main.rkt"
         "../repository.rkt")

(provide (all-defined-out))

;; A fresh handoff repository holding one empty directory, `arc`, inside a
;; fresh project repository, both deleted after `body` returns.
(: call-with-repository (All (A) (-> (-> Path A) A)))
(define (call-with-repository body)
  (define project (make-temporary-file "handoff-test-~a" 'directory))
  (dynamic-wind
   void
   (λ ()
     (run-git project "init" "-q")
     (define repository (build-path project repository-path))
     (make-directory* (build-path repository "arc"))
     (run-git repository "init" "-q")
     (body repository))
   (λ () (delete-directory/files project))))

;; Git in `directory`, failing the test when it exits non-zero.
(: run-git (-> Path-String String * Void))
(define (run-git directory . arguments)
  (unless (apply git directory arguments)
    (error 'git "failed in ~a: ~a" directory arguments)))

(: write-file (-> Path String Void))
(define (write-file path text)
  (make-parent-directory* path)
  (call-with-output-file* path #:exists 'truncate/replace
    (λ ([port : Output-Port]) (write-string text port)))
  (void))

(struct outcome ([code : Exit-Status] [stdout : String] [stderr : String]) #:transparent)

;; Run the command line in-process from `directory`.
(: handoff (-> Path-String String * outcome))
(define (handoff directory . arguments)
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define code
    (parameterize ([current-directory directory]
                   [current-output-port stdout]
                   [current-error-port stderr])
      (main (list->vector arguments))))
  (outcome code (get-output-string stdout) (get-output-string stderr)))

;; Every file under `directory` with its contents, for asserting that nothing
;; was written.
(: snapshot (-> Path (Listof (Pairof Path Bytes))))
(define (snapshot directory)
  (for/list ([path (in-list (find-files file-exists? directory))])
    (cons path (file->bytes path))))

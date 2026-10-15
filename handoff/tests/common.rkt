#lang typed/racket/base
;; Fixtures shared by the tests.

(require racket/file
         racket/port
         "../main.rkt"
         "../model.rkt")

(provide (all-defined-out))

;; A fresh handoff repository holding one empty directory, `arc`, inside a
;; fresh project repository, both deleted after `body` returns. Git ignores
;; the user's configuration, commits as a fixed identity, and runs no
;; background maintenance.
(: call-with-repository (All (A) (-> (-> Path A) A)))
(define (call-with-repository body)
  (define project (make-temporary-file "handoff-test-~a" 'directory))
  (define environment (environment-variables-copy (current-environment-variables)))
  (for ([name+value (in-list '(("GIT_CONFIG_GLOBAL" . "/dev/null")
                               ("GIT_CONFIG_NOSYSTEM" . "1")
                               ("GIT_AUTHOR_NAME" . "Test")
                               ("GIT_AUTHOR_EMAIL" . "test@example.com")
                               ("GIT_COMMITTER_NAME" . "Test")
                               ("GIT_COMMITTER_EMAIL" . "test@example.com")
                               ;; Git runs maintenance detached after a commit,
                               ;; and its lock can vanish mid-walk in cleanup.
                               ("GIT_CONFIG_COUNT" . "1")
                               ("GIT_CONFIG_KEY_0" . "maintenance.auto")
                               ("GIT_CONFIG_VALUE_0" . "false")))])
    (environment-variables-set! environment
                                (string->bytes/utf-8 (car name+value))
                                (string->bytes/utf-8 (cdr name+value))))
  (dynamic-wind
   void
   (λ ()
     (parameterize ([current-environment-variables environment])
       (run-git project "init" "-q")
       (define repository (build-path project repository-path))
       (make-directory* (build-path repository "arc"))
       (run-git repository "init" "-q")
       (body repository)))
   (λ () (delete-directory/files project))))

(define start-text "next = \"Continue\"\nworkflow = \"handoff:shared.md\"\nread_first = []\n")

;; A repository whose `arc` passes lint. Its one check must never run during
;; lint.
(: call-with-arc (All (A) (-> (-> Path Path A) A)))
(define (call-with-arc body)
  (call-with-repository
   (λ ([repository : Path])
     (define arc (build-path repository "arc"))
     (for ([name (in-list state-files)])
       (write-file (build-path arc name) ""))
     (write-file (build-path arc "start.toml") start-text)
     (write-file (build-path repository "shared.md") "workflow")
     (write-file (build-path arc "checks.toml")
                 "[[check]]\nid = \"x\"\nclaim = \"example\"\ncommand = \"touch SHOULD_NOT_EXIST\"\n")
     (body repository arc))))

;; Git in `directory`, failing the test when it exits non-zero.
(: run-git (-> Path-String String * Void))
(define (run-git directory . arguments)
  (void (apply git directory arguments)))

(: write-file (-> Path String Void))
(define (write-file path text)
  (make-parent-directory* path)
  (call-with-output-file* path #:exists 'truncate/replace
    (λ ([port : Output-Port]) (write-string text port)))
  (void))

(struct outcome ([code : Exit-Code] [stdout : String] [stderr : String]) #:transparent)

;; Run the command line in-process from `directory`.
(: handoff (->* (Path-String) (#:stdin String) #:rest String outcome))
(define (handoff directory #:stdin [stdin ""] . arguments)
  (run-handoff directory stdin arguments))

(: run-handoff (-> Path-String String (Listof String) outcome))
(define (run-handoff directory stdin arguments)
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define code
    (parameterize ([current-directory directory]
                   [current-input-port (open-input-string stdin)]
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

;; Run from the repository, naming it explicitly.
(: handoff* (->* (Path) (#:stdin String) #:rest String outcome))
(define (handoff* repository #:stdin [stdin ""] . arguments)
  (run-handoff repository stdin (list* "--repository" (path->string repository) arguments)))

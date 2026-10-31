#lang typed/racket/base
;; Refusing to close an arc, and the references to it from other arcs.

(require racket/function
         (except-in typed/rackunit check)
         "../main.rkt"
         "fixtures.rkt")

(: closing-tag (->* () (#:entries (Listof log-entry)
                        #:dirty (Listof String)
                        #:staged (Listof String)
                        #:external-references (Listof external-reference)
                        #:tag-exists? Boolean)
                    String))
(define (closing-tag #:entries [entries (arc-log clean)]
                     #:dirty [dirty null]
                     #:staged [staged null]
                     #:external-references [external-references null]
                     #:tag-exists? [tag-exists? #f])
  (closing-tag-or-refuse "a" entries dirty staged external-references tag-exists?))

(: other (-> scope-path (Listof scope-path) (Pairof String start)))
(define (other workflow paths)
  (cons "b"
        (start "n"
               workflow
               (map (λ ([path : scope-path]) (read-first path "a reason")) paths))))

(define own-workflow (scope-path 'arc (string->path "workflow.md")))

(test-case "the tag is closed/ and the arc's name"
  (check-equal? (closing-tag) "closed/a"))

(test-case "an arc with a pending entry is refused"
  (check-exn #rx"^a/log/3 has a pending log entry; commit it with handoff update --commit or remove it first$"
             (thunk (closing-tag #:entries (append (arc-log clean)
                                                   (list (log-entry 3 #f #f #f)))))))

(test-case "an arc without a committed entry is refused"
  (check-exn #rx"^a has no committed log entry; record the closing handoff first$"
             (thunk (closing-tag #:entries null))))

(test-case "an arc with uncommitted changes is refused, listing them"
  (check-exn #rx"^a has uncommitted changes; commit or remove them first:\n   M a/tasks.md\n  \\?\\? a/notes.md$"
             (thunk (closing-tag #:dirty (list " M a/tasks.md" "?? a/notes.md")))))

(test-case "staged changes anywhere are refused, listing them"
  (check-exn #rx"^the index has staged changes; commit or unstage them first:\n  topic.md$"
             (thunk (closing-tag #:staged (list "topic.md")))))

(test-case "references from other arcs are refused, listing them"
  (check-exn #rx"^other arcs refer to files in a; change their start.toml first:\n  b/start.toml: read-first handoff:a/plan.md$"
             (thunk (closing-tag #:external-references
                                (list (external-reference "b"
                                                          'read-first
                                                          (scope-path 'handoff (string->path "a/plan.md"))))))))

(test-case "an existing tag is refused"
  (check-exn #rx"^the tag closed/a already exists$"
             (thunk (closing-tag #:tag-exists? #t))))

(test-case "a read-first into the arc is a reference"
  (check-equal? (external-references-to-arc "a" (list (other own-workflow
                                                      (list (scope-path 'handoff (string->path "a/plan.md"))))))
                (list (external-reference "b" 'read-first (scope-path 'handoff (string->path "a/plan.md"))))))

(test-case "a workflow in the arc is a reference"
  (check-equal? (external-references-to-arc "a" (list (other (scope-path 'handoff (string->path "a/workflow-x.md"))
                                                      null)))
                (list (external-reference "b" 'workflow (scope-path 'handoff (string->path "a/workflow-x.md"))))))

(test-case "the arc's directory itself is a reference"
  (check-equal? (external-references-to-arc "a" (list (other own-workflow
                                                      (list (scope-path 'handoff (string->path "a/"))))))
                (list (external-reference "b" 'read-first (scope-path 'handoff (string->path "a/"))))))

(test-case "paths outside the arc are not references"
  (check-equal? (external-references-to-arc "a" (list (other own-workflow
                                                      (list (scope-path 'handoff (string->path "ab/plan.md"))
                                                            (scope-path 'handoff (string->path "topic.md"))
                                                            (scope-path 'arc (string->path "a/plan.md"))
                                                            (scope-path 'project (string->path "a/plan.md"))))))
                null))

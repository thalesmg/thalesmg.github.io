#lang racket

(provide (all-defined-out))

(require pollen/core pollen/cache txexpr)

(define base-dir (current-directory))

(define (all-posts)
  (filter-map (lambda (f)
                (and (string-suffix? (path->string f) ".pm")
                     (build-path "posts" f)))
              (directory-list "posts")))

(define (gather-tags)
  (let* ([posts (all-posts)]
         [tags->posts (for*/fold ([acc (hash)])
                                 ([post posts]
                                  [ptag (hash-ref (cached-metas post)
                                                  'tags '())])
                        (values (hash-update acc ptag
                                             (lambda (ps) (cons post ps))
                                             '())))])
      tags->posts))

(define (make-tag-pages tags->posts)
  (make-directory* (build-path (current-directory) "tags"))
  (for ([tag->posts (hash->list tags->posts)])
    (let ([tag (car tag->posts)]
          [post-paths (map
                       (lambda (p)
                         (~a "../" p))
                       (cdr tag->posts))])
      (with-output-to-file (build-path "tags" (string-append tag ".html.pm"))
        (lambda ()
          (displayln "#lang pollen")
          (displayln "")
          (displayln (format "◊(define tagged-posts '~a)" post-paths))
          (displayln (format "◊(define-meta title \"Posts tagged ~a\")" tag))
          (displayln "◊ul{")
          (displayln
           "◊(for/splice ([p ◊tagged-posts])
                        ◊(make-post-entry (symbol->string p)))")
          (displayln "}"))
        #:exists 'replace)))
  ;; needed to avoid breaking pagetree...
  "")

(define (make-tag-list tags)
  `(span [[class "f6"]]
         "tags: "
         ,(for/splice ([tag tags])
            `(a [[href ,(string-append* (list "/tags/" tag ".html"))]
                 [class "ba br3"]]
                ,tag) " ; ")))

(define (make-post-entry path)
  (let* ([metas (cached-metas path)]
         [tags (select-from-metas 'tags metas)]
         [date (select-from-metas 'date metas)]
         [url (regexp-replace #rx"poly\\.pm$" path "html")]
         [title (hash-ref metas 'title)])
    `(li
      ,(txexpr 'a `((href ,url)) (list title)) " - " (i [[class "f3"]] ,date)
      (ul (li ,(make-tag-list tags))))))

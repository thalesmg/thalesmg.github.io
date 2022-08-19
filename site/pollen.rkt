#lang racket

(provide (all-defined-out))

(require pollen/core pollen/cache txexpr)

(define (make-tag-list tags)
  `(span [[class "f6"]]
         "tags: "
         ,(for/splice ([tag tags])
            `(a [[href ,(string-append* (list "../tags/" tag ".html"))]
                 [class "ba br3"]]
                ,tag) " ; ")))

(define (make-post-entry path)
  (let* ([_ (println (current-directory))]
         [metas (cached-metas path)]
         [tags (select-from-metas 'tags metas)]
         [date (select-from-metas 'date metas)]
         [url (regexp-replace #rx"poly\\.pm$" path "html")]
         [title (hash-ref metas 'title)])
    `(li
      ,(txexpr 'a `((href ,url)) (list title)) " - " (i [[class "f3"]] ,date)
      (ul (li ,(make-tag-list tags))))))

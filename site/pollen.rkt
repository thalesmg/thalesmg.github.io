#lang racket

(provide (all-defined-out)
         current-unescaped-tags
         ;alectryon
         )

(require pollen/core
         pollen/decode
         pollen/cache
         txexpr
         (only-in xml current-unescaped-tags)
         (only-in "alectryon.rkt" alectryon2 alectryon-blocks alectryon-blocks-raw))
(require (prefix-in pyg: pollen/unstable/pygments))

(define (alectryon . elems)
  `(alec-wrapper (alectryon ,@elems)))

(define (root . elements)
  (let* ([all-alectryon-snippets
          (select* 'alec-wrapper (cons 'root elements))]
         [alectryon-snippet->txexpr
          (when all-alectryon-snippets
            (let ([blocks (alectryon-blocks-raw
                           (map (λ (x)
                                  (string-append* (cdr x)))
                                all-alectryon-snippets))])
              (for/hash ([snip all-alectryon-snippets]
                         [block blocks])
                (values snip block))))])
    (txexpr
     'root empty
     (decode-elements
      elements
      #:txexpr-proc
      (match-lambda
        [(and node (list 'alec-wrapper (and node1 (list 'alectryon code ...))))
         (list 'raw (values (hash-ref alectryon-snippet->txexpr node1)))]
        [node
         node])
      ))))

(define (highlight lang
                   #:line-numbers? [line-numbers? #f]
                   . code)
  `(div [[class "sourceCode"]]
        (code [[class "sourceCode"]]
              ,(apply pyg:highlight
                      lang
                      #:line-numbers? line-numbers?
                      code))))

(define (new-footnotes) (cons (void) (hash)))
(define all-footnotes (new-footnotes))
(define (footnote id)
  (let ([refid
         (match all-footnotes
           [(cons mod fns)
            (let ([curr (variable-reference->module-source (#%variable-reference))])
              (if (eq? curr mod)
                  (if (hash-has-key? fns id)
                      #f
                      (begin
                        (set! all-footnotes
                              (cons mod
                                    (hash-set fns id (add1 (hash-count fns)))))
                        (~a "fnref" (add1 (hash-count fns)))))
                  (begin
                    (set! all-footnotes
                         (cons curr (hash id 1)))
                    (~a "fnref" 1))))])])
    (match all-footnotes
      [(cons mod fns)
       (reference-footnote id refid)])))

(define (reference-footnote id [refid #f])
  (match all-footnotes
    [(cons mod fns)
     (define fnid (~a "fn" (hash-ref fns id)))
     (if refid
         (txexpr 'a `((id ,refid) (href ,(~a "#" fnid)))
                 `((sup ,(~a (hash-ref fns id)))))
         (txexpr 'a `((href ,(~a "#" fnid)))
             `((sup ,(~a (hash-ref fns id))))))]))

(define (deffn id-symbol . text)
  (let ([id (symbol->string id-symbol)])
    (match all-footnotes
      [(cons mod fns)
       (define refid (~a "fnref" (hash-ref fns id)))
       (define fnid (~a "fn" (hash-ref fns id)))
       (txexpr 'li `((id ,fnid) (href ,(~a "#" refid)))
               (append text `(" " ,(hyperlink (~a "#" refid)
                                              "↩︎"))))])))

(define base-dir (current-directory))

(define (hyperlink url . text)
  (txexpr 'a `((href ,url)) text))

(define (all-posts)
  (sort
   (filter-map (lambda (f)
                 (and (string-suffix? (path->string f) ".pm")
                      (build-path "posts" f)))
               (directory-list "posts"))
   (λ (a b)
     (path<? b a))))

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
          [post-paths (sort
                       (map
                        (lambda (p)
                          (~a "../" p))
                        (cdr tag->posts))
                       string>?)])
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
  (when/splice
   tags
   `(span [[class "f6"]]
          "tags: "
          ,(for/splice ([tag tags])
             `(a [[href ,(string-append* (list "/tags/" tag ".html"))]
                  [class "ba br3"]]
                 ,tag) " ; "))))

(define (make-post-entry path)
  (let* ([metas (cached-metas path)]
         [tags (select-from-metas 'tags metas)]
         [date (select-from-metas 'date metas)]
         [url (regexp-replace #rx"poly\\.pm$" path "html")]
         [title (hash-ref metas 'title)])
    `(li
      ,(txexpr 'a `((href ,url)) (list title)) " - " (i [[class "f3"]] ,date)
      (ul (li ,(make-tag-list tags))))))

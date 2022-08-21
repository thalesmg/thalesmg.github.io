#lang racket

(provide (all-defined-out))

(require racket/system
         rackjure/threading
         (only-in html read-html-as-xml)
         json
         xml)

(define prn0 displayln)
(define prn1 displayln)

(define-values (alec-in alec-out alec-pid alec-err alec-proc)
  (values #f #f #f #f #f))

(define start
  (λ (alectryon-executable)
    (match (find-executable-path alectryon-executable)
      [(? path? alectryon)
       (prn1 (~a "launching alectryon"))
       (match (process (~a alectryon
                           " --frontend coq.json "
                           " --backend snippets-html "
                           " --stdin-filename stdin "
                           " - "))
         [(list in out pid err proc)
          (set!-values (alec-in alec-out alec-pid alec-err alec-proc)
                       (values in out pid err proc))
          (file-stream-buffer-mode out 'line)
          (prn1 "abri o alec")
          ;; (match (read-line alec-err 'any)
          ;;   [(? eof-object?) (prn0 (~a "Alectryon no err"))]
          ;;   [x (prn0 (~a x))])
          ;; (match (read-line alec-in 'any)  ;; consume "ready" line or EOF
          ;;   [(? eof-object?) (prn0 (~a "Alectryon not responding"))]
          ;;   [_ (void)])
          ]
         [_ (prn0 (~a "`" alectryon "` failed."))])]
      [#f
       (prn0 (~a "Alectryon executable `" alectryon-executable "` not found"))])))

(define (running?)
  (and alec-proc
       (eq? (alec-proc 'status) 'running)))

(define (stop) ;; -> void
  (when (running?)
    (begin0 (or (alec-proc 'exit-code) (alec-proc 'kill))
      (close-input-port alec-in)
      (close-output-port alec-out)
      (close-input-port alec-err)
      (set! running? #f)))
  (void))

(exit-handler
 (let ([old-exit-handler (exit-handler)])
   (lambda (v)
     (stop)
     (old-exit-handler v))))

(define (decode-ampersands-in-attributes x)
  (match x
    [`(,tag ([,ks ,vs] ...) . ,els)
     `(,tag
       ,(for/list ([k ks]
                   [v vs])
          (list k (regexp-replace* "&amp;" v "\\&")))
       ,@(map decode-ampersands-in-attributes els))]
    [v v]))

(define (read-html-as-xexprs) ;; (-> (listof xexpr?))
  (~>> (read-html-as-xml)
       (element #f #f 'root '())
       xml->xexpr
       decode-ampersands-in-attributes
       cddr))

(define (alectryonize code
                      #:alectryon-executable [alectryon-executable "alectryon"])
  (define (default code)
    `((pre () (code () ,code))))
  (unless (running?)
    (start alectryon-executable))
  (cond [(running?)
         (displayln (jsexpr->string code) alec-out)
         (close-output-port alec-out)

         ;; Read back the highlighted code
         (let loop ([s ""])
           (match (read-line alec-in 'any)
             [(? eof-object?)
              (~> s
                  (string-split "<!-- alectryon-block-end -->")
                  (~>> identity
                       (map (λ (s)
                              (with-input-from-string s
                                read-html-as-xexprs)))))]
             [(? string? v) (loop (~a s v "\n"))]
             [_ (copy-port alec-err (current-output-port)) ;echo error msg
                (default code)]))]
        [else (default code)]))

(define (alectryonize-raw code
                      #:alectryon-executable [alectryon-executable "alectryon"])
  (define (default code)
    `((pre () (code () ,code))))
  (unless (running?)
    (start alectryon-executable))
  ;; (with-output-to-file "/tmp/porra.json"
  ;;   (lambda ()
  ;;     (displayln (jsexpr->string code))))
  (cond [(running?)
         (displayln (jsexpr->string code) alec-out)
         (close-output-port alec-out)

         ;; Read back the highlighted code
         (let loop ([s ""])
           (match (read-line alec-in 'any)
             [(? eof-object?)
              (~> s
                  (string-split "<!-- alectryon-block-end -->")
                  )]
             [(? string? v) (loop (~a s v "\n"))]
             [_ (copy-port alec-err (current-output-port)) ;echo error msg
                (default code)]))]
        [else (default code)]))

(define (alectryon-blocks
         #:alectryon-executable [alectryon-executable "alectryon"]
         blocks)
  (filter-not
   (λ (x)
     (equal? x (list "\n")))
   (alectryonize blocks #:alectryon-executable alectryon-executable)))

(define (alectryon-blocks-raw
         #:alectryon-executable [alectryon-executable "alectryon"]
         blocks)
  (filter-not
   (λ (x)
     (equal? x (list "\n")))
   (alectryonize-raw blocks #:alectryon-executable alectryon-executable)))

(define (alectryon2
         #:alectryon-executable [alectryon-executable "alectryon"]
         . codelines)
  (define code (string-append* codelines))
  `(div ()
        ,@(alectryonize code #:alectryon-executable alectryon-executable)))

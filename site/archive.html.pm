#lang pollen

◊(define-meta title "Archives")

Here you can find all my previous posts:

◊ul{
◊(for/splice ([post (all-posts)])
   (make-post-entry (path->string post)))
}

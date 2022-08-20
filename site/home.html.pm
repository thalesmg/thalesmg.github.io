#lang pollen

◊(define-meta title "Home")

◊ul{
◊(for/splice ([post (all-posts)])
   (make-post-entry (path->string post)))
}

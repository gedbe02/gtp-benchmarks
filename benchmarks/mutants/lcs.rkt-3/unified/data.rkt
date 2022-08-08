(module data racket/base
  (#%module-begin
   (provide (struct-out label) (struct-out suffix-tree) (struct-out node))
   (define-struct label (datum i j) #:mutable)
   (define-struct suffix-tree (root))
   (define-struct node (up-label parent children suffix-link) #:mutable)))

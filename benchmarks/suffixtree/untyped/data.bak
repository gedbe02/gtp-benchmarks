#lang racket/base

(provide
  (struct-out label)
  (struct-out suffix-tree)
  (struct-out node))

;datum (or string vector)
;(= i (number and (</c j)))
;(= j (length datum))
(define-struct label (datum i j) #:mutable)

;; A suffix tree consists of a root node.
(define-struct suffix-tree (root))

;; up-label: label
;; parent: (union #f node)
;; children: (listof node)
;; suffix-link: (union #f node)
(define-struct node (up-label parent children suffix-link) #:mutable)

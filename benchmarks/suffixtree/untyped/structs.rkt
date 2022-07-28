#lang racket/base
(require
         racket/list)

(require racket/contract)

(require "label.rkt"
  (except-in "data.rkt" make-label))

(provide
 (contract-out
 [tree?
  (-> any/c boolean?)]
 [make-tree
  (-> tree?)]
 [tree-root
  (-> tree? node?)]
 [new-suffix-tree
  (-> tree?)]
 [node-find-child
  (-> node? label-element? (or/c node? #f))]
 [node-root?
  (-> node? boolean?)]
 [node-position-at-end?
  (-> node? number? boolean?)]
 [node-add-leaf!
  (-> node? label? node?)]
 [node-up-splice-leaf!
  (->i ([node node?]
        [offset number?]
        [leaf-label label?])
       (values [result1 (node offset leaf-label) node?]
               [result2 (node offset leaf-label) node?]))]
 [node-follow/k
  (-> node? label? (-> node? any/c) (-> node? number? any/c) (-> node? label? number? any/c) (-> node? number? label? number? any/c) any/c)]))


;; new-suffix-tree: void -> suffix-tree
;; Builds a new empty suffix-tree.
(define (new-suffix-tree)
  (make-suffix-tree
   ;; The root node has no label, no parent, an empty list of
   ;; children.  Its suffix link is invalid, but we set it to #f.
   (let ((root (make-node (make-label (make-vector 0)) #f (list) #f)))
     root)))


(define (node-root? node)
  (eq? #f (node-parent node)))


;; node-add-leaf!: node label -> node
;; Attaches a new leaf node to an internal node.  Returns thew new leaf.
(define (node-add-leaf! node label)
  (let ((leaf (make-node label node (list) #f)))
    (node-add-child! node leaf)
    leaf))


;; node-add-child!: node node -> void
;; Adds to the node list.
(define/contract (node-add-child! node child)
  (->i ([node node?]
        [child node?])
       [result (node child)
               void?])
  (set-node-children! node (cons child (node-children node))))


;; node-remove-child!: node node -> void
;; Removes child node.
(define/contract (node-remove-child! node child)
  (->i ([node node?]
        [child node?])
       [result (node child)
               void?])
  (set-node-children! node (remq child (node-children node))))


;; Constructor for children is a list at the moment.  TODO: change
;; children representation based on density?
(define children-list list)

(provide [contract-out (children-list list)])

;; node-leaf?: node -> boolean
;; Returns true if node is a leaf
(define/contract (node-leaf? node)
  (->i ([node node?])
       [result (node)
               boolean?])
  (empty? (node-children node)))


;; node-find-child: node label-element -> (union node #f)
;;
;; Finds the first child node whose up-label starts with (label-ref
;; label 0).  If none can be found, returns #f.
(define/contract (node-find-child node label-element)
  (->i ([node node?]
        [label-element (or/c char? symbol?)])
       [result (node label-element)
               (or/c node? #f)])
  (define (loop children)
    (cond ((null? children) #f)
          ((label-element-equal? label-element (label-ref (node-up-label (first children)) 0))
           (first children))
          (else
           (loop (rest children)))))
  (loop (node-children node)))


;; node-up-split!: node number -> node
;; Introduces a new node that goes between this node and its parent.
(define/contract (node-up-split! node offset)
  (->i ([node node?]
        [offset (and/c integer? (or/c positive? zero?))])
       [result (node offset)
               node?])
  (let* ((label (node-up-label node))
         (pre-label (sublabel label 0 offset))
         (post-label (sublabel label offset))
         (parent (node-parent node))
         (new-node (make-node pre-label parent (children-list node) #f)))
    (set-node-up-label! node post-label)
    (node-remove-child! parent node)
    (set-node-parent! node new-node)
    (node-add-child! parent new-node)
    new-node))


;; node-up-splice-leaf!: node offset label -> (values node node)
;;
;; Adds a new leaf at a splice joint between the node and its
;; parent.  Returns both the joint and the leaf.
(define (node-up-splice-leaf! node offset leaf-label)
        (let* ((split-node (node-up-split! node offset))
         (leaf (node-add-leaf! split-node leaf-label)))
    (values split-node leaf)))


;; tree-contains?: tree label -> boolean
;; Returns true if the tree contains the given label.
(define/contract (tree-contains? tree label)
  (->i ([tree suffix-tree?]
        [label label?])
       [result (tree label)
               boolean?])
  (node-follow/k (suffix-tree-root tree)
                 label
                 (lambda args #t)
                 (lambda args #t)
                 (lambda args #f)
                 (lambda args #f)))

;; node-follow/k: node label (node -> A)
;;                           (node number -> B)
;;                           (node label number -> C)
;;                           (node number label number -> D)
;;                    -> (union A B C D)
;; 
;; Traverses the node's edges along the elements of the input label.
;; Written in continuation-passing-style for leakage containment.
;; One of the four continuation arguments will be executed.
(define/contract node-follow/k
  (-> node? label? (-> node? any/c) (-> node? number? any/c) (-> node? label? number? any/c) (-> node? number? label? number? any/c) any/c)
  (lambda (node original-label
                matched-at-node/k
                matched-in-edge/k
                mismatched-at-node/k
                mismatched-in-edge/k)
    (letrec
        ((EDGE/k
          ;; follows an edge
          (lambda (node label label-offset)
            (let ((up-label (node-up-label node)))
              (let loop ((k 0))
                (cond
                 ((= k (label-length up-label))
                  (NODE/k node label (+ label-offset k)))
                 ((= (+ label-offset k) (label-length label))
                  (matched-in-edge/k node k))
                 ((label-element-equal? (label-ref up-label k)
                                        (label-ref label (+ k label-offset)))
                  (loop (add1 k)))
                 (else
                  (mismatched-in-edge/k node k label
                                        (+ k label-offset))))))))
         ;; follows a node
         (NODE/k
          (lambda (node label label-offset)
            (if (= (label-length label) label-offset)
                (matched-at-node/k node)
                (let ((child (node-find-child
                              node
                              (label-ref label label-offset))))
                  (if child
                      (EDGE/k child label label-offset)
                      (mismatched-at-node/k node label label-offset)))))))
      (NODE/k node (label-copy original-label) 0))))


;; node-position-at-end?: node number -> boolean
;;
;; Returns true if the position defined by node and the up-label
;; offset are pointing at the end of the node.
(define (node-position-at-end? node offset)
  (label-ref-at-end? (node-up-label node) offset))

;; --- from suffixtree.rkt

(define tree? suffix-tree?)

(define make-tree new-suffix-tree)

(define tree-root suffix-tree-root)


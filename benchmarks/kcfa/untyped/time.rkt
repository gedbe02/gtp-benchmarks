#lang racket

(require
  racket/list
  "structs.rkt"
  "benv.rkt"
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
  )

;; ---

(provide
 (contract-out
  [time-zero
   (listof Time?)]
  ;; k
  [tick
   (configurable-ctc
    [max (->i ([call Stx-type/c]
               [time Time?])
              [result (call time)
                      (and/c Time?
                             (length=/c 1 #;(k))
                             (prefix-of/c (cons (Stx-label call) time)))])]
    [types (Stx-type/c Time? . -> . Time?)])]
  [alloc
   (configurable-ctc
    [max (->i ([time Time?])
              [result (time)
                      (->i ([var Var?])
                           [result (var)
                                   (and/c Binding-type/c
                                          (Binding/c (equal?/c var)
                                                     (equal?/c time)))])])]
    [types (Time? . -> . (Var? . -> . Binding-type/c))])])
 )

;; =============================================================================

(define/ctc-helper natural? exact-nonnegative-integer?)

;; ---
;(define-type Value Closure)

(define/ctc-helper ((length-is/c compare) n)
  ;; ll: we only implemented up to one level of currying :(
  (λ (l) (compare (length l) n)))
(define/ctc-helper length<=/c (length-is/c <=))
(define/ctc-helper length=/c (length-is/c =))
(define/ctc-helper ((prefix-of/c l) pref)
  (list-prefix? pref l))

;(: take* (All (A) (-> (Listof A) Natural (Listof A))))
(define(take* l n)
  (for/list ([e (in-list l)]
             [i (in-range n)])
    e))

;; ---

;(: time-zero Time)
(define time-zero
  '())

;(: k (Parameterof Natural))
#;(define/contract k
    (configurable-ctc
     [max (parameter/c (and/c natural?
                              (=/c 1)))]
     [types (parameter/c natural?)])
    (make-parameter 1))

;(: tick (-> Stx Time Time))
(define (tick call time)
  (define label (Stx-label call))
  (take* (cons label time) 1 #;(k)))

;(: alloc (-> Time (-> Var Addr)))
(define (alloc time)
  (λ (var)
    (Binding var time)))


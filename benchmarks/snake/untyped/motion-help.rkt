#lang racket
(require "data.rkt"
         "cut-tail.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

;; next-head : Posn Direction -> Posn
;; Compute next position for head.
(define (next-head seg dir)
  (cond [(equal? "right" dir) (posn (add1 (posn-x seg)) (posn-y seg))]
        [(equal? "left" dir)  (posn (sub1 (posn-x seg)) (posn-y seg))]
        [(equal? "down" dir)  (posn (posn-x seg) (sub1 (posn-y seg)))]
        [else                 (posn (posn-x seg) (add1 (posn-y seg)))]))

;; snake-slither : Snake -> Snake
;; move the snake one step
(define (snake-slither snk)
  (let ([d (snake-dir snk)])
    (snake d
           (cons (next-head (car (snake-segs snk))
                            d)
                 (cut-tail (snake-segs snk))))))

;; snake-grow : Snake -> Snake
;; Grow the snake one segment.
(define (snake-grow snk)
  (let ([d (snake-dir snk)])
    (snake d
           (cons (next-head (car (snake-segs snk))
                            d)
                 (snake-segs snk)))))

(provide
 (contract-out
  [snake-slither
   (configurable-ctc
    [max (->i ([snk snake-type?])
              [result (snk)
                      (and/c
                       ;; intermediate ctcs
                       ;; (compose (=/c (length (snake-segs snk)))
                       ;;          length snake-segs)
                       (match snk
                         [(snake d (and segs
                                        (cons segs/h segs/t)))
                          (snake/c d
                                   (cons/c (posn=?/c (next-head segs/h d))
                                           (snake-segs=?/c
                                            (drop-right segs 1))))]
                         [_ #f]))])]
    [types (snake-type? . -> . snake-type?)])]
  [snake-grow
   (configurable-ctc
    [max (->i ([snk snake-type?])
              [result (snk)
                      (and/c
                       ;; intermediate ctcs
                       ;; (compose (=/c (add1 (length (snake-segs snk))))
                       ;;          length snake-segs)
                       ;; (compose (equal?/c (snake-segs snk))
                       ;;          rest snake-segs)
                       (match snk
                         [(snake d (and segs
                                        (cons segs/h segs/t)))
                          (snake/c d
                                   (cons/c (posn=?/c (next-head segs/h d))
                                           (snake-segs=?/c segs)))]))])]
    [types (snake-type? . -> . snake-type?)])]))
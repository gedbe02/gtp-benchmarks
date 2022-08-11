#lang racket
(require "data.rkt"
         "const.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")


;; snake-wall-collide? : Snake -> Boolean
;; Is the snake colliding with any of the walls?
(define (snake-wall-collide? snk)
  (head-collide? (car (snake-segs snk))))

;; head-collide? : Posn -> Boolean
(define (head-collide? p)
  (or (<= (posn-x p) 0)
      (>= (posn-x p) BOARD-WIDTH)
      (<= (posn-y p) 0)
      (>= (posn-y p) BOARD-HEIGHT)))

(define/ctc-helper (truthy->bool x) (if x #t #f))
(define/ctc-helper memf? (compose truthy->bool memf))

;; snake-self-collide? : Snake -> Boolean
(define (snake-self-collide? snk)
  (segs-self-collide? (car (snake-segs snk))
                      (cdr (snake-segs snk))))

;; segs-self-collide? : Posn Segs -> Boolean
(define (segs-self-collide? h segs)
  (cond [(empty? segs) #f]
        [else (or (posn=? (car segs) h)
                  (segs-self-collide? h (cdr segs)))]))
(provide
 (contract-out
  [snake-wall-collide?
   (configurable-ctc
    [max (->i ([snk snake-type?])
              [result (snk)
                      (match snk
                        [(snake _ (cons h _)) (head-collide? h)]
                        [_ #f])])]
    [types (snake-type? . -> . boolean?)])]
  [snake-self-collide?
   (configurable-ctc
    [max (->i ([snk snake-type?])
              [result (snk)
                      (match snk
                        [(snake _ (cons h t))
                         (memf? (posn=?/c h) t)]
                        [_ #f])])]
    [types (snake-type? . -> . boolean?)])]))

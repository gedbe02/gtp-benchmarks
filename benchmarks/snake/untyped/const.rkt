#lang racket
(require "data.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(define GRID-SIZE
  30)
(define BOARD-HEIGHT
  20)
(define BOARD-WIDTH
  30)
(define (BOARD-HEIGHT-PIXELS)
  (* GRID-SIZE BOARD-HEIGHT))
(define (BOARD-WIDTH-PIXELS)
  (* GRID-SIZE BOARD-WIDTH))
(define (SEGMENT-RADIUS)
  (/ GRID-SIZE 2))
(define (FOOD-RADIUS)
  (SEGMENT-RADIUS))
(define (WORLD)
  (world (snake "right" (cons (posn 5 3) empty))
         (posn 8 12)))

(provide
 (contract-out
  [WORLD
   (configurable-ctc
    [max (-> (world/c (snake/c "right"
                               (snake-segs=?/c (list (posn 5 3))))
                      (posn/c 8 12)))]
    [types (-> world-type?)])]
  [GRID-SIZE
   (configurable-ctc
    [max (=/c 30)]
    [types natural?])]
  [BOARD-HEIGHT-PIXELS
   (configurable-ctc
    [max (-> (=/c (* GRID-SIZE BOARD-HEIGHT)))]
    [types (-> natural?)])]
  [BOARD-WIDTH
   (configurable-ctc
    [max (=/c 30)]
    [types natural?])]
  [BOARD-HEIGHT
   (configurable-ctc
    [max (=/c 20)]
    [types natural?])]))


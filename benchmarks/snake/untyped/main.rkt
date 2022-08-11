#lang racket

(require "data.rkt"
         "const.rkt"
         "handlers.rkt"
         "motion.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(define (replay w0 hist)
  (reset!)
  (for/fold ([w w0])
            ([cmd (in-list hist)])
    (match cmd
      [`(on-key ,(? string? ke))
       (handle-key w ke)]
      [`(on-tick)
       (world->world w)]
      [`(stop-when)
       (game-over? w)
       w]))
  (void))

(define DATA
  (with-input-from-file "../base/snake-hist.rktd" read))
(define LOOPS
  1)

(define (main hist)
  (define w0 (WORLD))
  (cond [(list? hist)
         (for ((_i (in-range LOOPS)))
           (replay w0 hist))]
        [else
         (error "bad input")]))

(time (main DATA))

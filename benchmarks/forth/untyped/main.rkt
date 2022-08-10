#lang racket

(require (only-in "eval.rkt"
                  forth-eval*
                  ))
(require (only-in racket/file file->lines)
         "../../../ctcs/precision-config.rkt"
         racket/contract
         (only-in racket/math natural?))

;; =============================================================================

(define LOOPS
  1)

(define (main lines)

  (for ((i (in-range LOOPS)))
    (define-values [_e _s] (forth-eval* lines))
    (void)))

(define lines
  (file->lines "../base/history-100.txt"))

(time (main lines))

#lang racket

(require #;racket/contract
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "streams.rkt")

;;--------------------------------------------------------------------------------------------------

;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
(define (count-from n)
  (make-simple-stream n (lambda () (count-from (add1 n)))))

(define/ctc-helper ((divisible-by/c divisor) x)
  (zero? (modulo x divisor)))

;; `sift n st` Filter all elements in `st` that are equal to `n`.
;; Return a new simple-stream.
(define (sift n st)
  (define-values (hd tl) (simple-stream-unfold st))
  (cond [(= 0 (modulo hd n)) (sift n tl)]
        [else (make-simple-stream hd (lambda () (sift n tl)))]))

(define/ctc-helper prime? (let () (local-require math/number-theory) prime?))

(define/ctc-helper (sieved-simple-stream-following/c sieved-n)
  (and/c (simple-streamof (and/c integer? (not/c (divisible-by/c sieved-n))))
         (simple-stream/dc any/c
                           (λ (first)
                             (-> (sieved-simple-stream-following/c first))))))

;; `sieve st` Sieve of Eratosthenes
(define (sieve st)
  (define-values (hd tl) (simple-stream-unfold st))
  (make-simple-stream hd (lambda () (sieve (sift hd tl)))))

;; simple-stream of prime numbers
(define primes
  (sieve (count-from 2)))

(define N-1
  20)

(define (main)
  (void (simple-stream-get primes N-1)))

(time (main))

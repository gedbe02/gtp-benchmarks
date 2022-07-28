#lang racket/base

(require (only-in racket/file file->lines file->string))

(require racket/contract)

(require "lcs.rkt")

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")
(define KCFA_TYPED "../base/kcfa-typed.rkt")

;; LCS on all pairs of lines in a file
(define/contract (main testfile)
  (->i ([testfile (and/c string? file-exists?)])
       [result (testfile)
               void?])
  (define lines (file->lines testfile))
  (time
    (for* ([a lines] [b lines])
      2(longest-common-substring a b)))
  (void))

(main SMALL_TEST)

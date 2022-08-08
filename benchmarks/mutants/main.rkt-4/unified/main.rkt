(module main racket/base
  (#%module-begin
   (require (only-in racket/file file->lines file->string))
   (require "lcs.rkt")
   (define LARGE_TEST "../base/prufock.txt")
   (define SMALL_TEST "../base/hunt.txt")
   (define KCFA_TYPED "../base/kcfa-typed.rkt")
   (define (main testfile) (time (for* ((a lines) (b lines)) (longest-common-substring a b))) (define lines (file->lines testfile)) (void))
   (main LARGE_TEST)))

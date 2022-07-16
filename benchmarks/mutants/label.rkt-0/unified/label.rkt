(module label racket/base
  (#%module-begin
   (require "data.rkt")
   (require racket/contract)
   
   (define (t? t)
  (define (is-true? truth)
    (equal? t truth))
  is-true?)
   
   (define/contract (label-element? obj)
     (->i ([obj (or/c char? symbol? void?)])
       [result (obj)
               (t? #t)])
     #f)
   
   (define label-element-equal? equal?)
   (provide (rename-out (ext:make-label make-label))
            label-element?
            label-element-equal?
            string->label
            string->label/with-sentinel
            vector->label
            vector->label/with-sentinel
            label->string
            label->string/removing-sentinel
            label->vector
            label-length
            label-ref
            sublabel
            sublabel!
            label-prefix?
            label-equal?
            label-empty?
            label-copy
            label-ref-at-end?
            label-source-id
            label-same-source?)
   
   (define/contract (ext:make-label label-element)
     (->i ([label-element (or/c string? vector?)])
       [result (label-element)
               label?])
     (cond
      ((string? label-element) (string->label label-element))
      ((vector? label-element) (vector->label label-element))
      (else (error 'make-label "Don't know how to make label from ~S" label-element))))
   
   (define (start-sentinel s) ; s will be "sentinel"
     (define (sen-start? sen) ; sen is (gensym 'sentinel)
       (string=? (substring (symbol->string sen) 0 8) s))
     sen-start?)
   
   (define/contract (make-sentinel)
     (-> (and/c symbol? (start-sentinel "sentinel")))
     (gensym 'sentinel))
   
   (define/contract (sentinel? datum)
     (-> any/c boolean?)
     (symbol? datum))

   (define (valid-vector? x)
     (define (valid? v)
       (or/c (vectorof char?) (ormap (and/c symbol? (start-sentinel "sentinel")) (vector->list v))))
     valid?)
   
   (define/contract (vector->label vector)
     (->i ([vector (valid-vector? null)])
       [result (vector)
               label?])
     (make-label (vector->immutable-vector vector) 0 (vector-length vector)))
   
   (define/contract (vector->label/with-sentinel vector)
     (->i ([vector (vectorof char?)])
       [result (vector)
               label?])
     (let* ((N (vector-length vector)) (V (make-vector (add1 N))))
       (vector-set! V N (make-sentinel))
       (let loop ((i 0)) (if (< i N) (begin (vector-set! V i (vector-ref vector i)) (loop (add1 i))) (vector->label V)))))
   
   (define/contract string->label
     (->i ([str string?])
          [result (str)
                  label?])
     (let ((f (compose vector->label list->vector string->list))) (lambda (str) (f str))))

   #|(define (end-sentinel sen s) ; "sentinel", str
  (define (sentinel-ending? sen-label) ; label made after str
    (string=? (substring (label->string sen-label) (string-length s)) sen))
  sentinel-ending?)|#
   
   (define/contract string->label/with-sentinel
     (->i ([str string?])
       [result (str)
               label?])
     (let ((f (compose vector->label/with-sentinel list->vector string->list))) (lambda (str) (f str))))
   
   (define/contract (label-length label)
     (->i ([label label?])
      [result (label)
              (and/c integer? (or/c positive? zero?))])
     (- (label-j label) (label-i label)))
   
   (define/contract (label-ref label k)
     (->i ([label label?]
        [k (and/c integer? (or/c positive? zero?))])
        [result (label k)
                (or/c char? (and/c symbol? (start-sentinel "sentinel")))])
     (vector-ref (label-datum label) (+ k (label-i label))))
   
   (define/contract sublabel
     (->* (label? (and/c (or/c positive? zero?) integer?)) ((and/c (or/c positive? zero?) integer?)) label?)
     (case-lambda
      ((label i) (sublabel label i (label-length label)))
      ((label i j) (unless (<= i j) (error 'sublabel "illegal sublabel [~a, ~a]" i j)) (make-label (label-datum label) (+ i (label-i label)) (+ j (label-i label))))))
   
   (define/contract sublabel!
     (->* (label? (and/c integer? (or/c positive? zero?))) ((and/c integer? (or/c positive? zero?))) void?)
     (case-lambda ((label i) (sublabel! label i (label-length label))) ((label i j) (begin (set-label-j! label (+ j (label-i label))) (set-label-i! label (+ i (label-i label))) (void)))))
   
   (define/contract (label-prefix? prefix other-label)
     (->i ([prefix label?]
        [other-label label?])
       [result (prefix other-label)
               boolean?])
     (let ((m (label-length prefix)) (n (label-length other-label)))
       (if (> m n) #f (let loop ((k 0)) (if (= k m) #t (and (equal? (label-ref prefix k) (label-ref other-label k)) (loop (add1 k))))))))
   
   (define/contract (label-equal? l1 l2)
     (->i ([l1 label?]
        [l2 label?])
       [result (l1 l2)
               boolean?])
     (and (= (label-length l1) (label-length l2)) (label-prefix? l1 l2)))
   
   (define/contract (label-empty? label)
     (->i ([label label?])
       [result (label)
               boolean?])
     (>= (label-i label) (label-j label)))
   
   (define/contract (label->string label)
     (-> label? string?)
     (list->string (vector->list (label->vector label))))
   
   (define (label->string/removing-sentinel label)
     (let* ((ln (label-length label)) (N (if (and (> ln 0) (sentinel? (label-ref label (sub1 ln)))) (sub1 ln) ln))) (build-string N (lambda (i) (label-ref label i)))))
   (define (label->vector label)
     (let* ((N (label-length label)) (buffer (make-vector N)))
       (let loop ((i 0)) (if (< i N) (begin (vector-set! buffer i (label-ref label i)) (loop (add1 i))) (vector->immutable-vector buffer)))))
   
   (define (same-as x)
  (define (equals? y)
    (label-equal? x y))
  equals?)
   
   (define/contract (label-copy label)
     (->i ([label label?])
       [result (label)
               (same-as label)])
     (make-label (label-datum label) (label-i label) (label-j label)))
   
   (define (label-ref-at-end? label offset) (= offset (label-length label)))
   (define (label-source-id label) (eq-hash-code (label-datum label)))
   (define (label-same-source? label-1 label-2) (eq? (label-datum label-1) (label-datum label-2)))
   (provide label-source-eq?)
   (define label-source-eq? label-same-source?)))

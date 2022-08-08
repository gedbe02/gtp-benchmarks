#lang racket/base

(require
  "data.rkt"
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt")

(require racket/contract)
(require racket/vector)
;; Label implementation.  Labels are like strings, but also allow for
;; efficient shared slicing.
;;
;; TODO later: see if we can generalize labels to be more than just
;; text strings.  It might be useful to have them as arbitrary
;; elements, perhaps as vectors?

;; FIXME: set custom writer for labels to be displayed for debugging
;; purposes.
;; (http://download.plt-scheme.org/doc/299.400/html/mzscheme/mzscheme-Z-H-11.html#node_sec_11.2.10)


;; label-element? object -> true
;; Every value is considered to be a possible label-element.
(define (label-element? obj)
  #t)

;; When comparing label elements, we use equal?.
;;
(define label-element-equal?
  equal?)


(provide
 (contract-out
  [rename ext:make-label make-label
          (configurable-ctc
           [max
            (->i ([label-element (or/c string? vector?)])
                 [result (label-element)
                         (lambda (result) 
                           (and (label? result)
                                (and (= (label-i result) 0)
                                     (= (label-j result) (cond [(string? label-element) (string-length label-element)]
                                                               [(vector? label-element) (vector-length label-element)])))))])]
           [types
            (-> (or/c string? vector?) label?)])]
                                       
  [label-element?
   (configurable-ctc
    [max
     (->i ([obj any/c])
          [result (obj)
                  #t])]
    [types
     (-> any/c boolean?)])]
  [label-element-equal?
   (configurable-ctc
    [max
     (->i ([l1 label-element?]
           [l2 label-element?])
          [result (l1 l2)
                  boolean?])]
    [types
     (-> label-element? label-element? boolean?)])]
  [string->label
   (configurable-ctc
    [max
     (->i ([s string?])
          [result (s)
                  (lambda (result)
                    (and (label? result)
                         (and (equal? (label-datum result) s)
                              (and (= (label-i result) 0)
                                   (= (label-j result) (string-length s))))))])]
    [types (-> string? label?)])]
  [string->label/with-sentinel
   (configurable-ctc
    [max
     (->i ([s string?])
          [result (s)
                  (lambda (result)
                    (and (label? result)
                         (and (symbol? (vector-ref (label-datum result) (- (vector-length (label-datum result)) 1)))
                              (and (equal? (list->string
                                            (vector->list
                                             (vector-take (label-datum result) (- (vector-length (label-datum result)) 1)))) s)
                                   (and (= (label-i result) 0)
                                        (= (label-j result) (+ (string-length s) 1)))))))])]
    [types
     (-> string? label?)])]
  [vector->label
   (configurable-ctc
    [max
     (->i ([vector vector?])
          [result (vector)
                  (lambda (result)
                    (and (label? result)
                         (and (equal? (label-datum result) vector)
                              (and (= (label-i result) 0)
                                   (= (label-j result) (vector-length vector))))))])]
    [types
     (-> vector? label?)])]
  [vector->label/with-sentinel
   (configurable-ctc
    [max
     (->i ([vector vector?])
          [result (vector)
                  (lambda (result)
                    (and (label? result)
                         (and (symbol? (vector-ref (label-datum result) (- (vector-length (label-datum result)) 1)))
                              (and (equal? (vector-take (label-datum result) (- (vector-length (label-datum result)) 1)) vector)
                                   (and (= (label-i result) 0)
                                        (= (label-j result) (+ (vector-length vector) 1)))))))])]
    [types
     (-> vector? label?)])]
  [label-length
   (configurable-ctc
    [max
     (->i ([label (lambda (l)
                    (and (label? l)
                         (and (vector? (label-datum l))
                              (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                   (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))))))])
          [result (label)
                  (lambda (result)
                    (and (= (- (label-j label) (label-i label)) result)
                         (and (integer? result)
                              (or (positive? result) (zero? result)))))])]
    [types
     (-> number?)])]
  [label-ref
   (configurable-ctc
    [max
     (->i ([label (lambda (l)
                    (and (label? l)
                         (and (vector? (label-datum l))
                              (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                   (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))))))]
           [k (label) (and/c (and/c integer? (or/c positive? zero?)) (<=/c (label-j label)))])
          [result (label k)
                  (or/c char? symbol?)])]
    [types
     (-> label? (or/c char? symbol?))])]
  [sublabel
   (configurable-ctc
    [max
     (->i ([label (lambda (l)
                    (and (label? l)
                         (and (vector? (label-datum l))
                              (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                   (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))))))]
           [n (and/c integer? (or/c positive? zero?))])
          ([num (n) (and/c (and/c integer? (or/c positive? zero?)) (>/c n))])
          [result (label n num)
                  (lambda (result)
                    (and (label? result)
                         (and (vector? (label-datum result))
                              (and (symbol? (vector-ref (label-datum result) (- (vector-length (label-datum result)) 1)))
                                   (and (and (integer? (label-i result)) (or (positive? (label-i result)) (zero? (label-i result))))
                                        (and (and (integer? (label-j result)) (or (positive? (label-j result)) (zero? (label-j result))))
                                             (> (label-j result) (label-i result))))))))])]
    [types
     (-> label? number? label?)])]
  [sublabel!
   (configurable-ctc
    [max
     (->i ([label (lambda (l)
                    (and (label? l)
                         (and (vector? (label-datum l))
                              (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                   (and (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))
                                        (>= (label-j l) (label-i l)))))))]
           [n (and/c integer? (or/c positive? zero?))])
          ([num (n) (and/c (and/c integer? (or/c positive? zero?)) (>/c n))])
          [result (label n num)
                  void?])]
    [types
     (-> label? number? void?)])]
  [label-prefix?
   (configurable-ctc
    [max
     (->i ([prefix (lambda (l)
                     (and (label? l)
                          (and (vector? (label-datum l))
                               (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                    (and (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))
                                         (>= (label-j l) (label-i l)))))))]
           [other-label (lambda (l)
                          (and (label? l)
                               (and (vector? (label-datum l))
                                    (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                         (and (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))
                                              (>= (label-j l) (label-i l)))))))])
          [result (prefix other-label)
                  boolean?])]
    [types
     (-> label? label? boolean?)])]
  [label-equal?
   (configurable-ctc
    [max
     (->i ([l1 (lambda (l)
                 (and (label? l)
                      (and (vector? (label-datum l))
                           (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                (and (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))
                                     (>= (label-j l) (label-i l)))))))]
           [l2 (lambda (l)
                 (and (label? l)
                      (and (vector? (label-datum l))
                           (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                (and (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))
                                     (>= (label-j l) (label-i l)))))))])
          [result (l1 l2)
                  boolean?])]
    [types
     (-> label? label? boolean?)])]
  [label-empty?
   (configurable-ctc
    [max
     (->i ([label (lambda (l)
                    (and (label? l)
                         (and (vector? (label-datum l))
                              (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                   (and (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))
                                        (>= (label-j l) (label-i l)))))))])
          [result (label)
                  boolean?])]
    [types
     (-> label? boolean?)])]
  [label-copy
   (configurable-ctc
    [max
     (->i ([label (lambda (l)
                    (and (label? l)
                         (and (vector? (label-datum l))
                              (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                   (and (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))
                                        (>= (label-j l) (label-i l)))))))])
          [result (label)
                  (lambda (l)
                    (and (label? l)
                         (and (vector? (label-datum l))
                              (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                   (and (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))
                                        (>= (label-j l) (label-i l)))))))])]
    [types
     (-> label? label?)])]
  [label-ref-at-end?
   (configurable-ctc
    [max
     (->i ([label (lambda (l)
                    (and (label? l)
                         (and (vector? (label-datum l))
                              (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                   (and (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))
                                        (>= (label-j l) (label-i l)))))))]
           [offset (label) (and/c (and/c integer? (or/c positive? zero?)) (<=/c (label-j label)))])
          [result (label offset)
                  boolean?])]
    [types
     (-> label? number? boolean?)])]
  [label-source-id
   (configurable-ctc
    [max
     (->i ([label (lambda (l)
                    (and (label? l)
                         (and (vector? (label-datum l))
                              (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                   (and (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))
                                        (>= (label-j l) (label-i l)))))))])
          [result (label)
                  (and/c integer? (or/c positive? zero?))])]
    [types
     (-> label? number?)])]
  [label-same-source?
   (configurable-ctc
    [max
     (->i ([label-1 (lambda (l)
                      (and (label? l)
                           (and (vector? (label-datum l))
                                (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                     (and (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))
                                          (>= (label-j l) (label-i l)))))))]
           [label-2 (lambda (l)
                      (and (label? l)
                           (and (vector? (label-datum l))
                                (and (and (integer? (label-i l)) (or (positive? (label-i l)) (zero? (label-i l))))
                                     (and (and (integer? (label-j l)) (or (positive? (label-j l)) (zero? (label-j l))))
                                          (>= (label-j l) (label-i l)))))))])
          [result (label-1 label-2)
                  (and/c boolean (eq? (label-datum label-1) (label-datum label-2)))])]
    [types
     (-> label? label? boolean?)])])
 ; these aren't used except for debugging
 label->string
 label->string/removing-sentinel
 label->vector)


;; make-label: label-element -> label
;; Constructs a new label from either a string or a vector of things.
(define (ext:make-label label-element)
  (cond ((string? label-element) (string->label label-element))
        ((vector? label-element) (vector->label label-element))
        (else
         (error 'make-label "Don't know how to make label from ~S" label-element))))


(define (make-sentinel)
  (gensym 'sentinel))

(define (sentinel? datum)
  (symbol? datum))

;; vector->label vector
;; Constructs a new label from the input vector.
(define (vector->label vector)
  (make-label (vector->immutable-vector vector)
              0 (vector-length vector)))


;; vector->label vector
;; Constructs a new label from the input vector, with a sentinel
;; symbol at the end.
(define (vector->label/with-sentinel vector)
  (let* ((N (vector-length vector))
         (V (make-vector (add1 N))))
    (vector-set! V N (make-sentinel))
    (let loop ((i 0))
      (if (< i N)
          (begin (vector-set! V i (vector-ref vector i))
                 (loop (add1 i)))
          (vector->label V)))))

;; string->label: string -> label
;; Constructs a new label from the input string.
(define string->label
  (let ((f (compose vector->label list->vector string->list)))
    (lambda (str) (f str))))


;; string->label/with-sentinel: string -> label
;; Constructs a new label from the input string, attaching a unique
;; sentinel symbol at the end of the label.
;;
;; Note: this label can not be converted in whole back to a string:
;; the sentinel character interferes with string concatenation
(define string->label/with-sentinel
  (let ((f (compose vector->label/with-sentinel list->vector string->list)))
    (lambda (str) (f str))))


;; label-length: label -> number?
;; Returns the length of the label.
(define (label-length label)
  (- (label-j label) (label-i label)))


;; label-ref: label number? -> char
;; Returns the kth element in the label.
(define (label-ref label k)
  (vector-ref (label-datum label) (+ k (label-i label))))


;; sublabel: label number number -> label
;; Gets a slice of the label on the half-open interval [i, j)
(define sublabel
  (case-lambda
    ((label i)
     (define s (sublabel label i (label-length label)))
     ;(displayln (format "label:(datum:~a i:~a j:~a) i:~a" (label-datum s) (label-i s) (label-j s) i))
     s)
    ((label i j)
     (unless (<= i j)
       (error 'sublabel "illegal sublabel [~a, ~a]" i j))
     (define t (make-label (label-datum label)
                           (+ i (label-i label))
                           (+ j (label-i label))))
     ;(displayln (format "label:(datum:~a i:~a j:~a) i:~a j:~a" (label-datum t) (label-i t) (label-j t) i j))
     t)))


;; sublabel!: label number number -> void
;; destructively sets the input label to sublabel.
(define sublabel!
  (case-lambda
    ((label i)
     (sublabel! label i (label-length label)))
    ;(displayln (format "datum:~a i:~a" (label-datum label) (label-i label))))
    ((label i j)
     ;(displayln (format "datum:~a i:~a j:~a" (label-datum label) (label-i label) (label-j label)))
     (begin
       ;; order dependent code ahead!
       (set-label-j! label (+ j (label-i label)))
       (set-label-i! label (+ i (label-i label)))
       (void)))))


;; label-prefix?: label label -> boolean
;; Returns true if the first label is a prefix of the second label
(define (label-prefix? prefix other-label)
  ;(displayln (format "prefix:~a prefixi:~a prefixj:~a other:~a otheri:~a otherj:~a" (label-datum prefix) (label-i prefix) (label-j prefix) (label-datum other-label) (label-i other-label) (label-j other-label)))
  (let ((m (label-length prefix))
        (n (label-length other-label)))
    (if (> m n)                       ; <- optimization: prefixes
        ; can't be longer.
        #f
        (let loop ((k 0))
          (if (= k m)
              #t
              (and (equal? (label-ref prefix k) (label-ref other-label k))
                   (loop (add1 k))))))))


;; label-equal?: label label -> boolean
;; Returns true if the two labels are equal.
(define(label-equal? l1 l2)
  (and (= (label-length l1) (label-length l2))
       (label-prefix? l1 l2)))


;; label-empty?: label -> boolean
;; Returns true if the label is considered empty
(define (label-empty? label)
  (>= (label-i label) (label-j label)))


;; label->string: label -> string
;; Extracts the string that the label represents.
;; Precondition: the label must have originally come from a string.
;; Note: this operation is expensive: don't use it except for debugging.



;;;; this isn't used in the program
(define (label->string label)
  (list->string (vector->list (label->vector label))))


;;;;; this also isn't used in the program
(define (label->string/removing-sentinel label)
  (let* ([ln (label-length label)]
         [N (if (and (> ln 0) (sentinel? (label-ref label (sub1 ln))))
                (sub1 ln)
                ln)])
    (build-string N (lambda (i) (label-ref label i)))))


;; label->vector: label -> vector
;; Extracts the vector that the label represents.
;; Note: this operation is expensive: don't use it except for debugging.


;;;;; also not used
(define (label->vector label)
  (let* ((N (label-length label))
         (buffer (make-vector N)))
    (let loop ((i 0))
      (if (< i N)
          (begin
            (vector-set! buffer i (label-ref label i))
            (loop (add1 i)))
          (vector->immutable-vector buffer)))))


;; label-copy: label->label
;; Returns a copy of the label.
(define (label-copy label)
  (make-label (label-datum label) (label-i label) (label-j label)))


;; label-ref-at-end?: label number -> boolean
(define (label-ref-at-end? label offset)
  ;(displayln (format "i:~a j:~a offset:~a" (label-i label) (label-j label) offset))
  (= offset (label-length label)))


;; label-source-id: label -> number
(define (label-source-id label)
  (eq-hash-code (label-datum label)))

;; label-same-source?: label label -> boolean
(define (label-same-source? label-1 label-2)
  (eq? (label-datum label-1) (label-datum label-2)))

;; --- from suffixtree.rkt
(provide label-source-eq?)
(define label-source-eq? label-same-source?)

#lang racket

;; implements the model for the T path finder 

(provide 
 ;; type MBTA% = 
 ;; (Class mbta% 
 ;;        [find-path (-> Station Station [Listof Path])] 
 ;;        [render (-> [Setof Station] String)
 ;;        [station?  (-> String Boolean)]
 ;;        [station   (-> String (U Station [Listof Station])])
 ;; type Path  = [Listof [List Station [Setof Line]]]
 ;; interpretation: take the specified lines to the next station from here 
 ;; type Station = String
 ;; type Line is one of: 
 ;; -- E
 ;; -- D 
 ;; -- C
 ;; -- B 
 ;; -- Mattapan
 ;; -- Braintree
 ;; -- orange
 ;; -- blue 
 ;; as Strings
 
 ;; ->* [instance-of MBTA%]
 ;; read the specification of the T map from file and construct an object that can
 ;; -- convert a string to a (list of) station(s) 
 ;; -- find a path from one station to another
 (contract-out
  [read-t-graph
   (configurable-ctc
    ;; not the strongest contract I can think of here
    ;; maybe you could check if find-path returns all valid paths
    [max (-> (object/c
              (render (->m (set/c string?) string?))
              (station? (->m string? boolean?))
              (station (->m string? (or/c station? (listof station?))))
              (find-path (->m station? station?
                              (listof (listof (list/c station? (set/c line?))))))))]
    #;[max/sub1 (-> (object/c
                     (render (->m (set/c string?) string?))
                     (station? (->m string? boolean?))
                     (station (->m string? (or/c station? (listof station?))))
                     (find-path (->m station? station?
                                     (listof (listof (list/c station? (set/c line?))))))))]
    [types (-> (object/c
                (render (->m (set/c string?) string?))
                (station? (->m string? boolean?))
                (station (->m string? (or/c string? (listof string?))))
                (find-path (->m string? string?
                                (listof (listof (list/c string? (set/c string?))))))))])]
  [mbta%
   (configurable-ctc
    [max
     (class/c
      (render (->m (set/c string?) string?))
      (station? (->m string? boolean?))
      (station (->m string? (or/c station? (listof station?))))
      (find-path (->m station? station?
                      (listof (listof (list/c station? (set/c line?))))))
      (field [G graph?]
             [stations (listof station?)]
             [connection-on (-> station? station? (set/c line?))]
             [bundles (and/c (listof (list/c color? (set/c line?)))
                             (λ (res)
                               (andmap (λ (lst)
                                         (lines-in-color-file?
                                          (set->list(second lst))
                                          (file->lines (format SOURCE-DIRECTORY (first lst)))))
                                       res)))]))]
    #;[max/sub1
       (class/c
        (render (->m (set/c string?) string?))
        (station? (->m string? boolean?))
        (station (->m string? (or/c station? (listof station?))))
        (find-path (->m station? station?
                        (listof (listof (list/c station? (set/c line?))))))
        (field [G graph?]
               [stations (listof station?)]
               [connection-on (-> station? station? (set/c line?))]
               [bundles (listof (list/c color? (set/c line?)))]))]
    [types
     (class/c
      (render (->m (set/c string?) string?))
      (station? (->m string? boolean?))
      (station (->m string? (or/c string? (listof string?))))
      (find-path (->m string? string?
                      (listof (listof (list/c string? (set/c string?))))))
      (field [G graph?]
             [stations (listof station?)]
             [connection-on (-> string? string? (set/c string?))]
             [bundles (listof (list/c string? (set/c string?)))]))])]))

;; ===================================================================================================
(require "../base/my-graph.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "data.rkt"
         "helpers.rkt"
         racket/contract)


(define unweighted-graph/directed*
  unweighted-graph/directed)
(define attach-edge-property*
  attach-edge-property)
(define in-neighbors*
  in-neighbors)




;; type Lines       = [Listof [List String Connections]]
;; type Connections = [Listof Connection]
;; type Connection  = [List Station Station]

(define SOURCE-DIRECTORY
  "../base/~a.dat")

(define COLORS
  '("blue" "orange" "green" "red"))

;; ---------------------------------------------------------------------------------------------------
;; String -> [Maybe [Listof String]]
(define (line-specification? line)
  (define r (regexp-match #px"--* (.*)" line))
  (and r (string-split (second r))))

#| ASSUMPTIONS about source files:
   A data file has the following format: 
   LineSpecification 
   [Station
    | 
    LineSpecification
    ]* 
   A LineSpecification consists of dashes followed by the name of lines, separated by blank spaces. 
   A Station is the string consisting of an entire line, minus surrounding blank spaces. 
|#

;; ---------------------------------------------------------------------------------------------------
(define (read-t-graph)
  (define-values (all-lines bundles)
    (for/fold ((all-lines '()) (all-bundles '())) ((color COLORS))
      (define next (read-t-line-from-file color))
      (values (append next all-lines) (cons (list color (apply set (map first next))) all-bundles))))
  
  (define connections (apply append (map second all-lines)))
  (define stations (set-map (for/fold ((s* (set))) ((c connections)) (set-add s* (first c))) values))
  
  (define graph (unweighted-graph/directed* connections))
  (define-values (connection-on _  connection-on-set!) (attach-edge-property* graph #:init (set)))
  (for ((line (in-list all-lines)))
    (define name (first line))
    (define connections* (second line))
    (for ((c connections*))
      (define from (first c))
      (define to (second c))
      (connection-on-set! from to (set-add (connection-on from to) name))))
  
  (new mbta% [G graph][stations stations][bundles bundles][connection-on connection-on]))

;; ---------------------------------------------------------------------------------------------------
;; String[name of line ~ stem of filename] -> Lines
(define (read-t-line-from-file line-file)
  (define full-path (format SOURCE-DIRECTORY line-file))
  (for/list ([(name line) (in-hash (lines->hash (file->lines full-path)))])
    (list name (rest line))))

;; ---------------------------------------------------------------------------------------------------
;; [Listof String] -> [Hashof String [Cons String [Listof Connections]]]
(define (lines->hash lines0)
  (define names0 (line-specification? (first lines0)))
  (define pred0  (second lines0))
  (define Hlines0 (make-immutable-hash (for/list ([name names0]) (cons name (cons pred0 '())))))
  (let read-t-line ([lines (cddr lines0)][names names0][Hlines Hlines0])
    (cond
      [(empty? lines) Hlines]
      [else 
       (define current-stop (string-trim (first lines)))
       (cond
         [(line-specification? current-stop) 
          => 
          (lambda (names) (read-t-line (rest lines) names Hlines))]
         [else 
          (define new-connections
            (for/fold ([Hlines1 Hlines]) ([name (in-list names)])
              (define line (hash-ref Hlines1 name))
              (define predecessor (first line))
              (define connections 
                (list* (list predecessor current-stop)
                       (list current-stop predecessor)
                       (rest line)))
              (hash-set  Hlines1 name (cons current-stop connections))))
          (read-t-line (rest lines) names new-connections)])])))

;; ---------------------------------------------------------------------------------------------------
(define mbta%
  (class object% 
    (init-field
     ;; Graph 
     G
     ;; [Listof Station]
     stations
     ;; [Station Station -> Line]
     connection-on 
     ;; [Listof [List String [Setof Line]]]
     bundles)
    
    (super-new)
    
    (define/public (render b)
      (define r (memf (lambda (c) (subset? (second c) b)) bundles))
      (if r (first (first r)) (string-join (set-map b values) " ")))

    (define/public (station word)
      (define word# (regexp-quote word))
      (define candidates
        (for/list ([s stations] #:when (regexp-match word# s))
          s))
      (if (and (cons? candidates) (empty? (rest candidates)))
          (first candidates)
          candidates))
    
    (define/public (station? s)
      (cons? (member s stations)))
    
    (define/public (find-path from0 to)
      (define paths* (find-path/aux from0 to))
      (for/list ((path paths*))
        (define start (first path))
        (cond
          [(empty? (rest path)) (list start (set))]
          [else             
           (define next (connection-on start (second path)))
           (define-values (_ result)
             (for/fold ([predecessor start][r (list (list start next))]) ((station (rest path)))
               (values station (cons (list station (connection-on station predecessor)) r))))
           (reverse result)])))
    
    ;; Node Node -> [Listof Path]
    (define/private (find-path/aux from0 to)
      (let search ([from from0][visited '()])
        (cond
          [(equal? from to) (list (list from))]
          [(member from visited) (list)]
          [else
           (define visited* (cons from visited))
           (for/fold ((all-paths '())) ([n (in-neighbors* G from)])
             (define paths-from-from-to-to
               (map (lambda (p) (cons from p)) (search n visited*)))
             (append all-paths paths-from-from-to-to))])))))

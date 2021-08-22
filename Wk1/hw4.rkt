
#lang racket
(require test-engine/racket-tests)

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; int int int -> listofint
;; produce list from low to high with stride interval
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; list of string * string -> list of string
;; purpose = append each element with the suffix 
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append suffix s)) xs))

;; listofint int -> listofint
;; if int is negative/or list is empty raise error, else return
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (list-ref xs (remainder n (length xs)))]))


;; stream int -> list
;; return list of first int values of the stream
(define (stream-for-n-steps stream n)
  (cond [(= n 0) null]
        [#t (cons (car (stream)) (stream-for-n-steps (cdr (stream)) (- n 1)))]))

;; stream
;; stream of nat except multiples of 5 are negated
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 5) 0)
                    (cons (- x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; stream
;; elements of stream alternate between "dan.jpg" and "dog.jpg", starts with dan.jpg
(define dan-then-dog
  (local [(define (dtd acc)
            (if (= (remainder acc 2) 0)
                (cons "dan.jpg" (lambda () (dtd (+ acc 1))))
                (cons "dog.jpg" (lambda () (dtd (+ acc 1))))))]
    (lambda () (dtd 0))))

;; stream -> stream
;; for each element in original stream create new stream with (0, e)
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

;; list'a list'a-> stream
;; produce stream where element is pairs from the list ('a 'a)
(define (cycle-lists xs ys)
  (local [(define (cycle-acc x-acc y-acc)
            (cons (cons (list-nth-mod xs x-acc) (list-nth-mod ys y-acc))
                  (lambda () (cycle-acc (+ x-acc 1) (+ y-acc 1)))))]
    (lambda () (cycle-acc 0 0))))

;; value vector -> pair
;; finds first pair whos car contains value v (if not a pair skip)
(define (vector-assoc v vec)
  (local [(define (vector-assoc-acc acc)
              (cond [(= acc (vector-length vec)) #f]
                    [(pair? (vector-ref vec acc)) (if (equal? (vector-ref (vector-ref vec acc) 0) v)
                                     (vector-ref vec acc)
                                     (vector-assoc-acc (+ acc 1)))]
                    [#t (vector-assoc-acc (+ acc 1))]))]
    (vector-assoc-acc 0)))

;; list int -> (value -> vector pair)
;; returns function that when given v outputs (assoc v xs)
;; uses a memoization
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)] ;list of pairs (arg . (pair))
           [acc 0]
           [f (lambda (v)
                (let ([ans (assoc v xs)]
                      [cached (vector-assoc v cache)])
                  (if cached 
                      cached
                      (if ans
                          (begin
                            (vector-set! cache acc ans)
                            (set! acc (remainder (+ acc 1) n))
                            ans)
                           #f))))])
    f))
                      
              
 
    

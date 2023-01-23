
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
(require rackunit)
;; put your code below

; int int int -> list of int
; assume the last int is a positive
;produce a list of int from low to high inclusive if possiable
;(check-equal? (sequence  0 0 1) (list 0))
;(check-equal?  (sequence 3 11 2) (list 11 9 7 5 3))
;(check-equal?  (sequence 3 8 3) (list  6 3 ))
;(check-equal?  (sequence 3 2 1) empty)
;(check-expect  (sequence 0 5 1) (list 0 1 2 3 4 5) )


(define (sequence low high stride)
  (if (<= low  high)
      (cons low (sequence (+ stride low) high stride))
      empty))
     

(check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test")


;list of string , string => List of string
;. Each element of the output should be the corresponding element of the input appended
;with suffix

(define (string-append-map los s)
  (map (λ(suffix) (string-append suffix s)) los))

(check-equal?  (string-append-map 
                (list "dan" "dog" "curry" "dog2") 
                ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test")

;list of a` , int => error | a`

(define (list-nth-mod  loa n)
  (local [(define i (remainder n (length loa)))
          (define (list-nth-mod  loa acc)
            (cond [(negative? n)  (error "list-nth-mod: negative number")]
                  [(empty? loa)   (error "list-nth-mod: empty list")]
                  [else
                   (if (= acc i)
                       (first loa)
                       (list-nth-mod (rest loa) (add1 acc)))]))]
    (list-nth-mod loa 0)))
                       
(check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test")


;stream int -> list of int
; returns a list holding the first n values produced by s in order
;Assume n is a positive int

(define (stream-for-n-steps s n)
  (if (= n 0)
      empty
      (local [(define a-s (s))]
        (cons (car a-s) (stream-for-n-steps (cdr a-s) (- n 1))))))
  
;Write a stream  of natural numbers except numbers divisble by 5 are negated
(define (stream-maker f arg)
  (λ() (cons arg (stream-maker f (f arg)))))

(define funny-number-stream (stream-maker (λ (x)
                                            (local [(define s-n (+ (abs x) 1))]
                                              (if (= (modulo s-n 5) 0)
                                                  (- s-n)
                                                  s-n))) 1))
                                                  

;Write a stream  of strings alternating between  "dan.jpg" and "dog.jpg" where "dan.jpg" is the start

(define dan-then-dog (stream-maker (λ (s) (if (string=? s "dan.jpg")
                                              "dog.jpg"
                                              "dan.jpg")) "dan.jpg"))
                                              
  
;Stream => stream
;convert the value of the stream to be (0. v) stream

(define (stream-add-zero s)
  (local [(define new-stream (λ () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))]
    new-stream))



;listOf x  ListOf y => stream
;Assume the lists are not empty
;The elements produced by the stream are pairs where the first part is from xs and the second part is from ys

(define (cycle-lists xs ys)
  (local [(define list1 xs)
          (define list2 ys)
          (define (f list1 list2) (λ () (cond [(empty? list1) ((f xs list2))]
                                              [(empty? list2) ((f list1 ys))]
                                              [else
                                               (cons (cons (first list1) (first list2)) (f (rest list1) (rest list2)))])))]
    (f list1 list2)))



;Value Vector => Boolean | pair
; return the first pair which it is car = value else return false
;Assume that the vector 



(define (vector-assoc v vr)
  (local [(define (vector-assoc vr acc)
            (cond [(= (vector-length vr) acc) false]
                  [else
                   (local [(define p (vector-ref vr acc))]
                     (if (pair? p)
                         (if (equal? (car p) v)
                             p
                             (vector-assoc vr (+ acc 1)))
                         (vector-assoc vr (+ acc 1))))]))]
    
    (vector-assoc vr 0)))
                       
         
;ListOfX Int =>  ((f v) (assoc v xs))
(define (cached-assoc xs n)
  (local [(define pos 0)
          (define vr (make-vector n))
          (define returned (λ (v)
                             (local [(define ans (vector-assoc v vr))]
                               (if  ans
                                    ans
                                    (local [(define new-ans   (assoc v xs))]
                                      (if new-ans
                                          (begin (vector-set! vr pos new-ans) (if (=  pos  (- n 1)) (set! pos 0) (set! pos (+ pos 1))) new-ans)
                                          new-ans))))))]
    returned))



;Macro
;Assume e1 and e2 are numbers
;Assume e2 terminate 
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (local [(define first e1)]
       (local [(define (loop x)
                 (if (>= x  first)
                     #t
                     (begin x (loop e2))))]
         (loop e2)))]))

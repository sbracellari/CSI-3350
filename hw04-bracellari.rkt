#lang racket
(#%provide (all-defined))

#|
IMPORTANT:
Overall, you are allowed to change this file in any way that does *not* affect the
compilation of the file. If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:

   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the name of the function and the number of arguments of
     the pre-defined functions, because changing the number of arguments automatically changes
     the semantics of the function. Changing the name of the arguments is permitted since that
     change only affects the readability of the function, not the semantics.
   - you may write any number of helper functions

If you cannot come up with a correct solution then please make the answer-sheet
compile. If you have partial solutions that do not compile please comment them out,
if this is the case, the default definitions will have to be present since the tests
will be expecting functions with the names defined here.

Submission guidelines:
   - please rename the file to hw04-yourlastname-answer.rkt prior to submission
   - only the renamed file file needs to be uploaded
|#

;======================================01=======================================
#|
<step> ::=  <step>  <step>       "seq-step"
          | "up" number          "up-step"
          | "down" number        "down-step"
          | "left" number        "left-step"
          | "right" number       "right-step"
|#
;example of how to create the error message for the "up-step" constructor
;> (invalid-args-msg "up-step" "number?" '(1 2 3 4))
;where '(1 2 3 4) should be replaced by the actual violating value.

;;you can reorder the functions below if it better suits your needs
(define (up-step n)
  (if (number? n) 
    (list 'up-step n) 
    (error 'invalid-args-msg "up-step; number?" n)))


(define (down-step n)
  (if (number? n) 
    (list 'down-step n) 
    (error 'invalid-args-msg "down-step; number?" n)))


(define (left-step n)
  (if (number? n) 
    (list 'left-step n) 
    (error 'invalid-args-msg "left-step; number? " n)))


(define (right-step n)
  (if (number? n) 
    (list 'right-step n) 
    (error 'invalid-args-msg "right-step; number? " n)))


(define (seq-step st-1 st-2)
  (if (and (step? st-1) (step? st-2))
    (list 'seq-step st-1 st-2)
    (error 'invalid-args-msg "invalid step")))


;;====
(define (up-step? st)
  (and 
    (list? st)
    (equal? (first st) 'up-step)
    (number? (second st))))


(define (down-step? st)
  (and 
    (list? st)
    (equal? (first st) 'down-step)
    (number? (second st))))


(define (left-step? st)
  (and 
    (list? st) 
    (equal? (first st) 'left-step)
    (number? (second st)))) 


(define (right-step? st)
  (and 
    (list? st) 
    (equal? (first st) 'right-step)
    (number? (second st))))


(define (seq-step? st)
  (and 
    (list? st) 
    (equal? (first st) 'seq-step)
    (step? (second st))
    (step? (third st))))


;This is a predicate that tells you whether or not something is a step,
;it should return true when given either up, down, left, right or seq steps.
(define (step? st)
  (or (up-step? st) (left-step? st) (down-step? st) (right-step? st) (seq-step? st)))

;; to avoid needless duplication we will only implement one extractor to handle all the
;; simple steps, rather than four of them. 
;; So this should take: up, down, left and right steps.
(define (single-step->n st)
  (if (or (up-step? st) (down-step? st) (left-step? st) (right-step? st))
    (second st)
    (error 'invalid-args-msg "not a valid step")))


;;two extractors, one for each piece of data representing a sequential step
(define (seq-step->st-1 st)
  (if (seq-step? st)
    (second st) 
    (error 'invalid-args-msg "not a valid step")))


(define (seq-step->st-2 st)
  (if (seq-step? st)
    (third st)
    (error 'invalid-args-msg "not a valid step")))


;start-p is a pair containing coordinates
;;===================================
(define (value-of step start-p)
  (cond
    [(up-step? step) (list (first start-p) (+ (second start-p) (single-step->n step)))]
    [(down-step? step) (list (first start-p) (- (second start-p) (single-step->n step)))]
    [(right-step? step) (list (+ (first start-p) (single-step->n step)) (second start-p))]
    [(left-step? step) (list (- (first start-p) (single-step->n step)) (second start-p))]
    [(seq-step? step) (map + (value-of (seq-step->st-1 step) start-p) (value-of (seq-step->st-2 step) start-p))]
    [else (error 'invalid-args-msg "invalid step")]
  ))

; (display (value-of (seq-step (up-step 50) (seq-step (left-step 20) (right-step 40))) '(0 0)))

;======================================02=======================================

;we assume that the sets can contain only numbers between 0 and bound
(define bound 100)


;singleton-set take a number as input and returns a function that takes a number as an input and
;tells whether or not that number is in the set. This function creates a set with just one number x
(define (singleton-set x)
  (if (number? x)
    (lambda (n)
      (cond
        [(equal? n 'singleton-set) #t]
        [(equal? n 'x) x]
        [(and (> n 0) (< n bound) (= n x))]
        [else #f]))
    (error 'invalid-args-msg "singleton-set; number?" x)))


(define singleton1 (singleton-set 1))
(define singleton2 (singleton-set 2))
; (display (singleton1 0))(display " ")
; (display (singleton1 1))(display " ")
; (display (singleton2 2))(display " ")
; (display (singleton2 1))(display " ")
; (newline)

;the set of all elements that are in either 's1' or 's2'
(define (union s1 s2)
  (if (and (procedure? s1) (procedure? s2))
    (lambda (n)
      (cond
        [(equal? n 'union) #t]
        [(equal? n 's1) s1]
        [(equal? n 's2) s2]
        [(or (s1 n) (s2 n))]
        [else #f]))
    (error 'invalid-args-msg "union; procedure?" s1 s2)))

(define u12 (union singleton1 singleton2))
; (display (u12 1))(display " ")
; (display (u12 2))(display " ")
; (display (u12 3))(display " ")
; (newline)

;the set of all elements that are in both  in 's1' and 's2'
(define (intersection s1 s2)
  (if (and (procedure? s1) (procedure? s2))
    (lambda (n)
      (cond
        [(equal? n 'intersection) #t]
        [(equal? n 's1) s1]
        [(equal? n 's2) s2]
        [(and (s1 n) (s2 n))]
        [else #f]))
    (error 'invalid-args-msg "intersection; procedure?" s1 s2)))

(define i1 (intersection u12 singleton1))
; (display (i1 1))(display " ")
; (display (i1 2))(display " ")
; (display (i1 3))(display " ")
; (newline)

;the set of all elements that are in 's1', but that are not in 's2'
(define (diff s1 s2)
  (if (and (procedure? s1) (procedure? s2))
    (lambda (n)
      (cond
        [(equal? n 'diff) #t]
        [(equal? n 's1) s1]
        [(equal? n 's2) s2]
        [(and (s1 n) (not (s2 n)))]
        [else #f]))
    (error 'invalid-args-msg "diff; procedure?" s1 s2)))

(define d2 (diff u12 singleton1))
; (display (d2 1))(display " ")
; (display (d2 2))(display " ")
; (display (d2 3))(display " ")
; (newline)


;returns the subset of s, for which the predicate 'predicate' is true.
(define (filter predicate s)
  (if (procedure? s)
    (lambda (n)
      (cond
        [(equal? n 'filter) #t]
        [(equal? n 'predicate) predicate]
        [(equal? n 's) s]
        [(predicate n)]
        [else #f]))
    (error 'invalid-args-msg "filter; procedure?" s)))

(define u123 (union u12 (singleton-set 3)))
(define g2 (filter (lambda (x)(>= x 2)) u123))
; (display (g2 1))(display " g2 ")
; (display (g2 2))(display " ")
; (display (g2 3))(display " ")
; (newline)


;returns whether or not the predicate is true for all the elements
;of the given set s
(define (all? predicate s)
  (if (procedure? s)
    (andmap (lambda (n) (if (not (s n)) #t (predicate n)))  (range 1 100))
    (error 'invalid-args-msg "all?" predicate s)))

; (display (all? (lambda (x)(>= x 2)) u123))(newline)


;returns whether or not the set contains at least an element for which
;the predicate is true. s below is the parameter standing for a given set
(define (exists? predicate s)
  (if (procedure? s)
    (ormap (lambda (n) (and (predicate n) (s n))) (range 1 100))
    (error 'invalid-args-msg "exists?" predicate s)))

; (display (exists? (lambda (x)(>= x 2)) u123))(newline)

;returns a new set where "op" has been applied to all elements
; NOTE: just because a procedure/function has the word "map" in it, it 
;       doesn't mean you have to use map higher order function to implement it. 
;       Map is a functional operation with well defined behavior that 
;       is not tied to any implementation.
(define (map-set op s)
  (if (procedure? s)
    (lambda (n)
      (ormap (lambda (i) (and (s i) (= (op i) n))) (range 1 100)))
    (error 'invalid-args-msg "map-set" op s)))

(define ms149 (map-set (lambda (x) (* x x)) u123))
; (display (ms149 1))(display " ms149 ")
; (display (ms149 2))(display " ")
; (display (ms149 3))(display " ")
; (display (ms149 4))(display " ")
; (display (ms149 5))(display " ")
; (display (ms149 9))(display " ")
;(newline)


;=====================================03====================================
; FYI:
;  to emphasize the procedural-based approach to implement "step" data type and to
;  contrast it with the data structure-based approach for "step" implementation 
;  used in 01, here we add "-proc" suffix to each corresponding function name.

;====p3-a================
(define (up-step-proc n)
  (if (number? n)
    (lambda (x)
      (cond
        [(equal? x 'up-step-proc) #t]
        [(equal? x 'n) n]
        [else #f]))
    (error 'invalid-args-msg "up-step-proc; number?" n)))


(define (down-step-proc n)
  (if (number? n)
    (lambda (x)
      (cond
        [(equal? x 'down-step-proc) #t]
        [(equal? x 'n) n]
        [else #f]))
    (error 'invalid-args-msg "down-step-proc; number?" n)))


(define (left-step-proc n)
  (if (number? n)
    (lambda (x)
      (cond
        [(equal? x 'left-step-proc) #t]
        [(equal? x 'n) n]
        [else #f]))
    (error 'invalid-args-msg "left-step-proc; number?" n)))


(define (right-step-proc n)
  (if (number? n)
    (lambda (x)
      (cond
        [(equal? x 'right-step-proc) #t]
        [(equal? x 'n) n]
        [else #f]))
    (error 'invalid-args-msg "right-step-proc; number?" n)))


(define (seq-step-proc st-1 st-2)
  (if (and (step-proc? st-1) (step-proc? st-2))
    (lambda (x)
      (cond
        [(equal? x 'seq-step-proc) #t]
        [(equal? x 'st-1) st-1]
        [(equal? x 'st-2) st-2]
        [else #f]))
    (error 'invalid-args-msg "invalid step")))


;;====
(define (up-step-proc? st)
  (if (procedure? st)
    (st 'up-step-proc)
    (error 'error "up-step-proc?")))  

(define (down-step-proc? st)
  (if (procedure? st)
    (st 'down-step-proc)
    (error 'error "down-step-proc?")))  


(define (left-step-proc? st)
  (if (procedure? st)
    (st 'left-step-proc)
    (error 'error "left-step-proc?"))) 


(define (right-step-proc? st)
  (if (procedure? st)
    (st 'right-step-proc)
    (error 'error "right-step-proc?"))) 


(define (seq-step-proc? st)
  (if (procedure? st)
    (st 'seq-step-proc)
    (error 'error "seq-step-proc?")))  

;This is a predicate that tells you whether or not st is a step,
; it should return true when given either up, down, left, right or seq steps.
(define (step-proc? st)
  (or (up-step-proc? st) (down-step-proc? st) (left-step-proc? st) (right-step-proc? st) (seq-step-proc? st)))


;;to avoid needless duplication we will only implement one extractor to handle all the
;; simple steps, rather than four of them. So this should take: up, down, left and right 
;; steps. 
(define (single-step-proc->n st)
  (st 'n))


;;two extractors
(define (seq-step-proc->st-1 st)
  (st 'st-1))


(define (seq-step-proc->st-2 st)
  (st 'st-2))


;;========p3-b
(define (value-of-proc step-proc start-p)
  (cond
    [(up-step-proc? step-proc) (list (first start-p) (+ (second start-p) (single-step-proc->n step-proc)))]
    [(down-step-proc? step-proc) (list (first start-p) (- (second start-p) (single-step-proc->n step-proc)))]
    [(right-step-proc? step-proc) (list (+ (first start-p) (single-step-proc->n step-proc)) (second start-p))]
    [(left-step-proc? step-proc) (list (- (first start-p) (single-step-proc->n step-proc)) (second start-p))]
    [(seq-step-proc? step-proc) (map + (value-of-proc (seq-step-proc->st-1 step-proc) start-p) (value-of-proc (seq-step-proc->st-2 step-proc) start-p))]
    [else (error 'invalid-args-msg "invalid step-proc")]
  ))

(display (value-of-proc (seq-step-proc (up-step-proc 50) (seq-step-proc (left-step-proc 20) (right-step-proc 40))) '(0 0)))
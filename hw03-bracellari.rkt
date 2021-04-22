#lang racket
(#%provide (all-defined))

#|
If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:

   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the name and the number of arguments of the pre-defined functions,
     because changing the number of arguments automatically changes the semantics of the 
     function. Changing the name of the arguments is permitted since that change only
     affects the readability of the function, not the semantics.
   - you may write any number of helper functions as you want.
|#
;======================================01=======================================
(define (foldl-local op default-el lst)
  (cond
    [(empty? lst) default-el]
    [else (foldl-local op (op (first lst) default-el) (rest lst))]
  )
) 

;---

(define (foldr-local op default-el lst)
  (cond
    [(empty? lst) default-el]
    [else (op (first lst) (foldr-local op default-el (rest lst)))]
  )
)

;======================================02=======================================
; an implementation without foldl/foldr for fun
(define (andmap-local-v2 test-op lst)
  (cond
    [(empty? lst) #t]
    [else (and (test-op (first lst)) (andmap-local-v2 test-op (rest lst)))]
  )
)

; actual implementation for homework
(define (andmap-local test-op lst)
  (=
    (length (filter-local identity (foldr-local (lambda (e x) (cons (test-op e) x)) '() lst))) 
    (length lst))
)

;======================================03=======================================
(define (filter-local test-op lst)
  (cond
    [(empty? lst) empty]
    [(test-op (first lst)) (cons (first lst) (filter-local test-op (rest lst)))]
    [else (filter-local test-op (rest lst))]
  )
)

;======================================04=======================================
(define (map-reduce m-op r-op default-el lst)
  (foldr-local r-op default-el (map m-op lst))
)

(define add-forty-two (lambda (x) (+ x 42)))

;======================================05=======================================
(define (series n)
  (define lst (build-list (+ n 1) values))
  (foldr-local + 0.0 (map (lambda (x) (+ (/ (expt -1.0 x) (fact (+ x 1))))) lst))
)

;helper function
(define (fact n)
  (if (= n 1) n (* n (fact (- n 1))))
)

;======================================06=======================================
(define (zip lst1 lst2)
  (map list lst1 lst2)
)

;======================================07=======================================
(define (matrix-to-vector op mat)
  (cond 
    [(= (length (first mat)) 0) empty]
    [else (cons (list-op op (first (apply map list mat))) (matrix-to-vector op (map rest mat)))]
  )
)

(define (list-op op lst)
  (cond
    [(= (length lst) 1) (first lst)]
    [else (op (first lst) (list-op op (rest lst)))]
  )
)

(define example-matrix '((1 2 3 4)(5 6 7 8)(9 0 1 2)))
(define string-matrix '(("a" "c" "e")("b" "d" "f")))
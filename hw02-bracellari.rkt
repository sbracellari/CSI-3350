#lang racket
(#%provide (all-defined))
#|
If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:
   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the names of any definition
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     but you can change the names of the arguments if you deem it necessary.
   - make sure that you submit an answer sheet that compiles! If you cannot write
     a correct solution at least make it compile, if you cannot make it compile then
     comment it out. In the latter case, make sure that the default definitions
     for the problem are still present. 
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. 
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!
|#
;======================================01=======================================
(define (list-of-even-numbers? lst)
  (if (= (length lst) (length (filter even? (filter number? lst)))) #t #f)
)

;======================================02=======================================
;;for n > 0
;Sn = 1/1 + 1/4 + 1/9 + 1/16 + ...
(define (series-a n)
  (if (= 1.0 n) n (+ (/ 1.0 (* n n)) (series-a (- n 1.0))))
)

;====
;;for n >= 0
;Sn = 1 - 1/2 + 1/6 - 1/24 + ...

;helper function
(define (fact n)
  (if (= n 1) n (* n (fact (- n 1))))
)

(define (series-b n)
  (if (= 0 n) 1.0 (+ (/ (expt -1.0 n) (fact (+ n 1))) (series-b (- n 1.0))))
)

;======================================03=======================================
;helper function
(define (append-symbol symbol lst)
  (for/list ([n (in-range 0 (length lst))]) 
    (append (list symbol) (list-ref lst n) (list symbol)))
)

;helper function
(define (all-symbol symbol n)
  (build-list n (const symbol))
)

(define (carpet n)
  (if (= n 0) '((%))
    (let ((x (if (even? n) '% '+)))
    (append (list(all-symbol x (+ 1 (* 2 n))))
    (append-symbol x (carpet (- n 1)))
    (list (all-symbol x (+ 1 (* 2 n)))))
  ))
)

;======================================04=======================================
;helper function
(define (pascal-helper a b) 
  (if (or (= a b) (zero? b)) 1
    (+ (pascal-helper (- a 1) b)
    (pascal-helper (- a 1) (- b 1))))
)

(define (pascal n)
  (for/list ([a (in-range 0 n)])
    (for/list ([b (in-range 0 (+ a 1))])
      (pascal-helper a b)))
)
;======================================05=======================================
(define (balanced? in)
  (define lst (string->list in))

  (cond
    [(or (empty? lst) (and (not (string-contains? in "(")) (not (string-contains? in ")")))) #t]
    [(or 
      (and (string-contains? in "(") (not (string-contains? in ")")))
      (and (string-contains? in ")") (not (string-contains? in "(")))  
    ) #f]
    [(or 
      (equal? (list-ref lst 0) #\u0029) 
      (equal? (list-ref lst (- (length lst) 1)) #\u0028))
    #f]
    [(balanced? (substring in (+ 1 (index-of lst #\u0028)) (last (indexes-of lst #\u0029))))]
  )
)

;======================================06=======================================
(define (list-of-all? predicate lst)
  (if (= (length lst) (length (filter predicate lst))) #t #f)  
)

;======================================07=======================================
(define (create-mapping keys vals)
  (lambda (key) (find-val key keys vals))
)

(define (find-val key keys vals)
  (if (equal? (index-of keys key) #f) 
    (raise (string-append "Could not find mapping for symbol '" (symbol->string key))) 
  (list-ref vals (index-of keys key)))
)

(define roman-numerals-keys '(I II III IV V))
(define characters-vals '(a b c d e))

(define roman-to-character (create-mapping roman-numerals-keys characters-vals))
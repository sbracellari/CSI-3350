#lang racket
(#%provide (all-defined))

#|
If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:
   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the names of any definition
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     but you can change the names of the arguments if you deem it necessary.
   - make sure that you submit an asnwer sheet that compiles! If you cannot write
     a correct solution at least make it compile, if you cannot make it compile then
     comment it out. In the latter case, make sure that the default definitions
     for the problem are still present. Otherwise you may be penalized up to 25%
     of the total points for the homework.
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. 
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!

|#
;======================================01=======================================
;((3 + 3) * 9)
;equal to 54
(define (p1-1)
  (* (+ 3 3) 9)
)

;((6 * 9) / ((4 + 2) + (4 * 3)))
;equal to 3
(define (p1-2)
  (/ (* 6 9) (+ (+ 4 2) (* 4 3)))
)

;(2* ((20 - (91 / 7)) * (45 - 42)))
;equal to 42
(define (p1-3)
  (* 2 (* (- 20 (/ 91 7)) (- 45 42)))
)

;======================================02=======================================
;write your answer as a string; you do not need to write any special escape
;characters to distinguish new lines.
(define p2
  "Find the least nested operator. This will begin your scheme expression. 
  Leave the operands trailing the operator. If there is more than one operator 
  at the same level, you will take them in reverse PEMDAS order. If there are 
  multiple operators of the same type, remove all but one of them."
)

;======================================03=======================================
;;Write the definitions of x,y,z here:

(define x 2)
(define y 3)
(define z 4)

;======================================04=======================================
;you will need to have solved problem 3. The values x,y,z are not parameters
;of this function!
(define (p4)
  (if (= x y z) 0
    (if (>= x y )
      (if (>= y z)
        (+ x y)
        (+ x z))
      (if (>= x z)
        (+ x y)
        (+ y z))))
)

;======================================05=======================================
(define (p5)
    (if (= x y z) 0
    (if (<= x y )
      (if (<= y z)
        (+ x y)
        (+ x z))
      (if (<= x z)
        (+ x y)
        (+ y z))))
)

;======================================06=======================================
(define (p6)
  (equal? x y)
)

;======================================07=======================================
;same instructions as problem 02.
(define p7
  "The first one, (define thirty-five 35), is a variable declaration, i.e. it
  declares a variable named thirty-five with a value of 35. The second one,
  (define (thirty-five) 35), is a function declaration. It will declare a function
  that is called thirty-five that returns a value of 35."
)

;======================================08=======================================
;same instructions as problem 02.
(define p8
  "The quote (') will disable the evaluation of any arguments following it."
)

;======================================09=======================================
;same instructions as problem 02.
(define p9
  "The difference between list and quote is that list is a function. List will
  evaluate all arguments that follow it, whereas quote will not."
)

;======================================10=======================================
;same instructions as problem 02.
(define p10
  "Much more can be done with a string in Scheme than with a symbol. For example,
  length checking, lexicographic comparisons, substring manipulation, appending,
  list conversion and iteration. The main distinction between symbols and strings
  has do to with how they behave in memory. If two symbols are the same, they are
  the same in memory, too, whereas two strings can have the same contents but live
  different chunks of memory."
)

;======================================11=======================================
;(4 2 6 9)
(define (p11-1)
  (list 4 2 6 9)
)

;(spaceship
;  (name(serenity))
;  (class(firefly)))
(define (p11-2)
  (list 'spaceship (list 'name (list 'serenity)) (list 'class (list 'firefly)))
)

;(2 * ((20 - (91 / 7)) * (45 - 42)))
(define (p11-3)
  (list 2 '* (list (list 20 '- (list 91 '/ 7)) '* (list 45 '- 42)))
)

;======================================12=======================================
(define example '(a b c))

;(d a b c)
(define (p12-1 lst)
  (reverse (append (reverse lst) (list 'd)))
)

;(a b d a b)
(define (p12-2 lst)
  (append (list-set lst 2 'd) (take lst 2))
)

;(b c d a)
(define (p12-3 lst)
  (list-tail (append lst (list 'd 'a)) 1)
)

;======================================13=======================================
(define p13
  "eq? is used to check object equality in memory, whereas equal? is used to check
  equality at face value."
)

;======================================14=======================================
(define (create-error-msg sym val)
  (string-append 
    "This is a custom error message. Symbol "
    (symbol->string sym) 
    " was not paired with value "
    (number->string val))
)

;======================================15=======================================
(define (check-correctness pair)
  (cond
    [(and 
      (equal? (first pair) 'answer-to-everything) 
      (= (second pair) 42)) 
      #t]
    [(and 
      (not (equal? (first pair) 'answer-to-everything)) 
      (= (second pair) 42)) 
      #f]
    [(and 
      (not (equal? (first pair) 'answer-to-everything)) 
      (not (= (second pair) 42))) 
      #f]
    [(and 
      (equal? (first pair) 'answer-to-everything) 
      (not (= (second pair) 42))) 
      (raise (create-error-msg 'answer-to-everything 42))]
  )
)
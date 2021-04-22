#lang racket
(#%provide (all-defined))
(#%require (lib "eopl.ss" "eopl"))

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
   - please rename the file to hw05-yourlastname-answer.rkt prior to submission
   - only the renamed file file needs to be uploaded
|#

;=======================================01======================================
(define (invalid-args-msg fun-name-as-string
                          expected-value-type-as-predicate-string
                          received)
  (string-append "Invalid arguments in: " fun-name-as-string " --- "
                 "expected: " expected-value-type-as-predicate-string " --- "
                 "received: " (~a received)
                 )
)

;You can compare the contents of this answer sheet with the answer sheet of the
;previous homework to infer what is generated automatically by define-datatype.
(define-datatype step step?
  (up-step (n number?))
  (down-step (n number?))
  (left-step (n number?))
  (right-step (n number?))
  (seq-step (st-1 step?) (st-2 step?)))

;;===================================

;Check the interpreter in HW4 solution if you are having difficulties
(define (value-of stepin start-p)
  (cases step stepin
    (up-step (n) (list (first start-p) (+ (second start-p) n)))
    (down-step (n) (list (first start-p) (- (second start-p) n)))
    (left-step (n) (list (- (first start-p) n) (second start-p)))
    (right-step (n) (list (+ (first start-p) n) (second start-p)))
    (seq-step (st-1 st-2) (map + (value-of st-1 start-p) (value-of st-2 start-p)))
    (else (invalid-args-msg "value-of" "step?" stepin))))

; (display (value-of  (left-step 10)  '(0 0)))(newline)
; (display (value-of  (right-step 10)  '(0 0)))(newline)
; (display (value-of  (up-step 10)  '(0 0)))(newline)
; (display (value-of  (down-step 10)  '(0 0)))(newline)
; (display (value-of (seq-step (left-step 10) (right-step 20)  ) '(0 0)))(newline)
; (display (value-of  (seq-step (up-step 50)(seq-step (left-step 20) (right-step 40))) '(0 0)))(newline)
; (display (value-of  '(123) '(0 0)))

;=======================================02======================================
;2.a
(define (exception-no-binding-msg sym)
  (string-append "No binding for '" (~a sym)))

;
(define-datatype environment environment?
  (empty-env)
  (extend-env (symbol symbol?) (value number?) (env environment?))
  (extend-env-final (symbol symbol?) (value number?) (env environment?)))

; ;2.b
; ;Helper function to check if sym is of final variant in env or not 
(define (is-var-final? sym env)
  (cases environment env
    (empty-env () #f)
    (extend-env (s v e)
      (is-var-final? sym e))
    (extend-env-final (s v e)
      (if (eqv? sym s) 
        #t 
        (is-var-final? sym e)))))

; ;in the wrapper we first check to see if a variable is final or not,
; ;before adding it using one of the two constructors. This constructor
; ;wrapper will be used to create the AST while generating the correct errors.
(define (extend-env-constructor-wrapper sym val env final?)
  (if (is-var-final? sym env)
    (raise (string-append "cannot shadow symbol '" (~a sym) " because it is final"))
    (if final?
      (extend-env-final sym val env)
      (extend-env sym val env))))

; ;2.c
(define (apply-env env search-sym)
  (cases environment env
    (empty-env () (raise (exception-no-binding-msg search-sym)))
    (extend-env
      (saved-sym saved-val saved-env)
        (if (eqv? search-sym saved-sym)
          saved-val
          (apply-env saved-env search-sym)))
    (extend-env-final
      (saved-sym saved-val saved-env)
        (if (eqv? search-sym saved-sym)
          saved-val
          (apply-env saved-env search-sym)))))

(define s (empty-env))

(define s-env (extend-env-constructor-wrapper 'x 99
 (extend-env-constructor-wrapper 'z 100
   (extend-env-constructor-wrapper 'y 3
     (extend-env-constructor-wrapper  'x 2
       (empty-env) #f) #f) #f) #f))

(define s-env1 (extend-env-constructor-wrapper 'x 99
  (extend-env-constructor-wrapper 'z 100
    (extend-env-constructor-wrapper 'y 3
      (extend-env-constructor-wrapper  'x 2
        (empty-env)
          #t) #f) #f) #f))

(display (apply-env s 'x))(newline)
(display (apply-env s-env 'c))(newline)
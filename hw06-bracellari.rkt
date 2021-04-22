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
   - please rename the file to hw06-yourlastname-answer.rkt prior to submission
   - only the renamed file file needs to be uploaded
|#

(define (exception-no-binding-msg sym)
  (string-append "No binding for '" (~a sym))
  )

(define-datatype environment environment?
  (empty-env )
  (extend-env (symbol symbol?)(number number?) (env environment?)))


(define (apply-env env search-sym)
  (cases environment env
    (empty-env () (exception-no-binding-msg search-sym))
    (extend-env (x y z)
                (if (equal? search-sym x) y
                    (apply-env z search-sym)))))


(define lexical-spec
  '(
    (whitespace (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    )
  )


(define grammar-spec
  '(
    (program ("point" "main" "begin" expr "end") a-program)
    (expr ("(" expr "," expr ")") point-expr)
    (expr (number) num-expr)
    (expr ("origin?" "(" expr ")") origin?-expr)
    (expr ("if" "(" expr ")" "{" expr "}" "else" "{" expr "}") if-expr)
    (expr ("move" "(" expr dir-expr (arbno dir-expr) ")") move-expr)
    (expr (identifier) iden-expr)
    (expr ("{" (arbno var-expr) expr "}") block-expr)
    (var-expr ("val" identifier "=" expr) val)
    (dir-expr ("up" "(" number ")") up-expr)
    (dir-expr ("down" "(" number ")") down-expr)
    (dir-expr ("left" "(" number ")") left-expr)
    (dir-expr ("right" "(" number ")") right-expr)
  )
)

;===============================================================================
;============================= sllgen boilerplate ==============================
;===============================================================================
;this will create the AST datatype with define-datatype
;according to the lexical and grammar specifications.
(sllgen:make-define-datatypes lexical-spec grammar-spec)

;you can use this function to display the define-datatype
;expression used to generate the AST. Take some time to read it.
;you should be able to understand it by now.
(define (show-data-types)
  (sllgen:list-define-datatypes lexical-spec grammar-spec))

;parser is a one argument function that takes a string,
;scans & parses it and generates a resulting abstract
;syntax tree (ast). 
(define parser
  (sllgen:make-string-parser lexical-spec grammar-spec))

;you can use this function to find out more about how
;the string is broken up into tokens during parsing,
;this step is automatically included in the parser
;function. This is a one-argument function that takes a 
;string.
(define scanner
  (sllgen:make-string-scanner lexical-spec grammar-spec))


;Try this
; (show-data-types)

;===============================================================================
;================================ Value-of =====================================
;===============================================================================
;value-of takes as a parameter an AST resulted from a call to the
;parser function.

(define (run program-string)
  (value-of-program (parser program-string)))


;Cases for program
(define (value-of-program ast)
  (cases program ast
    (a-program (expr) (value-of-expr expr (empty-env)))))


;Cases for expression
(define (value-of-expr ast env)
  (cases expr ast
    (point-expr (expr1 expr2) (list (value-of-expr expr1 env)  (value-of-expr expr2 env)))
    (num-expr (number) number)
    (origin?-expr (expr) (if (equal? (value-of-expr expr env) '(0 0)) #t #f))
    (if-expr (expr1 expr2 expr3) (if (value-of-expr expr1 env) (value-of-expr expr2 env) (value-of-expr expr3 env)))
    (move-expr (expr dir-expr1 dir-expr2) 
      (apply map + (value-of-expr expr env)
        (map (lambda (x)
          (value-of-dir (value-of-expr expr env) x)) (append (list dir-expr1) dir-expr2))))
    (iden-expr (identifier) (apply-env env identifier))
    (block-expr (var-expr expr) 
      (define lst (map (lambda (x) (value-of-var x env)) var-expr))
      (value-of-expr expr (add-variables lst env)))))
       

;Cases for directional expressions
(define (value-of-dir point astDir)
  (cases dir-expr astDir
    (up-expr (num-expr) (list 0 num-expr))
    (down-expr (num-expr) (list 0 (- 0 num-expr)))
    (left-expr (num-expr) (list (- 0 num-expr) 0))
    (right-expr (num-expr) (list num-expr 0))))

;Cases for variable expressions
(define (value-of-var varAst env)
  (cases var-expr varAst
    (val (iden-expr num-expr) (list iden-expr (value-of-expr num-expr env)))))


;Moves a point following the commands in direction list (dirList)
(define (move point dirList)
  'UNIMPLEMENTED)

;Adds variable list (varList) to the environment
(define (add-variables varList env)
  (extend-env (first (first varList)) (second (first varList)) 
    (if (= 1 (length varList)) env (add-variables (rest varList) env))))

;Some test cases, read the code and try to figure out what the output is going to be
(newline)

; ; (50, 50)
; (run "point main begin (50,50) end")
; (newline)

; ; (50, 20)
; (run "point main begin { val a = 50 (a,20)} end")
; (newline)

; ; (50, 30)
; (run "point main begin { val a = 50 val b = 30 (a,b)} end")
; (newline)

; ; (40, 130)
; (run "point main begin { val a = 50 val b = 30 move ((a,b) left (10) up (100))} end")
; (newline)

; ; (139, 60)
; (run "point main begin { val a = 50 val b = 30 val c = 100
;                          move ((a,b) left (10) up (50) down (20) right (99))} end")
; (newline)

; ; (90, 90)
; (run "point main begin { val a = 50 val b = 30 val c = 100
;                          if (origin? ((0,0))) {(90,90)}
;                          else { move ((a,b) left (10) up (50) down (20) right (99))}} end")
; (newline)

; ; (139, 60)
; (run "point main begin { val a = 50 val b = 30 val c = 100
;                          if (origin? ((0,c))) {(90,90)}
;                          else { move ((a,b) left (10) up (50) down (20) right (99))}} end")
; (newline)

; ; (50, 20)
; (run "point main begin { val a = 50 val b = 30 val c = 100
;                          if (origin? ((0,0)))
;                             { {val d = 20 (a,d)}}
;                             else{move ((a,b) left (10) up (50) down (20) right (99))}} end")
; (newline)

; ; (101, 20)
; (run "point main begin { val a = 50 val b = 30 val c = 100
;                          if (origin? ((0,0)))
;                             { {val d = 20 val c = 101 (c,d)}}
;                          else{move ((a,b) left (10) up (50) down (20) right (99))}} end")
; (newline)

; ; (50, 50)
; (run "point main begin { val a = 50 val b = 30 val c = 100
;                          if (origin? ((0,0)))
;                             { {val d = 20 val c = 101
;                                if (origin? ((0,0))){(50,50)}else{(99,99)}}}
;                             else{move ((a,c) left (10) up (50) down (20) right (99))}} end")
; (newline)

; ; (99, 99)
; (run "point main begin { val a = 50 val b = 30 val c = 100
;                          if (origin? ((0,0)))
;                             { {val d = 20 val c = 101
;                                if (origin? ((0,c))){(50,50)}else{(99,99)}}}
;                             else{move ((a,c) left (10) up (50) down (20) right (99))}} end")
; (newline)

; ; (50, 131)
; (run "point main begin { val a = 50 val b = 30 val c = 100
;                          if (origin? ((0,0)))
;                             { {val d = 20 val c = 101
;                                if (origin? ((0,c))){(50,50)}else{move ((a,c) left (-1) up (50) down (20) right (99))}}}
;                             else{move ((a,c) left (10) up (50) down (20) right (99))}} end")
; (newline)

; ; (139, 130)
(run "point main begin { val a = 50 val b = 30 val c = 100
                         if (origin? (move ((a,c) left (10) up (50) down (20) right (99))))
                            { {val d = 20 val c = 101
                               if (origin? ((0,c))){(50,50)}else{move ((a,c) left (-1) up (50) down (20) right (99))}}}
                            else{move ((a,c) left (10) up (50) down (20) right (99))}} end")
(newline)

; ; (50, 50)
; (run "point main begin { val a = 50 val b = 30 val c = 100
;                          if (origin? (move ((a,c) left (50) down (100))))
;                             { {val d = 20 val c = 101
;                                if (origin? ((0,0))){(50,50)}else{move ((a,c) left (-1) up (50) down (20) right (99))}}}
;                             else{move ((a,c) left (10) up (50) down (20) right (99))}} end")
; (newline)

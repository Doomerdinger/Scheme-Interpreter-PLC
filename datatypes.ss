;; Parsed expression datatypes

;;Any types left out?
;(ormap  (lambda (pred) (pred x)) (list number? vector? boolean? symbol? string? pair? null?))
(define (literal? x) (or (vector? x) (string? x) (boolean? x) (number? x) (char? x) (null? x) (and (pair? x) (not (list? x))) (symbol? x)))

(define (scheme-value? x) #t)

(define-datatype expression expression?
	[var-exp (id symbol?)]
	[lambda-exp-dot (args (list-of symbol?)) (arglist symbol?) (bodies (list-of expression?))]
	[lambda-exp-vari (arglist symbol?) (bodies (list-of expression?))]

	[lambda-exp (args (list-of symbol?)) (bodies (list-of expression?))]
	[let-exp (vars (list-of symbol?)) (declarations (list-of expression?)) (bodies (list-of expression?))]

	[let-named-exp (name symbol?) (vars (list-of symbol?)) (declarations (list-of expression?)) (bodies (list-of expression?))]
	[let*-exp (vars (list-of symbol?)) (declarations (list-of expression?)) (bodies (list-of expression?))]
	[letrec-exp (vars (list-of symbol?)) (declarations (list-of expression?)) (bodies (list-of expression?))]
	
	[set!-exp (var symbol?) (expr expression?)]
	[define-exp (var symbol?) (expr expression?)]

	[begin-exp (exprs (list-of expression?))]
	
	[app-exp (rator expression?) (args (list-of expression?))]

	[while-exp (test-exps expression?) (bodies (list-of expression?))]

	[if-else-exp (test expression?) (consequent expression?) (altern expression?)]
	[if-exp (test expression?) (consequent expression?)]
	[lit-exp (id literal?)]
	[quote-exp (var scheme-value?)]
)
  
;; environment type definitions
(define-datatype environment environment?
	(empty-env-record)
	(extended-env-record (syms (list-of symbol?)) (vals (list-of scheme-value?)) (env cell?))
)

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
	[dummy-proc]
	[prim-proc (name symbol?)]
	[lambda-proc (bodies (list-of expression?)) (args (list-of symbol?)) (env cell?)]
	[lambda-vari-proc (bodies (list-of expression?)) (args symbol?) (env cell?)]
	[lambda-dot-proc (bodies (list-of expression?)) (args (list-of symbol?)) (arglist symbol?) (env cell?)]
)




;(eval-one-exp 
;	'(begin (define x 3) 
;		(or #f 
;			(begin (set! x 2) (> x 4))  
;			#f 
;			(begin (set! x 4) (> x 3)) 
;			(begin (set! x 1)))
;		x
;	)
;)

;(eval-one-exp ' 
;	(begin 
;		(define latest 1) 
;		(define total 1) 
;		(or 
;			(begin 
;				(set! latest (+ latest 1))		;l = 2
;				(set! total (+ total latest))	;t = 3
;				(> total 1)
;			)
;			(begin 
;				(set! latest (+ latest 1))	;l = 3
;				(set! total (+ total latest)) 	;t = 6
;				(> total 50)
;			)
;		)
;		total
;	)
;)

;(eval-one-exp ' 
;	(begin 
;		(define latest 1) 
;		(define total 1) 
;		(or 
;			(begin 
;				(set! latest (+ latest 1))		;l = 2
;				(set! total (+ total latest))	;t = 3
;				(> total 1)
;			)
;		)
;		total
;	)
;)
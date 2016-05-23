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

(define-datatype continuation continuation?
	(while-k
		(test-exp expression?)
		(bodies (list-of expression?))
		(env cell?)
		(k continuation?))
	(while-helper-k
		(test-exp expression?)
		(bodies (list-of expression?))
		(env cell?)
		(k continuation?))
	(if-else-k 
		(then-exp expression?)
		(else-exp expression?)
		(env cell?)
		(k continuation?))
	(if-k 
		(then-exp expression?)
		(env cell?)
		(k continuation?))
	(begin-k
		(env cell?)
		(k continuation?))
	(rator-k (rands (list-of expression?))
		(env cell?)
		(k continuation?))
	(rands-k (proc-value scheme-value?)
		(k continuation?)) ; etc
	(number-if-else-k
		(k continuation?)
		(fail procedure?)
		(vals (list-of scheme-value?))
		(cel cell?)
		(sym symbol?)
		;(env cell?)
	)
	(add-1-if-else-k
		(k continuation?)
	)
	(number-global-if-else-k
		(k continuation?)
		(fail continuation?)
		(vals (list-of scheme-value?))
	)
	(cons-vals-rands-k
		(rands (list-of expression?))
		(env cell?)
		(k continuation?)
	)
	(cons-with-value-k
		(firstVal scheme-value?)
		(k continuation?)
	)
	(last-elem-k
		(k continuation?)
	)

	(identity-k
	)

	(set!-exp-k
		(cel cell?)
		(sym symbol?)
		(k continuation?)
	)

	(mod-env-set!-k
		(cel cell?)
		(new scheme-value?)
		(sym symbol?)
		(k continuation?)
	)

	(mod-env-global-set!-k
		(new scheme-value?)
		(sym symbol?)
		(k continuation?)
	)

	(define-exp-k
		(sym symbol?)
		(k continuation?)
	)

	(mod-env-global-define-k
		(new scheme-value?)
		(sym symbol?)
		(k continuation?)
	)
)

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
	[dummy-proc]
	[prim-proc (name symbol?)]
	[lambda-proc (bodies (list-of expression?)) (args (list-of symbol?)) (env cell?)]
	[lambda-vari-proc (bodies (list-of expression?)) (args symbol?) (env cell?)]
	[lambda-dot-proc (bodies (list-of expression?)) (args (list-of symbol?)) (arglist symbol?) (env cell?)]
	[continuation-proc (k continuation?)]
)
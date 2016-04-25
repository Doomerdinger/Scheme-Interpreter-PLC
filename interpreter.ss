; top-level-eval evaluates a form in the global environment
(define (top-level-eval form)
	; later we may add things that are not expressions.
	(eval-exp form init-env))

; eval-exp is the main component of the interpreter
(define (eval-exp exp env)
	(let ((envir 
		(cases environment env
			[empty-env-record () init-env]
			[else env]
		)))
		(cases expression exp
			[lit-exp (datum) datum] ;;CHECK THIS FOR UNPARSING
			[var-exp (id)
				(apply-env envir id; look up its value.
					; procedure to call if id is in the environment 
					(lambda (x) x)
					(lambda () 
						(eopl:error 'apply-env ; procedure to call if id not in env
						"variable not found in environment: ~s" id)
					)
				)
			] 
			[app-exp (rator rands) 
				(let ([proc-value (eval-exp rator envir)] [args (eval-rands rands envir)]) 
					(apply-proc proc-value args))]
			[quote-exp (pression) pression]
			[if-else-exp (test consequent alternative) 
				(if (eval-exp test envir)
					(eval-exp consequent envir)
					(eval-exp alternative envir)
				)
			]
			[if-exp (test consequent) 
					(if (eval-exp test envir) 
							(eval-exp consequent envir))]
			[let-exp (vars declarations bodies)
				(let ((let-env (extend-env vars (eval-rands declarations envir) envir)))
					(last-elem (eval-rands bodies let-env)))]
					;(last-elem (map (lambda (body) (eval-exp body let-env)) bodies)))]
			[lambda-exp (args bodies) (lambda-proc bodies args envir)]
			[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]
		)
	)
)

; evaluate the list of operands, putting results into a list
(define (last-elem ls)
	(if (null? (cdr ls))
			(car ls)
			(last-elem (cdr ls))
	)
)

(define (eval-rands rands env) ;rans is a list of expressions, env is the environment
	(map (lambda (xp) (eval-exp xp env)) rands))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
(define (apply-proc proc-value args)
	(cases proc-val proc-value
		[prim-proc (op) (apply-prim-proc op args)];(map unparse-args args))]
		[lambda-proc (bodies lam-args env) (last-elem (eval-rands bodies (extend-env lam-args args env)))]
		[else (error 'apply-proc
		"Attempt to apply bad procedure: ~s" 
		proc-value)]
	)
)

; The names of primitive procedures
(define *prim-proc-names*
	'(+ - * add1 sub1 cons = quote / not zero? >= car cdr list null? eq? equal? length list->vector
		list? pair? vector->list vector? number? symbol? caar cadr cadar procedure? set-car! set-cdr!)
)

; For now, our initial global environment only contains procedure names.
; Recall that an environment associates a value (not an expression) with an identifier.
(define init-env
	(extend-env
		*prim-proc-names*
		(map prim-proc *prim-proc-names*)
		(empty-env)
	)
)

; Usually an interpreter must define each built-in procedure individually.
; We are "cheating" a little bit.
(define (apply-prim-proc prim-proc args)
	(case prim-proc
		[(+) (apply + args)]
		[(-) (apply - args)]
		[(*) (apply * args)]
		[(add1) (+ (1st args) 1)]
		[(sub1) (- (1st args) 1)]
		[(cons) (cons (1st args) (2nd args))]
		[(=) (apply = args)]
		[(quote) (1st args)] ;Shouldn't really need this.
		[(/) (apply / args)]
		[(not) (not (1st args))]
		[(zero?) (zero? (1st args))]
		[(>=) (>= (1st args) (2nd args))]
		[(car) (car (1st args))]
		[(cdr) (cdr (1st args))]
		[(list) (apply list args)]
		[(null?) (null? (1st args))]
		[(eq?) (eq? (1st args) (2nd args))]
		[(equal?) (equal? (1st args) (2nd args))]
		[(length) (length (1st args))]
		[(list->vector) (list->vector (1st args))]
		[(list?) (list? (1st args))]
		[(pair?) (pair? (1st args))]
		[(vector->list) (vector->list (1st args))]
		[(vector?) (vector? (1st args))]
		[(number?) (number? (1st args))]
		[(symbol?) (symbol? (1st args))]
		[(caar) (caar (1st args))]
		[(cadr) (cadr (1st args))]
		[(cadar) (cadar (1st args))]
		[(procedure?) (or (proc-val? (1st args)) (procedure? (1st args)))]
		[(set-car!) (set-car! (1st args) (2nd args))]
		[(set-cdr!) (set-cdr! (1st args) (2nd args))]
		[else (error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-op)]
	)
)

; "read-eval-print" loop.
(define (rep)
	(display "--> ")
	;; notice that we don't save changes to the environment...
	(let ([answer (top-level-eval (parse-exp (read)))])
	;; TODO: are there answers that should display differently?
		(eopl:pretty-print answer) (newline)
		(rep)
	)
)  ; tail-recursive, so stack doesn't grow.

(define (eval-one-exp x) (top-level-eval (parse-exp x)))
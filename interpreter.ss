; top-level-eval evaluates a form in the global environment
(define (top-level-eval form)
	; later we may add things that are not expressions.
	;(eval-exp form global-env))
	(eval-exp form (empty-env))
)

; eval-exp is the main component of the interpreter
; Everything put in the environment is in its most evauluated form
; with the exception of procedures, which are in their parced procedure form
(define (eval-exp exp cell)
	(let ((envir 
			cell		
			;(cases environment (deref cell)
			;	[empty-env-record () global-env]
			;	[else cell]
			;)
		))
		(cases expression exp
			[lit-exp (datum) datum]
			[var-exp (id)
				(apply-env envir id; look up its value.
					; procedure to call if id is in the environment 
					(lambda (x) x)
					(lambda () 
						(apply-env global-env id
							(lambda (x) x)
							(lambda ()
								(eopl:error 'apply-env ; procedure to call if id not in env
								"Variable not found in environment: ~s" id)
							)
						)
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
						(eval-exp consequent envir)
				)
			]

			[set!-exp (var expr)
				(modify-env-set! envir var (eval-exp expr envir))]

			[define-exp (var expr)
				(cases environment (deref envir)
					(empty-env-record () (modify-global-env-define var (eval-exp expr envir)))
					(extended-env-record (x y z) (eopl:error 'apply-env "Unable to use define outside of a global context!"))
				)
			]
				;A letrec expression of the form

				;(letrec ((var expr) ...) body1 body2 ...)

				;may be expressed in terms of let and set! as

				;(let ((var #f) ...)
				;  (let ((temp expr) ...)
				;    (set! var temp) ...
				;    (let () body1 body2 ...)))

			;[letrec-exp (vars declarations bodies)
			;	(let ((sym (gensym)))
			;		(let (map (lambda (var) (list var #f)) vars)
			;			(let (map (lambda (var dec) 
			;						(list (concat-symbols var sym) (eval-exp dec envir))) vars declarations)
			;				(append
			;					(map (lambda (var) (set! var (concat-symbols var sym))) vars)
			;					(list (let () bodies))
			;				)
			;			)
			;		)
			;	)
			;]

			[letrec-exp (vars declarations bodies)
				(let ((sym (gensym)))
					(let ((env1 (extend-env vars (map (lambda (var) #f) vars) envir)))
						(let ((env2 (extend-env
										(map (lambda (var) (concat-symbols var sym)) vars)
										(eval-rands declarations env1)
										env1
									)))
							(map (lambda (var) (modify-env-set! env2 var (eval-exp (var-exp (concat-symbols var sym)) env2))) vars)
		
							(last-elem (eval-rands bodies env2))
						)
					)
				)
			]

			[let-exp (vars declarations bodies)
				(let ((let-env (extend-env vars (eval-rands declarations envir) envir)))
					(last-elem (eval-rands bodies let-env)))]
					;(last-elem (map (lambda (body) (eval-exp body let-env)) bodies)))]
			[lambda-exp (args bodies) (lambda-proc bodies args envir)]
			[lambda-exp-vari (arglist body) (lambda-vari-proc body arglist envir)]
			[lambda-exp-dot (args arglist body) (lambda-dot-proc body args arglist envir)]
			
			[begin-exp (bodies) (last-elem (eval-rands bodies envir))]

			[while-exp (text-exp bodies) 
				(letrec ((while-loop (lambda ()
					(if (eval-exp text-exp envir)
						(begin 
							(eval-rands bodies envir)
							(while-loop)
						)
					))))
					(while-loop)
				)
			]

			[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]
		)
	)
)

;Always evaluates rands from left to right
(define (eval-rands rands env) ;rans is a list of expressions, env is the environment
	(if (null? rands)
		'()
		(let* ((firstPart (eval-exp (car rands) env))    ;let* is required for guaranteed left->right eval
			   (secondPart (eval-rands (cdr rands) env)))

			(cons firstPart secondPart)
		)
	)
)

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
(define (apply-proc proc-value args)
	(cases proc-val proc-value
		[prim-proc (op) (apply-prim-proc op args)];(map unparse-args args))]
		[lambda-proc (bodies lam-args env) (last-elem (eval-rands bodies (extend-env lam-args args env)))]
		[lambda-vari-proc (bodies lam-arglist env) (last-elem (eval-rands bodies (extend-env (list lam-arglist) (list args) env)))]
		[lambda-dot-proc (bodies lam-args lam-arglist env) (last-elem (eval-rands bodies (extend-env (append lam-args (list lam-arglist)) (nest-args-for-dot-lambda (length lam-args) args) env)))]
		[else (error 'apply-proc
		"Attempt to apply bad procedure: ~s" 
		proc-value)]
	)
)

(define (nest-args-for-dot-lambda num-required-args args)
	(if (= 0 num-required-args)
		(list args)
		(cons (car args) (nest-args-for-dot-lambda (- num-required-args 1) (cdr args)))
	)
)

; The names of primitive procedures
(define *prim-proc-names*
	'(+ - * / = add1 sub1 quotient cons quote not zero? > >= < <= car cdr list list-tail null? eq? eqv? equal? length list->vector
		list? pair? vector->list vector? vector-set! number? symbol? caar cadr cadar procedure? set-car! set-cdr!
		vector-ref vector map apply append assq)
)

; For now, our initial global environment only contains procedure names.
; Recall that an environment associates a value (not an expression) with an identifier.
(define (make-init-env)
	(extend-env
		*prim-proc-names*
		(map prim-proc *prim-proc-names*)
		(empty-env)
	)
)

(define global-env (make-init-env))

(define (reset-global-env)
	(set! global-env (make-init-env))
)

; Usually an interpreter must define each built-in procedure individually.
; We are "cheating" a little bit.
(define (apply-prim-proc prim-proc args)
	(case prim-proc
		[(>) (> (1st args) (2nd args))]
		[(<) (< (1st args) (2nd args))]
		[(>=) (>= (1st args) (2nd args))]
		[(<=) (<= (1st args) (2nd args))]

		[(add1) (+ (1st args) 1)]
		[(sub1) (- (1st args) 1)]
		[(quotient) (quotient (1st args) (2nd args))]

		[(+) (apply + args)]
		[(-) (apply - args)]
		[(*) (apply * args)]
		[(/) (apply / args)]

		[(=) (apply = args)]
		[(eq?) (eq? (1st args) (2nd args))]
		[(eqv?) (eqv? (1st args) (2nd args))]
		[(equal?) (equal? (1st args) (2nd args))]

		[(cons) (cons (1st args) (2nd args))]
		[(quote) (1st args)] ;Shouldn't really need this.
		[(not) (not (1st args))]
		[(zero?) (zero? (1st args))]
		[(car) (car (1st args))]
		[(cdr) (cdr (1st args))]
		[(list) (apply list args)]
		[(null?) (null? (1st args))]
		[(length) (length (1st args))]
		[(append) (append (1st args) (2nd args))]
		[(list-tail) (list-tail (1st args) (2nd args))]

		;[(gensym) (gensym)]
		;[(concat-symbols) (string->symbol (string-append (symbol->string x) (symbol->string y)))]
		[(assq) (assq (1st args) (2nd args))]

		[(list?) (list? (1st args))]
		[(pair?) (pair? (1st args))]
		[(number?) (number? (1st args))]
		[(symbol?) (symbol? (1st args))]
		[(caar) (caar (1st args))]
		[(cadr) (cadr (1st args))]
		[(cadar) (cadar (1st args))]
		[(procedure?) (or (proc-val? (1st args)) (procedure? (1st args)))]	;;Check this
		[(set-car!) (set-car! (1st args) (2nd args))]
		[(set-cdr!) (set-cdr! (1st args) (2nd args))]

		[(list->vector) (list->vector (1st args))]
		[(vector->list) (vector->list (1st args))]
		[(vector-ref) (vector-ref (1st args) (2nd args))]
		[(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
		[(vector) (list->vector args)]		;;Check this
		[(vector?) (vector? (1st args))]

		[(map) (map (lambda (x) (apply-proc (1st args) (list x))) (2nd args))] ;;Check this
		[(apply) (apply-proc (1st args) (2nd args))] ;;Check this

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

(define (eval-one-exp x) (top-level-eval (syntax-expand (parse-exp x))))
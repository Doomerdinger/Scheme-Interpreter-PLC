; top-level-eval evaluates a form in the global environment
(define (top-level-eval form)
	; later we may add things that are not expressions.
	;(eval-exp form global-env))
	(eval-exp form (empty-env) (identity-k))
)

(define (eval-exp exp env k) ;cps-version
	(cases expression exp ; look at typical cases
		[lit-exp (datum) (apply-k k datum)] ;Done
		[var-exp (id) (apply-env env id k (lambda ()
								(eopl:error 'apply-env ; procedure to call if id not in env
								"Variable not found in environment: ~s" id)
							))]

		[lambda-exp (args bodies)
			(apply-k k (lambda-proc bodies args env))] ;Done
		[lambda-exp-vari (arglist bodies)
			(apply-k k (lambda-vari-proc bodies arglist env))] ;Done
		[lambda-exp-dot (args arglist bodies)
			(apply-k k (lambda-dot-proc bodies args arglist env))] ;Done

		[app-exp (rator rands) ;DONE???
			(eval-exp rator env (rator-k rands env k))
		]
		[quote-exp (pression) (apply-k k pression)] ;Done
		[if-else-exp (test consequent alternative)  ;Done
			(eval-exp test env 
				(if-else-k 
					consequent
					alternative
					env
					k
				)
			)
		]
		[if-exp (test consequent) ;Done
			(eval-exp test env 
				(if-k 
					consequent
					env
					k
				)
			)
		]

		[set!-exp (var expr) ;Done
				;(eval-exp expr env
				;	(lambda (x)
				;		(modify-env-set! env var x k)))]

				(eval-exp expr env (set!-exp-k env var k))] ; modify-env-set!

		[define-exp (var expr) ; TODO
			(cases environment (deref env)
				(empty-env-record () 
					(eval-exp expr env 
						(define-exp-k
							var
							k
						)
					)
				)
				(extended-env-record (x y z) (eopl:error 'apply-env "Unable to use define outside of a global context!"))
			)
		]

		;[define-exp (var expr) ; TODO
		;	(cases environment (deref env)
		;		(empty-env-record () (modify-global-env-define var (eval-exp expr env))) ; modify-global-env-define
		;		(extended-env-record (x y z) (eopl:error 'apply-env "Unable to use define outside of a global context!"))
		;	)
		;]

		;[letrec-exp (vars declarations bodies)
		;	(let ((sym (gensym)))
		;		(let ((env1 (extend-env vars (map (lambda (var) #f) vars) env)))
		;			(let ((env2 (extend-env
		;							(map (lambda (var) (concat-symbols var sym)) vars)
		;							(eval-rands declarations env1)
		;							env1
		;						)))
		;				(map (lambda (var) (modify-env-set! env2 var (eval-exp (var-exp (concat-symbols var sym)) env2))) vars)
	
		;				(last-elem (eval-rands bodies env2))
		;			)
		;		)
		;	)
		;]

		[begin-exp (bodies) ;Done
			(eval-rands bodies env 
				(begin-k
					env
					k
				)
			)
		]

		[while-exp (test-exp bodies) ;Done
			(eval-exp test-exp env ;k)
				(while-k
					test-exp
					bodies
					env
					k
				)
			)
		]

		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]
	) ;etc
)

(define (apply-k k val)
	(cases continuation k
		[define-exp-k (var k)
			(modify-global-env-define var val k)
		]
		[begin-k (env k)
			(last-elem val k)
		]
		[while-k (test-exp bodies env k)
			(if val
				(eval-rands bodies env (while-helper-k test-exp bodies env k))
				(apply-k k (void))
			)
		]
		[while-helper-k (test-exp bodies env k)
			(eval-exp test-exp env (while-k test-exp bodies env k))
		]
		[if-else-k (then-exp else-exp env k)
			(if val
				(eval-exp then-exp env k)
				(eval-exp else-exp env k))
		]
		[if-k (then-exp env k)
			(if val
				(eval-exp then-exp env k)
				(apply-k k (void))
			)
		]
		[rator-k (rands env k)
			(eval-rands rands
			env
			(rands-k val k))
		]
		[rands-k (proc-value k)
			(apply-proc proc-value val k)
		]
		[number-if-else-k (k fail vals cel sym)
			(if (number? val)
				(apply-k k (list-ref vals val))
				(apply-env cel sym k fail)
			)
		]
		[add-1-if-else-k (k)
			(if val
				(apply-k k (+ 1 val))
				(apply-k k #f)
			)
		]
		[number-global-if-else-k (k fail vals)
			(if (number? val)
				(apply-k k (list-ref vals val))
				(fail)
			)
		]
	
		[last-elem-k (k)
			(last-elem val k)]

		[cons-vals-rands-k (rands env k)
			(eval-rands (cdr rands) env
				(cons-with-value-k val k)
			)
		]

		[cons-with-value-k (firstVal k)
			(apply-k k (cons firstVal val))
		]

		;[set!-exp-k (env new k)
		[set!-exp-k (env var k)
			(modify-env-set! env var val k)
		]

		[mod-env-set!-k (cel new sym k)
			(cases environment (deref cel) ;deref just remapping of car
				(empty-env-record () (modify-global-env-set! sym new k)) ;FIX THIS
				(extended-env-record (syms vals extendedCell)
					(if (number? val)
						(apply-k k 
							(cell-set! cel ;cell-set! is primitive (remapping of car-set!), no need for CPS
								(extended-env-record
									syms
									(list-replace-at-position vals val new) ;Just primitive procedures, no cps
									extendedCell
								)
							)
						)
						(modify-env-set! extendedCell sym new k)
					)
				)
			)
		]

		[mod-env-global-set!-k (new sym k)
			(cases environment (deref global-env)
				(empty-env-record () (eopl:error 'apply-k-mod-global "The global environment is empty. Wat."))
				(extended-env-record (syms vals extendedCell)
					(apply-k k (if (number? val)
						(cell-set! global-env
							(extended-env-record
								syms
								(list-replace-at-position vals val new)
								extendedCell
							)
						)
						(eopl:error 'apply-k-mod-global "Unable to find variable in an environment for set!: ~s" sym)
					))
				)
			)
		]

		[mod-env-global-define-k (new sym k)
			(cases environment (deref global-env)
				(empty-env-record () (eopl:error 'define-global-k "Global environment is empty for define-k, sym: ~s" sym))
				(extended-env-record (syms vals extendedCell)
					(apply-k k (cell-set! global-env
						(if (number? val)
							(extended-env-record
								syms
								(list-replace-at-position vals val new)
								extendedCell
							)
							(extended-env-record
								(cons sym syms)
								(cons new vals)
								extendedCell
							)
						)
					))
				)
			)
		]

		[identity-k ()
			val
		]

		[else (eopl:error 'apply-k "Bad continuation: ~a" k)]
	)
)

; eval-exp is the main component of the interpreter
; Everything put in the environment is in its most evauluated form
; with the exception of procedures, which are in their parced procedure form
(define (eval-exp-old exp cell)
	(let ((envir 
			cell		
			;(cases environment (deref cell)
			;	[empty-env-record () global-env]
			;	[else cell]
			;)
		))
		(cases expression exp
			[lit-exp (datum) datum] ;DONE
			[var-exp (id) ;DONE
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
			[app-exp (rator rands) ;DONE???
				(let ([proc-value (eval-exp rator envir)] [args (eval-rands rands envir)]) 
					(apply-proc proc-value args))]
			[quote-exp (pression) pression] ;DONE
			[if-else-exp (test consequent alternative) ;Done
				(if (eval-exp test envir)
					(eval-exp consequent envir)
					(eval-exp alternative envir)
				)
			]
			[if-exp (test consequent) ;Done
					(if (eval-exp test envir) 
						(eval-exp consequent envir)
				)
			]

			[set!-exp (var expr) ;Not Done
				(modify-env-set! envir var (eval-exp expr envir))]

			[define-exp (var expr) ;Not Done
				(cases environment (deref envir)
					(empty-env-record () (modify-global-env-define var (eval-exp expr envir)))
					(extended-env-record (x y z) (eopl:error 'apply-env "Unable to use define outside of a global context!"))
				)
			]

			[letrec-exp (vars declarations bodies) ;Not done
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

			;[let-exp (vars declarations bodies)
			;	(let ((let-env (extend-env vars (eval-rands declarations envir) envir)))
			;		(last-elem (eval-rands bodies let-env)))]

					;(last-elem (map (lambda (body) (eval-exp body let-env)) bodies)))]

			[lambda-exp (args bodies) (lambda-proc bodies args envir)] ;Done
			[lambda-exp-vari (arglist body) (lambda-vari-proc body arglist envir)] ;Done
			[lambda-exp-dot (args arglist body) (lambda-dot-proc body args arglist envir)] ;Done
			
			[begin-exp (bodies) (last-elem (eval-rands bodies envir))] ;Done

			[while-exp (text-exp bodies) ;??
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

(define (eval-rands rands env k)
	(if (null? rands)
		(apply-k k '())
		(eval-exp (car rands) env 
			(cons-vals-rands-k rands env k)
		)
	)
)

;Always evaluates rands from left to right
(define (eval-rands-old rands env) ;rans is a list of expressions, env is the environment
	(if (null? rands)
		'()
		(let* ((firstPart (eval-exp (car rands) env))    ;let* is required for guaranteed left->right eval
			   (secondPart (eval-rands (cdr rands) env)))

			(cons firstPart secondPart)
		)
	)
)

(define (apply-proc proc-value args k)
	(cases proc-val proc-value
		[prim-proc (op) 
			(case op

				[(map) (map (lambda (x) (apply-proc (1st args) (list x) k)) (2nd args))] ;Fix this?
				;[(map) (map-cps (lambda (x) (apply-proc (1st args) (list x) k)) (2nd args) k)]
				;(map (lambda (x) (apply-proc (1st args) (list x))) (2nd args))]
				[(apply) (apply-proc (1st args) (2nd args) k)]
				[(call/cc) (apply-proc (car args) (list (continuation-proc k)) k)]
				[(exit-list) args]
				[else (apply-k k (apply-prim-proc op args))] ;Fix this?
		)];(map unparse-args args))]
		[lambda-proc (bodies lam-args env) (eval-rands bodies (extend-env lam-args args env) (last-elem-k k))]
		[lambda-vari-proc (bodies lam-arglist env) (eval-rands bodies (extend-env (list lam-arglist) (list args) env) (last-elem-k k))]
		[lambda-dot-proc (bodies lam-args lam-arglist env) (eval-rands bodies (extend-env (append lam-args (list lam-arglist)) (nest-args-for-dot-lambda (length lam-args) args) env) (last-elem-k k))]
		[continuation-proc (k) (apply-k k (car args))]
		[else (error 'apply-proc
		"Attempt to apply bad procedure: ~s" 
		proc-value)]
	)
)

(define (nest-args-for-dot-lambda num-required-args args)
	(append (list-head args num-required-args) (list (list-tail args num-required-args)))
)

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
(define (apply-proc-old proc-value args)
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

(define (nest-args-for-dot-lambda-old num-required-args args)
	(if (= 0 num-required-args)
		(list args)
		(cons (car args) (nest-args-for-dot-lambda (- num-required-args 1) (cdr args)))
	)
)

; The names of primitive procedures
(define *prim-proc-names*
	'(+ - * / = add1 sub1 quotient cons quote not zero? > >= < <= car cdr list list-tail null? eq? eqv? equal? length list->vector
		list? pair? vector->list vector? vector-set! number? symbol? caar cadr cadar procedure? set-car! set-cdr!
		vector-ref vector map apply append assq newline display call/cc exit-list)
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

		[(newline) (newline)]
		[(display) (display (1st args))]

		;;[(gensym) (gensym)]
		;;[(concat-symbols) (string->symbol (string-append (symbol->string x) (symbol->string y)))]
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

		;[(call/cc) (apply-proc (car args) (list (continuation-proc k)) k)]

		;[(map) (map (lambda (x) (apply-proc (1st args) (list x))) (2nd args))] ;;Check this
		;[(apply) (apply-proc (1st args) (2nd args))] ;;Check this
		;[else (apply prim-proc args)]
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
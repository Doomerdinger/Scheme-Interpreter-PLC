;;Problem 4
;(load "chez-init.ss"); This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)
(define (length2? x) (if (list? x) (= (length x) 2) #f) )

(define (parse-exp datum)
	(cond
		[(symbol? datum) (var-exp datum)]
		[(literal? datum) (lit-exp datum)]
		[(list? datum)
			(cond
				[(eqv? (1st datum) 'lambda)
					(cond
						[(< (length datum) 3)
							(eopl:error 'parse-exp "too few arguments in lambda expression: ~s" datum)
						]
						[(symbol? (2nd datum))
							(lambda-exp-vari (2nd datum) (map parse-exp (cddr datum)))
						]
						[(and (pair? (2nd datum)) (not (list? (2nd datum))))
							(lambda-exp-dot (all-but-last-elem (2nd datum)) (last-elem (2nd datum)) (map parse-exp (cddr datum)))
						]
						[(and (or (pair? (2nd datum)) (null? (2nd datum))) ((list-of symbol?) (2nd datum)))
							(lambda-exp (2nd datum) (map parse-exp (cddr datum)))
						]
						[else (eopl:error 'parse-exp "incorrect arguments in lambda expression: ~s" datum)]
					)
				]
				[(eqv? (1st datum) 'if)
					(cond 
						[(= (length datum) 4)
							(if-else-exp
								(parse-exp (2nd datum))
								(parse-exp (3rd datum))
								(parse-exp (4th datum))
							)
						]
						[(= (length datum) 3)
							(if-exp
								(parse-exp (2nd datum))
								(parse-exp (3rd datum))
							)
						]
						[else (eopl:error 'parse-exp "if expression of incorrect length: ~s" datum)]
					)
				]
				[(eqv? (1st datum) 'let)
					(cond
						[(< (length datum) 3)
							(eopl:error 'parse-exp "too few arguments in let expression: ~s" datum)
						]
						[(and ((list-of list?) (2nd datum)) (andmap length2? (2nd datum)))
							(let ((vars (map car (2nd datum)))) 
								(if ((list-of symbol?) vars)
									(let-exp
										vars
										(map parse-exp (map 2nd (2nd datum)))
										(map parse-exp (cddr datum))
									)
									(eopl:error 'parse-exp "sytax error in let expression: ~s" datum)
								)
							)
						]
						[(symbol? (2nd datum))
							(if (< (length datum) 4)
								(eopl:error 'parse-exp "too few arguments in named let expression: ~s" datum)
								(let ((vars (map car (3rd datum)))) 
									(if ((list-of symbol?) vars)
										(let-named-exp
											(2nd datum)
											vars
											(map parse-exp (map 3rd (2nd datum)))
											(map parse-exp (cdddr datum))
										)
										(eopl:error 'parse-exp "sytax error in named let expression: ~s" datum)
									)
								)
							)
						]
						[else (eopl:error 'parse-exp "incorrect arguments in let expression: ~s" datum)]
					)
				]
				[(eqv? (1st datum) 'letrec)
					(cond
						[(< (length datum) 3)
							(eopl:error 'parse-exp "too few arguments in letrec expression: ~s" datum)
						]
						[(and ((list-of list?) (2nd datum)) (andmap length2? (2nd datum)))
							(let ((vars (map car (2nd datum)))) 
								(if ((list-of symbol?) vars)
									(letrec-exp
										vars
										(map parse-exp (map 2nd (2nd datum)))
										(map parse-exp (cddr datum))
									)
									(eopl:error 'parse-exp "sytax error in letrec expression: ~s" datum)
								)
							)
						]
						[else (eopl:error 'parse-exp "incorrect arguments in letrec expression: ~s" datum)]
					)
				]
				[(eqv? (1st datum) 'let*)
					(cond
						[(< (length datum) 3)
							(eopl:error 'parse-exp "too few arguments in let* expression: ~s" datum)
						]
						[(and ((list-of list?) (2nd datum)) (andmap length2? (2nd datum)))
							(let ((vars (map car (2nd datum)))) 
								(if ((list-of symbol?) vars)
									(let*-exp
										vars
										(map parse-exp (map 2nd (2nd datum)))
										(map parse-exp (cddr datum))
									)
									(eopl:error 'parse-exp "sytax error in let* expression: ~s" datum)
								)
							)
						]
						[else (eopl:error 'parse-exp "incorrect arguments in let* expression: ~s" datum)]
					)
				]
				[(eqv? (1st datum) 'set!)
					(if (= (length datum) 3)
						(if (symbol? (2nd datum)) 
							(set!-exp (2nd datum) (parse-exp (3rd datum)))
							(eopl:error 'parse-exp "first arg not a sumbol in set!: ~s" datum)
						)
						(eopl:error 'parse-exp "incorrect number of arguments in set!: ~s" datum)
					)
				]
				[(eqv? (1st datum) 'quote)
					(if (= (length datum) 2)
						(quote-exp (2nd datum))
						(eopl:error 'parse-exp "incorrect number of arguments in quote: ~s" datum)
					)
				]
				[else (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))]
			)
		]
		[else (eopl:error 'parse-exp "bad expression: ~s" datum)]
	)
)


(define (syntax-expand exp)
	(cond
		((expression? exp)
			(cases expression exp
				[var-exp (id) exp]
				[lambda-exp-dot (args arglist bodies) (lambda-exp-dot args arglist (map syntax-expand bodies))]
				[lambda-exp-vari (arglist bodies) (lambda-exp-vari arglist (map syntax-expand bodies))]

				[lambda-exp (args bodies) (lambda-exp args (map syntax-expand bodies))]
				[let-exp (vars declarations bodies)
					(app-exp (lambda-exp vars (map syntax-expand bodies)) (map syntax-expand declarations))
				]

				[let-named-exp (name vars declarations bodies) (let-named-exp name vars (map syntax-expand declarations) (map syntax-expand bodies))]
				[let*-exp (vars declarations bodies) (let*-exp vars (map syntax-expand declarations) (map syntax-expand bodies))]
				[letrec-exp (vars declarations bodies) (letrec-exp vars (map syntax-expand declarations) (map syntax-expand bodies))]
				[set!-exp (var expr) (set!-exp var (syntax-expand expr))]
				
				[app-exp (func args) 
					(cases expression func
						[var-exp (id)
							(cond
								[(eqv? id 'cond)
									#f ;; not done
								]
								
								[(eqv? id 'case)
									(letrec ((expand-case
											(lambda (comparison val-exp-pairs)
												(cond 
													((eqv? (var-exp 'else) (2nd (1st val-exp-pairs))) (expand-case (3rd (3rd (1st val-exp-pairs)))))
													; fuck this
											)))
											(expand-case (1st args) (cdr args))
									)
								]

								[(eqv? id 'begin) (app-exp (lambda-exp '() args) '())]

								[(eqv? id 'let*) ; NEEDS TO BE SYNTAX EXPANDED STILL!!!!!!!!!!!!!
									(fold-right (lambda (x y) (list 'let (list x) y)) (caddr exp) (cadr exp))]

								[(eqv? id 'and)
									(if (null? args)
										'(lit-exp #t)
										(fold-right 
											(lambda (current next) (if-else-exp (syntax-expand current) next '(lit-exp #f)))
											(car (last-pair args))
											args
										)
									)
								]
								
								[(eqv? id 'or) 
									(letrec 
										((expand-or 
											(lambda (args)
												(if (null? args)
													(lit-exp #f)
													(let ((expanded-car (syntax-expand (car args))))
														(if-else-exp expanded-car expanded-car (expand-or (cdr args)))
													)
												)
											)
										))
										(expand-or args)
									)
								]
								[else (app-exp func (map syntax-expand args))]
							)
							;(if (eqv? id 'while)
							;	;;WHILE EXPANSION
							;	;;do while expansion
							;	(let-exp '(l) (parse-exp `))
							;	(app-exp (syntax-expand func) (map syntax-expand args))
							;)
						]
						[else (app-exp (syntax-expand func) (map syntax-expand args))]
					)
					;(app-exp (syntax-expand func) (map syntax-expand args))

				]

				[if-else-exp (test consequent altern) (if-else-exp (syntax-expand test) (syntax-expand consequent) (syntax-expand altern))]
				[if-exp (test consequent) (if-exp (syntax-expand test) (syntax-expand consequent))]
				[lit-exp (id) exp]
				[quote-exp (var) exp]				
			)
		)
		((proc-val? exp)
			(cases proc-val exp
				[else exp]
			)
		)
	)
)

; (while cond bodies) -> (letrec(

;(while (< (car a) 100000) 
;	(set-car! a (* (car a) (car a)))
;	(set-car! a (quotient (car a) 2))
;)

;(let
;	((l
;		(lambda (c)
;			(if c
;				(apply begin X)
;			)
;		)
;	))

;)

;(let*
;	(
;		(l)
;		(o
;			(lambda (c) (if (c) l))
;		)
;	)
;)
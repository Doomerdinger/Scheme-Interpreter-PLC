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
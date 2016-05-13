(define cell 
	(lambda (val) (cons val 'this-is-a-cell))
)

(define cell-ref car)
(define cell-set! set-car!)
(define cell?
	(lambda (obj)
		(and (pair? obj) (eq? (cdr obj) 'this-is-a-cell)) ;;use something to make it more unique --(gensym)
	)
)

(define apply-env-ref
	(lambda (env sym)
		;original code for apply-env
	)
)

(define apply-env
	(lambda (env sym)
		(deref (apply-env-ref env sym))
	)
)

(define deref cell-ref)
(define set-ref! cell-set!)

(define swap
	(lambda (x y)
		(display x)
		(display y)
		(set! y x)
		(display x)
		(display y)
	)
)

(define-syntax swap!
	(syntax-rules ()
		[(_ x y) (let((temp x)) (set! x y) (set! y x))]
	)
)
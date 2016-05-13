; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
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

(define empty-env (lambda () (cell (empty-env-record))))

(define (extend-env syms vals env) (cell (extended-env-record syms vals env)))

(define (list-find-position sym los) (list-index (lambda (xsym) (eqv? sym xsym)) los))

(define (list-index pred ls)
	(cond ((null? ls) #f)
		((pred (car ls)) 0)
		(else 
			(let ((list-index-r (list-index pred (cdr ls))))
				(if (number? list-index-r)
					(+ 1 list-index-r)
					#f
				)
			)
		)
	)
)

(define (list-replace-at-position ls idx new)
	(if (= 0 idx)
		(cons new (cdr ls))
		(cons (car ls) (list-replace-at-position (cdr ls) (- idx 1) new))
	)
)

(define deref cell-ref)

; succeed and fail are procedures applied if the var is or isn't found, respectively.
(define (apply-env-ref env sym succeed fail)
	(cases environment env
		(empty-env-record () (fail))
		(extended-env-record (syms vals cell)
			(let ((pos (list-find-position sym syms)))
				(if (number? pos)
					(succeed (list-ref vals pos))
					(apply-env cell sym succeed fail)
				)
			)
		)
	)
)

(define (apply-env cell sym succeed fail)
	(apply-env-ref (deref cell) sym succeed fail)
)

(define (modify-env cell sym new)
	(cases environment (deref cell)
		(empty-env-record () (eopl:error 'apply-env "Fix this later!!"))
		(extended-env-record (syms vals extendedCell)
			(cell-set!
				cell
				(let ((pos (list-find-position sym syms)))
					(if (number? pos)
						(extended-env-record
							syms
							(list-replace-at-position vals pos new)
							extendedCell
						)
						(modify-env extendedCell sym new)
					)
				)
			)
		)
	)
)
; find the last element of a pair or list -- both (last-elem '(1 2 3 4))
; and (last-elem '(1 2 3 . 4)) return 4.

; Instead of throwing an exception when given '() as input, returns '()
(define (last-elem-old pr)
	(cond 
		((not (pair? pr)) pr)
		((null? (cdr pr)) (car pr))
		(else (last-elem-old (cdr pr)))
	)
)

(define (last-elem pr k)
	(cond 
		((not (pair? pr)) (apply-k k pr))
		((null? (cdr pr)) (apply-k k (car pr)))
		(else (last-elem (cdr pr) k))
	)
)

(define (map-cps proc-cps ls k)
	(if (null? ls) 
		(apply-k k '())
		(map-cps
			proc-cps
			(cdr ls)
			(lambda (results)
				(proc-cps 
					(car ls) 
					(lambda (result) 
						(apply-k k (cons result results))
					)
				)
			)
		)
	)
)

;(define (map-cps func lst continuation)
;	(cond
;		[(null? lst) (apply-continuation continuation '())]
;		[else
;			(apply-continuation continuation 
;				(func (car lst) 
;					(lambda (x) 
;						(cons x (map-cps func (cdr lst) (lambda (x) x)))
;					)
;				)
;			)
;		]
;	)
;)


; get all but the last element of a pair or list
; (e.g. (all-but-last-elem '(1 2 3)) and (all-but-last-elem '(1 2 . 3))
;			both evaluate to '(1 2)
(define (all-but-last-elem pr)
	(cond 
		((not (pair? pr)) '())
		((null? (cdr pr)) '())
		(else (cons (car pr) (all-but-last-elem (cdr pr))))
	)
)

(define (concat-symbols x y)
	(string->symbol (string-append (symbol->string x) (symbol->string y)))
)
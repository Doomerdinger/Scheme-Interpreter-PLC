; find the last element of a pair or list -- both (last-elem '(1 2 3 4))
; and (last-elem '(1 2 3 . 4)) return 4.

; Instead of throwing an exception when given '() as input, returns '()
(define (last-elem pr)
	(cond 
		((not (pair? pr)) pr)
		((null? (cdr pr)) (car pr))
		(else (last-elem (cdr pr)))
	)
)

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
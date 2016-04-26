; find the last element of a pair or list -- both (last-elem '(1 2 3 4))
; and (last-elem '(1 2 3 . 4)) return 4.
(define (last-elem pr)
	(cond 
		((not (pair? pr)) pr)
		((null? (cdr pr)) (car pr))
		(else (last-elem (cdr pr)))
	
)
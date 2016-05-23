(load "chez-init.ss")

(define-datatype kontinuation kontinuation?
  [init-k]
  [append-k (the-car symbol?)
	    (k kontinuation?)]
  [flatten-cdr-k (the-car (lambda (x) (or (symbol? x) (list? x))))
	    (k kontinuation?)]
  [flatten-car-k (flattened-cdr (list-of symbol?))
	    (k kontinuation?)]
  [identity-k]
)

(define apply-k 
  (lambda ()
	(cases kontinuation k
	       [init-k () ; v is the final answer
			 (pretty-print v)
			 (read-flatten-print)]
	       [append-k (the-car k); v is (append L1 (cdr L2))
			 (apply-k k (cons the-car v))]
	       [flatten-cdr-k (the-car k)
			 (if (list? the-car) ; v is the flattened cdr
			 	(begin
			 		(set! k (flatten-car-k v k))
			 		(set! v (flatten-cps the-car))
			 		(apply-k)
			 	)
			 	(begin
			 		(set! v (cons the-car v))
			 		(apply-k)
			 	)
		     )]
	       [flatten-car-k (flattened-cdr k); v is flattened car
			  (append-cps v flattened-cdr k)]
		   [identity-k ()
		   		v
		   ]
	       )))

(define (flatten-cps-imperitave ls)
	(let ([ls ls]
		  [k (init-k)]
		  [v '*unbound])

	(if (null? ls)
		(begin
			(set! v ls)
			(apply-k)
		)
		(begin
			(set! k (flatten-cdr-k (car ls) k))
			(set! v (flatten-cps-imperitave (cdr ls)))
			(apply-k)
		)
	)
))


   	(if (null? ls)
		(apply-k k ls)
		(flatten-cps (cdr ls)
			(flatten-cdr-k (car ls) k))))

(define append-cps 
  (lambda (L1 L2 k)
    (if (null? L1)
	(apply-k k L2)
	(append-cps (cdr L1)
		    L2
		    (append-k (car L1) k)))))

(define read-flatten-print
  (lambda ()
    (display "enter slist to flatten: ")
    (let ([slist (read)])
      (unless (eq? slist 'exit)
	(flatten-cps slist (init-k)))))) 


;(trace append-cps flatten-cps apply-k)
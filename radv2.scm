(load "help.scm")
(load "table1.scm")

(define (install-simplify-radical-package)
  ;; internal-procedures
  ;; representation of radicals
  (define (make-addition r1 r2)
    (cons r1 r2))
  (define (make-radical coefficient radicand)
    (cons coefficient radicand))
  (define (coefficient radical) (car radical))
  (define (radicand radical) (cdr radical))
  (define (same-radicand? r1 r2)
    (and (pair? r1)
	 (pair? r2)
         (eq? (radicand r1) (radicand r2))))

  (define (add-radicals r1 r2)
    (let ((radical1 (simplify r1))
	  (radical2 (simplify r2)))
      (cond ((same-radicand? radical1 radical2)
	     (make-radical (+ (coefficient radical1)
			      (coefficient radical2))
			   (radicand radical1)))
	    ((and (number? radical1)
		  (number? radical2))
	     (+ radical1 radical2))
	    (else (make-addition radical1 radical2)))))

  
  (define (simplify radical)
    (define (iter a b max)
      (cond ((>= a max) (make-radical (* 1 b) max))
	    ((perfect-square? max) (* b (sqrt max)))
	    ((= (remainder max a) 0)
	     (let ((n (/ max a)))
	       (if (perfect-square? n)
		   (make-radical (* b (sqrt n))
				 a)
		   (iter (+ a 1) b max))))
	    (else (iter (+ a 1) b max))))
    (iter 2 (coefficient radical) (radicand radical)))

  (define (perfect-square? n)
    (define (iter a b max)
      (cond ((>= a max) #f)
	    ((= (* a b) max) #t)
	    (else (iter (+ a 1) (+ b 1) max))))
    (iter 1 1 n))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'radical x))
  (put 'simplify '(radical)
       (lambda (x) (tag (simplify x))))
  (put 'add '(radical radical)
       (lambda (r1 r2) (tag (add-radicals r1 r2))))
  (put 'make-radical 'radical
       (lambda (c r) (tag (make-radical c r))))
  'done)

(define (simplify radical)
  (apply-generic 'simplify radical))

(define (add x y)
  (apply-generic 'add x y))

(define (make-radical coeff rad)
  ((get 'make-radical 'radical) coeff rad))

(install-simplify-radical-package)


  

(defpackage stream-cons
  (:use #:common-lisp
	#:iterate)
  (:nicknames #:scons)
  (:export #:scons
	   #:with-scons
	   #:head
	   #:tail
	   #:take
	   #:snth
	   #:snthtail
	   #:smap
	   #:srem
	   #:srem-if
	   #:filter
	   #:nils
	   #:zip
	   #:nums
	   #:primes))

(in-package stream-cons)

(defmacro freeze (&rest body)
  `(lambda () ,@body))

(defun force (l)
  (funcall l))

(define-compiler-macro force (l)
  `(funcall ,l))

(defmacro scons (a b)
  `(freeze (values ,a ,b)))

(defmacro with-scons ((a d) scons &body body)
  `(multiple-value-bind (,a ,d) (force ,scons)
     ,@body))

(defmacro-driver (FOR var IN-SCONS l)
  "Run across stream-cons"
  (let ((scons (gensym))
	(tail (gensym))
	(kwd (if generate 'generate 'for)))
    `(progn (with ,scons = ,l)
	    (,kwd (values ,var ,tail) next (if-first-time (funcall ,scons)
							  (funcall ,tail))))))

(defmacro-driver (FOR var ON-SCONS l)
  "Run across stream-cons"
  (let ((scons (gensym))
	(kwd (if generate 'generate 'for)))
    `(progn (with ,scons = ,l)
	    (,kwd (values nil ,var) next (if-first-time (values nil ,scons)
							(funcall ,var))))))

(defun head (l)
  (with-scons (a d) l
	      (declare (ignore d))
	      a))

(defun tail (l)
  (with-scons (a d) l
	      (declare (ignore a))
	      d))

(defun take (n l)
  (iter (repeat n)
	(for i in-scons l)
	(collect i)))

(defun snth (n l)
  (iter (repeat (1+ n))
	(for i in-scons l)
	(finally (return i))))

(defun snthtail (n l)
  (iter (repeat (1+ n))
	(for i on-scons l)
	(finally (return i))))

(defun smap (f &rest ls)
  (scons (apply f (mapcar #'head ls))
	 (apply #'smap f (mapcar #'tail ls))))

(defun srem (elm l &key (test #'eq))
  (with-scons (a d) l
    (if (funcall test elm a)
	(srem elm d :test test)
	(scons a (srem elm d :test test)))))

(defun srem-if (pred l)
  (with-scons (a d) l
    (if (funcall pred a)
	(srem-if pred d)
	(scons a (srem-if pred d)))))
  
(defun filter (p l)
  (srem-if (lambda (x) (not (funcall p x))) l))

(defun nils () (scons nil (nils)))

(defun zip (&rest ls)
  (reduce (lambda (l1 l2) (smap (lambda (e1 e2) (cons e1 e2)) l1 l2)) ls :from-end t :initial-value (nils)))

(defun nums (&optional (from 0) (by 1)) 
  (scons from (nums (+ from by) by)))

(defun eratosthenes ()
  (labels ((nmod (n) (lambda (x) (zerop (mod x n))))
	   (lazy-primes (l) (with-scons (a d) l
			      (scons a (lazy-primes (srem-if (nmod a) d))))))
    (lazy-primes (nums 2))))

(defun primes ()
  (labels ((prime? (n prs) 
		  (loop 
		     for x in prs
		     always (not (zerop (mod n x)))
		     when (< n (* x x))
		     do (return t)))
	   (lazy-primes  (n prs)
			 (if (prime? n (car prs))
			     (progn
			       (setf (cdr (cdr prs)) (list n))
			       (scons n (lazy-primes (1+ n) (cons (car prs) (cdr (cdr prs))))))
			     (lazy-primes (1+ n) prs))))
    (let ((a (list 2)))
      (scons 2 (lazy-primes 3 (cons a a))))))

(defun collatz (n)
  (scons n (collatz (if (zerop (mod n 2)) (/ n 2) (1+ (* n 3))))))

#|(defun lazy-fib (a b)
  (lcons a (lazy-fib b (+ a b))))
(defun lazy-bonatti (as)
  (lcons (car as) (lazy-bonatti (append (cdr as) (list (apply #'+ as))))))
(defun lazy-mod-bonatti (m as)
  (lcons (car as) (lazy-mod-bonatti m (append (cdr as) (list (mod (apply #'+ as) m))))))

(defun fibs () (lazy-fib 1 1))
(defun n-bonatti (n) (lazy-bonatti (make-list n :initial-element 1)))
(defun n-mod-bonatti (n m) (lazy-mod-bonatti m (make-list n :initial-element 1)))

(defun invest-circulation (n m)
  (let ((es (make-list n :initial-element 1)))
    (labels ((check (cnt l)
	       (if (equal (take n l) es)
		   cnt
		   (check (1+ cnt) (tail l)))))
      (check 1 (tail (n-mod-bonatti n m))))))|#


(defpackage stream-cons
  (:use #:common-lisp
	#:iterate)
  (:nicknames #:scons)
  (:export #:scons
	   #:slist
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
	   #:sappend
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
	    (while (if-first-time ,scons ,tail))
	    (,kwd (values ,var ,tail) next (if-first-time (funcall ,scons)
							  (funcall ,tail))))))

(defmacro-driver (FOR var ON-SCONS l)
  "Run across stream-cons"
  (let ((scons (gensym))
	(kwd (if generate 'generate 'for)))
    `(progn (with ,scons = ,l)
	    (while (if-first-time ,scons ,var))
	    (,kwd (values nil ,var) next (if-first-time (values nil ,scons)
							(funcall ,var))))))

(defmacro slist (&rest args)
  (reduce (lambda (x y) `(scons ,x ,y))
	  args
	  :from-end t
	  :initial-value nil))

(defun force-slist (l)
  (iter (for i in-scons l)
	(collect i)))

(defun head (l)
  (with-scons (a d) l
	      (declare (ignore d))
	      a))

(defun tail (l)
  (with-scons (a d) l
	      (declare (ignore a))
	      d))

(defun take (n l)
  (if (zerop n)
      nil
      (with-scons (a d) l
	(scons a (take (1- n) d)))))

(defun snth (n l)
  (iter (repeat (1+ n))
	(for i in-scons l)
	(finally (return i))))

(defun snthtail (n l)
  (iter (repeat (1+ n))
	(for i on-scons l)
	(finally (return i))))

(defun smap (f &rest ls)
  (if (some #'null ls)
      nil
      (iter (for l in ls)
	    (for (values a d) next (force l))
	    (collect a into as)
	    (collect d into ds)
	    (finally (return (scons (apply f as) (apply #'smap f ds)))))))

(defun srem (elm l &key (test #'eq))
  (if (null l)
      nil
      (with-scons (a d) l
	(if (funcall test elm a)
	    (srem elm d :test test)
	    (scons a (srem elm d :test test))))))

(defun srem-if (pred l)
  (if (null l)
      nil
      (with-scons (a d) l
	(if (funcall pred a)
	    (srem-if pred d)
	    (scons a (srem-if pred d))))))
  
(defun filter (p l)
  (srem-if (lambda (x) (not (funcall p x))) l))

(defun nils () (scons nil (nils)))

(defun zip (&rest ls)
  (if (some #'null ls)
      nil
      (reduce (lambda (l1 l2) (smap (lambda (e1 e2) (cons e1 e2)) l1 l2)) ls :from-end t :initial-value (nils))))

(defun sappend (&rest ls)
  (labels ((sappend1 (l1 l2) (if (null l1)
				 l2
				 (with-scons (a d) l1
				   (scons a (sappend1 d l2))))))
    (reduce #'sappend1 ls :from-end t :initial-value nil)))


(defun nums (&key (from 0) (by 1) (to nil))
  (if (and to (> from to))
      nil
      (scons from (nums :from (+ from by) :by by :to to))))

(defun eratosthenes ()
  (labels ((nmod (n) (lambda (x) (zerop (mod x n))))
	   (lazy-primes (l) (with-scons (a d) l
			      (scons a (lazy-primes (srem-if (nmod a) d))))))
    (lazy-primes (nums :from 2))))

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
  (if (= n 1)
      (scons 1 nil)
      (scons n (collatz (if (zerop (mod n 2)) (/ n 2) (1+ (* n 3)))))))

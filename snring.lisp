(defpackage :cl-snring 
  (:use :common-lisp)
  (:export f-reduce
	   f+
	   f*
	   f-
	   f=
	   f/=
	   f<=
	   f>=
	   f<
	   f>
	   ))

(in-package :cl-fring)

(defparameter one- '(-1000 -900 -800 -700 -600 -500 -400 -300 -200 -100 -90 -80 -70 -60 -50 -40 -30 -20 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1))

(defparameter one+ '(1 2 3 4 5 6 7 8 9 10 20 30 40 50 60 70 80 90 100 200 300 400 500 600 700 800 900 1000))  

;; Just a useful function

(defun flatten (obj)
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))

;;; Reducing words

(defun f-reduce (l)
  (loop for x from 0 to (- (length l) 2) do
       (if (zerop (+(nth x l)(nth (1+ x)l)))
	   (setf (nth x l) 0
		 (nth (1+ x)l) 0)))
  (if (member 0 l)
      (f-reduce (remove 0 l))
      l))

(defun f+ (&rest l)
  (f-reduce (flatten (append l))))

;;;; The multiplication is associative, but distribute only on the right over addition

(defun f*2 (l1 l2)
  (let (l)
    (setf l1 (f-reduce l1)
	  l2 (f-reduce l2))
    (loop for x in l1 do
	 (loop for y in l2 do
	      (if (eq (abs x)(abs y))
		  (push (* (signum y)x) l))))
    (f-reduce (reverse l))))

(defun f* (&rest l)
  (reduce #'f*2 l))

(defun f-inv (l)
  (mapcar #'- (reverse l)))

(defun f-2 (l1 &optional (l2 nil))
  (f+ l1 (f-inv l2)))

(defun f- (&rest l)
  (if (= 1 (length l))
      (f-inv (car l))
      (reduce #'f-2 l)))

;;; Note that this is a partially ordered set.
;;; Non-equality usually does not imply that one of two elements is greater or lesser than the other.

(defun f= (l1 l2)
  (equal l1 l2))

(defun f/= (l1 l2)
  (not (equal l1 l2)))

(defun f<= (l1 l2)
  (if (search l1 l2) t))

(defun f>= (l1 l2)
  (if (search l2 l1) t))

(defun f< (l1 l2)
  (if (and
       (< (length l1)(length l2))
       (search l1 l2))
      t))

(defun f> (l1 l2)
  (if (and
       (> (length l1)(length l2))
       (search l2 l1))
      t))

;; Scalar multiplication

(defun f-scale (l1 l2)
  (let (l r)
    (setf l1 (f-reduce l1)
	  l2 (f-reduce l2)
	  r  (reduce #'+ (mapcar #'signum l2)))
    (loop for x in l1 do
	 (loop for y from 0 to (1- (abs r)) do
              (push x l)))
    (reverse (mapcar (lambda (x) (* x (signum r))) l))))

(defun group-list (l)
  (let (nl (n nil) (c 0))
    (loop for x in l do
	 (if (not (equal n x))
	     (progn
	       (push (list n c) nl)
	       (setf c 1 n x))
	     (incf c)))
    (push (list n c) nl)
    (cdr (reverse nl))))

(defun ungroup-list (l)
  (let (nl)
    (loop for x in l do
	 (loop for y from 1 to (cadr x) do
	      (push (car x) nl)))
    (reverse nl)))

(defun f-unscale (l)
  (let (g)
    (setf l (group-list l))
    (setf g (loop for x in l collect (cadr x)))
    (setf g (reduce #'gcd g))
    (loop for x in l do (setf (cadr x) (/ (cadr x) g)))
    (values (ungroup-list l) g)))

;; Detemining the main prime factor 

(defun f-decompose (l)
  (let ((red (remove-duplicates
	      (mapcar #'abs l)))
	g ln r)
    (setf l (group-list l))
    (loop for x in red do
	 (loop for y in l do
	      (if (eq (abs (car y)) x)
		  (push (cadr y) g)))
	 (setf g (reduce #'gcd g))
	 (loop for y in l do
	      (if (eq (abs (car y)) x)
		  (setf (cadr y) (/ (cadr y) g))))
	 (push (list x g) ln)(setf g nil))
    (setf r (ungroup-list (reverse ln)))
    (values (ungroup-list l)
	    (if (equal r red) nil (reverse r)))))

;; Applying a transposition

(defun f-gate-apply (g l)
  (if (=(abs (car g))(abs (cadr g)))
      l
      (mapcar (lambda(x)
		(if (= (abs x) (abs (car g)))
		    (* (signum x)(cadr g))
		    (if (= (abs x) (abs (cadr g)))
			(* (signum x) (car g)) x)))
	      l)))

;; Simple replacement of one particular symbol 

(defun f-replace (g l)
  (mapcar (lambda (x)
	    (if (= x (car g))
		   (setf x (cadr g)) x))
	  l))
	  
;; Nicer to have two simple functions with "symmetrical" names 

(defun f-first (l)
  (car l))

(defun f-last (l)
  (car (last l)))

;; Implementing an isomorphism of the rational numbers field using only operations 
;; easily definable by the two basic operation of our near-ring

(defun add-2 (l1 l2 &key (first '(1)) (second '(2)))
  (let (a b c)
    (if (null l1) (setf l1 '(2)))
    (if (null l2) (setf l2 '(2)))
    (setf a (f* l1 second)
	  b (f* l2 second)
	  l1 (f-scale l1 b)
	  l2 (f-scale l2 a)
	  c (f-unscale
	     (f+ (f* first l1) (f* first l2) (f* second l1))))
    c))

(defun mul-2 (l1 l2 &key (first '(1)) (second '(2)))
  (let ((a (append first second)))
    (f-unscale (f* l1 l2 a))))
  
(defun inv-2 (l1 &key (first '(1)) (second '(2)))
  (f+ (f-scale (f-scale first (f* second l1))
	       (f-unscale (f* second l1)))
      (f-scale (f-scale second (f* first l1))
	       (f-unscale (f* second l1)))))

 (defun div-2 (l1 l2 &key (first '(1)) (second '(2)))
   (mul-2 l1 (inv-2 l2 :first first :second second)
	  :first first :second second))

(defpackage :cl-fring 
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

;;;; The multiplication is associative, but distributes only on the right over addition

(defun f*2 (l1 l2)
  (let (l)
    (f-reduce l1)
    (f-reduce l2)
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

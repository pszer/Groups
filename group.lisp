(defpackage :group
  (:use :cl)
  (:export
   :set-class
   :set-union
   :set-intersect
   :difference
   :+mod
   :*mod))
(in-package :group)

(load "predicates.lisp")
(load "misc.lisp")

(defclass set-class ()
  ((elements
   :initarg :set
   :initform '()
   :accessor elements))
  (:documentation "An unordered set."))

(defgeneric set-union (a b)
  (:documentation "Performs a union on two containers"))
(defgeneric set-intersect (a b)
  (:documentation "Performs an intersect on two containers"))
(defgeneric difference (a b)
  (:documentation "Gets the difference of container a from b"))

(defmethod set-union ((a set-class) (b set-class))
  (make-instance 'set-class :set (union (elements a) (elements b))))
(defmethod set-intersect ((a set-class) (b set-class))
  (make-instance 'set-class :set (intersection (elements a) (elements b))))
(defmethod difference ((a set-class) (b set-class))
  (make-instance 'set-class :set (set-difference (elements a) (elements b))))

(define-condition bad-argument (error)
  ((argument :initarg :arg :reader arg)
   (type-needed :initarg :type :reader type-needed)))
(define-condition bad-group-axioms (error)
  ((set-elements :initarg :elements :reader elements)
   (set-operation :initarg :operation :reader operation)
   (axiom :initarg :axiom :reader axiom)))

(defclass semi-group (set-class)
  ((operation
    :initarg :operation
    :initform (error 'bad-argument :arg nil :type-needed 'function)
    :accessor operation))
  (:documentation "Semi-group algebraic structure."))

(defun make-semigroup (set operation)
  (if (functionp operation)
      (make-instance 'semi-group :set set :operation operation)
      (error 'bad-argument :arg operation :type-needed 'function)))
(defmethod initialize-instance :after ((sg semi-group) &key)
  (unless (functionp (operation sg))
    (error "Algebraic structure operation is not a function.")))

(defclass group (semi-group)
  ((identity
    :reader group-identity))
  (:documentation "Group algebraic structure"))

(defun make-group (set operation)
  (restart-case (make-instance 'group :set set :operation operation)
    (make-semi-group () :report "Make semigroup instead of a group"
		     (make-semigroup set operation))))
(defmethod initialize-instance :after ((g group) &key (skip-test nil))
  (setf (slot-value g 'identity) (get-identity g))
  (unless skip-test
    (macrolet ((do-error (axiom) `(error 'bad-group-axioms :elements (elements g) :operation (operation g) :axiom ,axiom)))
      (cond ((not (closurep g))       (do-error 'closure))
	    ((not (group-identity g)) (do-error 'identity))
	    ((not (invertiblep g))    (do-error 'inversible))))))

(defgeneric get-identity (structure &optional operation)
  (:documentation "Gets the identity element in a structure under a binary operation."))
(defmethod get-identity ((s set-class) &optional operation)
  (loop for id in (elements s) thereis
       (when (loop for conj in (elements s)
		always (and (equal (funcall operation conj id) conj)
			    (equal (funcall operation id conj) conj)))
	 id)))
(defmethod get-identity ((sg semi-group) &optional (operation (operation sg)))
  (call-next-method sg operation))

(defgeneric get-inverse (structure element &optional identity operation)
  (:documentation "Gets the inverse element in a structure under a binary operation"))
(defmethod get-inverse ((s set-class) element &optional (id nil id-p) operation)
  (let ((identity (if (and id-p id) id (get-identity s operation))))
    (when identity
      (loop for inv in (elements s) thereis
	   (when (and (equal identity (funcall operation inv element))
		      (equal identity (funcall operation element inv)))
	     inv)))))
(defmethod get-inverse ((sg semi-group) element &optional id (operation (operation sg)))
  (call-next-method sg element id operation))
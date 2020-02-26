(load "misc.lisp")

(defclass set-class ()
  ((elements
    :initarg :set
    :initform '()
    :accessor elements)
   (set-format
    :initarg :format
    :initform #'identity
    :accessor set-format
    :documentation "How an element should be printed"))
  (:documentation "An unordered set."))

(defgeneric set-union (a b)
  (:documentation "Performs a union on two containers."))
(defgeneric set-intersect (a b)
  (:documentation "Performs an intersect on two containers."))
(defgeneric difference (a b)
  (:documentation "Gets the difference of container a from b."))

(defmethod set-union ((a set-class) (b set-class))
  (make-instance 'set-class :set (union (elements a) (elements b))))
(defmethod set-intersect ((a set-class) (b set-class))
  (make-instance 'set-class :set (intersection (elements a) (elements b))))
(defmethod difference ((a set-class) (b set-class))
  (make-instance 'set-class :set (set-difference (elements a) (elements b))))

(defgeneric order (structure)
  (:documentation "Gets the order of an algebraic structure (count of elements)."))
(defmethod order ((s set-class))
  (length (elements s)))

(define-condition bad-argument (error)
  ((argument :initarg :arg :reader argument)
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

(defun make-semigroup (set operation &optional (format #'identity))
  (if (functionp operation)
      (make-instance 'semi-group :set set :operation operation :format format)
      (error 'bad-argument :arg operation :type-needed 'function)))
(defmethod initialize-instance :after ((sg semi-group) &key)
  (unless (functionp (operation sg))
    (error "Algebraic structure operation is not a function.")))

(defclass group (semi-group)
  ((identity
    :reader group-identity))
  (:documentation "Group algebraic structure"))

(defun make-group (set operation &key (skip-test nil) (premade-identity nil) (format #'identity))
  (restart-case (make-instance 'group :set set :operation operation :skip-test skip-test
			       :premade-identity premade-identity :format format)
    (make-semi-group () :report "Make semigroup instead of a group"
		     (make-semigroup set operation))))
(defmethod initialize-instance :after ((g group) &key (skip-test nil) (premade-identity nil))
  (if premade-identity
      (setf (slot-value g 'identity) premade-identity)
      (setf (slot-value g 'identity) (get-identity g)))
  (unless skip-test
    (macrolet ((do-error (axiom) `(error 'bad-group-axioms :elements (elements g) :operation (operation g) :axiom ,axiom)))
      (cond ((not (closurep g))       (do-error 'closure))
	    ((not (group-identity g)) (do-error 'identity))
	    ((not (invertiblep g))    (do-error 'inversible))))))

(defgeneric get-identity (structure &optional operation)
  (:documentation "Gets the identity element in a structure under a binary operation."))
(defmethod get-identity ((elements list) &optional operation)
  (loop for id in elements thereis
       (when (loop for conj in elements
		always (and (equalp (funcall operation conj id) conj)
			    (equalp (funcall operation id conj) conj)))
	 id)))
(defmethod get-identity ((s set-class) &optional operation)
  (call-next-method (elements s) operation))
(defmethod get-identity ((sg semi-group) &optional (operation (operation sg)))
  (call-next-method sg operation))

(defgeneric get-inverse (structure element &optional identity operation)
  (:documentation "Gets the inverse element in a structure under a binary operation"))
(defmethod get-inverse ((elements list) element &optional (id nil id-p) operation)
  (let ((identity (if (and id-p id) id (get-identity elements operation))))
    (when identity
      (loop for inv in elements thereis
	   (when (and (equalp identity (funcall operation inv element))
		      (equalp identity (funcall operation element inv)))
	     inv)))))
(defmethod get-inverse ((s set-class) element &optional id operation)
  (call-next-method (elements s) element id operation))
(defmethod get-inverse ((sg semi-group) element &optional id (operation (operation sg)))
  (call-next-method sg element id operation))

(defgeneric format-set (structure)
  (:documentation "Prints the set using the sets set-format function."))
(defmethod format-set ((s set-class))
  (labels ((iter (r)
	     (unless (null r)
	       (format t "~a" (funcall (set-format s) (car r)))
	       (when (cdr r)
		 (format t ", ")
		 (iter (cdr r))))))
    (iter (elements s))))

(load "predicates.lisp")
(load "group-makers.lisp")

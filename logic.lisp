;;; allows for first-order logic checking

(load "group.lisp")

(defgeneric get-elements (structure)
  (:documentation "Gets the elements of an algebraic structure."))
(defmethod get-elements ((elements list))
  elements)
(defmethod get-elements ((s set-class))
  (elements s))

(defmacro for-all-quantifier (var structure clause)
  `(loop for ,var in (get-elements ,structure) always ,clause))
(defmacro for-not-all-quantifier (var structure clause)
  `(loop for ,var in (get-elements ,structure) never ,clause))
(defmacro there-exists-quantifier (var structure clause)
  `(loop for ,var in (get-elements ,structure) thereis ,clause))
(defmacro there-exists-not-quantifier (var structure clause)
  `(loop for ,var in (get-elements ,structure) thereis (not ,clause)))
(defmacro bind-group-quantifier (var op group clause)
  `(let ((,var ,group) (,op (operation ,group))) ,clause))

(defmacro logic (cur &body rest)
  (if (null rest)
      cur
      (let ((quantifier (car rest)))
	(case cur
	  ((A)  `(for-all-quantifier ,(car quantifier) ,(cadr quantifier) (logic ,@(cdr rest))))
	  ((E)  `(there-exists-quantifier ,(car quantifier) ,(cadr quantifier) (logic ,@(cdr rest))))
	  ((¬A) `(for-not-all-quantifier ,(car quantifier) ,(cadr quantifier) (logic ,@(cdr rest))))
	  ((¬E) `(there-exists-not-quantifier ,(car quantifier) ,(cadr quantifier) (logic ,@(cdr rest))))
	  ((IN) `(bind-group-quantifier ,(car quantifier) ,(cadr quantifier) ,(caddr quantifier) (logic ,@(cdr rest))))))))

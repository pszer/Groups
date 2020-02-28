(load "misc.lisp")

(defclass mapping ()
    ((fn
      :initarg :map
      :initform #'identity
      :accessor fn)
     (domain
      :initarg :domain
      :initform nil
      :accessor domain)
     (codomain
      :initarg :codomain
      :initform nil
      :accessor codomain)))

(define-condition bad-mapping (error)
  ((error-string :initarg :string :reader error-string)))

(defun make-mapping (fn domain &optional (codomain nil codomain-p))
  (when (and codomain-p (loop for a in domain thereis (not (member (funcall fn a) codomain))))
    (error 'bad-mapping :error-string "make-mapping: Incompatible codomain."))
  (make-instance 'mapping
		 :map fn
		 :domain domain
		 :codomain (if codomain-p
			       codomain
			       (mapcar fn domain))))
(defun make-pair-mapping (alist)
  "Takes list of mapping pairs, each pair is a list. Eg f:0->1 1->-1 is ((0 1) (1 -1))"
  (let ((domain   (loop for pair in alist collect (car pair)))
	(codomain (loop for pair in alist collect (cadr pair))))
    (make-mapping #'(lambda (x) (cadr (assoc x alist)))
		  domain
		  codomain)))
(defun make-pair-mapping* (alist)
  "Takes list of mapping pairs, each pair is a cons pair. Eg. f:->1 1->-1 is ((cons 0 1) (cons 1 -1))"
  (let ((domain   (loop for pair in alist collect (car pair)))
	(codomain (loop for pair in alist collect (cdr pair))))
    (make-mapping #'(lambda (x) (cdr (assoc x alist)))
		  domain
		  codomain)))
(defun make-permutation-mapping (permutation domain codomain)
  (if (not (= (length domain) (length codomain)))
      (error 'bad-mapping :error-string "make-permutation-mapping: Incompatible codomain.")
      (let ((alist (loop for p across permutation for x in domain collect (cons x (elt codomain (- p 1))))))
	(make-pair-mapping* alist))))

(defgeneric apply-map (mapping object)
  (:documentation "Applies a mapping"))
(defmethod apply-map (mapping object)
  (funcall (fn mapping) object))
(defun image (mapping)
  (labels ((iter (rest result)
	     (if (null rest)
		 result
		 (iter (cdr rest) (cons (apply-map mapping (car rest)) result)))))
    (nreverse (iter (domain mapping) '()))))

(defun injectivep (mapping)
  (labels ((iter (rest tested)
	     (if (null rest)
		 t
		 (let ((image-element (apply-map mapping (car rest))))
		   (if (member image-element tested)
		       nil
		       (iter (cdr rest) (cons image-element tested)))))))
    (iter (domain mapping) '())))

(defun surjectivep (mapping)
  (set-equal (codomain mapping) (image mapping)))

(defun bijectivep (mapping)
  (and (injectivep mapping) (surjectivep mapping)))

(defun homomorphismp (mapping domain-operation codomain-operation)
  (let ((domain (domain mapping)))
    (loop for a in domain always
	 (loop for b in domain always (equalp (funcall codomain-operation (apply-map mapping a) (apply-map mapping b))
					      (apply-map mapping (funcall domain-operation a b)))))))

(defun isomorphismp (mapping domain-operation codomain-operation)
  (and (bijectivep mapping) (homomorphismp mapping domain-operation codomain-operation)))

(defun all-bijections (domain codomain)
  "Set of all bijective maps :domain -> codomain where co/domain are sets of equal size."
  (let ((domain-length (length domain))
	(codomain-length (length codomain)))
    (if (= domain-length codomain-length)
	(labels ((permutation-map (permutation)
		   (make-permutation-mapping permutation domain codomain)))
	  (mapcar #'permutation-map (permutation-set domain-length)))
	nil)))
(defun all-isomorphisms (domain codomain)
  "Set of all isomorphic maps :domain -> codomain where co/domain are groups."
  (labels ((is-homomorphism-p (bijection)
	     (homomorphismp bijection (operation domain) (operation codomain))))
    (remove-if-not #'is-homomorphism-p (all-bijections (elements domain) (elements codomain)))))

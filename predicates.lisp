
(defgeneric closurep (structure &optional operation)
  (:documentation "Checks if a given structure is closed under an operation."))
(defgeneric identityp (structure &optional operation)
  (:documentation "Checks if a given structure has an identity under an operation."))
(defgeneric invertiblep (structure &optional id operation)
  (:documentation "Checks if a given structure has an identity and every element is invertible under an operation."))

(defmethod closurep ((s set-class) &optional operation)
  (let ((elements (elements s)))
    (loop for g in elements always
	 (loop for h in elements always (and (member (funcall operation g h) elements)
					     (member (funcall operation h g) elements))))))
(defmethod closurep ((sg semi-group) &optional (operation (operation sg)))
  (call-next-method sg operation))

(defmethod invertiblep ((s set-class) &optional id operation)
  (loop for g in (elements s) always
       (get-inverse s g id operation)))
(defmethod invertiblep ((sg semi-group) &optional id (operation (operation sg)))
  (call-next-method sg id operation))

(defmethod identityp ((s set-class) &optional operation)
  (when (get-identity s operation) t))
(defmethod identityp ((sg semi-group) &optional (operation (operation sg)))
  (call-next-method sg operation))

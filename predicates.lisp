(defgeneric closurep (structure &optional operation)
  (:documentation "Checks if a given structure is closed under an operation."))
(defgeneric identityp (structure &optional operation)
  (:documentation "Checks if a given structure has an identity under an operation."))
(defgeneric invertiblep (structure &optional id operation)
  (:documentation "Checks if a given structure has an identity and every element is invertible under an operation."))

(defmethod closurep ((s set-class) &optional operation)
  (let ((elements (elements s)))
    (loop for g in elements always
	 (loop for h in elements always (and (member (funcall operation g h) elements :test #'equalp)
					     (member (funcall operation h g) elements :test #'equalp))))))
(defmethod closurep ((sg semi-group) &optional (operation (operation sg)))
  (call-next-method sg operation))

(defmethod invertiblep ((s set-class) &optional id operation)
  (let ((identity (if id id (get-identity s operation))))
    (loop for g in (elements s) always
	 (get-inverse s g identity operation))))
(defmethod invertiblep ((sg semi-group) &optional id (operation (operation sg)))
  (call-next-method sg id operation))

(defmethod identityp ((s set-class) &optional operation)
  (when (get-identity s operation) t))
(defmethod identityp ((sg semi-group) &optional (operation (operation sg)))
  (call-next-method sg operation))

(defgeneric group-p (stucture &optional operation)
  (:documentation "Checks if a given set and an operation forms a group."))
(defmethod group-p ((s set-class) &optional operation)
  (and (closurep s operation) (identityp s operation) (invertiblep s nil operation)))
(defmethod group-p ((sg semi-group) &optional (operation (operation sg)))
  (call-next-method sg operation))

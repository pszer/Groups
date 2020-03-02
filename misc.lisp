(defun integers (a b)
  (loop for i from a to b collect i))
(defun primes (a b)
  (loop for i from a to b when (primep i) collect i))
(defun coprimes (a b &optional (value b))
  (loop for i from a to b when (coprimep i value) collect i))

(defun primep (p)
  (when (> p 1)
    (loop for i from 2 to (isqrt p) never (= 0 (mod p i)))))
(defun coprimep (a b)
  (= 1 (gcd a b)))

(defun mod-op (fn base)
  #'(lambda (&rest numbers) (mod (apply fn numbers) base)))
(defmacro +mod (base)
  `(mod-op #'+ ,base))
(defmacro -mod (base)
  `(mod-op #'- ,base))
(defmacro *mod (base)
  `(mod-op #'* ,base))

(defun euler-totient (x)
  (length (coprimes 1 x)))

(defun permutation-set (n)
  "Makes the set of all permutations on a set of size n"
  (let ((permutations '()))
    (labels ((permute (remaining previous)
	       (if (null remaining)
		   (setf permutations (cons (apply #'vector previous) permutations ))
		   (loop for i in remaining do (permute (remove i remaining)
							(cons i previous))))))
      (permute (integers 1 n) '())
      permutations)))
(defun make-permutation (&rest perms)
  (apply #'vector perms))

(defgeneric combination (s permutation)
  (:documentation "Applies permutation to a set"))
(defmethod combination ((s vector) permutation)
  (apply #'vector (loop for p across permutation collect (aref s (- p 1)))))
(defmethod combination ((s list) permutation)
  (loop for p across permutation collect (nth (- p 1) s)))

(defun power-set (list)
    (if (null list)
	'(())
	(let ((sets (power-set (cdr list)))
	      (element (car list)))
	  (append sets (loop for s in sets collect (cons element s))))))



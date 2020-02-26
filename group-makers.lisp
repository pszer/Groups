(defun add-mod-group (n)
  (make-group (integers 0 (- n 1)) (+mod n) :skip-test t :premade-identity 0))
(defun mul-mod-group (n)
  (make-group (coprimes 1 n) (*mod n) :skip-test t :premade-identity 1))

(defmacro cyclic-group (n)
  `(add-mod-group ,n))
(defmacro symmetric-group (n)
  `(make-group (permutation-set ,n) #'combination :skip-test t :premade-identity (apply #'make-permutation (integers 1 ,n))))

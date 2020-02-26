(defun add-mod-group (n)
  "Returns the additive group mod n. Elements are integers 0 to n-1"
  (make-group (integers 0 (- n 1)) (+mod n) :skip-test t :premade-identity 0))
(defun mul-mod-group (n)
  "Returns the multiplicative group mod n. Elements are integers coprime to n."
  (make-group (coprimes 1 n) (*mod n) :skip-test t :premade-identity 1))

(defmacro cyclic-group (n)
  "Returns the cyclic group order n which is just the additive group mod n."
  `(add-mod-group ,n))
(defmacro symmetric-group (n)
  "Returns the symmetric group n, which is the set of all permutations on a set size n."
  `(make-group (permutation-set ,n) #'combination :skip-test t :premade-identity (apply #'make-permutation (integers 1 ,n))))

(defun dihedral-fn (n)
  "The binary function for combining elements in the dihedral group"
  (macrolet ((to-pos (p) ; ep = p^-1e, given an ep this will give p
	       `(- (+ ,p 1)))
	     (inv (p)
	       `(mod (- n ,p) n)))
    #'(lambda (a b)
	(let* ((a-pos (>= a 0))
	       (b-pos (>= b 0))
	       (a-neg (not a-pos))
	       (b-neg (not b-pos)))
	  (cond ((and a-pos b-pos) (mod (+ a b) n))
		((and a-neg b-neg) (mod (+ (inv (to-pos a)) (to-pos b)) n))
		((and a-neg b-pos) (- -1 (mod (+ (to-pos a) b) n)))
		((and a-pos b-neg) (- -1 (inv (mod (+ a (inv (to-pos b))) n)))))))))
(defun dihedral-format (n)
  #'(lambda (x)
      (if (>= x 0)
	  (format nil "~a\°" (* x (/ 360 n)))
	  (format nil "r~3,'0d" (* (/ (- -1 x) 2) (/ 360 n))))))

(defmacro dihedral-group (n)
  "Returns the dihedral group order 2n."
  `(make-group (nreverse (integers ,(- n) ,(- n 1))) (dihedral-fn ,n)
	       :skip-test t :premade-identity 0 :format (dihedral-format ,n)))

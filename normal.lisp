(load "group.lisp")

(defun normal-subgroup-p (sub super)
  (and (subgroup-p sub super)
       (loop for inner-automorphism in (inner-automorphisms super) always
	    (set-equal (elements sub) (elements (apply-map inner-automorphism sub))))))

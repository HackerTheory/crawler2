(in-package :crawler2)

(defun all (&rest args)
  (apply #'every #'identity args))

(defun none (&rest args)
  (apply #'every #'null args))

(defun any (&rest args)
  (apply #'some #'identity args))

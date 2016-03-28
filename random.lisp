(in-package :crawler2)

(defvar *rng* nil)

(defun make-seed ()
  (mod
   (parse-integer
    (shuffle
     (format nil "~d~d"
             (get-universal-time)
             (get-internal-real-time))))
   (expt 2 48)))

(defmethod make-rng (stage)
  (setf *rng* (make-instance 'ranq1-random-number-generator)
        (random-seed *rng*) (seed stage))
  (format t "Random seed: ~A~%" (seed stage)))

(defmethod rng ((method (eql 'elt)) &key list)
  (random-element *rng* list))

(defmethod rng ((method (eql 'int)) &key (min 0) (max 1))
  (integer-random *rng* min max))

(defmethod rng ((method (eql 'inc)) &key (min 0.0) (max 1.0))
  (random-range-inclusive *rng* min max))

(defmethod rng ((method (eql 'odd)) &key (min 1) (max 3))
  (let ((num (rng 'inc :min min :max max)))
    (if (evenp num) (decf num) num)))

(defmethod rng ((method (eql 'bool)) &key (probability 0.5))
  (random-boolean *rng* probability))

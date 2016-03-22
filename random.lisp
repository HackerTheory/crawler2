(in-package :crawler2)

(defun make-seed ()
  (mod
   (parse-integer
    (shuffle
     (format nil "~d~d"
             (get-universal-time)
             (get-internal-real-time))))
   (expt 2 48)))

(defmethod set-seed ((stage labyrinth))
  (setf (random-seed *random-generator*) (seed stage)))

(defmethod rng ((method (eql 'elt)) &key list)
  (random-element *random-generator* list))

(defmethod rng ((method (eql 'int)) &key (min 0) (max 1))
  (integer-random *random-generator* min max))

(defmethod rng ((method (eql 'inc)) &key (min 0.0) (max 1.0))
  (random-range-inclusive *random-generator* min max))

(defmethod rng ((method (eql 'odd)) &key (min 1) (max 3))
  (when (evenp min) (decf min))
  (let ((num (rng 'inc :min min :max max)))
    (if (evenp num) (decf num) num)))

(defmethod rng ((method (eql 'bool)) &key (probability 0.5))
  (random-boolean *random-generator* probability))

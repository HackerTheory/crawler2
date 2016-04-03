(in-package #:crawler2)


;; Define a class which knows how many bits we have in a bitset.
;; We use the size for especially negate operations, and to truncate all
;; bit operations into that bit size.

;; Not used in the feature-set class yet, just stored here since we will need it
;; if we manage to use this code. We'll beed this in order to deal with
;; complement properly.
(defclass bit-set ()
  ((bit-size :initarg :bit-size
             :initform NIL
             :accessor bit-size)))

(defun make-bit-set (bit-size)
  (make-instance 'bit-set :bit-size bit-size))

(defmethod bs-and ((bit-set bit-set) &rest vals)
  (logand (reduce #'logand vals) (1- (expt 2 (bit-size bit-set)))))

(defmethod bs-ior ((bit-set bit-set) &rest vals)
  (logand (reduce #'logior vals) (1- (expt 2 (bit-size bit-set)))))

(defmethod bs-not ((bit-set bit-set) val)
  (logand (lognot val) (1- (expt 2 (bit-size bit-set)))))

(defmethod bs-string ((bit-set bit-set) val)
  (format nil (format nil "#b~~~D,'0B" (bit-size bit-set)) val))

(defun test-bit-set ()
  (let* ((bs (make-bit-set 32))
         (val (bs-ior bs 1 2 4 8 32)))

    (format t "(bs-not ~A) -> ~A~%"
            (bs-string bs val)
            (bs-string bs (bs-not bs val)))))


;; This is a rough draft of an alternate method to store feature sets
;; and do set theory with them. I don't know how well this scales, for
;; several hundreds of features, it might break down badly. It should
;; be tested. :) This code inserts feature keywords into the
;; feature-set as it encounters them, we might want a more restricted
;; policy like a specific registration phase of all known feature set
;; keywords and then we don't lazily add them.  Whatever, it is in the
;; needs that we pick one of those options.


(defclass feature-set ()
  ((snum :initarg :snum
         :initform 0
         :accessor snum)
   ;; This holds kword -> integers AND the disjoint set of integers -> kwords.
   (fstore :initarg :fstore
           :initform (make-hash-table) ;; expecting #'eql as default
           :accessor fstore)))

(defun make-feature-set ()
  (make-instance 'feature-set))

(defmethod serial-num ((feature-set feature-set))
  (let ((sn (snum feature-set)))
    (incf (snum feature-set))
    sn))

;; convert a single feature keyword argument to its appropriate bit
;; position or give it one and then return it.
(defmethod kword->bitpos ((feature-set feature-set) kword)
  (multiple-value-bind (val presentp)
      (gethash kword (fstore feature-set))
    (if presentp
        val
        (let ((new-serial-num (serial-num feature-set)))
          ;; First, associate the kword to the serialnum (bit position)
          (setf (gethash kword (fstore feature-set)) new-serial-num
                ;; second, associate the serialnum (bot position) to the kword.
                (gethash new-serial-num (fstore feature-set)) kword)
          ;; third, return the new serial number.
          new-serial-num))))


;; convert a bit position integer into a keyword, but error if it isn't present.
(defmethod bitpos->kword ((feature-set feature-set) bit-pos)
  (multiple-value-bind (kword presentp)
      (gethash bit-pos (fstore feature-set))
    (if presentp
        kword
        (error "feature-set: Can't convert bit-pos ~A to unknown keyword!" bit-pos))))

;; convert a list of features into an integer representing the bit pattern
;; and return the integer.
(defmethod fval ((feature-set feature-set) &rest feature-keywords)
  (cond
    ((null feature-keywords)
     0)
    (t
     ;; wander down the keyword list turning on the bits in the result integer.
     ;; if there are more bits than a fixnum it'll auto convert to a bignum.
     (loop :with result = 0
        ;; allows us to pass a list to this function or a rest list.
        :with fkeys = (if (not (symbolp (car feature-keywords)))
                          (car feature-keywords)
                          feature-keywords)
        :for kword :in fkeys
        :do (setf result
                  (logior result (ash 1 (kword->bitpos feature-set kword))))
        :finally (return result)))))

;; convert an integer into a list of features and return the feature list.
(defmethod kval ((feature-set feature-set) val)
  (cond
    ((minusp val)
     (error "kval: Illegal value <0 cannot be converted to keyword list!"))

    ((zerop val)
     (bitpos->kword feature-set val))

    (t
     (loop
        :with result = (list)
        :with bit-length = (integer-length val)
        :with mask = 1
        :for bit-pos :from 0 :below bit-length
        :do (unless (zerop (logand (ash mask bit-pos) val))
              (push (bitpos->kword feature-set bit-pos) result))
        :finally (return (nreverse result))))))

;; intersect a collection of lists of feature keywords and return the result
;; as a kword list.
(defmethod kword-intersect ((feature-set feature-set) &rest kword-sets)
  (let* (;; convert the keyword lists to a list of integers representing them.
         (val-sets (mapcar (lambda (kword-set)
                             (apply #'fval feature-set kword-set))
                           kword-sets))
         ;; Perform the feature intersection across all integers
         ;; representing feature sets. BEHOLD: Bit twiddling!
         (val-intersect (reduce #'logand val-sets)))
    ;; and convert it back to a keyword list and return it.
    (kval feature-set val-intersect)))

;; and so on with various other operators on kword lists and such....




(defun testme ()
  (let ((fs (make-feature-set)))

    ;; we lazily insert when we encounter a feature keyword while
    ;; trying to make a value for it, so don't make typos!
    (fval fs :red :orange :yellow :green :blue :indigo :violet)
    (fval fs :eye :nose :mouth :hair)

    ;; Now, let's convert some subsets of the above into values we care
    ;; about.
    (let* ((kword-list-0 (list :orange :red :violet :indigo))
           (kword-list-1 (list :eye))
           (kword-list-2 (list :mouth :eye :violet :red))
           (kword-list-3 (list :car :door :tire :horn))
           ;; use the fancy detection in FVAL about what I'm passing it.
           (val-0 (fval fs kword-list-0))
           (val-1 (fval fs kword-list-1))
           (val-2 (fval fs kword-list-2))
           ;; and show it still works the other way.
           (val-3 (apply #'fval fs kword-list-3)))

      ;; Undo the transformation to see if we got it right.
      (format t "Convert features ~(~S~) to val-0: ~A~% should be: ~(~S~)~% is: ~(~S~)~%~%"
              kword-list-0 val-0 kword-list-0 (kval fs val-0))
      (format t "Convert features ~(~S~) to val-1: ~A~% should be: ~(~S~)~% is: ~(~S~)~%~%"
              kword-list-1 val-1 kword-list-1 (kval fs val-1))
      (format t "Convert features ~(~S~) to val-2: ~A~% should be: ~(~S~)~% is: ~(~S~)~%~%"
              kword-list-2 val-2 kword-list-2 (kval fs val-2))
      (format t "Convert features ~(~S~) to val-3: ~A~% should be: ~(~S~)~% is: ~(~S~)~%~%"
              kword-list-3 val-3 kword-list-3 (kval fs val-3))

      ;; do intersection test
      (let ((int-0 (list :one :two :three :four :five))
            (int-1 (list :three :four :five :six :seven)))
        (format t "Intersection of:~%  ~(~S~)~% and~%  ~(~S~)~%is~%  ~(~S~)~%"
                int-0 int-1 (kword-intersect fs int-0 int-1))

        NIL))))

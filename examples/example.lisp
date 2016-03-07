(in-package :crawler2-examples)

(defvar *stage* nil)
(defvar *attrs* nil)
(defvar *tile-size* 8)

(defmethod draw ()
  (with-slots (width height) *stage*
    (background (gray 0.2))
    (dotimes (x width)
      (dotimes (y height)
        (draw-tile x y)))
    (sleep 1)))

(defmethod draw-tile (x y)
  (with-pen (make-pen :fill (select-color x y))
    (rect (* x *tile-size*)
          (* y *tile-size*)
          (1- *tile-size*)
          (1- *tile-size*))))

(defmethod select-color (x y)
  (let ((tile (tile *stage* x y)))
    (cond
      ((walkablep tile) (gray 1)))))

(defmethod mousebutton-event :after (window state ts button x y)
  (when (and (eq state :MOUSEBUTTONUP)
             (= button 1))
    (regenerate window)))

(defmethod close-window :after (window)
  (setf *stage* nil
        *attrs* nil))

(defmethod generate (stage-type attrs)
  (setf *stage* (apply #'make-stage stage-type attrs)))

(defmethod run :around (stage-type &rest attrs)
  (setf *attrs* attrs)
  (generate stage-type *attrs*)
  (call-next-method))

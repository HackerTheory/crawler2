(in-package :crawler2)

(defstruct (graphs (:conc-name nil)
                   (:constructor %make-graphs))
  connectable
  mst
  final)

(defun get-graph (stage type)
  (funcall type (graphs stage)))

(defun connectable-nodes ()
  (loop :for x :from 1 :to *region*
        :collect x))

(defun connectable-edges (connections)
  (loop :with weight = 0
        :for edge :in (hash-table-keys connections)
        :do (incf weight)
        :collect (cons edge weight)))

(defun mst-edges (stage)
  (let ((mst (mst (graphs stage))))
    (sort (copy-seq (edges mst)) #'edge<)))

(defun edge< (a b)
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((= (first a) (first b)) (edge< (rest a) (rest b)))
        (t (< (first a) (first b)))))

(defun make-graphs (connections)
  (let* ((connectable (populate (make-instance 'graph)
                                :nodes (connectable-nodes)
                                :edges-w-values (connectable-edges connections)))
         (mst (minimum-spanning-tree connectable)))
    (%make-graphs :connectable connectable
                 :mst mst
                 :final (copy mst))))

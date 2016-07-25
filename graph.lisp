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
  (loop :for edge :in (hash-table-keys connections)
        :collect (cons edge 1)))

(defun make-graphs (stage connections)
  (let* ((connectable (populate (make-instance 'graph)
                                :nodes (connectable-nodes)
                                :edges-w-values (connectable-edges connections)))
         (mst (minimum-spanning-tree connectable
                                     (populate (make-instance 'graph)
                                               :nodes (list (rng 'elt :list (nodes connectable)))))))
    (setf (graphs stage)
          (%make-graphs :connectable connectable
                        :mst mst
                        :final (copy mst)))))

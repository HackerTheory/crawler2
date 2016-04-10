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

(defun make-mst (graph)
  (assert (connectedp graph) (graph) "~S is not connected" graph)
  (let ((copy (copy graph))
        (tree (populate (make-instance 'graph)
                        :nodes (list (rng 'elt :list (nodes graph)))))
        (total-nodes (length (nodes graph))))
    (loop :until (= (length (nodes tree)) total-nodes)
          :do (when-let ((e (car (sort
                                  (remove-if-not
                                   {intersection (set-difference (nodes copy) (nodes tree))}
                                   (mapcan {node-edges copy} (nodes tree)))
                                  #'< :key {edge-value copy}))))
                (add-edge tree e (edge-value graph e))
                (delete-edge copy e)))
    tree))

(defun make-graphs (connections)
  (let* ((connectable (populate (make-instance 'graph)
                                :nodes (connectable-nodes)
                                :edges-w-values (connectable-edges connections)))
         (mst (make-mst connectable)))
    (%make-graphs :connectable connectable
                 :mst mst
                 :final (copy mst))))

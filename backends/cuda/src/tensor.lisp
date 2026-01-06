(in-package #:cl-tensor-backend-cuda)

(defclass cuda-tensor (ctp:tensor)
  ((%backend :initarg :backend :reader ctp:backend-of)
   (%dtype   :initarg :dtype   :reader ctp:dtype)
   (%shape   :initarg :shape   :reader ctp:shape)
   (%data    :initarg :data    :reader data)))

(defun %shape-of (object)
  (cond ((arrayp object) (apply #'vector (array-dimensions object)))
        (t #())))

;; For M12, we can implement host-backed tensors and defer device buffers until M13.
(defmethod ctp:tensor ((bk cuda-backend) object &key dtype order)
  (declare (ignore order))
  (unless (cuda-available-p)
    (error 'ctp:not-implemented :backend bk :missing-capabilities '(:tensor-from) :context :cuda))
  (make-instance 'cuda-tensor
                 :backend bk
                 :dtype (or dtype :float32)
                 :shape (%shape-of object)
                 :data object))

(defmethod ctp:to-array ((x cuda-tensor) &key copy element-type)
  (declare (ignore element-type))
  (unless (cuda-available-p)
    (error 'ctp:not-implemented :backend (ctp:backend-of x) :missing-capabilities '(:to-array) :context :cuda))
  (let ((d (data x)))
    (if copy
        (if (arrayp d)
            (let ((out (make-array (array-dimensions d))))
              (loop for i below (array-total-size d) do
                    (setf (row-major-aref out i) (row-major-aref d i)))
              out)
            d)
        d)))


;;;; backends/cpu/cpu-tensor.lisp — placeholder for later milestones

(in-package #:cl-tensor-backend-cpu)

(defclass cpu-tensor (cl-tensor-protocol:tensor)
  ((%backend :initarg :backend :reader cl-tensor-protocol:backend-of)
   (%dtype   :initarg :dtype   :reader cl-tensor-protocol:dtype)
   (%shape   :initarg :shape   :reader cl-tensor-protocol:shape)
   (%data    :initarg :data    :reader data))
  (:documentation "Simple CPU tensor wraps CL data and metadata."))

(defun %compute-shape (obj)
  (cond
    ((arrayp obj) (apply #'vector (array-dimensions obj)))
    (t #())))

(defun %coerce-dtype (obj dtype)
  (declare (ignore dtype))
  ;; For M4, trust provided dtype; conversion comes later.
  obj)

(defmethod cl-tensor-protocol:tensor ((bk cpu-backend) object &key dtype order)
  (declare (ignore order))
  (let* ((shape (%compute-shape object))
         (data (%coerce-dtype object dtype)))
    (make-instance 'cpu-tensor
                   :backend bk
                   :dtype (or dtype (if (arrayp object) :float64 :int32))
                   :shape shape
                   :data data)))

(defmethod cl-tensor-protocol:to-array ((x cpu-tensor) &key copy element-type)
  (declare (ignore element-type))
  (let ((data (data x)))
    (if copy
        (if (arrayp data)
            (copy-seq data)
            data)
        data)))

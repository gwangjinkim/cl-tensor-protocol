(in-package #:cl-tensor-backend-mlx)

(defclass mlx-tensor (ctp:tensor)
  ((%backend :initarg :backend :reader ctp:backend-of)
   (%dtype   :initarg :dtype   :reader ctp:dtype)
   (%shape   :initarg :shape   :reader ctp:shape)
   (arr      :initarg :arr     :reader arr)) )

(defun %shape-of (object)
  (cond
    ((arrayp object) (apply #'vector (array-dimensions object)))
    (t #())))

(defmethod ctp:tensor ((bk mlx-backend) object &key dtype order)
  (declare (ignore order))
  (unless (mlx-available-p)
    (error 'ctp:not-implemented :backend bk :missing-capabilities '(:tensor-from) :context :mlx))
  ;; For M9, rely on mx to create array from lisp input; fallback to store lisp arr if needed
  (let* ((shape (%shape-of object))
         (mx-arr (handler-case
                     (mx:array object)
                   (t () object))))
    (make-instance 'mlx-tensor
                   :backend bk
                   :dtype (or dtype :float32)
                   :shape shape
                   :arr mx-arr)))

(defmethod ctp:to-array ((t mlx-tensor) &key copy element-type)
  (declare (ignore copy element-type))
  (unless (mlx-available-p)
    (error 'ctp:not-implemented :backend (ctp:backend-of t) :missing-capabilities '(:to-array) :context :mlx))
  (let ((a (arr t)))
    (handler-case
        (mx:lisp<- a)
      (t () a))) )


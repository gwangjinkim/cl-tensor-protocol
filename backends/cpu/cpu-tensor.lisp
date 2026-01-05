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

;;; M5 shape ops

(defun %ensure-simple-vector (maybe)
  (etypecase maybe
    (simple-vector maybe)
    (list (coerce maybe 'simple-vector))))

(defun %prod (dims)
  (reduce #'* (coerce dims 'list) :initial-value 1))

(defmethod cl-tensor-protocol:reshape ((x cpu-tensor) new-shape &key copy)
  (declare (ignore copy))
  (let* ((old-shape (cl-tensor-protocol:shape x))
         (new-shape (%ensure-simple-vector new-shape)))
    (if (/= (%prod old-shape) (%prod new-shape))
        (cl:error 'cl-tensor-protocol:shape-error)
        ;; Materialize new array with row-major order
        (let* ((arr (data x))
               (flat (if (arrayp arr)
                         (let* ((n (%prod old-shape))
                                (v (make-array n)))
                           (loop for i below (array-total-size arr)
                                 do (setf (aref v i) (row-major-aref arr i)))
                           v)
                         (let ((v (make-array 1)))
                           (setf (aref v 0) arr) v)))
               (out (make-array (coerce new-shape 'list))))
          (loop for i below (length flat)
                do (setf (row-major-aref out i) (aref flat i)))
          (make-instance 'cpu-tensor
                         :backend (cl-tensor-protocol:backend-of x)
                         :dtype (cl-tensor-protocol:dtype x)
                         :shape new-shape
                         :data out)))))

(defmethod cl-tensor-protocol:transpose ((x cpu-tensor) axes)
  (let* ((axes (%ensure-simple-vector axes))
         (orig (cl-tensor-protocol:shape x))
         (rank (length orig)))
    (unless (= rank (length axes))
      (cl:error 'cl-tensor-protocol:shape-error))
    (unless (and (every (lambda (i) (and (typep i 'fixnum) (<= 0 i) (< i rank))) (coerce axes 'list))
                 (equal (sort (copy-seq (coerce axes 'list)) #'<) (loop for i below rank collect i)))
      (cl:error 'cl-tensor-protocol:shape-error))
    (let* ((new-shape (map 'simple-vector (lambda (i) (aref orig i)) axes))
           (arr (data x))
           (out (make-array (coerce new-shape 'list))))
      (labels ((to-index (lin shape)
                 (let* ((dims (coerce shape 'list))
                        (idxs (make-array (length dims)))
                        (n lin))
                   (loop for k from (1- (length dims)) downto 0
                         for d in (reverse dims) do
                         (multiple-value-bind (q r) (truncate n d)
                           (setf n q
                                 (aref idxs k) r)))
                   idxs))
               (from-index (idxs shape)
                 (let* ((dims (coerce shape 'list))
                        (lin 0))
                   (loop for k from 0 below (length dims) do
                         (setf lin (+ (* lin (nth k dims)) (aref idxs k))))
                   lin)))
        (when (arrayp arr)
          (loop for i below (array-total-size arr) do
                (let* ((old-idx (to-index i orig))
                       (new-idx (make-array (length axes))))
                  (loop for k from 0 below (length axes) do
                        (setf (aref new-idx k) (aref old-idx (aref axes k))))
                  (setf (row-major-aref out (from-index new-idx new-shape))
                        (row-major-aref arr i)))))
        (make-instance 'cpu-tensor
                       :backend (cl-tensor-protocol:backend-of x)
                       :dtype (cl-tensor-protocol:dtype x)
                       :shape new-shape
                       :data out)))))

(defun %slice-range (len start stop step)
  (let* ((st (or start 0))
         (sp (or stop len))
         (pt (or step 1)))
    (loop for i from st below sp by pt collect i)))

(defmethod cl-tensor-protocol:slice ((x cpu-tensor) spec)
  (let* ((shape (cl-tensor-protocol:shape x))
         (rank (length shape))
         (spec (coerce spec 'list)))
    (unless (= rank (length spec))
      (cl:error 'cl-tensor-protocol:shape-error))
    (let* ((arr (data x))
           (axes-indices
            (mapcar (lambda (axis-spec dim)
                      (ecase (first axis-spec)
                        (:all (loop for i below dim collect i))
                        (:range (destructuring-bind (&optional s e st) (rest axis-spec)
                                  (%slice-range dim s e st)))))
                    spec (coerce shape 'list)))
           (new-shape (map 'simple-vector #'length axes-indices))
           (out (make-array (coerce new-shape 'list))))
      (when (arrayp arr)
        (labels ((to-index (lin dims)
                   (let* ((ds (coerce dims 'list))
                          (idxs (make-array (length ds)))
                          (n lin))
                     (loop for k from (1- (length ds)) downto 0
                           for d in (reverse ds) do
                           (multiple-value-bind (q r) (truncate n d)
                             (setf n q
                                   (aref idxs k) r)))
                     idxs))
                 (nth* (n lst) (nth n lst)))
          (let* ((dims-out (mapcar #'length axes-indices))
                 (total (%prod (coerce dims-out 'simple-vector))))
            (loop for oi below total do
                  (let* ((out-idx (to-index oi dims-out))
                         (orig-idx (mapcar (lambda (k)
                                             (nth (aref out-idx k) (nth k axes-indices)))
                                           (loop for k from 0 below (length out-idx) collect k))))
                    (setf (apply #'aref out (coerce out-idx 'list))
                          (apply #'aref arr orig-idx)))))))
      (make-instance 'cpu-tensor
                     :backend (cl-tensor-protocol:backend-of x)
                     :dtype (cl-tensor-protocol:dtype x)
                     :shape new-shape
                     :data out))))

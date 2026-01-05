;;;; backends/cpu/cpu-ops.lisp — placeholder for later milestones

(in-package #:cl-tensor-backend-cpu)

(defun %assert-same-backend (a b)
  (unless (eq (cl-tensor-protocol:backend-of a)
              (cl-tensor-protocol:backend-of b))
    (cl:error 'cl-tensor-protocol:backend-error)))

(defun %assert-same-shape (a b)
  (unless (equalp (cl-tensor-protocol:shape a)
                  (cl-tensor-protocol:shape b))
    (cl:error 'cl-tensor-protocol:shape-error)))

(defun %ensure-tensor (x backend dtype shape)
  (etypecase x
    (cl-tensor-protocol:tensor x)
    (number (make-instance 'cpu-tensor :backend backend :dtype dtype :shape shape :data x))))

(defun %elementwise (op a b)
  (let* ((arr-a (cl-tensor-protocol:to-array a))
         (arr-b (cl-tensor-protocol:to-array b))
         (shape (cl-tensor-protocol:shape a))
         (out (make-array (coerce shape 'list))))
    (cond
      ((and (arrayp arr-a) (arrayp arr-b))
       (loop for i below (array-total-size out) do
             (setf (row-major-aref out i)
                   (funcall op (row-major-aref arr-a i) (row-major-aref arr-b i)))))
      ((arrayp arr-a)
       (loop for i below (array-total-size out) do
             (setf (row-major-aref out i)
                   (funcall op (row-major-aref arr-a i) arr-b))))
      ((arrayp arr-b)
       (loop for i below (array-total-size out) do
             (setf (row-major-aref out i)
                   (funcall op arr-a (row-major-aref arr-b i)))))
      (t (setf out (funcall op arr-a arr-b))))
    (make-instance 'cpu-tensor
                   :backend (cl-tensor-protocol:backend-of a)
                   :dtype (cl-tensor-protocol:dtype a)
                   :shape shape
                   :data out)))

;; add
(defmethod cl-tensor-protocol:add ((a cpu-tensor) (b cpu-tensor))
  (%assert-same-backend a b)
  (%assert-same-shape a b)
  (%elementwise #'+ a b))

(defmethod cl-tensor-protocol:add ((a cpu-tensor) (b number))
  (%elementwise #'+ a (make-instance 'cpu-tensor :backend (cl-tensor-protocol:backend-of a) :dtype (cl-tensor-protocol:dtype a) :shape (cl-tensor-protocol:shape a) :data b)))

(defmethod cl-tensor-protocol:add ((a number) (b cpu-tensor))
  (cl-tensor-protocol:add b a))

;; mul
(defmethod cl-tensor-protocol:mul ((a cpu-tensor) (b cpu-tensor))
  (%assert-same-backend a b)
  (%assert-same-shape a b)
  (%elementwise #'* a b))

(defmethod cl-tensor-protocol:mul ((a cpu-tensor) (b number))
  (%elementwise #'* a (make-instance 'cpu-tensor :backend (cl-tensor-protocol:backend-of a) :dtype (cl-tensor-protocol:dtype a) :shape (cl-tensor-protocol:shape a) :data b)))

(defmethod cl-tensor-protocol:mul ((a number) (b cpu-tensor))
  (cl-tensor-protocol:mul b a))

;; mm (matrix multiply, rank-2)
(defmethod cl-tensor-protocol:mm ((a cpu-tensor) (b cpu-tensor))
  #+nil(format t "~&[debug] mm entered: a=~A b=~A~%" a b)
  (%assert-same-backend a b)
  (let* ((sa (cl-tensor-protocol:shape a))
         (sb (cl-tensor-protocol:shape b)))
    (unless (and (= (length sa) 2) (= (length sb) 2))
      (cl:error 'cl-tensor-protocol:shape-error))
    (let* ((m (aref sa 0))
           (k (aref sa 1))
           (k2 (aref sb 0))
           (n (aref sb 1)))
      (unless (= k k2)
        (cl:error 'cl-tensor-protocol:shape-error))
      (let* ((arr-a (cl-tensor-protocol:to-array a :copy nil))
             (arr-b (cl-tensor-protocol:to-array b :copy nil))
             (C (make-array (list m n))))
        (dotimes (i m)
          (dotimes (j n)
            (let ((sum 0))
              (dotimes (p k)
                (incf sum (* (aref arr-a i p) (aref arr-b p j))))
              (setf (aref C i j) sum))))
        (make-instance 'cpu-tensor
                       :backend (cl-tensor-protocol:backend-of a)
                       :dtype (cl-tensor-protocol:dtype a)
                       :shape (vector m n)
                       :data C)))))

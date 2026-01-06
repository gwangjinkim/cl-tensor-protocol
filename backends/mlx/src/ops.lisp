(in-package #:cl-tensor-backend-mlx)

;; Elementwise helpers
(defun %assert-mlx ((x))
  (declare (ignore x))
  (unless (mlx-available-p)
    (error 'ctp:not-implemented :backend *mlx-backend* :missing-capabilities '(:add :mul) :context :mlx)))

(defun %ensure-mlx-tensor (bk x)
  (etypecase x
    (mlx-tensor x)
    (number (make-instance 'mlx-tensor
                           :backend bk
                           :dtype :float32
                           :shape #()
                           :arr (mx:array x)))))

(defun %eltwise (bk op a b)
  (let* ((ta (%ensure-mlx-tensor bk a))
         (tb (%ensure-mlx-tensor bk b))
         (arr-a (arr ta))
         (arr-b (arr tb))
         (out (ecase op
                (:add (mx:+ arr-a arr-b))
                (:mul (mx:* arr-a arr-b)))))
    (make-instance 'mlx-tensor :backend bk :dtype :float32 :shape (ctp:shape ta) :arr out))

;; add
(defmethod ctp:add ((a mlx-tensor) (b mlx-tensor))
  (%eltwise (ctp:backend-of a) :add a b))

(defmethod ctp:add ((a mlx-tensor) (b number))
  (%eltwise (ctp:backend-of a) :add a b))

(defmethod ctp:add ((a number) (b mlx-tensor))
  (%eltwise (ctp:backend-of b) :add a b))

;; mul
(defmethod ctp:mul ((a mlx-tensor) (b mlx-tensor))
  (%eltwise (ctp:backend-of a) :mul a b))

(defmethod ctp:mul ((a mlx-tensor) (b number))
  (%eltwise (ctp:backend-of a) :mul a b))

(defmethod ctp:mul ((a number) (b mlx-tensor))
  (%eltwise (ctp:backend-of b) :mul a b))

;; mm (matrix multiply, 2D)
(defmethod ctp:mm ((a mlx-tensor) (b mlx-tensor))
  (unless (mlx-available-p)
    (error 'ctp:not-implemented :backend (ctp:backend-of a) :missing-capabilities '(:mm) :context :mlx))
  (let* ((sa (ctp:shape a))
         (sb (ctp:shape b)))
    (unless (and (= (length sa) 2) (= (length sb) 2))
      (error 'ctp:shape-error))
    (let* ((m (aref sa 0))
           (k (aref sa 1))
           (k2 (aref sb 0))
           (n (aref sb 1)))
      (unless (= k k2) (error 'ctp:shape-error))
      (let* ((arr-a (arr a))
             (arr-b (arr b))
             (out (or (and (fboundp 'mxla:matmul)
                           (ignore-errors (mxla:matmul arr-a arr-b)))
                      (mx:@ arr-a arr-b))))
        (make-instance 'mlx-tensor
                       :backend (ctp:backend-of a)
                       :dtype :float32
                       :shape (vector m n)
                       :arr out)))))

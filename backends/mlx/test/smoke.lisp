(in-package #:cl-tensor-backend-mlx/test)

(in-suite :cl-tensor-backend-mlx/test)

(test m9-mlx-skeleton-tier-a
  (if (mlx-available-p)
      (let* ((bk (ctp:default-backend :prefer '(:mlx :cpu) :require '(:tensor-from :to-array :shape)))
             (a1 (make-array 3 :initial-contents '(1 2 3)))
             (t1 (ctp:tensor bk a1 :dtype :float32))
             (b1 (ctp:to-array t1))
             (a2 (make-array '(2 3)))
             (dummy (progn (setf (aref a2 0 0) 1 (aref a2 0 1) 2 (aref a2 0 2) 3
                                  (aref a2 1 0) 4 (aref a2 1 1) 5 (aref a2 1 2) 6) t))
             (t2 (ctp:tensor bk a2 :dtype :float32))
             (b2 (ctp:to-array t2)))
        (declare (ignore dummy))
        (is (eql :mlx (ctp:device-type bk)))
        (is (equalp #(3) (ctp:shape t1)))
        (is (equalp #(2 3) (ctp:shape t2)))
        (is (equalp a1 b1))
        (is (equalp a2 b2)))
      (skip "mlx-cl not available; skipping M9 tests")))


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

;;; M10 — elementwise ops (add/mul)

(test m10-mlx-add-mul
  (if (mlx-available-p)
      (let* ((bk (ctp:default-backend :prefer '(:mlx :cpu)
                                      :require '(:tensor-from :to-array :shape :add :mul)))
             (a (make-array 3 :initial-contents '(1 2 3)))
             (b (make-array 3 :initial-contents '(10 20 30)))
             (ta (ctp:tensor bk a :dtype :float32))
             (tb (ctp:tensor bk b :dtype :float32))
             (t-add (ctp:add ta tb))
             (t-adds (ctp:add ta 1))
             (t-mul (ctp:mul ta tb))
             (t-muls (ctp:mul 2 ta)))
        (is (equalp #(3) (ctp:shape t-add)))
        (is (equalp #(3) (ctp:shape t-adds)))
        (is (equalp #(3) (ctp:shape t-mul)))
        (is (equalp #(3) (ctp:shape t-muls)))
        (is (equalp (make-array 3 :initial-contents '(11 22 33)) (ctp:to-array t-add)))
        (is (equalp (make-array 3 :initial-contents '(2 3 4)) (ctp:to-array t-adds)))
        (is (equalp (make-array 3 :initial-contents '(10 40 90)) (ctp:to-array t-mul)))
        (is (equalp (make-array 3 :initial-contents '(2 4 6)) (ctp:to-array t-muls))))
      (skip "mlx-cl not available; skipping M10 tests")))

;;; M11 — matmul (2D)

(test m11-mlx-mm
  (if (mlx-available-p)
      (let* ((bk (ctp:default-backend :prefer '(:mlx :cpu)
                                      :require '(:tensor-from :to-array :shape :mm)))
             (a (let ((x (make-array '(2 3))))
                  (setf (aref x 0 0) 1 (aref x 0 1) 2 (aref x 0 2) 3
                        (aref x 1 0) 4 (aref x 1 1) 5 (aref x 1 2) 6)
                  x))
             (b (let ((x (make-array '(3 2))))
                  (setf (aref x 0 0) 7 (aref x 0 1) 8
                        (aref x 1 0) 9 (aref x 1 1) 10
                        (aref x 2 0) 11 (aref x 2 1) 12)
                  x))
             (ta (ctp:tensor bk a :dtype :float32))
             (tb (ctp:tensor bk b :dtype :float32))
             (tc (ctp:mm ta tb)))
        (is (equalp #(2 2) (ctp:shape tc)))
        (is (equalp (make-array '(2 2) :initial-contents '((58 64) (139 154)))
                    (ctp:to-array tc))))
      (skip "mlx-cl not available; skipping M11 tests")))

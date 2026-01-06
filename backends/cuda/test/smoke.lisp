(in-package #:cl-tensor-backend-cuda/test)

(in-suite :cl-tensor-backend-cuda/test)

(test m12-cuda-skeleton-tier-a
  (if (cuda-available-p)
      (let* ((bk (ctp:default-backend :prefer '(:cuda :cpu)
                                      :require '(:tensor-from :to-array :shape)))
             (a (make-array '(2 2) :initial-contents '((1 2) (3 4))))
             (t (ctp:tensor bk a :dtype :float32))
             (b (ctp:to-array t)))
        (is (eql :cuda (ctp:device-type bk)))
        (is (equalp #(2 2) (ctp:shape t)))
        (is (equalp a b)))
      (skip "cl-cuda not available; skipping M12 CUDA tests")))


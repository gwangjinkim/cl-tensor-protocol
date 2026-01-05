;;;; test/cpu-backend.lisp — M3 registry + CPU backend registration tests

(in-package #:cl-tensor-protocol/test)

(in-suite :cl-tensor-protocol/test)

(test m3-default-backend-returns-cpu
  (let ((bk (ctp:default-backend)))
    (is (typep bk 'ctp:backend))
    (is (eql :cpu (ctp:device-type bk)))))

(test m3-require-mm-succeeds
  (let ((bk (ctp:default-backend :require '(:mm))))
    (is (typep bk 'ctp:backend))
    (is (eql :cpu (ctp:device-type bk)))))

(test m3-registry-helpers
  (let ((all (ctp:available-backends))
        (cpu (ctp:find-backend :cpu)))
    (is (plusp (length all)))
    (is (typep cpu 'ctp:backend))
    (is (eql :cpu (ctp:device-type cpu)))))

;;; M4 — CPU tensor creation + to-array + shape

(test m4-scalar-roundtrip
  (let* ((bk (ctp:default-backend))
         (tx (ctp:tensor bk 42 :dtype :int32)))
    (is (typep tx 'ctp:tensor))
    (is (equalp #() (ctp:shape tx)))
    (is (eql :int32 (ctp:dtype tx)))
    (is (= 42 (ctp:to-array tx)))))

(test m4-2d-array-roundtrip
  (let* ((bk (ctp:default-backend))
         (a (make-array '(2 3))) )
    (setf (aref a 0 0) 1
          (aref a 0 1) 2
          (aref a 0 2) 3
          (aref a 1 0) 4
          (aref a 1 1) 5
          (aref a 1 2) 6)
    (let ((tx (ctp:tensor bk a :dtype :float64)))
      (is (typep tx 'ctp:tensor))
      (let ((sh (ctp:shape tx)))
        (is (typep sh 'simple-vector))
        (is (equalp #(2 3) sh))
        (is (loop for d across sh always (typep d 'fixnum))))
      (is (eql :float64 (ctp:dtype tx)))
      (is (equalp a (ctp:to-array tx))))))

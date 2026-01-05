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


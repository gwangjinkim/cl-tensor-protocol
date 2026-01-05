;;;; test/capability-tests.lisp — M2 capability model tests

(in-package #:cl-tensor-protocol/test)

(in-suite :cl-tensor-protocol/test)

(defclass fake-backend (ctp:backend) ())

(defmethod ctp:capabilities ((bk fake-backend))
  '(:mm :add))

(test m2-default-backend-capabilities
  (let ((bk (make-instance 'ctp:backend)))
    (is (equal '() (ctp:capabilities bk)))
    (is (not (ctp:supports? bk :mm)))
    (is (equal '(:mm) (ctp:missing-capabilities bk :mm)))
    (is (equal '(:mm :add) (ctp:missing-capabilities bk '(:mm :add))))
    (is (handler-case (progn (ctp:require-capabilities bk '(:mm) :context "m2") nil)
          (ctp:not-implemented (e)
            (and (eq (ctp::not-implemented-backend e) bk)
                 (equal '(:mm) (ctp::not-implemented-missing-capabilities e))
                 (string= (or (ctp::not-implemented-context e) "") "m2")))))))

(test m2-fake-backend-capabilities
  (let ((bk (make-instance 'fake-backend)))
    (is (ctp:supports? bk :mm))
    (is (not (ctp:supports? bk :mul)))
    (is (equal '(:mul) (ctp:missing-capabilities bk '(:mm :mul))))
    (is (eq t (ctp:require-capabilities bk '(:mm :add))))
    (is (eq t (ctp:require-capabilities bk :mm)))
    (is (handler-case (progn (ctp:require-capabilities bk '(:mm :mul) :context :test) nil)
          (ctp:not-implemented (e)
            (and (equal '(:mul) (ctp::not-implemented-missing-capabilities e))
                 (eql :test (ctp::not-implemented-context e))))))))


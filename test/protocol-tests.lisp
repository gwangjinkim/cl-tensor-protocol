;;;; test/protocol-tests.lisp — placeholder file to satisfy ASDF (M0)

(in-package #:cl-tensor-protocol/test)

;; Future protocol tests will live here in later milestones.

;;; M1 — conditions + dtype + base classes

(in-suite :cl-tensor-protocol/test)

(test m1-classes-exist
  (is (find-class 'ctp:backend nil))
  (is (find-class 'ctp:tensor nil)))

(test m1-conditions-signal
  (is (handler-case (progn (error 'ctp:shape-error) nil)
        (ctp:shape-error () t)))
  (is (handler-case (progn (error 'ctp:dtype-error) nil)
        (ctp:dtype-error () t)))
  (is (handler-case (progn (error 'ctp:backend-error) nil)
        (ctp:backend-error () t)))
  (is (handler-case (progn (error 'ctp:not-implemented :backend :cpu :missing-capabilities '(:mm) :context "m1") nil)
        (ctp:not-implemented () t))))

(test m1-not-implemented-slots
  (let ((c (make-condition 'ctp:not-implemented
                           :backend :cpu
                           :missing-capabilities '(:mm)
                           :context "m1")))
    (is (eql :cpu (ctp::not-implemented-backend c)))
    (is (equal '(:mm) (ctp::not-implemented-missing-capabilities c)))
    (is (string= "m1" (ctp::not-implemented-context c)))))

(test m1-dtype-set
  (is (every #'keywordp ctp::+dtypes+))
  (is (find :float32 ctp::+dtypes+))
  (is (find :int32 ctp::+dtypes+)))

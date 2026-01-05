;;;; test/suite.lisp — trivial M0 tests

(in-package #:cl-tensor-protocol/test)

(def-suite :cl-tensor-protocol/test)

(in-suite :cl-tensor-protocol/test)

(test sanity
  "Trivial M0 sanity test"
  (is (= 2 (+ 1 1))))

(defun run-tests ()
  (run! :cl-tensor-protocol/test))


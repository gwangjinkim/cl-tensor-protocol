(defpackage #:cl-tensor-backend-cuda/test
  (:use #:cl #:fiveam)
  (:local-nicknames (:ctp #:cl-tensor-protocol))
  (:export #:run-tests))

(in-package #:cl-tensor-backend-cuda/test)

(def-suite :cl-tensor-backend-cuda/test)
(in-suite :cl-tensor-backend-cuda/test)

(defun cuda-available-p ()
  (ignore-errors (and (find-package :cl-cuda) t)))

(defmacro skip (reason)
  `(progn (format t "~&[skip] ~a~%" ,reason) t))

(defun run-tests ()
  (run! :cl-tensor-backend-cuda/test))

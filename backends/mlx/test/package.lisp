(defpackage #:cl-tensor-backend-mlx/test
  (:use #:cl #:fiveam)
  (:local-nicknames (:ctp #:cl-tensor-protocol))
  (:export #:run-tests))

(in-package #:cl-tensor-backend-mlx/test)

(def-suite :cl-tensor-backend-mlx/test)
(in-suite :cl-tensor-backend-mlx/test)

(defun mlx-available-p ()
  (ignore-errors (and (find-package :mlx-cl) t)))

(defun run-tests ()
  (run! :cl-tensor-backend-mlx/test))


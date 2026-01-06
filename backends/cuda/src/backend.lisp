(in-package #:cl-tensor-backend-cuda)

(defclass cuda-backend (ctp:backend) ())

(defun cuda-available-p ()
  (ignore-errors (and (find-package :cl-cuda) t)))

(defparameter *cuda-backend* (make-instance 'cuda-backend))

(defmethod ctp:device-type ((bk cuda-backend)) :cuda)
(defmethod ctp:backend-name ((bk cuda-backend)) "cuda")

;; M12: Only Tier A capabilities are declared
(defmethod ctp:capabilities ((bk cuda-backend))
  '(:tensor-from :to-array :shape))

(defun register-cuda-backend ()
  (when (cuda-available-p)
    (ctp:register-backend *cuda-backend* :priority -20)))

(eval-when (:load-toplevel :execute)
  (register-cuda-backend))


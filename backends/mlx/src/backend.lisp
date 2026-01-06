(in-package #:cl-tensor-backend-mlx)

(defclass mlx-backend (ctp:backend) ())

(defun mlx-available-p ()
  (ignore-errors
    ;; If mlx-cl is present, this call/constant should be available. Best-effort check.
    (and (find-package :mlx-cl) t)))

(defparameter *mlx-backend* (make-instance 'mlx-backend))

(defmethod ctp:device-type ((bk mlx-backend)) :mlx)
(defmethod ctp:backend-name ((bk mlx-backend)) "mlx")

(defmethod ctp:capabilities ((bk mlx-backend))
  '(:tensor-from :to-array :shape :add :mul :mm))

(defun register-mlx-backend ()
  (when (mlx-available-p)
    (ctp:register-backend *mlx-backend* :priority -10)))

(eval-when (:load-toplevel :execute)
  (register-mlx-backend))

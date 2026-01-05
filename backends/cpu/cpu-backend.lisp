;;;; backends/cpu/cpu-backend.lisp — minimal CPU backend (M3)

(in-package #:cl-tensor-backend-cpu)

(defclass cpu-backend (cl-tensor-protocol:backend) ())

(defmethod cl-tensor-protocol:device-type ((bk cpu-backend)) :cpu)
(defmethod cl-tensor-protocol:backend-name ((bk cpu-backend)) "cpu")

;; For M3, declare at least :mm capability (per acceptance)
(defmethod cl-tensor-protocol:capabilities ((bk cpu-backend))
  '(:tensor-from :to-array :shape :reshape :transpose :slice :add :mul :mm :copy :as-dtype))

(defparameter *cpu-backend-instance* (make-instance 'cpu-backend))

(defun ensure-cpu-registered ()
  (cl-tensor-protocol:register-backend *cpu-backend-instance* :priority -100))

;; Register on load
(eval-when (:load-toplevel :execute)
  (ensure-cpu-registered))

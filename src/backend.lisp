;;;; src/backend.lisp — base backend class (M1)

(in-package #:cl-tensor-protocol)

(defclass backend ()
  ((name :initarg :name :reader backend-name :initform "unknown")
   (device :initarg :device :reader device-type :initform :unknown)
   (features :initarg :features :reader features :initform nil))
  (:documentation "Abstract backend descriptor with device type and features."))


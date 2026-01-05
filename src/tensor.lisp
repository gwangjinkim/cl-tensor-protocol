;;;; src/tensor.lisp — base tensor class + required generics (M1 minimal)

(in-package #:cl-tensor-protocol)

(defclass tensor ()
  ()
  (:documentation "Abstract tensor; concrete backends subclass and add storage."))

;; Required generics (no methods here yet; backends will implement)
(defgeneric backend-of (tensor)
  (:documentation "Return the backend instance that owns this tensor."))

(defgeneric dtype (tensor)
  (:documentation "Return the dtype keyword of the tensor."))

(defgeneric shape (tensor)
  (:documentation "Return the tensor shape as a simple-vector of dims."))


;;;; src/protocol.lisp — protocol generics per ABI (used incrementally)

(in-package #:cl-tensor-protocol)

;; Creation / conversion (M4 uses these; other ops added later)
(defgeneric tensor (backend object &key dtype order)
  (:documentation "Create a protocol tensor on BACKEND from OBJECT.
OBJECT is a scalar or CL array; DTYPE is a keyword; ORDER may be :c/:f/:auto."))

(defgeneric to-array (tensor &key copy element-type)
  (:documentation "Return a CL array or scalar view/copy of TENSOR."))

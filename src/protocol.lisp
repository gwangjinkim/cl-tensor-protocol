;;;; src/protocol.lisp — protocol generics per ABI (used incrementally)

(in-package #:cl-tensor-protocol)

;; Creation / conversion (M4 uses these; other ops added later)
(defgeneric tensor (backend object &key dtype order)
  (:documentation "Create a protocol tensor on BACKEND from OBJECT.
OBJECT is a scalar or CL array; DTYPE is a keyword; ORDER may be :c/:f/:auto."))

(defgeneric to-array (tensor &key copy element-type)
  (:documentation "Return a CL array or scalar view/copy of TENSOR."))

;; Shape ops (M5)
(defgeneric reshape (tensor new-shape &key copy)
  (:documentation "Return TENSOR reshaped to NEW-SHAPE (simple-vector)."))

(defgeneric transpose (tensor axes)
  (:documentation "Return TENSOR transposed according to AXES permutation."))

(defgeneric slice (tensor spec)
  (:documentation "Return a view/slice according to SPEC per 4.6 spec."))

;; Math ops (M6)
(defgeneric add (x y)
  (:documentation "Elementwise addition. Supports tensor⊗tensor (same backend), tensor⊗scalar, scalar⊗tensor."))

(defgeneric mul (x y)
  (:documentation "Elementwise multiplication. Supports tensor⊗tensor (same backend), tensor⊗scalar, scalar⊗tensor."))

(defgeneric mm (a b)
  (:documentation "Matrix multiply for rank-2 tensors on same backend."))

;;;; src/dtype.lisp — dtype set (M1)

(in-package #:cl-tensor-protocol)

;; Minimal dtype universe for v0.1 (documented here for M1 tests)
(defparameter +dtypes+
  '(:bool :int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64
    :float16 :bfloat16 :float32 :float64)
  "Supported dtype keywords for protocol v0.1.")


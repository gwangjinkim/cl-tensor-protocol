;;;; backends/cpu/package.lisp — package for CPU backend

(defpackage #:cl-tensor-backend-cpu
  (:use #:cl)
  (:import-from #:cl-tensor-protocol
    #:backend #:tensor #:to-array #:dtype #:shape #:backend-of
    #:register-backend #:device-type #:backend-name
    #:capabilities #:supports? #:missing-capabilities #:require-capabilities)
  (:export #:ensure-cpu-registered #:cpu-backend #:cpu-tensor))

(in-package #:cl-tensor-backend-cpu)

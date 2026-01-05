;;;; backends/cpu/package.lisp — package for CPU backend

(defpackage #:cl-tensor-backend-cpu
  (:use #:cl)
  (:import-from #:cl-tensor-protocol
    #:backend #:register-backend #:device-type #:backend-name
    #:capabilities #:supports? #:missing-capabilities #:require-capabilities)
  (:export #:ensure-cpu-registered))

(in-package #:cl-tensor-backend-cpu)


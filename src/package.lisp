;;;; src/package.lisp — base package for cl-tensor-protocol (M0 skeleton)

(defpackage #:cl-tensor-protocol
  (:use #:cl)
  (:shadow #:error)
  (:nicknames #:ctp)
  (:export
   ;; Classes
   #:backend #:tensor
   ;; Conditions
   #:error #:shape-error #:dtype-error #:backend-error #:not-implemented))

(in-package #:cl-tensor-protocol)

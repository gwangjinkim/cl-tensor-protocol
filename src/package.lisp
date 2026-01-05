;;;; src/package.lisp — base package for cl-tensor-protocol (M0 skeleton)

(defpackage #:cl-tensor-protocol
  (:use #:cl)
  (:shadow #:error)
  (:nicknames #:ctp)
  (:export
   ;; Classes
   #:backend #:tensor
   ;; Conditions
   #:error #:shape-error #:dtype-error #:backend-error #:not-implemented
   ;; Capabilities API
   #:capabilities #:supports? #:missing-capabilities #:require-capabilities
   ;; Registry
   #:register-backend #:available-backends #:find-backend #:default-backend
   ;; Base accessors
   #:backend-name #:device-type #:features
   ;; Tensor introspection + creation/conversion
   #:backend-of #:dtype #:shape #:rank #:size
   #:tensor #:to-array
   ;; Shape ops
   #:reshape #:transpose #:slice))

(in-package #:cl-tensor-protocol)

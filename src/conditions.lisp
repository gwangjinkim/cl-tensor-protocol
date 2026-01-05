;;;; src/conditions.lisp — protocol conditions (M1)

(in-package #:cl-tensor-protocol)

(define-condition error (condition)
  ()
  (:documentation "Base protocol error (do not use CL:ERROR directly)."))

(define-condition shape-error (error)
  ()
  (:documentation "Signaled on shape mismatches or invalid shapes."))

(define-condition dtype-error (error)
  ()
  (:documentation "Signaled on invalid dtype usage or conversions."))

(define-condition backend-error (error)
  ()
  (:documentation "Signaled on invalid cross-backend operations or backend failures."))

(define-condition not-implemented (error)
  ((backend :initarg :backend :reader not-implemented-backend)
   (missing-capabilities :initarg :missing-capabilities :reader not-implemented-missing-capabilities)
   (context :initarg :context :reader not-implemented-context))
  (:report (lambda (c s)
             (format s "Capability not implemented for ~a; missing ~a (~a)"
                     (not-implemented-backend c)
                     (not-implemented-missing-capabilities c)
                     (not-implemented-context c))))
  (:documentation "Signaled when a requested operation/capability is not implemented by a backend."))


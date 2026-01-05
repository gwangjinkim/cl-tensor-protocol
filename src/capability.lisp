;;;; src/capability.lisp — capability model + helpers (M2)

(in-package #:cl-tensor-protocol)

;; Capability keywords (documented values per AGENTS.md v0.1).
(defparameter +capabilities+
  '(:tensor-from :to-array :copy :as-dtype :shape :reshape :transpose :slice :add :mul :mm))

;; Default method: backends may specialize to declare support.
(defgeneric capabilities (backend)
  (:documentation "Return list of capability keywords implemented by BACKEND."))

(defmethod capabilities ((bk backend))
  '())

(defun supports? (backend capability)
  "Return T if BACKEND declares CAPABILITY."
  (member capability (capabilities backend)))

(defun missing-capabilities (backend caps)
  "Return a list of capabilities from CAPS that BACKEND does not support.
CAPS may be a single keyword or a list of keywords."
  (let ((req (if (listp caps) caps (list caps))))
    (loop for c in req unless (supports? backend c) collect c))
  )

(defun require-capabilities (backend caps &key context)
  "Ensure BACKEND supports CAPS; otherwise signal NOT-IMPLEMENTED with slots."
  (let ((missing (missing-capabilities backend caps)))
    (if (null missing)
        t
        (cl:error 'not-implemented :backend backend :missing-capabilities missing :context context))))

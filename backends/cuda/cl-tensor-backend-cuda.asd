(asdf:defsystem "cl-tensor-backend-cuda"
  :description "CUDA backend for cl-tensor-protocol (wraps cl-cuda)"
  :author "You"
  :license "MIT"
  :depends-on ("cl-tensor-protocol" "cl-cuda")
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "backend")
     (:file "tensor")
     (:file "ops")))) )

(asdf:defsystem "cl-tensor-backend-cuda/test"
  :depends-on ("cl-tensor-backend-cuda" "fiveam")
  :serial t
  :components
  ((:module "test"
    :components
    ((:file "package")
     (:file "smoke")))))

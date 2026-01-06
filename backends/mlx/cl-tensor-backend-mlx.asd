(asdf:defsystem "cl-tensor-backend-mlx"
  :description "MLX backend for cl-tensor-protocol (wraps mlx-cl)"
  :author "You"
  :license "MIT"
  :depends-on ("cl-tensor-protocol" "mlx-cl")
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "backend")
     (:file "tensor")
     (:file "ops")))) )

(asdf:defsystem "cl-tensor-backend-mlx/test"
  :depends-on ("cl-tensor-backend-mlx" "fiveam")
  :serial t
  :components
  ((:module "test"
    :components
    ((:file "package")
     (:file "smoke")))))

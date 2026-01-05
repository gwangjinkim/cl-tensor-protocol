(asdf:defsystem "cl-tensor-protocol"
  :description "Tiny, stable tensor backend protocol (CPU/Metal/CUDA adapters live elsewhere)"
  :author "You"
  :license "MIT"
  :depends-on ()
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "conditions")
     (:file "dtype")
     (:file "backend")
     (:file "tensor")
     (:file "registry")
     (:file "util")
     (:file "capabilities")
     (:file "protocol")))))

(asdf:defsystem "cl-tensor-protocol/test"
  :depends-on ("cl-tensor-protocol" "fiveam")
  :serial t
  :components
  ((:module "test"
    :components
    ((:file "package")
     (:file "suite")
     (:file "protocol-tests"))))
  :perform (test-op (o c)
             (uiop:symbol-call :fiveam :run! :cl-tensor-protocol/test)))

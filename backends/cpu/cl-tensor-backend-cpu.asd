;;;; backends/cpu/cl-tensor-backend-cpu.asd — CPU reference backend (M3 minimal)

(asdf:defsystem "cl-tensor-backend-cpu"
  :description "CPU reference backend for cl-tensor-protocol"
  :author "You"
  :license "MIT"
  :depends-on ("cl-tensor-protocol")
  :serial t
  :components
  ((:file "package")
   (:file "cpu-backend")
   (:file "cpu-tensor")
   (:file "cpu-ops")))

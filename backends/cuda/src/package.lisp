(defpackage #:cl-tensor-backend-cuda
  (:use #:cl)
  (:local-nicknames
   (:ctp #:cl-tensor-protocol)
   (:cu  #:cl-cuda))
  (:export #:cuda-available-p #:register-cuda-backend #:*cuda-backend*))

(in-package #:cl-tensor-backend-cuda)


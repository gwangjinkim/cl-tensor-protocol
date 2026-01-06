(defpackage #:cl-tensor-backend-mlx
  (:use #:cl)
  (:local-nicknames
   (:ctp #:cl-tensor-protocol)
   (:mx  #:mlx-cl)
   (:mxla #:mlx-cl.linalg))
  (:export #:register-mlx-backend #:mlx-available-p #:*mlx-backend*))

(in-package #:cl-tensor-backend-mlx)


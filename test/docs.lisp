;;;; test/docs.lisp — M8 docs existence and content sanity

(in-package #:cl-tensor-protocol/test)

(in-suite :cl-tensor-protocol/test)

(defun %read-file (path)
  (uiop:read-file-string path))

(test m8-docs-mlx-exist-and-mention-hygiene
  (let* ((p #P"docs/adapter-mlx-cl.md"))
    (is (probe-file p))
    (let ((s (%read-file p)))
      (is (search "mlx-cl" s))
      (is (or (search "Do NOT :use :mlx-cl" s)
              (search "Do NOT` :use :mlx-cl" s)
              (search "Do NOT` :use :mlx-cl" s)))
      (is (search "local-nicknames" s))
      (is (search "mm" s)))))

(test m8-docs-cuda-exist-and-mention-minimal-path
  (let* ((p #P"docs/adapter-cl-cuda.md"))
    (is (probe-file p))
    (let ((s (%read-file p)))
      (is (search "cl-cuda" s))
      (is (search "Tier A first" s))
      (is (or (search ":add/:mul" s)
              (and (search ":add" s) (search ":mul" s))))))
  t)


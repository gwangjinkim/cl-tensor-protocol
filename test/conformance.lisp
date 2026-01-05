;;;; test/conformance.lisp — Tiered conformance runner (M7)

(in-package #:cl-tensor-protocol/test)

(defun %run-if (bk cap thunk)
  (if (ctp:supports? bk cap)
      (funcall thunk)
      (format t "~&[skip] ~a not supported; skipping tests.~%" cap)))

(defun run-conformance (backend &key tiers)
  "Run conformance tests for BACKEND. TIERS can be a list among :A :B :C :D.
Runs only tests for capabilities the backend declares, and verifies that if a
capability is declared, corresponding tests run and pass."
  (let* ((tiers (or tiers '(:A :B :C :D)))
         (caps (ctp:capabilities backend)))
    (labels ((run-tier-a ()
               ;; :tensor-from :to-array :shape
               (when (every (lambda (c) (member c caps)) '(:tensor-from :to-array :shape))
                 (test m7-tier-a
                   (let* ((a (make-array '(1 1)))
                          (t (ctp:tensor backend a :dtype :int32)))
                     (is (equalp #(1 1) (ctp:shape t)))
                     (is (equalp a (ctp:to-array t)))))))
             (run-tier-b ()
               (when (every (lambda (c) (member c caps)) '(:reshape :transpose :slice))
                 (test m7-tier-b
                   (let* ((a (make-array '(2 3)))
                          (t (ctp:tensor backend a :dtype :int32)))
                     (is (equalp #(3 2) (ctp:shape (ctp:reshape t #(3 2))))
                         "reshape should work")
                     (is (equalp #(3 2) (ctp:shape (ctp:transpose t #(1 0))))
                         "transpose should work")
                     (let ((sl (ctp:slice t '((:range 0 1 1) (:range 0 1 1)))))
                       (is (equalp #(1 1) (ctp:shape sl)))))))
             (run-tier-c ()
               (when (every (lambda (c) (member c caps)) '(:add :mul :mm))
                 (test m7-tier-c
                   (let* ((a (make-array '(2 2) :initial-contents '((1 2) (3 4))))
                          (b (make-array '(2 2) :initial-contents '((10 20) (30 40))))
                          (ta (ctp:tensor backend a :dtype :int32))
                          (tb (ctp:tensor backend b :dtype :int32)))
                     (is (equalp (make-array '(2 2) :initial-contents '((11 22) (33 44)))
                                 (ctp:to-array (ctp:add ta tb))))
                     (is (equalp (make-array '(2 2) :initial-contents '((10 40) (90 160)))
                                 (ctp:to-array (ctp:mul ta tb))))
                     (let* ((a2 (let ((x (make-array '(2 3))))
                                  (setf (aref x 0 0) 1 (aref x 0 1) 2 (aref x 0 2) 3
                                        (aref x 1 0) 4 (aref x 1 1) 5 (aref x 1 2) 6)
                                  x))
                            (b2 (let ((x (make-array '(3 2))))
                                  (setf (aref x 0 0) 7 (aref x 0 1) 8
                                        (aref x 1 0) 9 (aref x 1 1) 10
                                        (aref x 2 0) 11 (aref x 2 1) 12)
                                  x))
                            (ta2 (ctp:tensor backend a2 :dtype :int32))
                            (tb2 (ctp:tensor backend b2 :dtype :int32))
                            (tc (ctp:mm ta2 tb2)))
                       (is (equalp (make-array '(2 2) :initial-contents '((58 64) (139 154)))
                                   (ctp:to-array tc)))))))
             (run-tier-d ()
               (when (every (lambda (c) (member c caps)) '(:copy :as-dtype))
                 (test m7-tier-d
                   (let* ((a (let ((x (make-array '(2 2))))
                                (setf (aref x 0 0) 1 (aref x 0 1) 2
                                      (aref x 1 0) 3 (aref x 1 1) 4)
                                x))
                          (t1 (ctp:tensor backend a :dtype :int32))
                          (t2 (ctp:copy t1))
                          (t3 (ctp:as-dtype t1 :float64)))
                     (is (equalp (ctp:shape t1) (ctp:shape t2)))
                     (is (not (eq (ctp:to-array t1) (ctp:to-array t2))))
                     (is (eql :float64 (ctp:dtype t3))))))))
      (dolist (tier tiers)
        (ecase tier
          (:A (run-tier-a))
          (:B (run-tier-b))
          (:C (run-tier-c))
          (:D (run-tier-d))))
      ;; run tests for this conformance invocation
      (run! :cl-tensor-protocol/test)))


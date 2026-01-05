;;;; test/cpu-backend.lisp — M3 registry + CPU backend registration tests

(in-package #:cl-tensor-protocol/test)

(in-suite :cl-tensor-protocol/test)

(test m3-default-backend-returns-cpu
  (let ((bk (ctp:default-backend)))
    (is (typep bk 'ctp:backend))
    (is (eql :cpu (ctp:device-type bk)))))

(test m3-require-mm-succeeds
  (let ((bk (ctp:default-backend :require '(:mm))))
    (is (typep bk 'ctp:backend))
    (is (eql :cpu (ctp:device-type bk)))))

(test m3-registry-helpers
  (let ((all (ctp:available-backends))
        (cpu (ctp:find-backend :cpu)))
    (is (plusp (length all)))
    (is (typep cpu 'ctp:backend))
    (is (eql :cpu (ctp:device-type cpu)))))

;;; M4 — CPU tensor creation + to-array + shape

(test m4-scalar-roundtrip
  (let* ((bk (ctp:default-backend))
         (tx (ctp:tensor bk 42 :dtype :int32)))
    (is (typep tx 'ctp:tensor))
    (is (equalp #() (ctp:shape tx)))
    (is (eql :int32 (ctp:dtype tx)))
    (is (= 42 (ctp:to-array tx)))))

(test m4-2d-array-roundtrip
  (let* ((bk (ctp:default-backend))
         (a (make-array '(2 3))) )
    (setf (aref a 0 0) 1
          (aref a 0 1) 2
          (aref a 0 2) 3
          (aref a 1 0) 4
          (aref a 1 1) 5
          (aref a 1 2) 6)
    (let ((tx (ctp:tensor bk a :dtype :float64)))
      (is (typep tx 'ctp:tensor))
      (let ((sh (ctp:shape tx)))
        (is (typep sh 'simple-vector))
        (is (equalp #(2 3) sh))
        (is (loop for d across sh always (typep d 'fixnum))))
      (is (eql :float64 (ctp:dtype tx)))
      (is (equalp a (ctp:to-array tx))))))

;;; M5 — CPU shape ops: reshape/transpose/slice

(test m5-reshape-correctness
  (let* ((bk (ctp:default-backend))
         (a (make-array '(2 3))) )
    (setf (aref a 0 0) 1
          (aref a 0 1) 2
          (aref a 0 2) 3
          (aref a 1 0) 4
          (aref a 1 1) 5
          (aref a 1 2) 6)
    (let* ((tx (ctp:tensor bk a :dtype :int32))
           (r (ctp:reshape tx #(3 2)))
           (b (ctp:to-array r)))
      (is (equalp #(3 2) (ctp:shape r)))
      (let ((exp (make-array '(3 2))))
        (setf (aref exp 0 0) 1 (aref exp 0 1) 2
              (aref exp 1 0) 3 (aref exp 1 1) 4
              (aref exp 2 0) 5 (aref exp 2 1) 6)
        (is (equalp exp b))))))

(test m5-reshape-mismatch
  (let* ((bk (ctp:default-backend))
         (a (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6))))
         (tx (ctp:tensor bk a :dtype :int32)))
    (is (handler-case (progn (ctp:reshape tx #(4 2)) nil)
          (ctp:shape-error () t)))))

(test m5-transpose-correctness
  (let* ((bk (ctp:default-backend))
         (a (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6))))
         (tx (ctp:tensor bk a :dtype :int32))
         (tr (ctp:transpose tx #(1 0)))
         (b (ctp:to-array tr)))
    (is (equalp #(3 2) (ctp:shape tr)))
    (let ((exp (make-array '(3 2) :initial-contents '((1 4) (2 5) (3 6)))))
      (is (equalp exp b)))))

(test m5-slice-correctness
  (let* ((bk (ctp:default-backend))
         (a (let ((arr (make-array '(4 5))))
              (let ((v 0))
                (dotimes (i (array-dimension arr 0))
                  (dotimes (j (array-dimension arr 1))
                    (setf (aref arr i j) v)
                    (incf v))))
              arr))
         (tx (ctp:tensor bk a :dtype :int32)))
    ;; slice rows 1..2, cols 0,2,4
    (let* ((sl (ctp:slice tx '((:range 1 3 1) (:range 0 5 2))))
           (b (ctp:to-array sl)))
      (is (equalp #(2 3) (ctp:shape sl)))
      (let ((exp (make-array '(2 3))))
        (setf (aref exp 0 0) 5  (aref exp 0 1) 7  (aref exp 0 2) 9
              (aref exp 1 0) 10 (aref exp 1 1) 12 (aref exp 1 2) 14)
        (is (equalp exp b))))
    ;; slice with :all on first axis
    (let* ((sl2 (ctp:slice tx '((:all) (:range 1 4 2))))
           (b2 (ctp:to-array sl2)))
      (is (equalp #(4 2) (ctp:shape sl2)))
      (let ((exp2 (make-array '(4 2) :initial-contents '((1 3) (6 8) (11 13) (16 18)))))
        (is (equalp exp2 b2))))))

(test m5-slice-mismatch
  (let* ((bk (ctp:default-backend))
         (a (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6))))
         (tx (ctp:tensor bk a :dtype :int32)))
    (is (handler-case (progn (ctp:slice tx '((:all))) nil)
          (ctp:shape-error () t)))))

;;; M6 — CPU math ops: add/mul/mm

(test m6-add-scalar-and-tensor
  (let* ((bk (ctp:default-backend))
         (a (make-array '(2 2) :initial-contents '((1 2) (3 4))))
         (tx (ctp:tensor bk a :dtype :int32))
         (rz (ctp:add tx 1))
         (lz (ctp:add 1 tx)))
    (is (equalp #(2 2) (ctp:shape rz)))
    (is (equalp (make-array '(2 2) :initial-contents '((2 3) (4 5))) (ctp:to-array rz)))
    (is (equalp (ctp:to-array rz) (ctp:to-array lz)))))

(test m6-add-tensor-tensor
  (let* ((bk (ctp:default-backend))
         (a (make-array '(2 2) :initial-contents '((1 2) (3 4))))
         (b (make-array '(2 2) :initial-contents '((10 20) (30 40))))
         (ta (ctp:tensor bk a :dtype :int32))
         (tb (ctp:tensor bk b :dtype :int32))
         (tc (ctp:add ta tb)))
    (is (equalp #(2 2) (ctp:shape tc)))
    (is (equalp (make-array '(2 2) :initial-contents '((11 22) (33 44))) (ctp:to-array tc)))))

(test m6-add-mismatch
  (let* ((bk (ctp:default-backend))
         (a (make-array '(2 2) :initial-contents '((1 2) (3 4))))
         (b (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6))))
         (ta (ctp:tensor bk a :dtype :int32))
         (tb (ctp:tensor bk b :dtype :int32)))
    (is (handler-case (progn (ctp:add ta tb) nil)
          (ctp:shape-error () t)))))

(test m6-mul-scalar-and-tensor
  (let* ((bk (ctp:default-backend))
         (a (make-array '(2 2) :initial-contents '((1 2) (3 4))))
         (tx (ctp:tensor bk a :dtype :int32))
         (rz (ctp:mul tx 2))
         (lz (ctp:mul 2 tx)))
    (is (equalp #(2 2) (ctp:shape rz)))
    (is (equalp (make-array '(2 2) :initial-contents '((2 4) (6 8))) (ctp:to-array rz)))
    (is (equalp (ctp:to-array rz) (ctp:to-array lz)))))

(test m6-mm-correctness
  (let* ((bk (ctp:default-backend))
         (a (let ((x (make-array '(2 3))))
              (setf (aref x 0 0) 1 (aref x 0 1) 2 (aref x 0 2) 3
                    (aref x 1 0) 4 (aref x 1 1) 5 (aref x 1 2) 6)
              x))
         (b (let ((x (make-array '(3 2))))
              (setf (aref x 0 0) 7 (aref x 0 1) 8
                    (aref x 1 0) 9 (aref x 1 1) 10
                    (aref x 2 0) 11 (aref x 2 1) 12)
              x))
         (ta (ctp:tensor bk a :dtype :int32))
         (tb (ctp:tensor bk b :dtype :int32))
         ;; Force arrays to ensure cpu-tensor paths; these are already arrays.
         (tc (ctp:mm ta tb)))
    (is (equalp #(2 2) (ctp:shape tc)))
    (is (equalp (make-array '(2 2) :initial-contents '((58 64) (139 154))) (ctp:to-array tc)))))

(test m6-mm-mismatch
  (let* ((bk (ctp:default-backend))
         (a (make-array '(2 3)))
         (b (make-array '(4 2)))
         (ta (ctp:tensor bk a :dtype :int32))
         (tb (ctp:tensor bk b :dtype :int32)))
    (is (handler-case (progn (ctp:mm ta tb) nil)
          (ctp:shape-error () t)))))

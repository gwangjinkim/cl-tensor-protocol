;;;; test/conformance.lisp — Tiered conformance runner (M7)

(in-package #:cl-tensor-protocol/test)

(defun run-conformance (backend &key tiers)
  "Run conformance checks for BACKEND. TIERS subset of :A :B :C :D.
Returns T on success; signals CTP:ERROR on failure.
Runs only checks for capabilities the backend declares. If a capability is
declared, corresponding checks must pass."
  (let* ((tiers (or tiers '(:A :B :C :D)))
         (caps (ctp:capabilities backend)))
    (labels ((assert-equal (x y)
               (unless (equalp x y)
                 (error 'ctp:error)))
             (rank-2-mat (rows cols init-list)
               (let ((arr (make-array (list rows cols))))
                 (loop for i below rows do
                       (loop for j below cols do
                             (setf (aref arr i j) (pop init-list))))
                 arr))
             (check-cap (cap fn)
               (when (member cap caps)
                 (funcall fn))))
      (dolist (tier tiers)
        (ecase tier
          (:A ;; per-capability checks
           (check-cap :tensor-from
             (lambda ()
               (let* ((a (make-array '(1 1)))
                      (t (ctp:tensor backend a :dtype :int32)))
                 (assert-equal #(1 1) (ctp:shape t)))))
           (check-cap :to-array
             (lambda ()
               (let* ((a (make-array '(1 1)))
                      (t (ctp:tensor backend a :dtype :int32)))
                 (assert-equal a (ctp:to-array t)))))
           (check-cap :shape
             (lambda ()
               (let* ((a (make-array '(2 3)))
                      (t (ctp:tensor backend a :dtype :int32)))
                 (assert-equal #(2 3) (ctp:shape t))))))
          (:B
           (check-cap :reshape
             (lambda ()
               (let* ((a (make-array '(2 3)))
                      (t (ctp:tensor backend a :dtype :int32)))
                 (assert-equal #(3 2) (ctp:shape (ctp:reshape t #(3 2)))))))
           (check-cap :transpose
             (lambda ()
               (let* ((a (make-array '(2 3)))
                      (t (ctp:tensor backend a :dtype :int32)))
                 (assert-equal #(3 2) (ctp:shape (ctp:transpose t #(1 0)))))))
           (check-cap :slice
             (lambda ()
               (let* ((a (make-array '(2 3)))
                      (t (ctp:tensor backend a :dtype :int32))
                      (sl (ctp:slice t '((:range 0 1 1) (:range 0 1 1)))))
                 (assert-equal #(1 1) (ctp:shape sl))))))
          (:C
           (check-cap :add
             (lambda ()
               (let* ((a (make-array '(2 2) :initial-contents '((1 2) (3 4))))
                      (b (make-array '(2 2) :initial-contents '((10 20) (30 40))))
                      (ta (ctp:tensor backend a :dtype :int32))
                      (tb (ctp:tensor backend b :dtype :int32)))
                 (assert-equal (make-array '(2 2) :initial-contents '((11 22) (33 44)))
                               (ctp:to-array (ctp:add ta tb))))))
           (check-cap :mul
             (lambda ()
               (let* ((a (make-array '(2 2) :initial-contents '((1 2) (3 4))))
                      (b (make-array '(2 2) :initial-contents '((10 20) (30 40))))
                      (ta (ctp:tensor backend a :dtype :int32))
                      (tb (ctp:tensor backend b :dtype :int32)))
                 (assert-equal (make-array '(2 2) :initial-contents '((10 40) (90 160)))
                               (ctp:to-array (ctp:mul ta tb))))))
           (check-cap :mm
             (lambda ()
               (let* ((a (rank-2-mat 2 3 '(1 2 3 4 5 6)))
                      (b (rank-2-mat 3 2 '(7 8 9 10 11 12)))
                      (ta (ctp:tensor backend a :dtype :int32))
                      (tb (ctp:tensor backend b :dtype :int32))
                      (tc (ctp:mm ta tb)))
                 (assert-equal (make-array '(2 2) :initial-contents '((58 64) (139 154)))
                               (ctp:to-array tc))))))
          (:D
           (check-cap :copy
             (lambda ()
               (let* ((a (make-array '(2 2)))
                      (t1 (ctp:tensor backend a :dtype :int32))
                      (t2 (ctp:copy t1)))
                 (assert-equal (ctp:shape t1) (ctp:shape t2))
                 (unless (eq (ctp:to-array t1) (ctp:to-array t2))
                   t))) )
           (check-cap :as-dtype
             (lambda ()
               (let* ((a (make-array '(1 1)))
                      (t1 (ctp:tensor backend a :dtype :int32))
                      (t2 (ctp:as-dtype t1 :float64)))
                 (assert-equal (ctp:shape t1) (ctp:shape t2))
                 t))))))
      t)))

;;; Fake partial backend declaring only :mm (minimal implementation for conformance)

(defclass fake-mm-backend (ctp:backend) ())
(defmethod ctp:device-type ((bk fake-mm-backend)) :fake)
(defmethod ctp:backend-name ((bk fake-mm-backend)) "fake-mm")
(defmethod ctp:capabilities ((bk fake-mm-backend)) '(:mm))

(defclass fake-mm-tensor (ctp:tensor)
  ((%backend :initarg :backend :reader ctp:backend-of)
   (%dtype   :initarg :dtype   :reader ctp:dtype)
   (%shape   :initarg :shape   :reader ctp:shape)
   (%data    :initarg :data    :reader data)))

(defun %shape-of (obj)
  (cond ((arrayp obj) (apply #'vector (array-dimensions obj)))
        (t #())))

(defmethod ctp:tensor ((bk fake-mm-backend) object &key dtype order)
  (declare (ignore order))
  (let ((shape (%shape-of object)))
    (make-instance 'fake-mm-tensor
                   :backend bk
                   :dtype (or dtype :int32)
                   :shape shape
                   :data object)))

(defmethod ctp:to-array ((x fake-mm-tensor) &key copy element-type)
  (declare (ignore element-type))
  (let ((data (data x)))
    (if (and copy (arrayp data))
        (let* ((dims (array-dimensions data))
               (out (make-array dims)))
          (loop for i below (array-total-size data) do
                (setf (row-major-aref out i) (row-major-aref data i)))
          out)
        data)))

(defmethod ctp:mm ((a fake-mm-tensor) (b fake-mm-tensor))
  (let* ((sa (ctp:shape a))
         (sb (ctp:shape b)))
    (unless (and (= (length sa) 2) (= (length sb) 2))
      (error 'ctp:shape-error))
    (let* ((m (aref sa 0))
           (k (aref sa 1))
           (k2 (aref sb 0))
           (n (aref sb 1)))
      (unless (= k k2) (error 'ctp:shape-error))
      (let* ((A (ctp:to-array a))
             (B (ctp:to-array b))
             (C (make-array (list m n))))
        (dotimes (i m)
          (dotimes (j n)
            (let ((sum 0))
              (dotimes (p k)
                (incf sum (* (aref A i p) (aref B p j))))
              (setf (aref C i j) sum))))
        (make-instance 'fake-mm-tensor
                       :backend (ctp:backend-of a)
                       :dtype (ctp:dtype a)
                       :shape (vector m n)
                       :data C)))))

;;; Tests invoking the conformance runner

(in-suite :cl-tensor-protocol/test)

(test m7-conformance-cpu
  (let ((bk (ctp:default-backend)))
    (is (eq t (run-conformance bk :tiers '(:A :B :C :D))))))

(test m7-conformance-fake-mm
  (let ((bk (make-instance 'fake-mm-backend)))
    (is (eq t (run-conformance bk :tiers '(:A :B :C :D))))))


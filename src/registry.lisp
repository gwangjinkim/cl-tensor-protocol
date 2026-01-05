;;;; src/registry.lisp — backend registry + selection (M3)

(in-package #:cl-tensor-protocol)

(defvar *backend-registry* nil
  "List of (priority . backend) pairs for registered backends.")

(defun register-backend (backend &key (priority 0))
  "Register BACKEND with PRIORITY. Returns BACKEND."
  (push (cons priority backend) *backend-registry*)
  backend)

(defun available-backends ()
  "Return backends sorted by descending PRIORITY."
  (mapcar #'cdr (sort (copy-list *backend-registry*) #'> :key #'car)))

(defun find-backend (key)
  "Find a backend by KEY. If KEY is a keyword, matches DEVICE-TYPE.
If KEY is a string, matches BACKEND-NAME case-insensitively."
  (etypecase key
    (keyword (find key (available-backends)
                   :key #'device-type :test #'eql))
    (string (find key (available-backends)
                  :key #'backend-name :test #'string-equal))))

(defun %best-by-device (device-type)
  (find device-type (available-backends)
        :key #'device-type :test #'eql))

(defun default-backend (&key prefer require)
  "Select a backend using PREFER order and REQUIREd capabilities.
PREFER is a list of device-type keywords like (:cuda :mlx :metal :cpu).
REQUIRE is a capability keyword or list."
  (let* ((prefer (or prefer '(:cuda :mlx :metal :cpu)))
         (req    require))
    (dolist (dt prefer)
      (let ((bk (%best-by-device dt)))
        (when bk
          (when (or (null req)
                    (null (missing-capabilities bk req)))
            (return-from default-backend bk)))))
    ;; If no backend matches requirements, signal not-implemented
    (cl:error 'not-implemented
              :backend nil
              :missing-capabilities (if (listp req) req (and req (list req)))
              :context 'default-backend)))


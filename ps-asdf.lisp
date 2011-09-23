(in-package #:3b-ps-asdf)

(defclass asdf::3b-ps-file (cl-source-file)
  ())

(defmethod output-files :around ((operation compile-op) (c asdf::3b-ps-file))
  (mapcar (lambda (f)
            (format t "~&output-files = ~s~%" f)
            (princ (merge-pathnames (make-pathname :type "js") f)))
          (call-next-method)))

(defmethod perform ((operation compile-op) (c asdf::3b-ps-file))
  (let ((source-file (component-pathname c))
        (output-file (first (output-files operation c)))
        (*compile-file-warnings-behaviour* (operation-on-warnings operation))
        (*compile-file-failure-behaviour* (operation-on-failure operation)))
    (format t "~&compile ps file ~s to ~s~%" source-file output-file)
    (format t "  flags = ~s~%" (asdf::compile-op-flags operation))
    (with-open-file (ps:*parenscript-stream* output-file :direction :output
                                          :if-exists :supersede)
      (ps:ps-compile-file source-file))))

(defmethod perform ((operation load-op) (c asdf::3b-ps-file))
 (declare (ignorable operation c))
 ;; we need to LOAD the original file to get the compile-time side effects
 ;; like macro definitions etc...
 (let ((source-file (component-pathname c)))
   (format t "~&load ~s~%" source-file)
   (with-open-stream (ps:*parenscript-stream* (make-broadcast-stream))
     (ps:ps-compile-file source-file))
   (format t "~&load ~s done~%" source-file))
  nil)


(defun get-module-ps-files (module &optional path)
  (loop for c in (asdf:module-components module)
     when (typep c 'asdf:module)
     append (get-module-ps-files c (cons (asdf:component-name c) path))
     else when (typep c 'asdf::3b-ps-file)
     collect (list (reverse (cons (component-name c) path))
                   (first (output-files (make-instance 'compile-op) c)))))

(defun make-system-dispatcher (system-designators &key (base-dir "/")
                               default-version)
  (let ((systems (mapcar 'asdf:find-system
                         (alexandria:ensure-list system-designators)))
        (url-map (make-hash-table :test 'equal)))
    (loop for system in systems
       for version = (if (slot-boundp system 'version)
                         (asdf:component-version system)
                         default-version)
       for files = (get-module-ps-files system)
       do (loop for (file js-file) in files
             do (setf (gethash (format nil "~a~a/~@[~a/~]~{~a~^/~}.js"
                                       base-dir (component-name system) version
                                       file) url-map)
                      js-file)))
    (format t "serving files ~s~%" (alexandria:hash-table-plist url-map))
  (lambda (request)
    (let ((f (gethash (hunchentoot:script-name request) url-map)))
      (when f
        (lambda ()
          (hunchentoot:handle-static-file f "text/javascript")))))))


;;;; quicklisp-systems.lisp

(in-package #:quicklisp-systems)

(defparameter *quicklisp-projects-directory* #p"~/quicklisp-controller/")
(defvar *systems-info*)
(defvar *systems-file* (asdf:system-relative-pathname :quicklisp-systems "systems"))

(defun register-all-asdf-files (&optional (quicklisp-projects-directory *quicklisp-projects-directory*))
  (let ((output
          (with-output-to-string (s)
            (uiop/run-program:run-program
             (format nil "/usr/bin/find -P ~a -name *.asd"
                     (merge-pathnames #p"upstream-cache/" quicklisp-projects-directory))
             :output s))))
    (with-input-from-string (s output)
      (loop for line := (read-line s nil nil)
            while line
            when (and (probe-file line)
                      ;;(not (search "asdf" line :test 'equalp))
                      (not (search "cl-quakeinfo" line :test 'equalp))
                      (not (search "qt-libs" line :test 'equalp))
                      (not (search "cl-geocode" line :test 'equalp)))
              do
                 (format *standard-output* "Loading ~a" line)
                 (handler-case (progn
                                 (asdf/find-system:load-asd line)
                                 (format *standard-output* ". Success.~%"))
                   (error (e)
                     ;;(error e)
                     (format *standard-output* ". ERROR.~%")
                     ))))))

(defun serialize-asdf-systems (systems stream)
  (loop for system in systems
        do
        #+nil(prin1 (list `(:name ,(asdf:component-name system)
                            :description ,(asdf/component:component-description system)
                            :author ,(asdf/system:system-author system)
                            :maintainer ,(asdf/system:system-maintainer system)
                            :homepage ,(asdf/system:system-homepage system)
                            :bug-tracker ,(asdf/system:system-bug-tracker system)
                            :license ,(asdf/system:system-license system)))
                    stream)
             (prin1 `(:name ,(slot-value system 'asdf/component::name)
                      :description ,(asdf/component:component-description system)
                      :long-description ,(asdf/component:component-long-description system)
                      :author ,(slot-value system 'asdf/system::author)
                      :mailto ,(slot-value system 'asdf/system::mailto)
                      :maintainer ,(slot-value system 'asdf/system::maintainer)
                      :homepage ,(slot-value system 'asdf/system::homepage)
                      :bug-tracker ,(slot-value system 'asdf/system::bug-tracker)
                      :version ,(slot-value system 'asdf/system::version)
                      :license ,(slot-value system 'asdf/system::licence))
                    stream)
             (terpri stream)))

(defun write-systems-file (&optional (path *systems-file*))
  (with-open-file (f path :direction :output :external-format :utf-8
                          :if-exists :supersede)
    (serialize-asdf-systems (asdf/system-registry:registered-systems*)
                            f)))

(defun read-systems-file (&optional (path *systems-file*))
  (setq *systems-info*
        (with-open-file (f path :direction :input :external-format :utf-8)
          (loop for system := (read f nil nil)
                while system
                collect system))))

(defmacro do-systems ((system &optional (path *systems-file*)) &body body)
  (alexandria:with-gensyms (f)
    `(with-open-file (,f ,path :direction :input :external-format :utf-8)
       (loop for ,system := (read ,f nil nil)
             while ,system
             do ,@body))))

(defun find-system-info (name)
  (do-systems (system)
    (when (equalp (getf system :name) name)
      (return-from find-system-info system))))

(defun apropos-system (string &optional search-description)
  (let (systems)
    (do-systems (system)
      (when (or (search string (getf system :name) :test 'equalp)
                (and search-description
                     (or (and (getf system :description)
                              (search string (getf system :description) :test 'equalp))
                         (and (getf system :long-description)
                              (search string (getf system :long-description) :test 'equalp)))))
        (push system systems)))
    systems))

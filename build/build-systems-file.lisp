;; This is for buildling a systems-file (the file quicklisp-systems uses to read ASDF systems descriptions.)

;; How to build a systems file:

;; - Clone quicklisp-projects and quicklisp-controller repositories.
;; - Setup quicklisp-controller: (quicklisp-controller:setup-directories "~/src/lisp/quicklisp-projects/")
;; - Update the list of Quicklisp systems using QUICKLISP-CONTROLLER::UPDATE-WHAT-YOU-CAN.
;; - Load all ASDF systems available in *QUICKLISP-CONTROLLER-DIRECTORY* using REGISTER-ALL-ASDF-FILES
;; - Use WRITE-SYSTEMS-FILE to serialize to a QUICKLISP-SYSTEM distribution file.
;; - Upload the file to the URL in QUICKLISP-SYSTEMS::*SYSTEMS-FILE-URL*

(require :quicklisp-systems)

(defpackage #:quicklisp-systems-file
  (:use #:cl))

(in-package #:quicklisp-systems-file)

(defparameter *quicklisp-controller-directory* #p"~/quicklisp-controller/")
(defvar *failed-asdf-files* nil
  "Contains a list of ASDF files that failed to be loaded and the error, after calling REGISTER-ALL-ASDF-FILES.")
(defparameter *conflictive-asdf-files* '("cl-quakeinfo" "qt-libs" "cl-geocode" "cl-geoip")
  "Some ASDF files cause conflicts when trying to be loaded. These are ignored.")

(defun find-files-do (path pattern function &optional (include-subdirectories t))
  "Find files in PATH using PATTERN. Invokes FUNCTION on found files.
If INCLUDE-SUBDIRECTORIES is T, then work recursively."
  (dolist (file (uiop/filesystem:directory-files path pattern))
    (funcall function file))
  (when include-subdirectories
    (dolist (subdir (uiop/filesystem:subdirectories path))
      (find-files-do subdir pattern function include-subdirectories))))

(defun register-all-asdf-files (&optional (quicklisp-controller-directory *quicklisp-controller-directory*))
  "Load all ASDF system definition files found in QUICKLISP-CONTROLLER-DIRECTORY."
  (setf *failed-asdf-files* nil)
  (format *standard-output* "Finding ASDF files...~%")
  (find-files-do
   (merge-pathnames #p"upstream-cache/" quicklisp-controller-directory)
   "*.asd"
   (lambda (file)
     ;; conflictive asdf system files
     (when (not (some (lambda (conflictive-system-name)
                        (search conflictive-system-name (princ-to-string file) :test 'equalp))
                      *conflictive-asdf-files*))
       (format *standard-output* "Loading ~a" file)
       (handler-case (progn
                       (asdf/find-system:load-asd file)
                       (format *standard-output* ". Success.~%"))
         (error (e)
           ;;(error e)
           (push (cons file e) *failed-asdf-files*)
           (format *standard-output* ". ERROR.~%")
           ))))))

(defun serialize-asdf-systems (systems stream)
  "Serialize all ASDF SYSTEMS to STREAM."
  (loop for system in systems
        do
           (prin1 `(:name ,(slot-value system 'asdf/component::name)
                    :description ,(asdf/component:component-description system)
                    :long-description ,(asdf/component:component-long-description system)
                    :author ,(slot-value system 'asdf/system::author)
                    :mailto ,(slot-value system 'asdf/system::mailto)
                    :maintainer ,(slot-value system 'asdf/system::maintainer)
                    :homepage ,(slot-value system 'asdf/system::homepage)
                    :bug-tracker ,(slot-value system 'asdf/system::bug-tracker)
                    :version ,(slot-value system 'asdf/system::version)
                    :license ,(slot-value system 'asdf/system::licence)
                    :depends-on ,(remove-if-not 'stringp (slot-value system 'asdf/system::depends-on)))
                  stream)
           (terpri stream)))

(defun write-systems-file (&optional (path quicklisp-systems::*systems-file*))
  (with-open-file (f path :direction :output :external-format :utf-8
                          :if-exists :supersede)
    (serialize-asdf-systems (asdf/system-registry:registered-systems*)
                            f)))

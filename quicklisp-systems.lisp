;; Copyright (C) 2021 Mariano Montone

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require :dexador)
(require :asdf)
(require :quicklisp)

(defpackage #:quicklisp-systems
  (:use #:cl))

(in-package #:quicklisp-systems)

(defparameter *quicklisp-controller-directory* #p"~/quicklisp-controller/")
(defvar *systems-file* (merge-pathnames "systems" (uiop/pathname:pathname-directory-pathname *load-pathname*)))
(defvar *failed-asdf-files* nil
  "Contains a list of ASDF files that failed to be loaded and the error, after calling REGISTER-ALL-ASDF-FILES.")
(defparameter *conflictive-asdf-files* '("cl-quakeinfo" "qt-libs" "cl-geocode")
  "Some ASDF files cause conflicts when trying to be loaded. These are ignored.")
(defparameter *systems-file-url* "https://bitbucket.org/mmontone/quicklisp-systems/downloads/systems")

(defmacro do-systems ((system &optional (path *systems-file*)) &body body)
  (let ((f (gensym)))
    `(when (probe-file ,path)
       (with-open-file (,f ,path :direction :input :external-format :utf-8)
	 (loop for ,system := (read ,f nil nil)
	       while ,system
	       do ,@body)))))

(defun register-all-asdf-files (&optional (quicklisp-controller-directory *quicklisp-controller-directory*))
  (setf *failed-asdf-files* nil)
  (format *standard-output* "Finding ASDF files...~%")
  (let ((output
          (with-output-to-string (s)
            (uiop/run-program:run-program
             (format nil "/usr/bin/find -P ~a -name *.asd"
                     (merge-pathnames #p"upstream-cache/" quicklisp-controller-directory))
             :output s))))
    (with-input-from-string (s output)
      (loop for line := (read-line s nil nil)
            while line
            when (and (probe-file line)
		      ;; conflictive asdf system files
		      (not (some (lambda (conflictive-system-name)
				   (search conflictive-system-name line :test 'equalp))
				 *conflictive-asdf-files*)))
              do
                 (format *standard-output* "Loading ~a" line)
                 (handler-case (progn
                                 (asdf/find-system:load-asd line)
                                 (format *standard-output* ". Success.~%"))
                   (error (e)
                     ;;(error e)
		     (push (cons line e) *failed-asdf-files*)
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
                      :license ,(slot-value system 'asdf/system::licence)
		      :depends-on ,(remove-if-not 'stringp (slot-value system 'asdf/system::depends-on)))
                    stream)
             (terpri stream)))

(defun write-systems-file (&optional (path *systems-file*))
  (with-open-file (f path :direction :output :external-format :utf-8
                          :if-exists :supersede)
    (serialize-asdf-systems (asdf/system-registry:registered-systems*)
                            f)))

(defun check-systems-list ()
  (and (probe-file *systems-file*) t))

(defun list-all-systems ()
  (let (systems)
    (do-systems (system)
      (push system systems))
    (sort systems 'string< :key (lambda (x) (getf x :name)))))

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

(defun apropos-author (author-name)
  (let (systems)
    (do-systems (system)
      (when (and (getf system :author)
		 (search author-name (getf system :author) :test 'equalp))
        (push system systems)))
    systems))

(defun download-systems-file (&optional (url *systems-file-url*))
  (format t "Downloading quicklisp systems file from ~a ~%" url)
  (dex:fetch url *systems-file* :if-exists :supersede)
  (format t "Systems file downloaded to ~a~%" *systems-file*))

(provide :quicklisp-systems)

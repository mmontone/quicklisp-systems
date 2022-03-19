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

(defvar *systems-file* (merge-pathnames "systems" (uiop/pathname:pathname-directory-pathname *load-pathname*)))
(defparameter *systems-file-url* "https://bitbucket.org/mmontone/quicklisp-systems/downloads/systems"
  "The URL from where to download the file with Quicklisp systems descriptions.")

(defmacro do-systems ((system &optional (path *systems-file*)) &body body)
  (let ((f (gensym)))
    `(when (probe-file ,path)
       (with-open-file (,f ,path :direction :input :external-format :utf-8)
         (loop for ,system := (read ,f nil nil)
               while ,system
               do ,@body)))))

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

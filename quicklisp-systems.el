;;; quicklisp-systems --- Utilities for querying Quicklisp systems.
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(require 'slime)

(defun quicklisp-apropos-systems (pattern)
  "Apropos Quicklisp systems."
  (interactive "sQuicklisp apropos:")
  )

(defun quicklisp-apropos-systems-names (pattern)
  "Apropos Quicklisp systems by name."
  (interactive "sQuicklisp apropos:")
  )

(defun quicklisp-load (system-name)
  "Load Quicklisp system."
  (interactive "sQuickload:")
  (slime-eval `(ql:quickload ,system-name)))

(defalias 'quickload 'quicklisp-load)

(defun quicklisp-systems-download ()
  "Download quicklisp-systems list."
  (interactive))

(defun quicklisp-systems-list ()
  "Show a buffer with all quicklisp systems"
  (interactive)
  (let ((systems (slime-eval `(quicklisp-systems::list-all-systems))))
    (let ((buffer (get-buffer-create "*quicklisp-systems: system list*")))
      (with-current-buffer buffer
	(dolist (system systems)
	  (insert (propertize (getf system :name) 'face 'bold))
	  (newline)
	  (when (and (getf system :description)
		     (stringp (getf system :description)))
	    (insert (getf system :description))
	    (newline)))
	(quicklisp-systems--open-buffer)))))

(defalias 'quicklisp-systems 'quicklisp-systems-list)

(defun quicklisp-systems-update ()
  "Update the list of Quicklisp systems."
  (interactive)
  (quicklisp-systems-download))

(defun quicklisp-systems--open-buffer ()
  (let ((buffer (current-buffer)))
    (setq buffer-read-only t)
    (local-set-key "q" 'quicklisp-systems--kill-current-buffer)
    (buffer-disable-undo)
    (set (make-local-variable 'kill-buffer-query-functions) nil)
    (goto-char 0)
    (pop-to-buffer buffer)))

(defun quicklisp-systems-show-system (system-name)
  "Show Quicklisp system SYSTEM-NAME."
  (interactive "sShow Quicklisp system:")
  (let ((system (slime-eval `(quicklisp-systems::find-system-info ,system-name))))
    (if (null system)
	(error "Quicklisp system not found: %s" system-name)
      (let ((buffer (get-buffer-create (format "*quicklisp-systems: %s*" system-name))))
	(with-current-buffer buffer
	  (insert (getf system :name))
	  (newline 2)
	  (when (getf system :description)
	    (insert (getf system :description))
	    (newline 2))
	  (when (getf system :long-description)
	    (insert (getf system :long-description))
	    (newline 2))
	  
	  (quicklisp-systems--open-buffer))))))

;; (quicklisp-systems-show-system "hunchentoot")
;; (quicklisp-systems-show-system "ten")

(provide 'quicklisp-systems)

;;; quicklisp-systems.el ends here

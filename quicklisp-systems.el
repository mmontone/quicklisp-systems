;; -*- lexical-binding: t -*-

;;; quicklisp-systems --- Utilities for querying Quicklisp systems.
;;; Commentary:
;;; Code:

(require 'slime)
(require 'cl)

(defun quicklisp-systems--kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun quicklisp-apropos-systems (pattern)
  "Apropos Quicklisp systems."
  (interactive "sQuicklisp apropos:")
  )

(defun quicklisp-apropos-systems-names (pattern)
  "Apropos Quicklisp systems by name."
  (interactive "sQuicklisp apropos:"))

(defun quicklisp-load (system-name)
  "Load Quicklisp system."
  (interactive "sQuickload:")
  (message "Quickloading %s..." system-name)
  (slime-eval `(ql:quickload ,system-name))
  (message "%s loaded." system-name))

(defalias 'quickload 'quicklisp-load)

(defun quicklisp-systems--print-systems-list (systems)
  (dolist (system systems)
    (cl-flet ((show-system (btn)
                           (quicklisp-systems-show-system (getf system :name))))
      (insert-text-button (getf system :name)
                          'face 'bold
                          'action (function show-system)
                          'follow-link t)
      (newline)
      (when (and (getf system :description)
                 (stringp (getf system :description)))
        (insert (getf system :description))
        (newline)))))

(defun quicklisp-systems--check-systems-list ()
  (when (not (slime-eval `(quicklisp-systems::check-systems-list)))
    (when (yes-or-no-p "Systems list is empty. Download? ")
      (quicklisp-systems-update))))

(defun quicklisp-systems-list ()
  "Show a buffer with all quicklisp systems"
  (interactive)
  (quicklisp-systems--check-systems-list)
  (let ((systems (slime-eval `(quicklisp-systems::list-all-systems))))
    (let ((buffer (get-buffer-create "*quicklisp-systems: system list*")))
      (with-current-buffer buffer
        (quicklisp-systems--print-systems-list systems)
        (quicklisp-systems--open-buffer)))))

(defalias 'quicklisp-systems 'quicklisp-systems-list)

(defun quicklisp-systems-update ()
  "Update the list of Quicklisp systems."
  (interactive)
  (message "Downloading list of Quicklisp systems...")
  (slime-eval `(quicklisp-systems::download-systems-file))
  (message "Quicklisp systems updated."))

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
          (insert (propertize (getf system :name) 'face 'bold))
          (newline 2)
          (when (getf system :description)
            (insert (getf system :description))
            (newline 2))
          (insert-button "Load"
                         'action (lambda (btn)
                                   (quicklisp-load system-name))
                         'follow-link t
                         'help-echo "Load Quicklisp system")
          (newline 2)
          (when (getf system :long-description)
            (insert (getf system :long-description))
            (newline 2))

          (quicklisp-systems--open-buffer))))))

;; (quicklisp-systems-show-system "hunchentoot")
;; (quicklisp-systems-show-system "ten")

(define-slime-contrib quicklisp-systems
  "Manage Quicklisp from Emacs"
  (:authors "Mariano Montone")
  (:license "GPL")
  ;;(:slime-dependencies slime-asdf)
  (:swank-dependencies quicklisp-systems))

(provide 'quicklisp-systems)

;;; quicklisp-systems.el ends here

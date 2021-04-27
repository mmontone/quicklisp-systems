;; -*- lexical-binding: t -*-

;;; quicklisp-systems --- Utilities for querying Quicklisp systems.
;;; Commentary:
;;; Code:

(require 'slime)
(require 'cl)

(defun quicklisp-systems--propertize-links (string)
  "Convert URL links in strings to buttons."
  (replace-regexp-in-string
   (rx (group (or string-start space "<"))
       (group "http" (? "s") "://" (+? (not (any space))))
       (group (? (any "." ">" ")"))
              (or space string-end ">")))
   (lambda (match)
     (let ((space-before (match-string 1 match))
           (url (match-string 2 match))
           (after (match-string 3 match)))
       (concat
        space-before
        (slime-help--button
         url
         'slime-help-link-button
         'url url)
        after)))
   string))

(defun quicklisp-systems--format-text (text)
  (quicklisp-systems--propertize-links text))

(defun quicklisp-systems--kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun quicklisp-systems-apropos (pattern)
  "Apropos Quicklisp systems."
  (interactive "sQuicklisp apropos:")
  (quicklisp-systems--check-systems-list)
  (let ((systems (slime-eval `(quicklisp-systems::apropos-system ,pattern t))))
    (let ((buffer (get-buffer-create (format "*quicklisp-systems: apropos %s*" pattern))))
      (with-current-buffer buffer
        (quicklisp-systems--print-systems-list systems)
        (quicklisp-systems--open-buffer)))))

(defun quicklisp-systems-apropos-name (pattern)
  "Apropos Quicklisp systems by name."
  (interactive "sQuicklisp apropos system name:")
  (quicklisp-systems--check-systems-list)
  (let ((systems (slime-eval `(quicklisp-systems::apropos-system ,pattern))))
    (let ((buffer (get-buffer-create (format "*quicklisp-systems: apropos name %s*" pattern))))
      (with-current-buffer buffer
        (quicklisp-systems--print-systems-list systems)
        (quicklisp-systems--open-buffer)))))

(defun quicklisp-systems-apropos-author (pattern)
  "Apropos Quicklisp systems by author."
  (interactive "sQuicklisp apropos author:")
  (quicklisp-systems--check-systems-list)
  (let ((systems (slime-eval `(quicklisp-systems::apropos-author ,pattern))))
    (let ((buffer (get-buffer-create (format "*quicklisp-systems: apropos author %s*" pattern))))
      (with-current-buffer buffer
        (quicklisp-systems--print-systems-list systems)
        (quicklisp-systems--open-buffer)))))

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
            (insert (quicklisp-systems--format-text (getf system :description)))
            (newline 2))
	  (when (stringp (getf system :author))
	    (insert (propertize "Author: " 'face 'bold))
	    (insert (getf system :author))
	    (newline))
	  (when (stringp (getf system :homepage))
	    (insert (propertize "Homepage: " 'face 'bold))
	    (insert (quicklisp-systems--format-text (getf system :homepage)))
	    (newline))
	  (when (stringp (getf system :bug-tracker))
	    (insert (propertize "Bug tracker: " 'face 'bold))
	    (insert (quicklisp-systems--format-text (getf system :bug-tracker)))
	    (newline))
	  (newline)
          (insert-button "Load"
                         'action (lambda (btn)
                                   (quicklisp-load system-name))
                         'follow-link t
                         'help-echo "Load Quicklisp system")
          (newline 2)
          (when (getf system :long-description)
            (insert (quicklisp-systems--format-text (getf system :long-description)))
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

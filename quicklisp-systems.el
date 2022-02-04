;;; quicklisp-systems --- Utilities for querying Quicklisp systems.     -*- lexical-binding: t -*-

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

;;; Commentary:
;;; Code:

(require 'slime)

(defface quicklisp-systems-button
  '((t (:box (:line-width 2 :color "dark grey")
	     :background "light grey"
	     :foreground "black")))
  "quicklisp-systems face for buttons"
  :group 'quicklisp-systems-faces)

(defface quicklisp-systems-title
  '((t :weight bold
       :height 1.2
       ))
  "quicklisp-systems face for system title"
  :group 'quicklisp-systems-faces)

(defface quicklisp-systems-attribute
  '((t (:inherit 'bold)))
  "quicklisp-systems face for system attribute"
  :group 'quicklisp-systems-faces)

(defcustom quicklisp-systems-search-url
  "https://www.google.com/search?q=common lisp "
  "URL used to search a Lisp library on the internet."
  :type 'string
  :group 'quicklisp-systems)

(defun quicklisp-systems-search-on-the-internet (library-name)
  (browse-url (concat quicklisp-systems-search-url library-name)))

(defun quicklisp-systems--horizontal-line (&rest width)
  (make-string (or width 80) ?\u2500))

(defun quicklisp-systems--follow-link (button)
  "Follow the URL specified by BUTTON."
  (browse-url (button-get button 'url)))

(defun quicklisp-systems--button (text type &rest properties)
  ;; `make-text-button' mutates our string to add properties. Copy
  ;; TEXT to prevent mutating our arguments, and to support 'pure'
  ;; strings, which are read-only.
  (setq text (substring-no-properties text))
  (apply #'make-text-button
         text nil
         :type type
         properties))

(define-button-type 'quicklisp-systems-link-button
  'action #'quicklisp-systems--follow-link
  'follow-link t
  'help-echo "Follow this link")

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
        (quicklisp-systems--button
         url
         'quicklisp-systems-link-button
         'url url)
        after)))
   string))

(defun quicklisp-systems--format-text (text)
  (quicklisp-systems--propertize-links text))

(defun quicklisp-systems--kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun quicklisp-systems-quit ()
  "Kill all quicklisp-systems buffers at once."
  (interactive)
  (mapcar 'kill-buffer
          (cl-remove-if-not
           (lambda (buffer)
             (string-prefix-p "*quicklisp-systems" (buffer-name buffer)))
           (buffer-list))))

(cl-defun quicklisp-systems-apropos (pattern)
  "Apropos Quicklisp systems."
  (interactive "sQuicklisp apropos: ")
  (quicklisp-systems--check-systems-list)
  (let ((systems (slime-eval `(quicklisp-systems::apropos-system ,pattern t)))
        (buffer-name (format "*quicklisp-systems: apropos %s*" pattern)))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (cl-return-from quicklisp-systems-apropos))
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (quicklisp-systems--print-systems-list systems)
        (quicklisp-systems--open-buffer)))))

(cl-defun quicklisp-systems-apropos-name (pattern)
  "Apropos Quicklisp systems by name."
  (interactive "sQuicklisp apropos system name: ")
  (quicklisp-systems--check-systems-list)
  (let ((systems (slime-eval `(quicklisp-systems::apropos-system ,pattern)))
        (buffer-name (format "*quicklisp-systems: apropos name %s*" pattern)))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (cl-return-from quicklisp-systems-apropos-name))
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (quicklisp-systems--print-systems-list systems)
        (quicklisp-systems--open-buffer)))))

(cl-defun quicklisp-systems-apropos-author (pattern)
  "Apropos Quicklisp systems by author."
  (interactive "sQuicklisp apropos author: ")
  (quicklisp-systems--check-systems-list)
  (let ((systems (slime-eval `(quicklisp-systems::apropos-author ,pattern)))
        (buffer-name (format "*quicklisp-systems: apropos author %s*" pattern)))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (cl-return-from quicklisp-systems-apropos-author))
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (quicklisp-systems--print-systems-list systems)
        (quicklisp-systems--open-buffer)))))

(defun quicklisp-load (cl-system-name)
  "Load Quicklisp system."
  (interactive "sQuickload: ")
  (message "Loading %s..." cl-system-name)
  (slime-eval `(ql:quickload ,cl-system-name))
  (message "%s loaded" cl-system-name))

(defalias 'quickload 'quicklisp-load)

(defun quicklisp-systems--print-systems-list (systems)
  (dolist (system systems)
    (cl-flet ((show-system (btn)
		           (ignore btn)
                           (quicklisp-systems-show-system (cl-getf system :name))))
      (insert-text-button (cl-getf system :name)
                          'face 'bold
                          'action (function show-system)
                          'follow-link t)
      (newline)
      (when (and (cl-getf system :description)
                 (stringp (cl-getf system :description)))
        (insert (cl-getf system :description))
        (newline)))))

(defun quicklisp-systems--check-systems-list ()
  (when (not (slime-eval `(quicklisp-systems::check-systems-list)))
    (when (yes-or-no-p "Systems list is empty. Download? ")
      (quicklisp-systems-update))))

(cl-defun quicklisp-systems-list ()
  "Show a buffer with all quicklisp systems"
  (interactive)
  (quicklisp-systems--check-systems-list)
  (let ((systems (slime-eval `(quicklisp-systems::list-all-systems)))
        (buffer-name "*quicklisp-systems: system list*"))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (cl-return-from quicklisp-systems-list))
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (quicklisp-systems--print-systems-list systems)
        (quicklisp-systems--open-buffer)))))

(defalias 'quicklisp-systems 'quicklisp-systems-list)

(defun quicklisp-systems-update ()
  "Update the list of Quicklisp systems."
  (interactive)
  (message "Downloading list of Quicklisp systems...")
  (slime-eval `(quicklisp-systems::download-systems-file))
  (message "Quicklisp systems updated"))

(defun quicklisp-systems--open-buffer ()
  (let ((buffer (current-buffer)))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (set (make-local-variable 'kill-buffer-query-functions) nil)
    (goto-char 0)
    (quicklisp-systems-mode)
    (pop-to-buffer buffer)))

(cl-defun quicklisp-systems-show-system (cl-system-name)
  "Show Quicklisp system CL-SYSTEM-NAME."
  (interactive "sShow Quicklisp system: ")
  (let ((system (slime-eval `(quicklisp-systems::find-system-info ,cl-system-name)))
        (buffer-name (format "*quicklisp-systems: %s*" cl-system-name)))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (cl-return-from quicklisp-systems-show-system))
    (if (null system)
        (error "Quicklisp system not found: %s" cl-system-name)
      (let ((buffer (get-buffer-create buffer-name)))
        (with-current-buffer buffer
          (insert (propertize (cl-getf system :name) 'face 'quicklisp-systems-title))
          (newline 2)
          (when (cl-getf system :description)
            (insert (quicklisp-systems--format-text (cl-getf system :description)))
            (newline 2))
          (when (cl-getf system :author)
            (insert (propertize "Author: " 'face 'quicklisp-systems-attribute))
	    (if (stringp (cl-getf system :author))
		(insert (cl-getf system :author))
	      (dolist (author (cl-getf system :author))
		(insert author " ")))
            (newline))
          (when (stringp (cl-getf system :homepage))
            (insert (propertize "Homepage: " 'face 'quicklisp-systems-attribute))
            (insert (quicklisp-systems--format-text (cl-getf system :homepage)))
            (newline))
          (when (stringp (cl-getf system :bug-tracker))
            (insert (propertize "Bug tracker: " 'face 'quicklisp-systems-attribute))
            (insert (quicklisp-systems--format-text (cl-getf system :bug-tracker)))
            (newline))
          (when (stringp (cl-getf system :version))
            (insert (propertize "Version: " 'face 'quicklisp-systems-attribute))
            (insert (quicklisp-systems--format-text (cl-getf system :version)))
            (newline))
          (when (cl-getf system :depends-on)
            (insert (propertize "Dependencies: " 'face 'quicklisp-systems-attribute))
            (dolist (dependency (cl-getf system :depends-on))
              (insert-button dependency
                             'action (lambda (btn)
				       (ignore btn)
                                       (quicklisp-systems-show-system dependency))
                             'follow-link t)
              (insert " "))
            (newline))
          (newline)

	  ;; buttons
          (insert-button "Load"
                         'action (lambda (btn)
				   (ignore btn)
                                   (quicklisp-load cl-system-name))
                         'follow-link t
                         'help-echo "Load Quicklisp system"
			 'face 'quicklisp-systems-button)
	  (when (not (stringp (cl-getf system :homepage)))
	    (insert " ")
	    (insert-button "Search on the internet"
			   'action (lambda (btn)
				     (ignore btn)
				     (quicklisp-systems-search-on-the-internet cl-system-name))
			   'follow-link t
			   'help-echo "Search the library on the internet"
			   'face 'quicklisp-systems-button))
	  (newline 2)

          (when (cl-getf system :long-description)
	    (insert (quicklisp-systems--horizontal-line))
            (newline 2)
            (insert (quicklisp-systems--format-text (cl-getf system :long-description)))
            (newline 2))

          (quicklisp-systems--open-buffer))))))

;; (quicklisp-systems-show-system "hunchentoot")
;; (quicklisp-systems-show-system "ten")

(defvar quicklisp-systems-mode-map
  (let ((map (make-keymap)))
    (define-key map "q" 'quicklisp-systems--kill-current-buffer)
    (define-key map "Q" 'quicklisp-systems-quit)
    map))

(define-minor-mode quicklisp-systems-mode
  "Quicklisp systems minor mode."
  :init-value nil
  :lighter " QuicklispSystems"
  :keymap quicklisp-systems-mode-map
  :group 'quicklisp-systems)

(easy-menu-define
 quicklisp-systems-mode-menu quicklisp-systems-mode-map
 "Menu for Quicklisp systems."
 '("Quicklisp"
   ["List all systems" quicklisp-systems-list
    :help "List all available Quicklisp systems"]
   ["Show system..." quicklisp-systems-show-system
    :help "Show information about Quicklisp system"]
   "---"
   ["Apropos..." quicklisp-systems-apropos
    :help "Search a system in Quicklisp"]
   ["Apropos name..." quicklisp-systems-apropos-name
    :help "Search Quicklisp systems by name"]
   ["Apropos author..." quicklisp-systems-apropos-author
    :help "Search Quicklisp systems by author"]
   "---"
   ["Load system..." quicklisp-load
    :help "Quickload a system"]
   ["Update systems list" quicklisp-systems-update
    :help "Update the list of Quicklisp systems"]
   ["Quit" quicklisp-systems-quit
    :help "Quit Quicklisp systems"]))

(defun quicklisp-systems--add-to-slime-menu ()
  (easy-menu-add-item 'menubar-slime nil '("---"))
  (easy-menu-add-item 'menubar-slime nil
		      '("Quicklisp"
			["Browse systems" quicklisp-systems
			 :help "Open Quicklisp systems list"]
			["Load system..." quicklisp-load
			 :help "Quickload a system"])))

(define-slime-contrib quicklisp-systems
  "Manage Quicklisp from Emacs"
  (:authors "Mariano Montone")
  (:license "GPL")
  (:swank-dependencies quicklisp-systems)
  (:on-load
   (quicklisp-systems--add-to-slime-menu)))

(provide 'quicklisp-systems)

;;; quicklisp-systems.el ends here

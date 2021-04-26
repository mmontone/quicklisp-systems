;;; quicklisp-systems --- Utilities for querying Quicklisp systems.
;;; Commentary:
;;; Code:

(require 'slime)

(defun quicklisp-apropos-systems (pattern)
  "Apropos Quicklisp systems."
  (interactive)
  )

(defun quicklisp-apropos-systems-names (pattern)
  "Apropos Quicklisp systems by name."
  (interactive)
  )

(defun quicklisp-load (system-name)
  "Load Quicklisp system."
  (interactive))

(defalias 'quickload 'quicklisp-load)

(defun quicklisp-systems-download ()
  "Download quicklisp-systems list."
  (interactive))

(defun quicklisp-systems ()
  "Show a buffer with all quicklisp systems"
  (interactive))

(defun quicklisp-systems-update ()
  "Update the list of Quicklisp systems."
  (interactive)
  (quicklisp-systems-download))

(provide 'quicklisp-systems)

;;; quicklisp-systems.el ends here

;;; pet-lib.el --- utility functions -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Small helpers shared by otherwise independent configuration modules.
;; Feature-specific commands belong with the feature that owns them.
;;
;;; Code:

(require 'project)

(defun pet/edit-emacs-configuration ()
  "Edit a file in the Emacs configuration."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (call-interactively 'project-find-file)))

(defun pet/is-mac ()
  "Return non-nil if running on macOS."
  (eq system-type 'darwin))

(provide 'pet-lib)
;;; pet-lib.el ends here

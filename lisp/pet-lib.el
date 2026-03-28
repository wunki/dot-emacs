;;; pet-lib.el --- utility functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pet-packages)

(defun pet/kill-region-or-backward-word ()
  "Kill the active region or the word backwards."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun pet/edit-emacs-configuration ()
  "Edit a file in the Emacs configuration."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (call-interactively 'project-find-file)))

(defun pet/is-mac ()
  "Return non-nil if running on macOS."
  (eq system-type 'darwin))

(defun pet/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun pet/move-beginning-of-line (arg)
  "Move to first non-whitespace, or beginning of line if already there.
ARG is passed to `beginning-of-visual-line'."
  (interactive "^p")
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      (beginning-of-visual-line arg))))

(provide 'pet-lib)
;;; pet-lib.el ends here

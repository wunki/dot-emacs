;;; lib.el --- my utility functions -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Add helpful functions to our Emacs.  All functions should start
;; with `pet' so we can easily find them.
;; 
;;; Code:
;;

(require 'packages)
(require 'ert)

(use-package s
  :commands (s-trim s-concat))

(defun pet/kill-region-or-backward-word ()
  "Kill the active region if it exists, otherwise kill the word backwards."
  (interactive)
  (if (region-active-p)
    (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun pet/edit-emacs-configuration ()
  "Edit a file in my Emacs configuration."
  (interactive)
  (let ((project-root "~/Developer/Configurations/dot-emacs"))
    (if (and project-root (file-directory-p project-root))
        (let ((default-directory project-root))
          (call-interactively 'project-find-file))
      (message "Emacs configuration not found at: %s" project-root))))

(defun pet/is-mac ()
  "Return non-nil if running on a mac."
  (eq system-type 'darwin))

(defun pet/is-bsd ()
  "Return non-nil if running on FreeBSD."
  (eq system-type 'berkeley-unix))

(defun pet/is-wsl ()
  "Return non-nil if running on Windows WSL."
  (and (string-equal system-type 'gnu/linux) (getenv "WSLENV")))

(defun pet/is-linux ()
  "Return non-nil if running on Linux."
  (and (not (pet/is-wsl)) (string-equal system-type "gnu/linux")))

(defun pet/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
        (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

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

(defun pet/eval-and-run-all-tests-in-buffer ()
  "Clear and run all test in the current buffer."
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert 't))

(defun pet/reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(provide 'lib)
;;; lib.el ends here

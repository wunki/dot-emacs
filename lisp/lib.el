;;; lib.el --- my utulity functions -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Add helpful functions to our Emacs. All functions should start
;; with `pet' so we can easily find them.
;; 
;;; Code:
;;

(require 'packages)

;; Modern API for working with files
(use-package f
  :commands f-executable?)

(use-package s
  :commands (s-trim s-concat))

(defun pet/kill-region-or-backward-word ()
  "Kill either the word backwards or the active region"
  (interactive)
  (if (region-active-p)
    (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun pet/find-config ()
  "Edit my configuration file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun pet/is-exec (command)
  "Returns true if `command' is an executable on the system search path"
  (f-executable?
    (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun pet/is-mac ()
  "Return non-nil if running on a mac."
  (interactive)
  (eq system-type 'darwin))

(defun pet/is-windows ()
  "Returns true if running on Windows WSL"
  (interactive)
  (string-match "-[Mm]icrosoft" operating-system-release))

(defun pet/is-linux ()
  "Returns true if running on native Linux"
  (interactive)
  (and (not (pet/is-windows)) (string-equal system-type "gnu/linux")))

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

(provide 'lib)
;;; lib.el ends here

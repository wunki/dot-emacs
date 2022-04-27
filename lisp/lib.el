;;; lib.el --- my utulity functions -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Add helpful functions to our Emacs.  All functions should start
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
  "Kill either the word backwards or the active region."
  (interactive)
  (if (region-active-p)
    (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun pet/find-config ()
  "Edit my configuration file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun pet/is-exec (command)
  "Return non-nil if COMMAND is an executable on the system search path."
  (f-executable?
    (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun pet/is-mac ()
  "Return non-nil if running on a mac."
  (interactive)
  (eq system-type 'darwin))

(defun pet/is-wsl ()
  "Return non-nil if running on Windows WSL."
  (interactive)
  (and (string-equal system-type 'gnu/linux) (getenv "WSLENV")))

(defun pet/is-linux ()
  "Return non-nil if running on Linux."
  (interactive)
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

(defun pet/eshell-here ()
  "Opens up a new shell in the directory associated with the current buffer's file.
The eshell is renamed to match that directory to make multiple eshell
windows easier."
  (interactive)
  (let*
    (
      (parent
        (if (buffer-file-name)
          (file-name-directory (buffer-file-name))
          default-directory))
      (height (/ (window-total-height) 3))
      (name (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))))
(global-set-key (kbd "C-c s") 'pet/eshell-here)

(defun eshell/q ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(provide 'lib)
;;; lib.el ends here

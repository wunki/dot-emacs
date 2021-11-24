;;; lib.el --- handcrafted helper methods -*- lexical-binding: t -*-

;;; Code:

(require 'packages)

;; Modern API for working with files
(use-package f)

(defun kill-region-or-backward-word ()
  "Kill either the word backwards or the active region"
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun find-config ()
    "Edit my configuration file"
    (interactive)
    (find-file "~/.emacs.d/init.el"))

(defun is-exec (command)
  "Returns true if `command' is an executable on the system search path"
  (f-executable? (s-trim (shell-command-to-string (s-concat "which " command)))))

(provide 'lib)
;;; lib.el ends here

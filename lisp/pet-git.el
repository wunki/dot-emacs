;;; pet-git.el --- source code management they call it -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;  This is where I introduce you to the magic of Magit.
;;
;;; Code:
;;

;; Required for Magit menu's
(use-package transient)

;; Magical Git GUI
(use-package magit
  :after transient
  :preface
  (defun pet/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil))

  :custom (git-commit-summary-max-length 72)
  :bind ("C-c g" . magit-status)
  :hook (git-commit-mode . pet/git-commit-auto-fill-everywhere))

;; Connection to Github
(use-package forge
  :after magit
  :bind ("C-c C-g" . forge-dispatch))

;; Create .gitignore files
(use-package gitignore-templates
  :commands (gitignore-templates-insert
             gitignore-templates-new-file))

;; Easy copy links to open files
(use-package git-link)

(provide 'pet-git)
;;; pet-git.el ends here

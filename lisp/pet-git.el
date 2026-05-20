;;; pet-git.el --- source code management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Magical Git GUI
(use-package magit
  :preface
  (defun pet/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil))
  :custom (git-commit-summary-max-length 72)
  :bind ("C-c g" . magit-status)
  :hook (git-commit-mode . pet/git-commit-auto-fill-everywhere))

;; GitHub/GitLab integration
(use-package forge
  :after magit
  :bind ("C-c C-g" . forge-dispatch))

;; Create .gitignore files
(use-package gitignore-templates
  :commands (gitignore-templates-insert
             gitignore-templates-new-file))

;; Copy links to files on forge
(use-package git-link)

;; Git diff in the fringe
(use-package diff-hl
  :demand t
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (after-revert . diff-hl-update)
         (dired-mode . diff-hl-dired-mode))
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(provide 'pet-git)
;;; pet-git.el ends here

;;; pet-git.el --- source code management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pet-packages)

(use-feature vc
  :custom
  ;; vc-dir now hides up-to-date files on refresh by itself.
  (vc-dir-auto-hide-up-to-date 'revert)
  ;; Allow rewriting already-pushed history (jj, force-pushed branches).
  (vc-allow-rewriting-published-history t))

;; Magical Git GUI
(use-package magit
  :preface
  (defun pet/git-commit-auto-fill-everywhere ()
    "Keep Git commit bodies within 72 columns."
    (setq-local fill-column 72)
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

;; Git changes in the fringe
(use-package git-gutter-fringe
  :functions (global-git-gutter-mode git-gutter:update-all-windows)
  :demand t
  :custom
  ;; Keep unsaved edits visible without polling as aggressively as the old
  ;; 20ms configuration did.
  (git-gutter:update-interval 0.2)
  :hook (magit-post-refresh . git-gutter:update-all-windows)
  :config
  ;; `git-gutter-fringe' keeps its bitmap foreground separate from the fringe
  ;; background, so themes control the change colors without a colored canvas.
  (dolist (bitmap '(git-gutter-fr:added
                    git-gutter-fr:modified
                    git-gutter-fr:deleted))
    (define-fringe-bitmap bitmap [#b00010000] 1 8 '(top t)))
  (global-git-gutter-mode 1))

(provide 'pet-git)
;;; pet-git.el ends here

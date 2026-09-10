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
  :functions (global-git-gutter-mode git-gutter:update-all-windows
              pet/git-gutter-theme-colors)
  :demand t
  :custom
  (git-gutter:update-interval 0.2)
  :hook (magit-post-refresh . git-gutter:update-all-windows)
  :config
  ;; Thin bars instead of the default wide bitmaps.
  (dolist (bitmap '(git-gutter-fr:added
                    git-gutter-fr:modified
                    git-gutter-fr:deleted))
    (define-fringe-bitmap bitmap [#b00110000] 1 8 '(top t)))

  ;; Modus-family themes (Modus, Ef, Cendre) only set a background on these
  ;; faces, but thin bars are drawn with the foreground.  Use the theme's diff
  ;; colors for that.
  (declare-function modus-themes-known-p "modus-themes")
  (declare-function modus-themes-get-color-value "modus-themes")
  (defun pet/git-gutter-theme-colors (theme &rest _)
    "Color the git-gutter fringe bars with THEME's diff colors."
    (when (and (fboundp 'modus-themes-known-p)
               (modus-themes-known-p theme))
      (let ((color (lambda (name) (modus-themes-get-color-value name t theme))))
        (custom-theme-set-faces
         theme
         `(git-gutter-fr:added ((t (:foreground ,(funcall color 'fg-added)))) t)
         `(git-gutter-fr:modified ((t (:foreground ,(funcall color 'fg-changed)))) t)
         `(git-gutter-fr:deleted ((t (:foreground ,(funcall color 'fg-removed)))) t)))))
  (add-hook 'enable-theme-functions #'pet/git-gutter-theme-colors)
  (mapc #'pet/git-gutter-theme-colors custom-enabled-themes)

  (global-git-gutter-mode 1))

(provide 'pet-git)
;;; pet-git.el ends here

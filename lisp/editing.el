;;; editing.el --- frictionless text editing -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Useful packages to make the text editing experience as frictionless
;; as possible.
;; 
;;; Code:
;;

(require 'lib)

;; Easily select larger chunks of text
(use-package expand-region
  :commands er/expand-region
  :bind ("C-c e" . er/expand-region))

;; Easily move to the actual beginning of the line, double-tap moves
;; to the first character
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c f" . crux-recentf-find-file)))

;; Move to the last change in the buffer
(use-package goto-last-change
  :bind (("C-;" . goto-last-change)))

;; Magical Git GUI
(use-package magit
  :preface
  (defun my/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil))
  :custom (git-commit-summary-max-length 50)
  :bind ("C-c g" . magit-status)
  :hook (git-commit-mode-hook . my/git-commit-auto-fill-everywhere))

;; Show git changes in the gutter
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

;; Setup auto-completion with company
(use-package company
  :commands global-company-mode
  :delight
  :bind (:map company-active-map
              ("C-w" . pet/kill-region-or-backward-word))
  :custom
  (company-idle-delay 0.2) ; change to nil if you want to start auto-complete on tab
  :init
  (global-company-mode)
  :hook
  (company-mode . company-tng-mode))

;; Balance and mold those parenthesis
(use-package paredit
  :commands paredit-mode
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode lisp-mode eval-expression-minibuffer-setup slime-repl-mode) . paredit-mode))

;; Distraction-free screen for writing
(use-package olivetti
  :commands (olivetti-mode)
  :init
  (setq olivetti-body-width .4)
  :config
  (defun pet/writing-mode ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 2)
          (olivetti-mode t))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (text-scale-decrease 2))))
  :bind
  (("<f9>" . pet/writing-mode)))

;; Automatic parens matching
(electric-pair-mode 1)

(provide 'editing)
;;; editing.el ends here

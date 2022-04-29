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
  :bind (("C-a" . crux-move-beginning-of-line)))

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

;; Show line changes in the gutter
(use-package git-gutter
  :commands global-git-gutter-mode
  :init (global-git-gutter-mode))

;; Setup auto-completion with company
(use-package company
  :commands global-company-mode
  :custom
  (company-idle-delay 0.2) ; change to nil if you want to start auto-complete on tab
  :init
  (global-company-mode)
  :bind
  ("M-TAB" . company-complete)
  ("TAB" . company-indent-or-complete-common))

;; Balance and mold those parenthesis
(use-package paredit
  :commands paredit-mode
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode lisp-mode eval-expression-minibuffer-setup) . paredit-mode))

;; Automatic parens matching
(electric-pair-mode 1)

(provide 'editing)
;;; editing.el ends here

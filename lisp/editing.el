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

(use-package forge
  :after magit
  :bind ("C-c C-g" . forge-dispatch))

;; Easy copy links to open files
(use-package git-link)

;; Balance and mold those parenthesis
(use-package paredit
  :delight paredit-mode
  :commands paredit-mode
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode lisp-mode eval-expression-minibuffer-setup slime-repl-mode clojure-mode racket-mode mrepl-mode) . paredit-mode))

;; Automatic parens matching
(electric-pair-mode 1)

(provide 'editing)
;;; editing.el ends here

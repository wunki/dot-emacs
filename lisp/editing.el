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

(use-package git-gutter-fringe
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 0.02)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package forge
  :after magit
  :bind ("C-c C-g" . forge-dispatch))

;; Show git changes in the gutter
(use-package git-gutter
  :disabled
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 1))

(use-package copilot
  :straight (:host github
             :repo "zerolfx/copilot.el"
             :files ("dist" "*.el"))
  
  :preface
  (defun pet/copilot-tab ()
    (interactive)
    (or (copilot-accept-completion)
	    (indent-for-tab-command)))

  :bind (:map copilot-mode-map
              ("<tab>" . pet/copilot-tab)
              ("s-n" . copilot-next-completion)
              ("s-p" . copilot-previous-completion)
              ("s-w" . copilot-accept-completion-by-word)
              ("s-l" . copilot-accept-completion-by-line)))

;; Balance and mold those parenthesis
(use-package paredit
  :commands paredit-mode
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode lisp-mode eval-expression-minibuffer-setup slime-repl-mode clojure-mode racket-mode) . paredit-mode))

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

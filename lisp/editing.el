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

;; Go to the last place I visited the file.
(use-package saveplace
  :custom
  (save-place-file "~/.cache/emacs/saveplace")
  :config
  (save-place-mode +1))

;; Auto-saves buffers when I switch between buffers.
(use-package super-save
  :commands super-save-mode
  :config
  (super-save-mode +1))

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
  (defun pet/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil))

  (defun pet/turn-off-whitespace-mode ()
    "Disable the whitespace in Magit buffers"
    (setq-local whitespace-style nil))

  :custom (git-commit-summary-max-length 50)
  :bind ("C-c g" . magit-status)
  :hook ((git-commit-mode . pet/git-commit-auto-fill-everywhere)
         (magit-section-mode . pet/turn-off-whitespace-mode)))

(use-package forge
  :after magit
  :bind ("C-c C-g" . forge-dispatch))

;; Easy copy links to open files
(use-package git-link)

;; Use smartparens like paredit
(use-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          ielm-mode
          clojure-mode
          cider-repl-mode) . smartparens-strict-mode))

;; Automatic parens matching
(electric-pair-mode 1)

(provide 'editing)
;;; editing.el ends here

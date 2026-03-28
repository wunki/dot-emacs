;;; pet-editing.el --- frictionless text editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pet-lib)

;; Expand selection
(use-package expand-region
  :bind ("C-c e" . er/expand-region))

;; Remember cursor position
(use-feature saveplace
  :config
  (save-place-mode +1))

;; Auto-save visited files (built-in, replaces super-save)
(auto-save-visited-mode 1)
(setq auto-save-visited-interval 1)

;; Smart beginning of line
(global-set-key (kbd "C-a") 'pet/move-beginning-of-line)

;; Jump to last change
(use-package goto-last-change
  :bind ("C-;" . goto-last-change))

;; Structural editing for Lisp
(use-package paredit
  :hook ((clojure-mode . paredit-mode)
         (clojure-ts-mode . paredit-mode)
         (cider-repl-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-data-mode . paredit-mode)))

;; Snippets (tempel: lighter, native capf integration with corfu)
(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :init
  (defun pet/tempel-setup-capf ()
    "Add tempel to completion-at-point-functions."
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook ((prog-mode . pet/tempel-setup-capf)
         (text-mode . pet/tempel-setup-capf)
         (conf-mode . pet/tempel-setup-capf)))

(use-package tempel-collection
  :after tempel)

(provide 'pet-editing)
;;; pet-editing.el ends here

;;; navigation.el --- moving around in Emacs -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; The goal here is to navigate through Emacs with as little
;; as friction possible.
;; 
;;; Code:
;;

(require 'lib)

;; Swap command and option on a mac
(when (pet/is-mac)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; Emacs completions in the mini buffer
(use-package vertico
  :commands vertico-mode
  :custom (vertico-cycle t)
  :init (vertico-mode))

;; Store recent commands and completions
(use-package savehist
  :init (savehist-mode))

;; Extra metadata in the minibuffer
(use-package marginalia
  :after vertico
  :commands marginalia-mode
  :init (marginalia-mode))

;; Improve the completions style by removing any order
(use-package orderless
  :init
  (setq
    completion-styles
    '(orderless)
    completion-category-defaults
    nil
    completion-category-overrides
    '((file (styles partial-completion)))))

;; Enhanced version of isearch
(use-package swiper
  :bind (("C-s" . swiper)))

;; Suggests the next key, depending on the pressed key
(use-package which-key
  :config
  :hook (after-init . which-key-mode))

;; Filebrowser
(use-package treemacs
  :defer t
  :commands
  (treemacs-follow-mode
    treemacs-filewatch-mode
    treemacs-fringe-indicator-mode
    treemacs-hide-gitignored-files-mode)
  :custom
  (treemacs-user-mode-line-format 'none)
  (treemacs-no-png-images
    t
    "png images don't display nicely on hidpi displays")
  (progn
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map
    global-map
    ("C-M-t" . treemacs)
    ("M-0" . treemacs-select-window)))

(provide 'navigation)
;;; navigation.el ends here

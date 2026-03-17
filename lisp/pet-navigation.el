;;; pet-navigation.el --- moving around -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pet-lib)

;; Swap command and option on Mac
(use-feature emacs
  :init
  (when (pet/is-mac)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'super)))

;; Minibuffer completion
(use-package vertico
  :custom (vertico-cycle t)
  :init (vertico-mode))

(use-feature vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Rich annotations in minibuffer
(use-package marginalia
  :after vertico
  :init (marginalia-mode))

;; Orderless completion
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

;; Enhanced search and navigation commands
(use-package consult
  :bind (("M-s M-g" . consult-ripgrep)
         ("M-s M-f" . consult-find)
         ("M-s M-o" . consult-outline)
         ("M-s M-l" . consult-line)
         ("M-s M-b" . consult-buffer)))

;; Persist minibuffer history
(use-feature savehist
  :init (savehist-mode))

;; Recent files
(use-feature recentf
  :defer 1
  :config (recentf-mode)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100))

;; which-key is built-in since Emacs 30
(use-feature which-key
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.5)
  (which-key-sort-order 'which-key-description-order)
  :config
  (which-key-mode))

;; Jump anywhere with Avy
(use-package avy
  :custom
  (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  :bind (("C-c c" . avy-goto-char-2)
         ("C-c l" . avy-goto-line)))

;; Window switching
(use-package ace-window
  :custom
  (aw-keys '(?a ?o ?e ?u ?h ?t ?n ?l))
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window)))

;; Context actions on minibuffer candidates
(use-package embark
  :bind (("C-." . embark-act)
         ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Edit grep results in-place
(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

;; Popup buffer management
(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(provide 'pet-navigation)
;;; pet-navigation.el ends here

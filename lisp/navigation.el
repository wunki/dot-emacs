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
(use-package emacs
  :init
  (when (pet/is-mac)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)))

;; Emacs completions in the mini buffer
(use-package vertico
  :commands vertico-mode
  :straight
  (vertico
    :files (:defaults "extensions/*")
    :includes
    (vertico-buffer
      vertico-directory
      vertico-flat
      vertico-indexed
      vertico-mouse
      vertico-quick
      vertico-repeat
      vertico-reverse))
  :custom (vertico-cycle t)
  :init (vertico-mode))

;; Mimic Ivy for directory completion
(use-package vertico-directory
  :after vertico
  :bind
  (:map
    vertico-map
    ("RET" . vertico-directory-enter)
    ("DEL" . vertico-directory-delete-char)
    ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Store recent commands and completions
(use-package savehist
  :init (savehist-mode))

;; Jump to recent files
(use-package recentf
  :custom
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 100)
  :hook (after-init . recentf-mode))

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
  :delight
  :hook (after-init . which-key-mode))

;; Quickly move around with Avy
(use-package avy
  :custom
  (avy-keys
    '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)
    "map the keys to my homerow on dvorak")
  :bind (("C-c c" . avy-goto-char-2) ("C-c l" . avy-goto-line)))

(use-package ace-window
  :custom
  (aw-keys '(?a ?o ?e ?u ?h ?t ?t ?n ?l))
  :bind (("M-o" . ace-window)))

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
    (treemacs-fringe-indicator-mode 'only-when-focused)
    (treemacs-hide-gitignored-files-mode t))
  :bind
  (:map
    global-map
    ("C-c t" . treemacs)
    ("C-c T" . treemacs-select-window)))

(provide 'navigation)
;;; navigation.el ends here

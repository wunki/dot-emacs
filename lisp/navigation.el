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
(use-feature emacs
  :init
  (when (pet/is-mac)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)))

;; Emacs completions in the mini buffer
(use-package vertico
  :commands vertico-mode
  :custom (vertico-cycle t)
  :init (vertico-mode))

;; Mimic Ivy for directory completion
(use-feature vertico-directory
  :after vertico
  :bind
  (:map
    vertico-map
    ("RET" . vertico-directory-enter)
    ("DEL" . vertico-directory-delete-char)
    ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Extra metadata in the minibuffer
(use-package marginalia
  :after vertico
  :commands marginalia-mode
  :init (marginalia-mode))

;; The `orderless' package lets the minibuffer use an out-of-order
;; pattern matching algorithm.  It matches space-separated words or
;; regular expressions in any order.  In its simplest form, something
;; like "ins pac" matches `package-menu-mark-install' as well as
;; `package-install'.  This is a powerful tool because we no longer
;; need to remember exactly how something is named.
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

;; The `consult' package provides lots of commands that are enhanced
;; variants of basic, built-in functionality.  One of the headline
;; features of `consult' is its preview facility, where it shows in
;; another Emacs window the context of what is currently matched in
;; the minibuffer.
;; TODO find the right key bindings.
(use-package consult
  :ensure t
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))

;; Store recent commands and completions
(use-feature savehist
  :init (savehist-mode))

;; Jump to recent files
(use-feature recentf
  :defer 1
  :config (recentf-mode)
  :custom
  (recentf-max-menu-items 100 "Offer more recent files in menu")
  (recentf-max-saved-items 100 "Save more recent files"))

;; Suggests the next key, depending on the pressed key
(use-package which-key
  :demand t
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.5)
  (which-key-sort-order 'which-key-description-order)
  :config
  (which-key-mode))

;; Quickly move around with Avy
(use-package avy
  :custom
  (avy-keys
    '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)
    "map the keys to my homerow on dvorak")
  :bind (("C-c c" . avy-goto-char-2)
         ("C-c l" . avy-goto-line)))

;; Easily move between windows
(use-package ace-window
  :custom
  (aw-keys '(?a ?o ?e ?u ?h ?t ?t ?n ?l))
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window)))

;; Helper functions to manage projects.
(use-package project-x
  :ensure (project-x
           :host github
           :repo "karthink/project-x"
           :files ("*.el"))
  :after project
  :config
  (setq project-x-save-interval 600)
  (project-x-mode 1))

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

(provide 'navigation)
;;; navigation.el ends here

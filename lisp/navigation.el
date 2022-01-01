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

;; Generic completion framework for the minibuffer
(use-package ivy
  :commands ivy-mode
  :custom
  (ivy-initial-inputs-alist nil)
  (ivy-height 4)
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :config (ivy-mode t))

;; Counsel enhanced versions of emacs commands
(use-package counsel
  :bind
  (("C-x C-m" . counsel-M-x)
    ("M-x" . counsel-M-x)
    ("C-x C-f" . counsel-find-file)
    ("C-x t" . wunki/counsel-find-notes)))

;; Sorts and filters candidates for Ivy
(use-package prescient)
(use-package ivy-prescient
  :requires prescient
  :commands ivy-prescient-mode
  :config (ivy-prescient-mode t))

;; Enhanced version of isearch
(use-package swiper
  :bind (("M-s" . counsel-grep-or-swiper)))

;; Presents menus for ivy commands
(use-package ivy-hydra)

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

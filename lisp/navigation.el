;;; navigation.el --- enhancements to make it easier to navigate emacs -*- lexical-binding: t -*-

;;; Code:

(require 'lib)

;; Generic completion framework for the minibuffer
(use-package ivy
  :delight
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-height 4)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode t))

;; Counsel enhanced versions of emacs commands
(use-package counsel
  :bind (("C-x C-m" . counsel-M-x)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x t" . wunki/counsel-find-notes)))

;; Sorts and filters candidates for Ivy
(use-package prescient)
(use-package ivy-prescient
  :requires prescient
  :config
  (ivy-prescient-mode t))

;; Enhanced version of isearch
(use-package swiper
  :bind (("M-s" . counsel-grep-or-swiper)))

;; Presents menus for ivy commands
(use-package ivy-hydra)

;; Suggests the next key, depending on the pressed key
(use-package which-key
  :config
  (add-hook 'after-init-hook 'which-key-mode))

(use-package treemacs
  :ensure t
  :defer t
  :init
  :config
  (progn
    (treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("C-M-t"   . treemacs)
        ("M-0" . treemacs-select-window)))

;; Swap command and option on a mac
(when (is-mac)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(provide 'navigation)
;;; navigation.el ends here

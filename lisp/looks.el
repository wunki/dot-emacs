;;; looks.el --- make Emacs look pretty -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; We want Emacs to look nice, with as minimal distraction as possible.
;; 
;;; Code:
;;
(require 'lib)

;; Easily scale the font size up and down
(use-package default-text-scale
  :demand
  :commands default-text-scale-mode
  :config (default-text-scale-mode))

;; Set the font, depending on the system
(defvar petars-font
  (cond
   ((pet/is-bsd) "Triplicate T4 10")
   ((pet/is-linux) "Berkeley Mono 9")
   ((pet/is-wsl) "Triplicate T4 17")
   ((pet/is-mac) "MonoLisa 14")))
(pet/set-font petars-font)

;; TODO: move this to the right location.
(defun pet/let-text-breath ()
  "Let the text breath a little bit more."
  (setq-local default-text-properties '(line-spacing 0.20 line-height 1.20)))
;; (add-hook 'text-mode-hook 'pet/let-text-breath)
;; (add-hook 'prog-mode-hook 'pet/let-text-breath)

;; Don't show any bars or toolbars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

;; Add some spacing
(setq frame-resize-pixelwise t
      default-frame-alist (append (list
                                   '(vertical-scroll-bars . nil)
                                   '(internal-border-width . 14)
                                   '(right-fringe   . 0)
                                   '(tool-bar-lines . 0))))

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . nil))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; load my own themes
(use-package autothemer
  :init
  (add-to-list
   'custom-theme-load-path
   (concat user-emacs-directory "themes/"))
  :config
  (load-theme 'kanagawa t))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-height 25)
  :hook (after-init . doom-modeline-mode))

(use-package modus-themes
  :disabled
  :ensure t
  :bind ("C-c C-t" . modus-themes-toggle)
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)
  :init
  (load-theme 'modus-vivendi-tinted t))

(use-package ef-themes
  :disabled
  :ensure t
  :commands ef-themes-select
  :bind ("C-c C-t" . ef-themes-load-random)
  :init
  (ef-themes-select 'ef-day))

(use-package rainbow-mode
  :commands rainbow-mode
  :diminish
  :hook ((web-mode . rainbow-mode)
         (css-mode . rainbow-mode)))

(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(provide 'looks)
;;; looks.el ends here

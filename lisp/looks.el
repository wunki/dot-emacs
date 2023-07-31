;;; looks.el --- make Emacs look pretty -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; We want Emacs to look nice, with as minimal distraction as possible.
;; That's why we use the Nano theme from rougier.
;; 
;;; Code:
;;
(require 'lib)

;; Set the font, depending on the system
(defvar pet/var-my-font
  (cond
   ((pet/is-bsd) "Triplicate T4 10")
   ((pet/is-linux) "IBM Plex Mono 9")
   ((pet/is-wsl) "IBM Plex Mono 15")
   ((pet/is-mac) "Triplicate T4 13")) "The font used across Emacs.")
(pet/set-font pet/var-my-font)

;; Don't show any bars or toolbars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

;; Add some spacing
(setq frame-resize-pixelwise t
      default-frame-alist (append (list
                                   '(vertical-scroll-bars . nil)
                                   '(internal-border-width . 16)
                                   '(right-fringe   . 0)
                                   '(tool-bar-lines . 0))))

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . nil))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-themes
  :functions (doom-themes-treemacs-config doom-themes-org-config)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-atom")
  :config
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (load-theme 'doom-meltbus t))

(use-package theme-looper
  :bind ("C-c t" . theme-looper-select-theme))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-height 25)
  :hook (after-init . doom-modeline-mode))

;; Easily scale the font size up and down
(use-package default-text-scale
  :demand
  :commands default-text-scale-mode
  :config (default-text-scale-mode))

(use-package rainbow-mode
  :commands rainbow-mode
  :diminish
  :hook ((web-mode . rainbow-mode)
         (css-mode . rainbow-mode)))

(provide 'looks)
;;; looks.el ends here

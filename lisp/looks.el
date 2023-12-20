;;; looks.el --- make Emacs look pretty -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; We want Emacs to look nice, with as minimal distraction as possible.
;; 
;;; Code:
;;
(require 'lib)

;; Set the font, depending on the system
(defvar pet/var-my-font
  (cond
   ((pet/is-bsd)
    "Triplicate A Code 10")
   ((pet/is-linux)
    "IBM Plex Mono 10")
   ((pet/is-wsl)
    "BlexMono Nerd Font 15")
   ((pet/is-mac)
    "MonoLisa 12"))
  "My font used across Emacs.")
(pet/set-font pet/var-my-font)

;; Don't show any bars or toolbars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

;; Add some spacing, let it breath
(setq-default line-spacing 0) ;; increase this for fonts like Berkeley
(setq frame-resize-pixelwise t
      default-frame-alist (append (list
                                   '(vertical-scroll-bars . nil)
                                   '(internal-border-width . 7)
                                   '(right-fringe   . 0)
                                   '(tool-bar-lines . 0))))

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(use-package all-the-icons
  :if window-system)

(use-package
 modus-themes
 :custom
 (modus-themes-italic-constructs t)
 (modus-themes-bold-constructs t)
 :config
 ; (load-theme 'modus-vivendi-tinted :no-confirm)
 )

(use-package
 ef-themes
 :custom
 (ef-themes-italic-constructs t)
 (ef-themes-bold-constructs t)
 :config
 (load-theme 'ef-duo-dark :no-confirm))

(use-package almost-mono-themes
  :config
  ;(load-theme 'almost-mono-black :no-confirm)
  )

(use-package stimmung-themes
  :straight (stimmung-themes :host github :repo "motform/stimmung-themes")
  :demand t
  :ensure t
  :config
  ;(stimmung-themes-load-dark)
  )

(use-package mood-line
  :demand
  :commands mood-line-mode
  :config
  (mood-line-mode 1))

;; Toggle the modeline on and off
(use-package hide-mode-line
  :bind ("C-c m" . global-hide-mode-line-mode))

;; Easily scale the font size up and down
(use-package default-text-scale
  :demand
  :commands default-text-scale-mode
  :config (default-text-scale-mode))

(use-package rainbow-mode
  :commands rainbow-mode
  :delight
  :hook ((web-mode . rainbow-mode)
         (css-mode . rainbow-mode)))

(provide 'looks)
;;; looks.el ends here

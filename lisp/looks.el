;;; looks.el --- make Emacs look pretty -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; We want Emacs to look nice, with as minimal distraction as possible.
;; 
;;; Code:
;;
(require 'lib)

(use-package faces
  :straight (:type built-in)
  :custom
  (face-font-family-alternatives
   '(("MonoLisa" "Consolas" "Monaco" "Monospace")))
  :custom-face
  (variable-pitch ((t (:family "Gill Sans"))))
  (fixed-pitch ((t (:family "MonoLisa"))))
  (default ((t (:family "MonoLisa" :height 120)))))

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
 ef-themes
 :custom
 (ef-themes-italic-constructs t)
 (ef-themes-bold-constructs t)
 :config
 ;(load-theme 'ef-duo-dark :no-confirm)
 )

(use-package almost-mono-themes
  :config
  (load-theme 'almost-mono-black :no-confirm))

(use-package lambda-line
  :straight (:type git :host github :repo "lambda-emacs/lambda-line") 
  :config
  (lambda-line-mode) 
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))

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

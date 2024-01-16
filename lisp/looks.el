;;; looks.el --- make Emacs look pretty -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; We want Emacs to look nice, with as minimal distraction as possible.
;; 
;;; Code:
;;
(require 'lib)

;; My current font of choice, this changes by the day. Ouch.
(defvar pet/pref-font "MonoLisa")

;; Set the font
(use-package faces
  :straight (:type built-in)
  :custom
  (face-font-family-alternatives
   '(("Consolas" "Monaco" "Monospace")))
  :custom-face
  (variable-pitch ((t (:family "Geist"))))
  (fixed-pitch ((t (:family ,pet/pref-font))))
  (default ((t (:family ,pet/pref-font :height 120)))))

;; Don't show any bars or toolbars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

;; Add some spacing for those fonts which need it.
(setq-default line-spacing 2)
(setq frame-resize-pixelwise t
      default-frame-alist (append (list
                                   '(vertical-scroll-bars . nil)
                                   '(internal-border-width . 0)
                                   '(right-fringe   . 0)
                                   '(tool-bar-lines . 0))))

;; On the Mac, use dark mode and hide the top bar.
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; Enable this to remove the top bar.
  ;(add-to-list 'default-frame-alist '(undecorated-round . t))
  )

;; Add some more padding
(use-package spacious-padding
  :if (pet/is-mac))

;; Theme of choice
(use-package modus-themes
  :demand ;; the `bind' below lazy loads the pkg, which we don't want.
  :init
  (setq modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
  :bind ("<f5>" . modus-themes-toggle)
  :config
  (customize-set-variable 'modus-themes-common-palette-overrides
        '((bg-region bg-lavender)
          (fg-region unspecified)
          ,@modus-themes-preset-overrides-faint))
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-prompts '(bold))
  (load-theme 'modus-vivendi-tinted :no-confirm)
  (set-face-attribute 'bold nil :weight 'semibold))

;; Toggle the modeline on and off
(use-package hide-mode-line
  :bind ("C-c m" . global-hide-mode-line-mode))

;; Easily scale the font size up and down
(use-package default-text-scale
  :demand
  :commands default-text-scale-mode
  :config (default-text-scale-mode))

;; Color hex codes
(use-package rainbow-mode
  :commands rainbow-mode
  :delight
  :hook ((web-mode . rainbow-mode)
         (css-mode . rainbow-mode)))

;; Automatically size the working window
(use-package golden-ratio
  :delight
  :config
  (golden-ratio-mode +1)
  (setq golden-ratio-auto-scale nil)
  ;; Make golden ratio play nice with other modes
  (dolist (cmd '(ace-window
                 magit-status
                 avy-goto-char
                 avy-goto-char-2
                 avy-goto-word-0
                 avy-goto-word-1
                 avy-goto-line))
    (add-to-list 'golden-ratio-extra-commands
                 cmd)))

(provide 'looks)
;;; looks.el ends here

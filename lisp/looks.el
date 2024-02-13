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

;; Set the font.
(use-package fontaine
  :demand
  :config
  (setq-default text-scale-remap-header-line t)
  (setq-default fontaine-presets
        '((regular)
          (lisp
           :default-family "Triplicate A Code"
           :default-height 140)
          (presentation
           :default-height 160)
          (t
           :default-family "MonoLisa"
           :default-weight regular
           :default-height 130
           ;; nil means it falls back to the default value above
           :fixed-pitch-family nil
           :fixed-pitch-weight nil
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil
           :variable-pitch-family "iA Writer Duo S"
           :variable-pitch-weight nil,
           :variable-pitch-height 1.0
           :bold-family nil
           :bold-weight medium
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))
  (fontaine-set-preset 'regular))

;; Don't show any bars or toolbars.
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

;; show the column at line 100
(setq fill-column 100)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; On the Mac, use dark mode and hide the top bar.
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; Enable this to remove the top bar.
                                        ;(add-to-list 'default-frame-alist '(undecorated-round . t))
  )

;; Add some more padding
(use-package spacious-padding
  :if (pet/is-mac)
  :custom
  (spacious-padding-subtle-mode-line
   '(:mode-line-active success :mode-line-inactive shadow))
  :config
  (spacious-padding-mode 1))

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

;; Color hex codes
(use-package rainbow-mode
  :commands rainbow-mode
  :delight
  :hook ((web-mode . rainbow-mode)
         (css-mode . rainbow-mode)))

(provide 'looks)
;;; looks.el ends here

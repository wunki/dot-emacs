;;; looks.el --- make Emacs look pretty -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; We want Emacs to look nice, with as minimal distraction as possible.
;; 
;;; Code:
;;
(require 'lib)

;; Set the font.
(use-package fontaine
  :demand
  :config
  (setq-default text-scale-remap-header-line t)
  (setq-default fontaine-presets
        '((regular)
          (geist
           :default-family "Geist Mono 1.1"
           :default-height 120
           :line-spacing nil)
          (berkeley
           :default-family "Berkeley Mono 1.2"
           :default-height 120
           :line-spacing nil)
          (lisa
           :default-family "MonoLisa"
           :default-height 120)
          (iosevka
           :default-family "Iosevka Comfy Wide"
           :default-height 120)
          (lisp
           :default-family "Triplicate A Code"
           :default-height 140)
          (presentation
           :default-height 160)
          (t
           :default-family "Dank Mono"
           :default-weight regular
           :default-height 140
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
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'geist))
  (fontaine-mode))

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

;; On the Mac, use dark mode and hide the top bar.
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; Enable this to remove the top bar.
  ;; (add-to-list 'default-frame-alist '(undecorated-round . t))
  )

(use-package standard-themes
  :disabled
  :demand)

;; Theme of choice
(use-package modus-themes
  :disabled
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
        modus-themes-variable-pitch-ui nil
        modus-themes-mixed-fonts t
        modus-themes-prompts '(bold))
  (load-theme 'modus-vivendi-tinted :no-confirm)
  (set-face-attribute 'bold nil :weight 'semibold))

(use-package ef-themes
  :disabled
  :demand
  :config
  (load-theme 'ef-autumn :no-confirm))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-ayu-mirage t)
  (doom-themes-org-config))

;; Some buffers should like different than others
(use-package solaire-mode
  :config
  (solaire-global-mode 1))

;; Ligatures
(use-package ligature
  :config
  (ligature-set-ligatures
   'prog-mode
   '(                                   ; Group A
     ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
     ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
                                        ; Group B
     "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
     "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
     "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
     "|=" "//=" "/="
                                        ; Group C
     "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
     "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
     "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
                                        ; Group D
     "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
                                        ; Group E
     "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
     "{!--" "//" "///" "!!"
                                        ; Group F
     "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
                                        ; Group G
     "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
     "--" "---"))
  (global-ligature-mode t))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package nerd-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-check-simple-format 1))

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

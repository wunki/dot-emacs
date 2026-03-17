;;; pet-looks.el --- make Emacs look pretty -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pet-lib)

;; Fonts
(use-package fontaine
  :demand
  :config
  (setq-default text-scale-remap-header-line t)
  (setq-default fontaine-presets
                '((regular)
                  (default
                   :default-family "Space Mono"
                   :default-height 120
                   :line-spacing 0.1)
                  (t
                   :default-family "Maple Mono NF"
                   :default-weight regular
                   :default-height 130
                   :fixed-pitch-family nil
                   :fixed-pitch-weight nil
                   :fixed-pitch-height 1.0
                   :fixed-pitch-serif-family nil
                   :variable-pitch-family "iA Writer Duo S"
                   :variable-pitch-weight nil
                   :variable-pitch-height 1.0
                   :bold-family nil
                   :bold-weight medium
                   :italic-family nil
                   :italic-slant italic
                   :line-spacing nil)))
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'default))
  (fontaine-mode))

;; No chrome
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

;; Frame defaults
(setq-default line-spacing 2)
(setq frame-resize-pixelwise t
      default-frame-alist (append (list
                                   '(vertical-scroll-bars . nil)
                                   '(internal-border-width . 10)
                                   '(right-fringe . 0)
                                   '(tool-bar-lines . 0))))

;; macOS dark mode
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Theme
(use-package modus-themes
  :demand
  :init
  (setq modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
  :bind ("<f5>" . modus-themes-toggle)
  :config
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-variable-pitch-ui nil
        modus-themes-mixed-fonts t
        modus-themes-prompts '(bold)
        modus-themes-common-palette-overrides
        '((bg-region bg-lavender)
          (fg-region unspecified)))
  (load-theme 'modus-vivendi-tritanopia :no-confirm)
  ;; SpaceMono only has Regular/Bold. Map bold to regular weight
  ;; so syntax highlighting uses color only, not heavy strokes.
  (set-face-attribute 'bold nil :weight 'regular))

;; Ligatures
(use-package ligature
  :config
  (ligature-set-ligatures
   'prog-mode
   '(".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
     ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
     "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
     "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
     "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
     "|=" "//=" "/="
     "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
     "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
     "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
     "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
     "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
     "{!--" "//" "///" "!!"
     "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
     "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
     "--" "---"))
  (global-ligature-mode t))

;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-check-simple-format t))

;; Toggle modeline
(use-package hide-mode-line
  :bind ("C-c m" . global-hide-mode-line-mode))

;; Colorize hex codes
(use-package rainbow-mode
  :hook ((web-mode . rainbow-mode)
         (css-mode . rainbow-mode)))

(provide 'pet-looks)
;;; pet-looks.el ends here

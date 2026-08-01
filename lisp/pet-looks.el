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
                  (ibm
                   :default-family "IBM Plex Mono"
                   :line-spacing 0.15
                   )
                  (source-code-pro
                   :default-family "Source Code Pro"
                   :line-spacing 0.2
                   )
                  (space
                   :default-family "Space Mono"
                   )
                  (maple
                   :default-family "Maple Mono Normal")
                  (t
                   :default-family "Maple Mono Normal"
                   :default-weight regular
                   :default-height 120
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
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'source-code-pro))
  (fontaine-mode))

;; Variable pitch for text modes
(add-hook 'text-mode-hook #'variable-pitch-mode)

;; No chrome
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

;; Frame defaults
(setq frame-resize-pixelwise t
      default-frame-alist (append (list
                                   '(vertical-scroll-bars . nil)
                                   '(internal-border-width . 10)
                                   '(right-fringe . 8)
                                   '(tool-bar-lines . 0))))

;; macOS titlebar
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Themes
(use-package ef-themes
  :defer t
  :init
  (setq ef-themes-to-toggle '(ef-duo-dark ef-duo-light))
  :bind ("<f5>" . ef-themes-toggle)
  :config
  (setq ef-themes-variable-pitch-ui nil
        ef-themes-mixed-fonts t
        ef-themes-headings
        '((0 variable-pitch 1.5)
          (1 variable-pitch 1.3)
          (2 variable-pitch 1.2)
          (agenda-date 1.3)
          (agenda-structure variable-pitch light 1.8)
          (t variable-pitch))))

(use-package modus-themes
  :defer t
  :config
  (setq modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted)
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-variable-pitch-ui nil
        modus-themes-mixed-fonts t
        modus-themes-prompts '(bold)
        modus-themes-common-palette-overrides
        '((bg-region bg-lavender)
          (fg-region unspecified))))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (with-eval-after-load 'org (doom-themes-org-config))
  ;; Emacs 31 rejects a temporary inheritance cycle while doom-themes
  ;; applies its Gnus faces. Define the faces first and break the cycle in
  ;; their base specification; doom-pine then applies its intended values.
  (when (>= emacs-major-version 31)
    (require 'gnus)
    (face-spec-set 'gnus-group-news-low
                   '((t (:inherit gnus-group-mail-1)))
                   'face-defface-spec))
  (load-theme 'doom-pine :no-confirm))

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
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-check-icon nil))

;; Toggle modeline
(use-package hide-mode-line
  :bind ("C-c m" . global-hide-mode-line-mode))

(provide 'pet-looks)
;;; pet-looks.el ends here

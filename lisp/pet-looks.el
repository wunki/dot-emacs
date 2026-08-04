;;; pet-looks.el --- make Emacs look pretty -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Fonts, themes, frame chrome, and other visual presentation.
;; Graphical-frame setup is deferred when Emacs runs as a daemon.
;;
;;; Code:

(require 'pet-packages)

(defvar ns-pop-up-frames)

;; Apply fonts only after a graphical frame exists.  A daemon starts without
;; one, while a direct terminal session should never load Fontaine at all.
(defun pet/setup-fontaine-for-frame (frame)
  "Apply the saved Fontaine preset to graphical FRAME."
  (when (display-graphic-p frame)
    (with-selected-frame frame
      (fontaine-set-preset (or (fontaine-restore-latest-preset) 'ibm))
      (fontaine-mode 1))))

(use-package fontaine
  :commands (fontaine-mode fontaine-restore-latest-preset fontaine-set-preset)
  :init
  (setq-default text-scale-remap-header-line t)
  (setq-default fontaine-presets
                '((regular)
                  (ibm
                   :default-family "IBM Plex Mono"
                   :line-spacing 0.15)
                  (go
                   :default-family "GoMono Nerd Font"
                   :line-spacing 0.2)
                  (source-code-pro
                   :default-family "Source Code Pro"
                   :line-spacing 0.2)
                  (space
                   :default-family "Space Mono")
                  (martian
                   :default-family "Martian Mono"
                   :default-weight light
                   :default-width semi-condensed
                   :line-spacing 0.15)
                  (intel
                   :default-family "Intel One Mono")
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
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'pet/setup-fontaine-for-frame)
    (pet/setup-fontaine-for-frame (selected-frame))))

;; Variable pitch for text modes
(add-hook 'text-mode-hook #'variable-pitch-mode)

;; No chrome
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

;; Frame defaults.  Update the settings we own without discarding parameters
;; installed by Emacs, the daemon, or other packages.
(setq frame-resize-pixelwise t)
(dolist (parameter '((vertical-scroll-bars . nil)
                     (internal-border-width . 10)
                     (right-fringe . 8)
                     (tool-bar-lines . 0)))
  (setf (alist-get (car parameter) default-frame-alist)
        (cdr parameter)))

;; macOS frames may be created only after daemon startup.
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;; Open files from Finder in the current frame.
  (setq ns-pop-up-frames nil))

(defun pet/disable-enabled-themes (&rest _)
  "Disable active themes before a new theme is loaded."
  (mapc #'disable-theme custom-enabled-themes))

(advice-add 'load-theme :before #'pet/disable-enabled-themes)

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
        modus-themes-common-palette-overrides
        '((bg-region bg-lavender)
          (fg-region unspecified))))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (with-eval-after-load 'org (doom-themes-org-config))
  (load-theme 'doom-meltbus :no-confirm)
  ;; Make Corfu's selected candidate and Orderless matches easy to read.
  (custom-theme-set-faces
   'doom-meltbus
   '(corfu-current ((t (:background "#303030" :foreground "#ffffff" :extend t))))
   ;; Show only diff-hl's thin fringe bitmap, not its colored backdrop.
   '(diff-hl-insert ((t (:foreground "#448844" :background "black"))))
   '(diff-hl-change ((t (:foreground "#da8548" :background "black"))))
   '(diff-hl-delete ((t (:foreground "#f8b0b0" :background "black"))))
   '(orderless-match-face-0 ((t (:background "#303030" :foreground "#ffffff" :weight bold))))
   '(orderless-match-face-1 ((t (:background "#303030" :foreground "#ffffff" :weight bold))))
   '(orderless-match-face-2 ((t (:background "#303030" :foreground "#ffffff" :weight bold))))
   '(orderless-match-face-3 ((t (:background "#303030" :foreground "#ffffff" :weight bold))))))

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

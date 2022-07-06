;;; looks.el --- make Emacs look pretty -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; We want Emacs to look nice, with as minimal distraction as possible.
;; 
;;; Code:
;;

;;; Code:
(require 'lib)

(use-package default-text-scale
  :demand
  :commands default-text-scale-mode
  :config (default-text-scale-mode))

;; Set the font, depending on the system
(defvar petars-font
  (cond
   ((pet/is-linux) "Iosevka SS08 11")
   ((pet/is-wsl) "Iosevka SS15 18")
   ((pet/is-mac) "Iosevka SS15 16")))

(pet/set-font petars-font)

;; Don't show any bars or toolbars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package modus-themes
  :commands (modus-themes-load-themes modus-themes-load-operandi)
  :init
  (setq modus-themes-mode-line '(accented borderless (padding . 4))
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend)
        modus-themes-fringes 'subtle
        modus-themes-tabs-accented t
        modus-themes-paren-match '(bold intense)
        modus-themes-org-blocks 'tinted-background
        modus-themes-region '(bg-only))
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))

(use-package fontset
  :straight (:type built-in) ;; only include this if you use straight
  :config
  ;; Use symbola for proper unicode
  (when (member "Symbola" (font-family-list))
    (set-fontset-font
     t 'symbol "Symbola" nil)))

;; Semantic parser for languages, which will give us nicer
;; syntax highlighting.
(use-package tree-sitter
  :if (executable-find "tree-sitter")
  :defines tree-sitter-major-mode-language-alist
  :hook
  (
    ((rustic-mode python-mode css-mode elixir-mode zig-mode)
      .
      tree-sitter-mode)
    ((rustic-mode python-mode css-mode elixir-mode zig-mode)
      .
      tree-sitter-hl-mode))
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist
    '(rustic-mode . rust))
  (add-to-list 'tree-sitter-major-mode-language-alist
    '(zig-mode . zig))
  (add-to-list 'tree-sitter-major-mode-language-alist
    '(elixir-mode . elixir)))

(use-package tree-sitter-langs
  :if (executable-find "tree-sitter")
  :after tree-sitter)

(use-package rainbow-mode
  :commands rainbow-mode
  :diminish
  :hook ((web-mode . rainbow-mode) (css-mode . rainbow-mode)))

(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(provide 'looks)
;;; looks.el ends here

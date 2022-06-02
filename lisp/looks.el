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
   ((pet/is-linux) "Iosevka SS15 13")
   ((pet/is-wsl) "Iosevka SS15 18")
   ((pet/is-mac) "Iosevka SS15 16")))

(pet/set-font petars-font)

;; Don't show any bars or toolbars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package kaolin-themes
  :demand
  :commands kaolin-treemacs-theme
  :custom
  (kaolin-themes-italic-comments t)
  (kaolin-themes-git-gutter-solid nil)
  :config
  (load-theme 'kaolin-mono-dark t)
  (kaolin-treemacs-theme))

(use-package lambda-line
  :commands (lambda-line-mode)
  :straight (:type git :host github :repo "lambda-emacs/lambda-line")
  :custom
  (lambda-line-position 'bottom) ;; Set position of status-line
  (lambda-line-abbrev t) ;; abbreviate major modes
  (lambda-line-hspace "  ")  ;; add some cushion
  (lambda-line-prefix t) ;; use a prefix symbol
  (lambda-line-prefix-padding nil) ;; no extra space for prefix
  (lambda-line-status-invert nil)  ;; no invert colors
  (lambda-line-gui-ro-symbol  " RO") ;; symbols
  (lambda-line-gui-mod-symbol " M")
  (lambda-line-gui-rw-symbol  " RW")
  (lambda-line-space-top +.10)  ;; padding on top and bottom of line
  (lambda-line-space-bottom -.10)
  (lambda-line-symbol-position 0) ;; adjust the vertical placement of symbol
  :config
  ;; activate lambda-line
  (lambda-line-mode)
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
   (setq-default mode-line-format (list "%_"))
   (setq mode-line-format (list "%_"))))

(use-package fontset
  :straight (:type built-in) ;; only include this if you use straight
  :config
  ;; Use symbola for proper unicode
  (when (member "Symbola" (font-family-list))
    (set-fontset-font
     t 'symbol "Symbola" nil)))

;; Semantic parser for languages, which will give us nicer
;; syntax highlighting
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

(provide 'looks)
;;; looks.el ends here

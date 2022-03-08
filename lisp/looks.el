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

(if (pet/is-linux)
  (push '(font . "Iosevka SS02 12") default-frame-alist)
  (push '(font . "Iosevka SS02-16") default-frame-alist))

;; Get rid of any bars
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

(use-package doom-modeline
  :commands doom-modeline-mode
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-major-mode-color-icon nil)
  (doom-modeline-buffer-encoding nil))

;; Semantic parser for languages, which will give us nicer
;; syntax highlighting
(use-package tree-sitter
  :if (executable-find "tree-sitter")
  :defines tree-sitter-major-mode-language-alist
  :hook
  (
    ((rustic-mode python-mode css-mode elixir-mode)
      .
      tree-sitter-mode)
    ((rustic-mode python-mode css-mode elixir-mode)
      .
      tree-sitter-hl-mode))
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist
    '(rustic-mode . rust))
  (add-to-list 'tree-sitter-major-mode-language-alist
    '(elixir-mode . elixir)))

(use-package tree-sitter-langs
  :if (executable-find "tree-sitter")
  :after tree-sitter)

(provide 'looks)
;;; looks.el ends here

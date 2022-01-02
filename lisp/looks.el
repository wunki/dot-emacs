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

(if (pet/is-linux)
  (push '(font . "Iosevka SS02 12") default-frame-alist)
  (push '(font . "MonoLisa-14") default-frame-alist))

;; Get rid of any bars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package kaolin-themes
  :demand
  :commands kaolin-treemacs-theme
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
  :straight
  (tree-sitter
    :type git
    :host github
    :repo "ubolonton/emacs-tree-sitter"
    :files ("lisp/*.el" "src" "Cargo.toml" "Cargo.lock"))
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
  :straight
  (tree-sitter-langs
    :type git
    :host github
    :repo "ubolonton/emacs-tree-sitter"
    :files ("langs/*.el" "langs/queries"))
  :after tree-sitter)

(provide 'looks)
;;; looks.el ends here

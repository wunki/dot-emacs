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

;; Easily scale the font size up and down
(use-package default-text-scale
  :demand
  :commands default-text-scale-mode
  :config (default-text-scale-mode))

;; Set the font, depending on the system
(defvar petars-font
  (cond
   ((pet/is-linux) "JetBrains Mono 9")
   ((pet/is-wsl) "Iosevka SS15 18")
   ((pet/is-mac) "Triplicate T4 13")))

(pet/set-font petars-font)

;; Don't show any bars or toolbars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

;; Add some spacing
(setq frame-resize-pixelwise t
      default-frame-alist    (append (list
                                      '(vertical-scroll-bars . nil)
                                      '(internal-border-width . 18)
                                      '(right-fringe   . 0)
                                      '(tool-bar-lines . 0))))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package stimmung-themes
  :straight (stimmung-themes :host github :repo "motform/stimmung-themes")
  :commands stimmung-themes-load-light
  :demand t
  :ensure t
  :config (stimmung-themes-load-light))

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
  :hook ((web-mode . rainbow-mode)
         (css-mode . rainbow-mode)))

(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(provide 'looks)
;;; looks.el ends here

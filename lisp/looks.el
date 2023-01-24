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
   ((pet/is-bsd) "Triplicate T4 10")
   ((pet/is-linux) "Triplicate T4 12")
   ((pet/is-wsl) "Triplicate T4 17")
   ((pet/is-mac) "IBM Plex Mono 13")))

(pet/set-font petars-font)

;; Don't show any bars or toolbars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

;; Add some spacing
(setq frame-resize-pixelwise t
      default-frame-alist    (append (list
                                      '(vertical-scroll-bars . nil)
                                      '(internal-border-width . 14)
                                      '(right-fringe   . 0)
                                      '(tool-bar-lines . 0))))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package stimmung-themes
  :straight (stimmung-themes :host github :repo "motform/stimmung-themes")
  :commands stimmung-themes-load-light
  :demand t
  :ensure t
  :config
  (load-theme 'stimmung-themes-light t))

(use-package modus-themes
  :ensure
  :commands (modus-themes-load-themes modus-themes-load-vivendi)
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-region '(bg-only no-extend))
  (modus-themes-syntax '(faint yellow-comments))
  (modus-themes-mode-line '(accented borderless (padding . 4) (height . 0.9)))
  (modus-themes-paren-match '(bold))
  :init
  ;; (modus-themes-load-themes)
  :config
  ;; (modus-themes-load-vivendi)
  :bind ("C-c C-t" . modus-themes-toggle))

(use-package ef-themes
  :ensure)

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

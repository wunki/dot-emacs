;;; languages.el --- setup differeng languages -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Configuration for the different languages that I may use.
;;
;;; Code:
;;
(require 'lib)

;; Display Emacs lisp results inline.
(use-package eros
  :demand
  :commands eros-mode
  :config (eros-mode t))

(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :custom
  (elisp-autofmt-python-bin "python3"))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path
   'inherit
   "inherit the load path so it can find all libraries")
  (flycheck-check-syntax-automatically
   '(mode-enabled save)
   "only check on save"))

;; Tree-sitter
(use-package treesit
  :straight (:type built-in)
  :preface
  (defun pet/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css "https://github.com/tree-sitter/tree-sitter-css")
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (python "https://github.com/tree-sitter/tree-sitter-python")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (yaml "https://github.com/ikatyang/tree-sitter-yaml")
               (java "https://github.com/tree-sitter/tree-sitter-java")
               (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
               (heex "https://github.com/phoenixframework/tree-sitter-heex")
               (zig "https://github.com/GrayJack/tree-sitter-zig")
               (rust "https://github.com/tree-sitter/tree-sitter-rust")
               (clojure "https://github.com/sogaiu/tree-sitter-clojure")))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  (dolist (mapping '((python-mode . python-ts-mode)
                     (java-mode . java-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
                     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (pet/setup-install-grammars))

;; Clojure

;; (use-package clojure-ts-mode
;;   :straight (clojure-ts-mode :type git
;;                              :host github
;;                              :repo "clojure-emacs/clojure-ts-mode"))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode lisp-mode eval-expression-minibuffer-setup slime-repl-mode clojure-mode racket-mode)))

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo))

(use-package clj-refactor
  :after clojure-mode
  :commands cljr-add-keybindings-with-prefix
  :hook clojure-mode
  :custom
  (cljr-assume-language-context "clj")
  :config
  (cljr-add-keybindings-with-prefix "C-c C-r"))

(use-package cider
  :custom
  ;; We use clojure-lsp for showing documentation
  (cider-eldoc-display-for-symbol-at-point nil)

  ;; we don't need the help banner anymore
  (cider-repl-display-help-banner nil)

  ;; save files when evaluating them
  (cider-save-file-on-load t)

  ;; specify the history file
  (cider-history-file "~/.config/emacs/nrepl-history")

  ;; auto-select the error buffer when it's displayed
  (cider-auto-select-error-buffer t)

  ;; pretty print results in repl
  (cider-repl-use-pretty-printing t)

  ;; hide nrepl buffers when switching between buffers
  (nrepl-hide-special-buffers t)

  ;; don't prompt for symbols, try to use the one currently at prompt
  (cider-prompt-for-symbol nil)

  ;; clean up the buffer before saving
  :hook (before-save . cider-format-buffer)
  :bind (:map cider-mode-map
         ("C-c C-l" . cider-find-and-clear-repl-output)
         :map cider-repl-mode-map
         ("C-c C-l" . cider-repl-clear-buffer)))

;; Elixir
(use-package elixir-ts-mode)

;; Documentation
(use-package eldoc
  :straight nil
  :delight
  :hook (prog-mode-hook . eldoc-mode)
  :init
  (global-eldoc-mode 1))

(use-package eldoc-box
  :hook (eldoc-mode-hook . eldoc-box-hover-mode)
  :commands eldoc-box--default-upper-corner-position-function
  :init
  (setq eldoc-box-position-function #'eldoc-box--default-upper-corner-position-function
        eldoc-box-clear-with-C-g t))

;; Rust
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :preface
  (defun pet/rustic-mode-hook ()
    (when buffer-file-name
      (setq-local buffer-save-without-query t)))
  :bind
  (:map
    rustic-mode-map
    ("M-?" . lsp-find-references)
    ("C-c C-c l" . flycheck-list-errors))
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-format-on-save t)

  :config
  ;; if we don't delete rust-mode, after saving, the buffer will
  ;; jump to rust-mode instead of rustic-mode. Which we do not want.
  (setq auto-mode-alist (rassq-delete-all 'rust-mode auto-mode-alist))

  ;; change emacs PATH to include cargo/bin
  (setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
  (setq exec-path (append exec-path '("~/.cargo/bin")))
  (add-hook 'rustic-mode-hook 'pet/rustic-mode-hook))

;; Zig
(use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode))

;; Fish, my shell of choice
(use-package fish-mode)

;; Yaml, yaml
(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
    ("\\.md\\'" . gfm-mode)
    ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Common Lisp
(use-package sly
  :custom
  (inferior-lisp-program "sbcl"))

;; Racket
(use-package racket-mode
  :hook (racket-mode . racket-xp-mode))

;; Docker
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; Highlight TODO keywords in source code
(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

(provide 'languages)
;;; languages.el ends here

;;; init.el --- initialize all modules -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This is where it all starts, we start by setting up our packages
;; where we use the excellent straight package manager, combined with
;; the use-package to declare the packages we want to use.
;; 
;;; Code:
;;

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'packages)
(require 'config)
(require 'lib)
(require 'bindings)
(require 'navigation)
(require 'looks)
(require 'completion)
(require 'editing)
(require 'notes)
(require 'language-server)
(require 'languages)
(require 'ai)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(gptel sqlformat hl-todo fish-mode rustic eldoc-box elixir-ts-mode treesit-auto flycheck elisp-autofmt eros lsp-grammarly lsp-mode flycheck-eglot olivetti git-link gitignore-templates forge goto-last-change super-save kind-icon orderless hide-mode-line doom-modeline kanagawa-theme catppuccin-theme ef-themes restart-emacs helpful no-littering s diminish zig-mode yasnippet yaml-mode which-key web-mode vertico swiper spacious-padding sly rainbow-mode rainbow-delimiters racket-mode popper paredit modus-themes markdown-mode marginalia magit fontaine expand-region exec-path-from-shell dockerfile-mode denote delight crux cider ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

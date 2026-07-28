;;; pet-language-server.el --- eglot configuration -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Eglot is the built-in LSP client since Emacs 29.
;;
;;; Code:

(require 'pet-lib)

(use-feature eglot
  :preface
  (defun pet/eglot-format-buffer-on-save ()
    "Format the buffer on save."
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

  :config
  (setq eglot-autoshutdown t
        eglot-autoreconnect t
        eglot-extend-to-xref t
        eglot-send-changes-idle-time 0.5
        eglot-ignored-server-capabilities '(:foldingRangeProvider)
        ;; Render LSP hover docs with the built-in markdown-ts viewer
        ;; (experimental) instead of falling back to plain text.
        eglot-documentation-renderer 'markdown-ts-view-mode
        ;; Some servers make the inline "code action here" hints noisy.
        eglot-code-action-indications nil)
  ;; Let orderless work with eglot completions
  (add-to-list 'completion-category-overrides '(eglot (styles orderless basic)))
  (add-to-list 'eglot-server-programs
               '((clojure-mode clojurescript-mode clojurec-mode
                  clojuredart-mode clojure-ts-mode)
                 "clojure-lsp"))

  :hook (((clojure-mode clojurescript-mode clojurec-mode clojuredart-mode
           clojure-ts-mode) . eglot-ensure)
         ((clojure-mode clojurescript-mode clojurec-mode clojuredart-mode
           clojure-ts-mode) . pet/eglot-format-buffer-on-save))

  :bind (:map eglot-mode-map
              ("C-c C-f" . eglot-format-buffer)
              ("C-c a r" . eglot-rename)
              ("C-c C-c" . eglot-code-actions)))

;; LSP snippet expansion via tempel
(use-package eglot-tempel
  :after (eglot tempel)
  :config
  (eglot-tempel-mode 1))

(provide 'pet-language-server)
;;; pet-language-server.el ends here

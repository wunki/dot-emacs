;;; language-server.el --- setup LSP server -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Setup the `eglot' package to intrude as little as possible
;; and work with the languages I use.
;;
;;; Code:
;;
(require 'lib)
(require 'project)
(require 'eglot)

(defun pet/eglot-organize-imports ()
  "Organizes the imports."
  (interactive)
  (eglot-code-actions nil nil "source.organizeImports" t))

(defun pet/eglot-format-buffer-on-save ()
  "Format's the buffer on save."
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(defun pet/eglot-organize-imports-on-save ()
  "Organizes the imports on save."
  (add-hook 'before-save-hook #'pet/eglot-organize-imports nil t))

(use-package eglot
  :straight (eglot :type git
                   :host nil
                   :repo "git://git.sv.gnu.org/emacs.git"
                   :files ("lisp/progmodes/eglot.el")
  :config
  ;; we can't add this to :hook because we want it to be
  ;; buffer local.
  (setq eglot-autoshutdown t
        eglot-autoreconnect t
        eglot-extend-to-xref t
        eglot-events-buffer-size nil
        eglot-send-changes-idle-time 0.5
        eglot-ignored-server-capabilities '(:hoverProvider :inlayHintProvider))
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "nextls" "--stdio=true"))
  (add-to-list 'eglot-server-programs '(c-mode "clangd"))
  :hook (((zig-mode elixir-ts-mode heex-ts-mode go-mode c-mode) . eglot-ensure)
         (go-mode . pet/eglot-format-buffer-on-save)
         (go-mode . pet/eglot-organize-imports-on-save)
         (elixir-ts-mode . pet/eglot-format-buffer-on-save))
  :bind (:map eglot-mode-map
              ("C-c C-f" . #'eglot-format-buffer)
              ("C-c a r" . #'eglot-rename)
              ("C-c C-c" . #'eglot-code-actions))))

(use-package eglot-grammarly
  :straight (:host github :repo "emacs-grammarly/eglot-grammarly")
  :hook ((markdown-mode org-mode) . (lambda ()
                                      (require 'eglot-grammarly)
                                      (eglot-ensure))))

;; Setup Go, which needs to look for the go module.
(defun project-find-go-module (dir)
  "Find the root of the project by finding go.mod file in DIR."
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  "I have no idea what this does with PROJECT."
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)


(provide 'language-server)
;;; language-server.el ends here

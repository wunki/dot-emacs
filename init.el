;;; init.el --- initialize all modules -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs 30 configuration using built-in package.el and use-package.
;;
;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'pet-packages)

;; Load secrets (API keys, passwords). Fails gracefully if GPG key
;; is unavailable, so the rest of the config still loads.
;; (let ((secrets-file (expand-file-name "lisp/pet-secrets.el.gpg"
;;                                       user-emacs-directory)))
;;   (condition-case err
;;       (when (file-exists-p secrets-file)
;;         (load-library secrets-file))
;;     (error (message "Could not load secrets: %s" (error-message-string err)))))

(require 'pet-config)
(require 'pet-lib)
(require 'pet-bindings)
(require 'pet-navigation)
(require 'pet-looks)
(require 'pet-completion)
(require 'pet-icons)
(require 'pet-editing)
(require 'pet-git)
(require 'pet-notes)
(require 'pet-language-server)
(require 'pet-languages)
(require 'pet-irc)
(require 'pet-ai)

(provide 'init)
;;; init.el ends here

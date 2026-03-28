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

(require 'pet-config)
(require 'pet-lib)
(require 'pet-bindings)
(require 'pet-navigation)
(require 'pet-icons)
(require 'pet-looks)
(require 'pet-completion)
(require 'pet-editing)
(require 'pet-git)
(require 'pet-notes)
(require 'pet-language-server)
(require 'pet-languages)
(require 'pet-irc)
(require 'pet-ai)

(provide 'init)
;;; init.el ends here

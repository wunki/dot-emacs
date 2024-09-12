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

(require 'pet-packages)
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

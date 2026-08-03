;;; init.el --- initialize all modules -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs 30 configuration using built-in package.el and use-package.
;;
;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'elisp-flymake-byte-compile-load-path
             (expand-file-name "lisp" user-emacs-directory))

;; Trust the content for my elisp files
(add-to-list 'trusted-content
             (file-name-as-directory
              (abbreviate-file-name
               (file-truename user-emacs-directory))))

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
(require 'pet-tramp)
(require 'pet-notes)
(require 'pet-lisp)
(require 'pet-language-server)
(require 'pet-languages)
(require 'pet-ai)

(provide 'init)
;;; init.el ends here

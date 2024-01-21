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
 '(package-selected-packages '(vertico-directory visual-line-mode dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

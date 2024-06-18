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

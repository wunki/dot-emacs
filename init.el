;; Add my configuration directory to the load path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'config)
(require 'lib)
(require 'bindings)
(require 'packages)
(require 'looks)
(require 'editing)
(require 'notes)

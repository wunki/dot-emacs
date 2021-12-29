
;; Add my configuration directory to the load path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'packages)
(require 'config)
(require 'lib)
(require 'bindings)
(require 'navigation)
(require 'looks)
(require 'editing)
(require 'notes)
(require 'language-server)
(require 'languages)

(if (is-linux)
  (require 'mail))

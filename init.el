
;; Add my configuration directory to the load path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'config)
(require 'lib)
(require 'bindings)
(require 'packages)
(require 'navigation)
(require 'looks)
(require 'editing)
(require 'notes)

;; Languages
(require 'clojure)

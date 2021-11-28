
;; Add my configuration directory to the load path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'config)
(require 'packages)
(require 'lib)
(require 'bindings)
(require 'navigation)
(require 'looks)
(require 'editing)
(require 'notes)

(if (is-linux)
  (require 'mail))

;; Languages
(require 'clojure)

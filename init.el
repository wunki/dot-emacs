
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

;; This is where I put the stuff which may not be useful to you directly
(defvar pet/my-own-dir (expand-file-name "my-own" user-emacs-directory))
(when (file-exists-p pet/my-own-dir)
  (message "Loading personal configuration files in %s..." pet/my-own-dir)
  (mapc 'load (directory-files pet/my-own-dir 't "^[^#\.].*\\.el$")))

(if (pet/is-linux)
  (require 'mail))

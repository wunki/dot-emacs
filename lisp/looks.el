;;; looks.el --- Setup the looks of Emacs -*- lexical-binding: t -*-

;;; Code:

(require 'lib)

(use-package nano-theme
  :straight (:host github
	     :repo "rougier/nano-theme")
  :config
  (nano-mode)
  (nano-dark))

(use-package nano-modeline
  :straight (:host github
	     :repo "rougier/nano-modeline")
  :config (nano-modeline-mode))

;; Set the font
(if (is-linux)
    (set-frame-font "Iosevka SS02 12" nil t)
  (set-frame-font "Iosevka SS02 18" nil t))

;; Get rid of any bars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(provide 'looks)
;;; looks.el ends here

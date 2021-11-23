;;; looks.el --- Setup the looks of Emacs -*- lexical-binding: t -*-

;;; Code:

(use-package nano-theme
  :straight (:host github
	     :repo "rougier/nano-theme")
  :config (nano-light))

(use-package nano-modeline
  :straight (:host github
	     :repo "rougier/nano-modeline")
  :config (nano-modeline-mode))

;; Set the font
(set-frame-font "Iosevka SS02 10" nil t)

;; Get rid of any bars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(provide 'looks)
;;; looks.el ends here

;;; looks.el --- Setup the looks of Emacs -*- lexical-binding: t -*-

;;; Code:

(require 'lib)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package kaolin-themes
  :config
  (load-theme 'kaolin-mono-dark t)
  (kaolin-treemacs-theme))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-major-mode-color-icon nil))

;; Set the font
(if (is-linux)
    (set-frame-font "Iosevka SS02 12" nil t)
  (set-frame-font "Jetbrains Mono 15" nil t))

;; Get rid of any bars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(provide 'looks)
;;; looks.el ends here

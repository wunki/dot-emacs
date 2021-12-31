;;; bindings.el --- bindings not specific to a mode -*- lexical-binding: t -*-

(require 'lib)

;;; Code:

;; Kill region, or when nothing selected, the word.
(global-set-key (kbd "C-w") 'pet/kill-region-or-backward-word)

;; Always indent after a newline.
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Quickly edit my config
(global-set-key (kbd "C-c I") 'pet/find-config)

;; Toggle between themes
(global-set-key (kbd "C-c t") 'nano-theme-toggle)

(provide 'bindings)
;;; bindings.el ends here

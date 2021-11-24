;;; bindings.el --- bindings not specific to a mode -*- lexical-binding: t -*-

(require 'lib)

;;; Code:

;; Kill region, or when nothing selected, the word.
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

;; Always indent after a newline.
(define-key global-map (kbd "RET") 'newline-and-indent)

(provide 'bindings)
;;; bindings.el ends here

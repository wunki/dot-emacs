;;; bindings.el --- bindings not specific to a mode -*- lexical-binding: t -*-

(require 'lib)

;;; Code:

;; Kill region, or when nothing selected, the word.
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

(provide 'bindings)
;;; bindings.el ends here

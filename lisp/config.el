;;; config.el --- Emacs tweakers, not dependant on packages -*- lexical-binding: t -*-

;;; Code:

(setq initial-scratch-message nil)  ; don't show the scratch message
(global-auto-revert-mode 1)         ; update the buffer when a file changes
(delete-selection-mode 1)           ; delete marked region 

(provide 'config)
;;; config.el ends here


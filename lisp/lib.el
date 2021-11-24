;;; lib.el --- handcrafted helper methods -*- lexical-binding: t -*-

;;; Code:

(defun kill-region-or-backward-word ()
  "Kill either the word backwards or the active region"
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(provide 'lib)
;;; lib.el ends here

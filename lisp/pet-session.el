;;; pet-session.el --- preserve state across Emacs clients -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Preserve the last terminal client's window layout while the daemon remains
;; alive.  State is kept in memory, applies only to terminal frames, and the
;; most recently closed terminal frame wins.
;;

;;; Code:

(require 'server)

;; Recreate the last terminal layout when reconnecting to the daemon.
(defvar pet/terminal-window-state nil
  "Window state from the most recently closed terminal frame.")

(defun pet/save-terminal-window-state (frame)
  "Remember the window layout of terminal FRAME before it is deleted."
  (when (and (daemonp)
             (frame-live-p frame)
             (not (display-graphic-p frame)))
    (condition-case error
        (setq pet/terminal-window-state
              (window-state-get (frame-root-window frame)))
      (error
       (message "Could not save terminal window layout: %s"
                (error-message-string error))))))

(defun pet/restore-terminal-window-state ()
  "Restore the window layout from the last closed terminal frame."
  (when (and pet/terminal-window-state
             (not (display-graphic-p)))
    (condition-case error
        (window-state-put pet/terminal-window-state
                          (frame-root-window)
                          'safe)
      (error
       (message "Could not restore terminal window layout: %s"
                (error-message-string error))))))

(add-hook 'delete-frame-functions #'pet/save-terminal-window-state)
(add-hook 'server-after-make-frame-hook
          #'pet/restore-terminal-window-state)

(provide 'pet-session)
;;; pet-session.el ends here

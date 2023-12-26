;;; bindings.el --- key bindings -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Setup key bindings for either Emacs or our own functions.
;; Package specific bindings should be done in `use-package'
;; declarations.
;; 
;;; Code:
;;

(require 'lib)

;;; Code:

;; Kill region, or when nothing selected, the word
(global-set-key (kbd "C-w") 'pet/kill-region-or-backward-word)

;; Always indent after a newline
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Easier to get to the command menu
(define-key global-map (kbd "C-x C-m") 'execute-extended-command)

;; Quickly edit my config
(global-set-key (kbd "C-c I") 'pet/edit-emacs-configuration)

;; Rename file and buffer
(global-set-key (kbd "C-c r") 'pet/rename-file-and-buffer)

(provide 'bindings)
;;; bindings.el ends here

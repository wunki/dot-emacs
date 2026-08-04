;;; pet-bindings.el --- key bindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pet-lib)
(require 'pet-editing)

(defvar mac-command-modifier)
(defvar mac-option-modifier)
(declare-function ns-do-hide-emacs "ns-win")

;; Always indent after a newline
(define-key global-map (kbd "RET") #'newline-and-indent)

;; Easier to get to the command menu
(define-key global-map (kbd "C-x C-m") #'execute-extended-command)

;; Quickly edit my config
(global-set-key (kbd "C-c I") #'pet/edit-emacs-configuration)

;; Rename file and buffer
(global-set-key (kbd "C-c r") #'pet/rename-file-and-buffer)

;; Global text scaling
(global-set-key (kbd "C-x C-=") #'global-text-scale-adjust)
(global-set-key (kbd "C-x C-+") #'global-text-scale-adjust)
(global-set-key (kbd "C-x C-0") #'global-text-scale-adjust)
(global-set-key (kbd "C-x C--") #'global-text-scale-adjust)

;; Rearrange the window layout without manual split/kill
(global-set-key (kbd "C-x w t")   #'window-layout-transpose)
(global-set-key (kbd "C-x w r")   #'window-layout-rotate-clockwise)
(global-set-key (kbd "C-x w f h") #'window-layout-flip-leftright)
(global-set-key (kbd "C-x w f v") #'window-layout-flip-topdown)

;; macOS uses Command as Meta and Option as Super.
(when (pet/is-mac)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super)
  (global-set-key (kbd "M-h") #'ns-do-hide-emacs)
  (global-set-key (kbd "s-h") #'mark-paragraph))

(provide 'pet-bindings)
;;; pet-bindings.el ends here

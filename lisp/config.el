;;; config.el --- Emacs tweaks, not dependant on packages -*- lexical-binding: t -*-

;;; Code:

(require 'lib)

;; Make sure we always use UTF-8.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

(setq inhibit-splash-screen t)      ; don't show the splash screen
(setq initial-scratch-message nil)  ; don't show the scratch message
(setq vc-follow-symlinks t)         ; just follow symlinks, don't ask me
(defalias 'yes-or-no-p 'y-or-n-p)   ; ask for y/n instead of 'yes' or 'no'
(setq ispell-program-name "aspell") ; use aspell for spell checking
(setq sentence-end-double-space nil); double spaces do NOT end a sentence
(setq ring-bell-function 'ignore)   ; no loud noises please

;; Default modes
(global-visual-line-mode 1)         ; let lines wrap
(global-auto-revert-mode 1)         ; update the buffer when a file changes
(delete-selection-mode 1)           ; delete marked region

;; Stop emacs from adding configuration to my setup
(setq custom-file (make-temp-file "emacs-custom"))

;; Setup indentation
(set-default 'indent-tabs-mode nil) ; we don't want to use tabs for indentation
(setq-default tab-width 2)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default py-indent-offset 2)
(setq-default nxml-child-indent 2)
(setq-default c-basic-offset 2)
(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-style-padding 2
              web-mode-script-padding 2)

;; Emacs writes backup files to `filename~` by default. This is messy,
;; so let's tell it to write them to `~/.emacs.d/backup` instead.
;; If you have an accident, check this directory - you might get lucky.
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old
    )

;; If available, use `xdg-open' to open URLs.
(when (is-exec "xdg-open")
  (setq-default
   browse-url-browser-function (quote browse-url-generic)
   browse-url-generic-program "xdg-open"))

;; aspell for spell-checking


(provide 'config)
;;; config.el ends here

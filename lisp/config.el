;;; config.el --- emacs internal configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This sets up sane defaults for our Emacs configuration.
;; 
;;; Code:
;;
(require 'lib)

;; Buffer encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; No startup screen
(setq inhibit-startup-screen t)

;; No startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
(setq initial-buffer-choice nil)

;; No frame title
(setq frame-title-format nil)

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
;; 
;; TODO: Still undecided if I want to use this or not,
;; and what keeps me more in the flow of work.
;; (setq pop-up-windows nil)

;; No empty line indicators
(setq indicate-empty-lines nil)

;; We have enough memory available, let's accumulate
;; before we assimilate
(setq gc-cons-threshold (* 100 1024 1024))

;; Set to 1MB, this should help LSP
(setq read-process-output-max (* 1024 1024))

;; No cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; Moderate font lock
(setq font-lock-maximum-decoration nil)

;; Fill column at 80
(setq fill-column 80)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; Completion style, see
;; gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
(setq completion-styles '(basic substring))

;; Just follow symlinks
(setq vc-follow-symlinks t)

;; Ask for y/n instead of 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; No load noises please
(setq ring-bell-function 'ignore)

;; Enable visual-line-mode. Change auto-fill-mode to AF and swap
;; remove visual-line-mode
(use-package emacs
  :init
  (global-visual-line-mode 1)
  :delight
  (auto-fill-function " AF")
  (visual-line-mode))

;; Update the buffer when a file changes
(global-auto-revert-mode 1)

;; Delete a marked region
(delete-selection-mode 1)

;; Use shift + direction to switch between windows
(windmove-default-keybindings)

;; highlight the current line, turned off for now
(global-hl-line-mode 0)

;; change the default cursor
(setq-default cursor-type 'box)

;; highlight the matching paren
(show-paren-mode 1)

;; This will stop Emacs from changing my config by
;; having it write to a temporary file
(setq custom-file (make-temp-file "emacs-custom"))

;; Setup indentation
(set-default 'indent-tabs-mode nil)
(setq-default tab-width 4)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; Emacs writes backup files to `filename~` by default. This is messy,
;; so let's tell it to write them to `~/.config/emacs/backups` instead.
;; If you have an accident, check this directory - you might get lucky
(setq
  backup-directory-alist
  '(("." . "~/.config/emacs/backups"))
  backup-by-copying t ; Don't delink hardlinks
  version-control t ; Use version numbers on backups
  delete-old-versions t ; Automatically delete excess backups
  kept-new-versions 20 ; how many of the newest versions to keep
  kept-old-versions 5 ; and how many of the old
  )

;; Move auto-saved files to their own directory
(setq auto-save-file-name-transforms
  `((".*" "~/.config/emacs/auto-saves/" t)))

;; If available, use `xdg-open' to open URLs.
(when (executable-find "xdg-open")
  (setq-default
    browse-url-browser-function
    (quote browse-url-generic)
    browse-url-generic-program "xdg-open"))

;; Don't show warnings if we have native compilation enabled
(when
  (and
    (fboundp 'native-comp-available-p)
    (native-comp-available-p)
    (setq native-comp-async-report-warnings-errors nil)))

;; Improved scrolling
(pixel-scroll-precision-mode)

;; Configure shell environment
(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize exec-path-from-shell-copy-env)
  :init
  (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package vterm
  :config
  (defun turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))
  :hook (vterm-mode . turn-off-chrome))

(use-package vterm-toggle
  :commands (vterm-toggle)
  :custom
  (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
  (vterm-toggle-scope 'project)
  :bind (("C-c s" . #'vterm-toggle)))

;; Ability to hide emacs on the mac
(when (pet/is-mac)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "s-h") 'mark-paragraph))

;; Open files from Finder in the same window
(when (pet/is-mac)
  (setq ns-pop-up-frames nil))

(provide 'config)
;;; config.el ends here

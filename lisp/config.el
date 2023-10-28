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

(defun display-startup-echo-area-message ()
  "Replace the original message with a custom one."
  (message "Home is where your REPL is."))

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
(setq initial-buffer-choice nil)

;; Only show filename in the frame title
(setq frame-title-format "%b")

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No empty line indicators
(setq indicate-empty-lines nil)

;; Improve the performance for loading themes
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

;; We have enough memory available, let's accumulate
;; before we assimilate
(setq gc-cons-threshold (* 100 1024 1024))

;; Set to 1MB, this should help LSP
(setq read-process-output-max (* 1024 1024))

;; No cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

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

(use-package dired
  :straight (:type built-in)
  :commands (dired-omit-mode dired-hide-details-mode)
  :hook (dired-mode . (lambda ()
                        (dired-omit-mode 1)
                        (dired-hide-details-mode 1)))
  :custom
  (dired-omit-files "^\.?#\|\.DS_Store")
  (dired-omit-verbose nil))

;; Enable visual-line-mode. Change auto-fill-mode to AF and swap
;; remove visual-line-mode
(use-package emacs
  :straight (:type built-in)
  :init
  (global-visual-line-mode 1)
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  :delight
  (auto-fill-function " AF")
  (visual-line-mode))

;; Update the buffer when a file changes
(global-auto-revert-mode 1)

;; Delete a marked region
(delete-selection-mode 1)

;; Use shift + direction to switch between windows.
(windmove-default-keybindings)

;; Highlight the current line.
(global-hl-line-mode 1)
(hl-line-mode -1)

;; Show the current column
(column-number-mode -1)

;; Show the current line
(line-number-mode -1)

;; Change the default cursor
(setq-default cursor-type 'box)

;; Blink the cursor
(blink-cursor-mode -1)

;; Highlight the matching paren
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
  `(("." . ,(concat user-emacs-directory "backups/")))
  backup-by-copying t ; Don't delink hardlinks
  version-control t ; Use version numbers on backups
  delete-old-versions t ; Automatically delete excess backups
  kept-new-versions 20 ; how many of the newest versions to keep
  kept-old-versions 5 ; and how many of the old
  )

;; Move auto-saved files to their own directory
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-saves/") t)))

;; If available, use `xdg-open' to open URLs.
(when (executable-find "xdg-open")
  (setq-default
    browse-url-browser-function
    (quote browse-url-generic)
    browse-url-generic-program "xdg-open"))

;; When on WSL
(when (pet/is-wsl)
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic))))

;; Don't show warnings if we have native compilation enabled
(when
    (and
     (fboundp 'native-comp-available-p)
     (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil))

;; Improved, buttery smooth scrolling, only available on Emacs 29
(pixel-scroll-precision-mode)

;; Configure shell environment
(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize exec-path-from-shell-copy-env)
  :init
  (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package vterm
  :preface
  (defun enable-boring ()
    "Make the shell as boring as possible"
    (hl-line-mode -1)
    (display-line-numbers-mode -1))
  :custom
  (vterm-shell "fish")
  :hook (vterm-mode . enable-boring))

(use-package goto-addr
  :after vterm
  :straight (:type built-in)
  :bind (:map goto-address-highlight-keymap
              ("C-c C-o" . goto-address-at-point))
  :hook (((magit-process-mode vterm-mode) . goto-address-mode))
  :config
  (progn
    (setq goto-address-mail-face 'link)
    (setq goto-address-mail-mouse-face 'highlight)))

(use-package vterm-toggle
  :commands (vterm-toggle)
  :custom
  (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
  (vterm-toggle-scope 'project)
  :bind (("C-c s" . #'vterm-toggle)))

(use-package restart-emacs
  :commands restart-emacs)

;; Ability to hide emacs on the mac
(when (pet/is-mac)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "s-h") 'mark-paragraph))

;; Open files from Finder in the same window
(when (pet/is-mac)
  (setq ns-pop-up-frames nil))

(provide 'config)
;;; config.el ends here

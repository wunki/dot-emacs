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

;; Show nothing in the title
(setq frame-title-format nil)
(setq ns-use-proxy-icon nil)

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

;; But let's assimilate, once we no longer focus on Emacs
(add-function :after
              after-focus-change-function
              (lambda () (unless (frame-focus-state) (garbage-collect))))

;; Set to 1MB, this should help LSP
(setq read-process-output-max (* 1024 1024))

;; Always load the newer byte code
(setq load-prefer-newer t)

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

;; Don't create lockfiles, the files starting with .#
(setq create-lockfiles nil)

;; Whitespace mode, only enabled manually when needed.
(use-package whitespace
  :elpaca nil
  :commands whitespace-mode
  :config
  ; When we run the `whitespace-cleanup' command
  (setq-default whitespace-action
                '(cleanup auto-cleanup))
  (setq-default whitespace-style
                '(face
                  spaces
                  empty
                  tabs
                  newline
                  trailing
                  space-mark
                  tab-mark
                  newline-mark))

  (setq-default whitespace-display-mappings
                '(
                  ;; space -> · else .
                  (space-mark 32 [183] [46])
                  ;; new line -> ¬ else $
                  (newline-mark ?\n [172 ?\n] [36 ?\n])
                  ;; carriage return (Windows) -> ¶ else #
                  (newline-mark ?\r [182] [35])
                  ;; tabs -> » else >
                  (tab-mark ?\t [187 ?\t] [62 ?\t]))))

(use-feature dired
  :commands (dired-omit-mode dired-hide-details-mode)
  :hook (dired-mode . (lambda ()
                        (dired-omit-mode 1)
                        (dired-hide-details-mode 1)))
  :custom
  (dired-omit-files "^\.?#\|\.DS_Store")
  (dired-omit-verbose nil))

;; Update the buffer when a file changes
(global-auto-revert-mode 1)

;; Delete a marked region
(delete-selection-mode 1)

;; Use shift + direction to switch between windows.
(windmove-default-keybindings)

;; Highlight the current line only for program and text.
;; Previously I had it global, but that was a tad bit too much.
;; (add-hook 'prog-mode-hook #'hl-line-mode)
;; (add-hook 'text-mode-hook #'hl-line-mode)

;; When writing text, use variable pitch.
(add-hook 'text-mode-hook #'variable-pitch-mode)

;; Show the current column in the modeline
(column-number-mode -1)

;; Show the current line in the modeline
(line-number-mode -1)

;; Change the default cursor
(setq-default cursor-type 'box)

;; Blink the cursor
(blink-cursor-mode -1)

;; Highlight the matching paren
(show-paren-mode 1)

;; Setup indentation
(set-default 'indent-tabs-mode nil)
(setq-default tab-width 4)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

(use-package no-littering
  :demand t
  :init
  (require 'no-littering)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  ;; also theme backup files
  (no-littering-theme-backups))

;; Configure backups
(setq backup-by-copying t         ; Don't delink hardlinks
      version-control t           ; Use version numbers on backups
      delete-old-versions t       ; Automatically delete excess backups
      kept-new-versions 20        ; how many of the newest versions to keep
      kept-old-versions 5         ; and how many of the old
 )

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

(use-package helpful
  :config
  (setq helpful-max-buffers 1) ; but actually we want it to reuse buffer
  :bind (("C-h f" . #'helpful-callable)
         ("C-h F" . #'helpful-function) ; exclude macros
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-h x" . #'helpful-command)
         ;; Lookup the current symbol at point. C-c C-d is
         ;; a common keybinding for this in lisp modes.
         ("C-c C-d" . #'helpful-at-point)))

;; Configure shell environment
(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize exec-path-from-shell-copy-env)
  :custom
  (exec-path-from-shell-shell-name "/opt/homebrew/bin/fish")
  :init
  (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package goto-addr
  :elpaca nil
  :bind (:map goto-address-highlight-keymap
              ("C-c C-o" . goto-address-at-point))
  :hook (((magit-process-mode vterm-mode) . goto-address-mode))
  :config
  (progn
    (setq goto-address-mail-face 'link)
    (setq goto-address-mail-mouse-face 'highlight)))

(use-package eat
  :commands eat
  :elpaca (eat
           :type git
           :host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el")))
  :bind (("C-c t" . eat)
         :map project-prefix-map
         ("t" . eat-project))
  :custom
  (eat-shell "/opt/homebrew/bin/fish")
  (eat-kill-buffer-on-exit t))

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

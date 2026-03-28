;;; pet-config.el --- emacs internal configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pet-lib)

;; Buffer encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; No startup noise
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(defun display-startup-echo-area-message ()
  "Replace the original message with a custom one."
  (message "Home is where your REPL is."))

;; Scratch buffer
(setq initial-scratch-message ";;;  -*- lexical-binding: t; -*-\n")
(setq initial-buffer-choice nil)

;; Frame
(setq frame-title-format nil
      ns-use-proxy-icon nil)

;; No dialogs
(setq use-file-dialog nil
      use-dialog-box nil)

;; No empty line indicators
(setq indicate-empty-lines nil)

;; Use ripgrep for xref
(setq xref-search-program 'ripgrep)

;; Clear previous themes before loading new ones
(advice-add 'load-theme :before
            (lambda (&rest _) (mapc #'disable-theme custom-enabled-themes)))

;; Larger read buffer for LSP/subprocess performance
(setq read-process-output-max (* 1024 1024))

;; Always load newer byte code
(setq load-prefer-newer t)

;; No cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; Fill column
(setq fill-column 80)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; Follow symlinks
(setq vc-follow-symlinks t)

;; y/n instead of yes/no
(setq use-short-answers t)

;; No bell
(setq ring-bell-function 'ignore)

;; No lockfiles
(setq create-lockfiles nil)

;; Whitespace mode
(use-feature whitespace
  :commands whitespace-mode
  :config
  (setq-default whitespace-action '(cleanup auto-cleanup))
  (setq-default whitespace-style
                '(face spaces empty tabs newline trailing
                  space-mark tab-mark newline-mark))
  (setq-default whitespace-display-mappings
                '((space-mark 32 [183] [46])
                  (newline-mark ?\n [172 ?\n] [36 ?\n])
                  (newline-mark ?\r [182] [35])
                  (tab-mark ?\t [187 ?\t] [62 ?\t]))))

(use-feature dired
  :commands (dired-omit-mode dired-hide-details-mode)
  :hook (dired-mode . (lambda ()
                        (dired-omit-mode 1)
                        (dired-hide-details-mode 1)))
  :custom
  (dired-omit-files "^\.?#\|\.DS_Store")
  (dired-omit-verbose nil))

;; Auto-revert
(global-auto-revert-mode 1)

;; Delete selection
(delete-selection-mode 1)

;; Shift + direction to switch windows
(windmove-default-keybindings)

;; Cursor
(setq-default cursor-type 'box)
(blink-cursor-mode 1)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Temp buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)
(setq window-min-height 1)

;; No littering
(use-package no-littering
  :demand t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (no-littering-theme-backups))

;; Backups
(setq backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

;; Browser on Linux
(when (executable-find "xdg-open")
  (setq-default browse-url-browser-function 'browse-url-generic
                browse-url-generic-program "xdg-open"))

;; Suppress native-comp warnings
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors nil))

;; Smooth scrolling
(pixel-scroll-precision-mode)

(use-package helpful
  :custom
  (helpful-max-buffers 1)
  :bind (("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)))

;; Shell environment (needed on macOS)
(use-package exec-path-from-shell
  :if (pet/is-mac)
  :init
  (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

;; Project-local toolchains via mise
(use-package mise
  :if (file-executable-p (expand-file-name "~/.local/bin/mise"))
  :hook (after-init . global-mise-mode)
  :init
  ;; GUI Emacs can miss the shims directory even when shells are fine.
  ;; Prepending it here keeps subprocess lookup boring.
  (let* ((shims-dir (expand-file-name "~/.local/share/mise/shims"))
         (path (getenv "PATH"))
         (path-entries (and path (split-string path path-separator t))))
    (when (and (file-directory-p shims-dir)
               (not (member shims-dir path-entries)))
      (setenv "PATH" (concat shims-dir path-separator path))
      (add-to-list 'exec-path shims-dir)))
  :custom
  (mise-executable (expand-file-name "~/.local/bin/mise")))

(use-feature goto-addr
  :bind (:map goto-address-highlight-keymap
              ("C-c C-o" . goto-address-at-point))
  :hook ((magit-process-mode eat-mode) . goto-address-mode)
  :config
  (setq goto-address-mail-face 'link
        goto-address-mail-mouse-face 'highlight))

;; Terminal
(use-package eat
  :after project
  :commands eat
  :bind (("C-c t" . eat)
         :map project-prefix-map
         ("t" . eat-project))
  :custom
  (eat-kill-buffer-on-exit t)
  :config
  (setq explicit-shell-file-name "/opt/homebrew/bin/zsh"))

;; Hide Emacs on Mac
(when (pet/is-mac)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "s-h") 'mark-paragraph))

;; Open files from Finder in same frame
(when (pet/is-mac)
  (setq ns-pop-up-frames nil))

(provide 'pet-config)
;;; pet-config.el ends here

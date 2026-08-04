;;; pet-config.el --- emacs internal configuration -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Core Emacs defaults, file behavior, and process environment setup.
;;
;;; Code:

(require 'autorevert)
(require 'pet-packages)
(require 'xref)

(defvar ns-use-proxy-icon)

;; Startup and display
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

(defun display-startup-echo-area-message ()
  "Replace the original message with a custom one."
  (message "Home is where your REPL is."))

(setq initial-scratch-message ";;;  -*- lexical-binding: t; -*-\n"
      frame-title-format nil
      ns-use-proxy-icon nil
      use-file-dialog nil
      use-dialog-box nil
      cursor-in-non-selected-windows nil
      ring-bell-function #'ignore)

;; Editing and navigation defaults
(setq-default fill-column 80)
(setq xref-search-program 'ripgrep
      confirm-nonexistent-file-or-buffer nil
      vc-follow-symlinks t
      use-short-answers t
      create-lockfiles nil)

;; C-w with no active region kills the previous word instead of erroring.
(setq kill-region-dwim 'emacs-word)

;; After delete-pair, push a mark so C-x C-x reselects what was inside.
(setq delete-pair-push-mark t)

;; Keep C-h l (view-lossage) live-updating.
(setq view-lossage-auto-refresh t)

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
  (dired-omit-verbose nil)
  ;; Hide the absolute directory path in dired-hide-details-mode.
  (dired-hide-details-hide-absolute-location t))

;; Human-readable sizes (KB/MB) in ibuffer.
(use-feature ibuffer
  :custom (ibuffer-human-readable-size t))

;; Auto-revert
(setq auto-revert-verbose nil)
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
(temp-buffer-resize-mode 1)
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

;; Smooth scrolling
(pixel-scroll-precision-mode 1)

(use-package helpful
  :custom
  (helpful-max-buffers 1)
  :bind (("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)))

;; GUI applications and systemd daemons inherit a minimal PATH, so import the
;; login shell environment for those processes.  A standalone terminal Emacs
;; already inherits the shell's PATH and needs no adjustment.
(use-package exec-path-from-shell
  :if (or (daemonp) (display-graphic-p))
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

(provide 'pet-config)
;;; pet-config.el ends here

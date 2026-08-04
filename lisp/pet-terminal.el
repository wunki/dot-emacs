;;; pet-terminal.el --- terminal and SSH client support -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Terminal capabilities shared by local, tmux, and SSH Emacs clients.
;; Client layout persistence belongs in `pet-session'.
;;
;;; Code:

(require 'pet-packages)
(require 'project)

;; Send kills through tmux to its paste buffer and the local clipboard via OSC 52.
(use-feature term/tmux
  :defer t
  :config
  (add-to-list 'xterm-tmux-extra-capabilities 'setSelection))

(use-package eat
  :after project
  :commands eat
  :preface
  (defun pet/eat-display-tweaks ()
    "Make Eat render full-screen terminal interfaces correctly.
Stop recentering, which corrupts in-place redraws such as progress bars
\(see emacs-eat issue #145)."
    (setq-local scroll-conservatively 10000))
  :bind (("C-c t" . eat)
         :map project-prefix-map
         ("t" . eat-project))
  :custom
  (eat-kill-buffer-on-exit t)
  :hook (eat-mode . pet/eat-display-tweaks)
  :config
  ;; Prefer Fish where installed without assuming a Homebrew path.
  (when-let* ((fish (executable-find "fish")))
    (setq explicit-shell-file-name fish)))

;; KKP distinguishes modified keys that terminals otherwise report identically.
(use-package kkp
  :config
  (global-kkp-mode 1))

;; Mouse and tooltip support for terminal frames, including SSH clients.
(use-feature xt-mouse
  :config
  (xterm-mouse-mode 1))

(tty-tip-mode 1)

(provide 'pet-terminal)
;;; pet-terminal.el ends here

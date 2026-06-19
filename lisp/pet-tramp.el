;;; pet-tramp.el --- remote editing over SSH -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Optimised TRAMP setup for editing on a remote machine over SSH.
;;
;; Connection reuse and keepalives live in ~/.ssh/config (ControlMaster auto,
;; ControlPersist, ServerAliveInterval).  The job here is to make TRAMP defer
;; to that and stay out of the way, so eglot, project.el, compile and Magit
;; behave the same on /ssh:host: paths as they do locally.
;;
;;; Code:

(use-feature tramp
  :defer t
  :config
  ;; ssh is quicker to set up than the default scp method and reuses the
  ;; ControlMaster socket from ~/.ssh/config.
  (setq tramp-default-method "ssh")

  ;; ~/.ssh/config already defines ControlMaster/ControlPath/ControlPersist.
  ;; Tell TRAMP not to inject its own competing -o options and to rely on ours.
  (setq tramp-use-ssh-controlmaster-options nil)

  ;; Use the remote login shell's own PATH so eglot and compile find language
  ;; servers and tools in ~/.local/bin, mise shims, etc.  This is the single
  ;; most common reason "eglot can't find the server" over TRAMP.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Cache remote file attributes longer than the 10s default to cut round
  ;; trips during normal editing.  Buffers still revert on save/visit.
  (setq remote-file-name-inhibit-cache 60)

  ;; Skip lock-file churn on the remote (we disable lockfiles globally anyway).
  (setq remote-file-name-inhibit-locks t)

  ;; Quieter logging means less overhead on every remote operation.  Bump back
  ;; to 6+ temporarily when debugging a connection.
  (setq tramp-verbose 1)

  ;; Don't consult auth-source while completing remote host/file names.
  (setq tramp-completion-use-auth-sources nil)

  ;; VC fires several synchronous git calls on every remote file visit (branch,
  ;; working revision, mode-line, plus diff-hl's git diff), each a round-trip.
  ;; Treat remote dirs as non-VC so opens are near-instant.  Magit is unaffected
  ;; (it doesn't go through VC); local files keep their fringe diffs.
  (setq vc-ignore-dir-regexp
        (format "\\(?:%s\\)\\|\\(?:%s\\)"
                vc-ignore-dir-regexp tramp-file-name-regexp)))

;; Only Git is in use here.  Probing the other VC backends on every file visit
;; is pure latency over TRAMP (and wasted work locally).  Magit, Forge and
;; diff-hl all keep working.
(setq vc-handled-backends '(Git))

;; global-auto-revert-mode leaves remote files alone by default
;; (auto-revert-remote-files is nil), so we don't poll the wire on a timer.
;; Left explicit as a reminder not to flip it on without thinking.
(setq auto-revert-remote-files nil)

(provide 'pet-tramp)
;;; pet-tramp.el ends here

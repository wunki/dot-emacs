;;; pet-irc.el --- IRC with ERC -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-feature erc
  :commands erc
  :config
  (setq erc-server "irc.libera.chat"
        erc-nick "wunki"
        erc-user-full-name "Petar Radosevic"
        erc-track-shorten-start 8
        erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs" "#zig"))
        erc-kill-buffer-on-part t
        erc-auto-query 'bury
        erc-quit-reason (lambda (s) (or s "People go but how they left always stays..."))))

(provide 'pet-irc)
;;; pet-irc.el ends here

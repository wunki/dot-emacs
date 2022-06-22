;;; irc.el --- setup differeng languages -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Connect to IRC with ERC.
;;
;;; Code:
;;
(use-package erc
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

(use-package erc-image
  :after (erc emojify)
  :config
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image))

(provide 'irc)
;;; irc.el ends here

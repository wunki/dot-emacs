;;; lucy-theme.el --- A theme for the Lucy langugae -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Petar Radosevic
;;
;; Author: Petar Radosevic <petar@petar.dev>
;; Maintainer: Petar Radosevic <petar@petar>
;; URL: https://github.com/wunki/lucy-theme
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (autothemer "0.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;  A theme for the Lucy programming language.
;;
;;  Heavily inspired by the oh-lucy theme found in VSCode.
;;
;;; Code:
(require 'autothemer)

;;; Code:
(autothemer-deftheme
 lucy
 "A theme for the lucy language"
 ((((class color) (min-colors #xFFFFFF))) ;; GUI/24bit only

  
  (lucy-foreground                     "#d7d7d7")
  (lucy-background                     "#1e1d23")

  (region-bg                      "#292c38")
  (region-fg                      "#615262")

  (cursor-bg                      "#000000")
  (cursor-fg                      "#d7d7d7")
  
  (red                            "#fb7da7")
  (red-alt                        "#D95555")
  (green                          "#76c5a4")
  (green-alt                      "#8CD881")
  (blue                           "#51c7da")
  (black                          "#29292E")
  (black-alt                      "#1A191E")
  (white                          "#DED7D0")
  (white-alt                      "#DED7D0")
  (gray                           "#938884")
  (gray-alt                       "#7F737D")
  (gray-alt-2                     "#413E41")
  (gray-alt-3                     "#322F32")
  (gray-alt-4                     "#686069")
  (orange                         "#fdad5d")
  (orange-alt                     "#E39A65")
  (purple                         "#af98e6")
  (pink                           "#BDA9D4")
  (yellow                         "#e3cf65"))
  

  ;; Customize faces
  (
  (default                                       (:background lucy-background :foreground lucy-foreground))
  (border                                        (:background lucy-background :foreground black))
  (button                                        (:foreground green))
  (child-frame                                   (:background black :foreground black))
  (child-frame-border                            (:background black :foreground black))
  (cursor                                        (:background blue :foreground black :bold t))
  (error                                         (:foreground red))
  (fringe                                        (:foreground gray-alt))
  (glyph-face                                    (:background gray-alt-2))
  (glyphless-char                                (:foreground gray-alt-2))
  (header-line                                   (:background black))
  (highlight                                     (:background gray-alt-3 :foreground purple))
  (hl-line                                       (:background gray))
  (homoglyph                                     (:foreground blue))
  (internal-border                               (:background lucy-background))
  (line-number                                   (:foreground gray-alt-2))
  (line-number-current-line                      (:foreground pink :background gray :bold t))
  (lv-separator                                  (:foreground blue :background gray))
  (match                                         (:background yellow :foreground black))
  (menu                                          (:background black :foreground lucy-foreground))
  (mode-line                                     (:background black))
  (mode-line-inactive                            (:background nil :foreground gray-alt-2 :bold nil))
  (mode-line-active                              (:background black :foreground white-alt :bold nil))
  (mode-line-highlight                           (:foreground yellow))
  (mode-line-buffer-id                           (:foreground green :bold t))
  (numbers                                       (:background pink))
  (region                                        (:background blue))
  (separator-line                                (:background black))
  (shadow                                        (:background black))
  (success                                       (:foreground green))
  (vertical-border                               (:foreground gray-alt-2))
  (warning                                       (:foreground yellow))
  (window-border                                 (:background lucy-background))
  (window-divider                                (:foreground gray))
  (hi-yellow                                     (:background yellow :foreground lucy-background))

  (font-lock-comment-face                        (:foreground gray-alt-4 :italic t))
  (font-lock-string-face                         (:foreground yellow))
  (font-lock-keyword-face                        (:foreground red))
  (font-lock-function-name-face                  (:foreground green))

  
  ))

(provide-theme 'lucy)
;;; lucy-theme.el ends here

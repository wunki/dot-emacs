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

  
  (foreground                     "#d7d7d7")
  (background                     "#1a1d27")

  (region-bg                      "#817081")
  (region-fg                      "#615262")

  (cursor-bg                      "#000000")
  (cursor-fg                      "#d7d7d7")
  
  (red                            "#FF7DA3")
  (red-alt                        "#D95555")
  (green                          "#7EC49D")
  (green-alt                      "#8CD881")
  (blue                           "#8BB8D0")
  (black                          "#29292E")
  (black-alt                      "#1A191E")
  (white                          "#DED7D0")
  (white-alt                      "#DED7D0")
  (gray                           "#938884")
  (gray-alt                       "#7F737D")
  (gray-alt-2                     "#413E41")
  (gray-alt-3                     "#322F32")
  (orange                         "#E0828D")
  (orange-alt                     "#E39A65")
  (purple                         "#B898DD")
  (pink                           "#BDA9D4")
  (yellow                         "#EFD472"))
  

  ;; Customize faces
  ((default                       (:foreground foreground :background background))))

(provide-theme 'lucy)
;;; lucy-theme.el ends here

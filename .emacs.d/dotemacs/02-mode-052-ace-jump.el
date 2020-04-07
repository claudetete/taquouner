;;; 02-mode-052-ace-jump.el --- configuration of ace jump mode -*- lexical-binding: t -*-

;; Copyright (c) 2017-2020 Claude Tete
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;

;; Author: Claude Tete <claude.tete@gmail.com>
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [[SUBHEADER.move quickly and easily with ace jump
;; see http://dl.dropbox.com/u/3254819/AceJumpModeDemo/AceJumpDemo.htm
;; ]]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package ace-jump-mode
  :bind
  ("<f12>"   . ace-jump-mode)
  ;; can also use <C-u f9>
  ("<M-f12>" . ace-jump-char-mode)
  ;; can also use <C-u C-u f9>
  ("<C-f12>" . ace-jump-line-mode)
  ("<S-f12>" . ace-jump-mode-pop-mark)

  :init
  ;; to enable jump back
  (autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)

  :config
  (ace-jump-mode-enable-mark-sync)
  ;; to enable only in the current window
  (setq ace-jump-mode-scope 'window))


(provide '02-mode-052-ace-jump)

;;; 02-mode-052-ace-jump.el ends here

;;; shortcut-global.el --- a config file for global Emacs shortcut

;; Copyright (c) 2006, 2007, 2008, 2009, 2010, 2011, 2012 Claude Tete
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;; Keywords: config, shorcut, emacs
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.5
;; Created: October 2006
;; Last-Updated: April 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-shortcut-global'
;;              var     `section-shortcut'

;;; Change Log:
;; 2012-04-20 (1.5)
;;    add CUA mode
;; 2012-04-03 (1.4)
;;    add shortcut for fill-paragraph
;; 2012-03-30 (1.3)
;;    translate comments in english
;; 2011-08-03 (1.2)
;;    add shortcut for pair of parentheses
;; 2011-07-27 (1.1)
;;    change bind of C-cpp to align
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; home and end keys
(global-set-key         "\e[7~"                 'beginning-of-line)
(global-set-key         "\e[8~"                 'end-of-line)

;;;; if C-h do not do backspaces
;;(normal-erase-is-backspace-mode)

;; close current buffer and its window
(global-set-key         [f4]                    'kill-buffer-and-window)

;;;; run ispell (dictionnary) (set language in `section-misc')
;;;; never used
;;(global-set-key         [f7]                    'ispell-buffer)
;;(global-set-key         [S-f7]                  'ispell-word)
;;(global-set-key         [\M-f7]                 'ispell-region)

;;;; insert printf or ifdef for debug (used in epita kaneton project)
;;(global-set-key         [f7]                    'printf-debug-shortcut)
;;(global-set-key         [f8]                    'ifdef-debug-shortcut)

;;;; run apropos for the word at point
;;(global-set-key [f1]                            'vectra-apropos-on-word)

;; execute the most recent keyboard macro
(global-set-key         [f8]                    'call-last-kbd-macro)

;; search a file in a directory (recursively) to open it
(global-set-key         "\M-f"                  'find-name-dired)

;;;; search a file in the current folder (recursively)
;;(global-set-key         "\M-g"                  'find-grep-dired)

;; go to line #
(global-set-key         "\M-l"                  'goto-line)

;; replace string
(global-set-key         "\M-r"                  'replace-string)

;; save current buffer as a bookmark
(global-set-key         "\C-cv"                 'bookmark-set)

;;;; ??
;;(global-set-key         "\C-up"                 'backward-sentence)

;; delete backear a word
(global-set-key         "\C-z"                  'backward-kill-word)

;; open calendar with `C-c c'
(global-set-key         "\C-cc"                 'calendar)

;; copie la ligne dans la kill-ring `C-j' (ne plus faire C-k C-y)
(global-set-key         "\C-j"                  'push-line)

;; aligne la region suivant une regexp (avec des espaces)
(global-set-key         "\C-cpp"                'align)
(global-set-key         "\C-cpl"                'align-regexp)

;; supprime une paire de parentheses/crochets/accolades ensembles
;; voir "\M-(" pour en creer deux nouvelles entourant la region
(global-set-key         (kbd "C-(")             'delete-pair)

;; rafraichis le buffer courant
(global-set-key         (kbd "M-p")             'revert-buffer)

;; switch between header/source file
(global-set-key         (kbd "C-`")             'ff-find-related-file)

;; switch between others header/source files
(global-set-key         [\C-f4]                 'ff-find-other-file)

;; incremental search at point
(global-set-key         (kbd "C-M-x")           'isearch-forward-at-point)

;; fill paragraph at point
(global-set-key         (kbd "C-c ]")           'fill-paragraph)

;;
;;; HOME/END
;; REQUIREMENT: var     `section-external-home-end'
(when section-external-home-end
  ;; bind home avec les nouvelles fonctionnalites
  (global-set-key       (kbd "<home>")          'pc-keys-home)
  ;;
  ;; bind home avec les nouvelles fonctionnalites
  (global-set-key       (kbd "<end>")           'pc-keys-end)
  )

;;
;;; OUTLINE
;; REQUIREMENT: var     `section-mode-outline'
(when section-mode-outline
  ;; bind toogle hide/show block
  (global-set-key       (kbd "C-c h")           'outline-toggle-children)
  )

;;
;;; CUA
;; not used
(when section-shortcut-global-cua (message "    8.1.1 CUA Shortcut...")
  (cua-mode t)
  (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
  (transient-mark-mode 1) ; No region when it is not highlighted
  (setq cua-keep-region-after-copy t) ; Standard Windows behaviour
  (message "    8.1.1 CUA Shortcut... Done"))

;;; shortcut-global.el ends here

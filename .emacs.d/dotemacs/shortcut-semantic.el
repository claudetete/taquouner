;;; shortcut-semantic.el --- a config file for semantic mode shortcut

;; Copyright (c) 2010, 2011, 2012 Claude Tete
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

;; Keywords: config, semantic, bovinate, cedet, shortcut
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.3
;; Created: October 2010
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-shortcut-semantic'
;;              var     `section-shortcut'
;;              var     `section-mode-cedet-semantic'

;;; Change Log:
;; 2012-05-28 (1.3)
;;    add new  shortcut to 'grep' in project all occurences
;; 2012-03-28 (1.2)
;;    change shortcut and add go/back to the tag
;; 2012-03-20 (1.1)
;;    add shortcut for tags
;; 2011-08-10 (1.0)
;;    update shortcut to use the colon
;; 2011-07-25 (0.1)
;;    creation from scratch (no history since)


;;; Code:
(when section-mode-cedet-semantic
  ;; go to the tag with shift + left click
  (global-set-key       (kbd "<S-down-mouse-1>")        'ignore)
  (global-set-key       (kbd "<S-mouse-1>")             'semantic-goto-definition)

  ;; go to the tag
  (global-set-key       (kbd "M-.")                     'semantic-goto-definition)
  (global-set-key       [(control  >)]                  'semantic-goto-definition)

  ;; find all references of a symbol by regexp
  (global-set-key       (kbd "C-M-.")                   'semantic-symref-regexp)

  ;; return back after "go to the tag"
  ;; you can use the default shortcut: M-xB, which ask where you want go back
  (global-set-key         [(control  <)]                  'semantic-pop-tag-mark)
)

;;
;;; GNU GLOBAL
;; REQUIREMENT: var     `section-shortcut-tags-gnu-global'
;;              var     `section-mode-gnu-global'
(when section-mode-gnu-global
  (when section-shortcut-tags-gnu-global
    ;; ignore mouse button 'next' and put go back to button 'previous'
    (global-set-key (kbd "<mouse-3>") 'ignore)
    ;;(global-set-key (kbd "<mouse-4>") 'gtags-pop-stack)
    )
  )

;;; shortcut-semantic.el ends here

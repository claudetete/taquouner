;;; shortcut-semantic.el --- a config file for semantic mode shortcut

;; Copyright (c) 2010-2013 Claude Tete
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
;; Version: 1.6
;; Created: October 2010
;; Last-Updated: May 2013

;;; Commentary:
;;
;; load by `dotemacs/shortcut.el'
;; REQUIREMENT: var     `section-shortcut-semantic'
;;              var     `section-shortcut'
;;              var     `section-mode-cedet-semantic'

;;; Change Log:
;; 2013-05-07 (1.6)
;;    add shortcut for smart-forward mode
;; 2013-04-10 (1.5)
;;    add helm imenu shortcut
;; 2012-08-01 (1.4)
;;    fix bug with Shift+Click, Back on mouse work with semantic, right click
;;    display a menu like ecb method
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
  (global-set-key       (kbd "<S-mouse-1>")             '(lambda (event)
                                                           (interactive "e") ; the function get a event parameter (from mouse click)
                                                           (mouse-set-point event) ; set the point at the mouse position
                                                           (semantic-goto-definition (point))))
  ;; go to the tag
  (global-set-key       (kbd "M-.")                     'semantic-goto-definition)
  (global-set-key       [(control  >)]                  'semantic-goto-definition)

  ;; find all references of a symbol by regexp
  (global-set-key       (kbd "C-M-.")                   'semantic-symref-regexp)

  ;; return back after "go to the tag"
  ;; you can use the default shortcut: M-xB, which ask where you want go back
  (global-set-key       [(control  <)]                  'semantic-pop-tag-mark)
  (global-set-key       (kbd "<mouse-4>")               'semantic-pop-tag-mark)

  ;; right click will open menu with list of variable/function/include
  (global-set-key       (kbd "<mouse-3>")               'imenu)
  ;; add helm menu shortcut
  (when section-mode-helm-imenu
    (global-set-key     (kbd "C-c C-f")                 'helm-imenu))

)

;;
;;; GNU GLOBAL
;; REQUIREMENT: var     `section-shortcut-tags-gnu-global'
;;              var     `section-mode-gnu-global'
(when section-mode-gnu-global
  (when section-shortcut-tags-gnu-global
    ;; ignore mouse button 'next' and put go back to button 'previous'
    ;;(global-set-key (kbd "<mouse-5>") 'ignore)
    ;;(global-set-key (kbd "<mouse-4>") 'gtags-pop-stack)
    )
  )

(when section-mode-smart-forward
  ;; c mode
  (add-hook 'c-mode-common-hook
    '(lambda ()
       ;; set smart-forward mode shortcut
       (local-set-key   (kbd "<M-up>")                  'smart-up)
       (local-set-key   (kbd "<M-down>")                'smart-down)
       (local-set-key   (kbd "<M-left>")                'smart-backward)
       (local-set-key   (kbd "<M-right>>")              'smart-forward)))
  ;; perl mode
  (add-hook 'cperl-mode-hook
    '(lambda ()
       ;; set smart-forward mode shortcut
       (local-set-key   (kbd "<M-up>")                  'smart-up)
       (local-set-key   (kbd "<M-down>")                'smart-down)
       (local-set-key   (kbd "<M-left>")                'smart-backward)
       (local-set-key   (kbd "<M-right>>")              'smart-forward)))
  ;; python mode
  (add-hook 'python-mode-hook
    '(lambda ()
       ;; set smart-forward mode shortcut
       (local-set-key   (kbd "<M-up>")                  'smart-up)
       (local-set-key   (kbd "<M-down>")                'smart-down)
       (local-set-key   (kbd "<M-left>")                'smart-backward)
       (local-set-key   (kbd "<M-right>>")              'smart-forward)))
  ) ; (when section-mode-smart-forward


(provide 'shortcut-semantic)

;;; shortcut-semantic.el ends here

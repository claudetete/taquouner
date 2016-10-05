;;; shortcut-tags.el --- a config file for tags shortcut

;; Copyright (c) 2006-2016 Claude Tete
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

;; Keywords: config, shortcut, tags
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 2.2
;; Created: September 2010
;; Last-Updated: September 2016

;;; Commentary:
;;
;; load by `dotemacs/shortcut.el'
;; REQUIREMENT: var     `section-shortcut-tags'

;;; Change Log:
;; 2016-09-28 (2.2)
;;    add helm/gtags and helm/projectile shortcuts
;; 2014-03-26 (2.1)
;;    modify gtags find file function shortcut
;; 2013-04-10 (2.0)
;;    change find file shortcut + clean up
;; 2012-08-01 (1.9)
;;    move some shortcuts
;; 2012-07-19 (1.8)
;;    hide ecb compile window after select a tag + condition when ecb is enable
;; 2012-06-12 (1.7)
;;    remove old way to bind keys
;; 2012-05-28 (1.6)
;;    do not use gtags find with grep with cedet (replaced see
;;    shortcut-semantic.el)
;; 2012-05-04 (1.5)
;;    add shortcuts to search symbol assignation
;; 2012-03-30 (1.4)
;;    translate comments in English
;; 2012-03-02 (1.3)
;;    add condition about semantic
;; 2011-08-10 (1.2)
;;    update shortcut to use the colon
;; 2011-07-08 (1.1)
;;    add shortcuts for gtags
;; 2010-10-13 (1.0)
;;    split .emacs file
;; 2010-09-11 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;;
;;; ETAGS
;; REQUIREMENT: var     `section-shortcut-tags-exuberant-ctags'
(when section-shortcut-tags-exuberant-ctags (message "    8.7.1 Etags Shortcuts...")
  ;; completion with tag file (show a list)
  (global-set-key       (kbd "C-/")             'complete-tag)
  ;; search in tag file
  (global-set-key       "\C-c\,"                'tags-search)
  ;; next result for tag search
  (global-set-key       (kbd "C-,")             'tags-loop-continue)

  ;; previous result for tag search
  (global-set-key       (kbd "C->")             'pop-tag-mark)
  (message "    8.7.1 Etags Shortcuts... Done"))

;;
;;; GTAGS
;; REQUIREMENT: var     `section-shortcut-tags-gnu-global'
;;              var     `section-mode-gnu-global'
;;              var     `section-mode-cedet-semantic'
(when section-mode-gnu-global
  (when section-shortcut-tags-gnu-global (message "    8.7.2 Gtags Shortcuts...")
    (if section-mode-cedet-ecb
      (progn
        (define-key gtags-select-mode-map (kbd "<return>") '(lambda ()
                                                              (interactive)
                                                              (gtags-select-tag)
                                                              (ecb-toggle-compile)
                                                              ))
        (define-key gtags-select-mode-map (kbd "<kp-enter>") '(lambda ()
                                                                (interactive)
                                                                (gtags-select-tag)
                                                                (ecb-toggle-compile)
                                                                ))
        )
      (if section-mode-helm
        (progn
          (eval-after-load "helm-gtags"
            '(progn
               (define-key helm-gtags-mode-map  (kbd "C-M-.")   'helm-gtags-find-pattern)
               (define-key helm-gtags-mode-map  (kbd "M-.")     'helm-gtags-dwim)
               (define-key helm-gtags-mode-map  (kbd "C-<")     'helm-gtags-pop-stack)
               (define-key helm-gtags-mode-map  (kbd "C-.")     'helm-gtags-find-rtag)
               (define-key helm-gtags-mode-map  (kbd "C-M-,")   'helm-gtags-update-tags)
               (define-key helm-gtags-mode-map  (kbd "<f3>")    'helm-gtags-next-history)
               ))
          ) ; (progn
        (progn
          ;; find tag
          (global-set-key         (kbd "M-.")            'ggtags-find-tag-dwim)
          (global-set-key         [(control >)]          'ggtags-find-tag-dwim)
          (global-set-key         (kbd "M->")            'semantic-goto-definition)

          ;; go back after find tag
          (global-set-key         (kbd "M-*")             'ggtags-navigation-mode-abort)
          (global-set-key         (kbd "M-<kp-multiply>") 'ggtags-navigation-mode-abort)
          (global-set-key         [(control <)]           'ggtags-navigation-mode-abort)
          (global-set-key         (kbd "M-<")             'semantic-pop-tag-mark)

          ;; find all references (regexp)
          (global-set-key         (kbd "C-M-.")           'ggtags-find-reference)
          ) ; (progn
        ) ; (progn
      ) ; (if section-mode-cedet-ecb

    (when (and section-mode-helm section-mode-projectile)
      ;; find file in the gnu global project (regexp) (need new function of gtags see function.el)
      (global-set-key           (kbd "C-c C-f")         'helm-projectile-find-file)
      ) ; (when (and section-mode-helm section-mode-projectile)

    ;; find all references (regexp)
    (global-set-key     (kbd "C-M-=")           'gtags-find-with-grep-symbol-assigned)
    (message "    8.7.2 Gtags Shortcuts... Done")
    ) ; when section-shortcut-tags-gnu-global
  ) ; when section-mode-gnu-global

;;
;;; DATA DEBUG
;; REQUIREMENT: var     `section-mode-c-data-debug'
;; ??
(when section-mode-c-data-debug
  (global-set-key       "\M-:"                  'data-debug-eval-expression))


(provide 'shortcut-tags)

;;; shortcut-tags.el ends here

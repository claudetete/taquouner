;;; shortcut-tags.el --- a config file for tags shortcut

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

;; Keywords: config, shortcut, tags
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.3
;; Created: September 2010
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-shortcut-tags'

;;; Change Log:
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
;; REQUIREMENT: var     `section-shortcut-tags-exhuberant-ctags'
(when section-shortcut-tags-exhuberant-ctags (message "    8.7.1 Etags Shortcuts...")
  ;; completion avec le fichier de tags (affiche une liste)
  (global-set-key       [?\C-/]                 'complete-tag)
  ;; recherche dans le fichier de tags
  (global-set-key       "\C-c\,"                'tags-search)
  ;; resultat suivant dans la recherche dans le fichier de tags
  (global-set-key       [\C-,]                  'tags-loop-continue)

  ;; resultat precedent dans la recherche dans le fichier de tags
  (global-set-key       [\C->]                  'pop-tag-mark)
  (message "    8.7.1 Etags Shortcuts... Done"))

;;
;;; GTAGS
;; REQUIREMENT: var     `section-shortcut-tags-gnu-global'
;;              var     `section-mode-gnu-global'
;;              var     `section-mode-cedet-semantic'
(when section-mode-gnu-global
  (when section-shortcut-tags-gnu-global (message "    8.7.2 Gtags Shortcuts...")
    (if section-mode-cedet-semantic nil
      ;; cycles to next result
      ;; After doing gtags-find-(tag|rtag|symbol|with-grep)
      (global-set-key   (kbd "M-,")             'ww-next-gtag)

      ;; find tag
      (global-set-key   "\M-."                  'gtags-find-tag)

      ;; resultat precedent dans la recherche dans le fichier de tags
      (global-set-key   "\M-*"                  'gtags-pop-stack)
      (global-set-key   (kbd "M-<kp-multiply>") 'gtags-pop-stack)
      ) ; if section-mode-cedet-semantic nil

    ;; recherche le fichier (pour ...-file-custom voir gtags modifie)
    (global-set-key     "\C-cf"                 'gtags-find-file)
    (global-set-key     "\C-cf"                 'gtags-find-file-custom)

    ;; find all references of regex
    (global-set-key       (kbd "C-M-.")         'gtags-find-with-grep)
    (message "    8.7.2 Gtags Shortcuts... Done")
    ) ; when section-shortcut-tags-gnu-global
  ) ; when section-mode-gnu-global

;; surligner toute les occurences 'regex'
(global-set-key         "\C-cx"                 'highlight-regexp)

;; rechercher toutes les occurences
;; (mode de recherche le plus proche des editeurs classiques)
(global-set-key         "\C-ce"                 'occur)

;;
;;; DATA DEBUG
;; REQUIREMENT: var     `section-mode-c-data-debug'
;; evaluation d'une variable
(when section-mode-c-data-debug
  (global-set-key       "\M-:"                  'data-debug-eval-expression))

;;; shortcut-tags.el ends here

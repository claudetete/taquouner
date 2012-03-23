;;; shortcut-ecb.el --- a config file for shortcut of ecb mode

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

;; Keywords: config, ecb, mode, shortcut
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.1
;; Created: August 2010
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-shortcut-ecb'
;;              var     `section-shortcut'
;;              var     `section-mode-cedet-ecb'

;;; Change Log:
;; 2012-03-23 (1.2)
;;    add shortcut for methods & source
;; 2011-07-21 (1.1)
;;    add shortcut for methods & source
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2010-08-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;;;; cacher/afficher la fenetre de ecb /* ne veut pas fonctionner */
;;(global-set-key         [f1]                    'ecb-toggle-ecb-windows)

;;
;; lance la fenetre de compile pour grep
(global-set-key         [f2]                    'ecb-toggle-compile-window)
(global-set-key         (kbd "<mouse-5>")       'ecb-toggle-compile-window)

;;
;; agrandir ecb
(global-set-key         "\C-cw"                 'ecb-toggle-width)

;;
;; ouvrir la fenetre d'arborescence
(global-set-key         "\C-cq"                 'ecb-myopen-directories)
(global-set-key         "\M-q"                  'ecb-myopen-directories)
;;
;; ouvrir la fenetre des fichiers sources
(global-set-key         "\C-ca"                 'ecb-myopen-sources)
(global-set-key         "\M-a"                  'ecb-myopen-sources)
;;
;; ouvrir la fenetre des fonctions
(global-set-key         "\C-cz"                 'ecb-myopen-methods)
(global-set-key         "\M-\\"                 'ecb-myopen-methods)
;;
;; Aller a la fenetre "History" de ECB (liste des fichiers ouverts)
(global-set-key         "\M-z"                  'ecb-goto-window-history)

;; cacher/afficher la fenetre de ecb
(global-set-key         "\C-c\\"                'ecb-toggle-ecb-windows)

;; ouvrir la fenetre des fonctions

;; ouvrir la fenetre des sources

;;; shortcut-ecb.el ends here

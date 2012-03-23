;;; display-ecb.el --- a config file for ecb display setting

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

;; Keywords: config, display, ecb
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.3
;; Created: October 2010
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-display-ecb'
;;              var     `section-mode-cedet-ecb'

;;; Change Log:
;; 2012-03-12 (1.3)
;;    add condition about ecb active or not
;; 2012-07-09 (1.1)
;;    add max size of compil window
;; 2011-04-21 (1.1)
;;    add delay before refresh
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2010-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
(when section-mode-cedet-ecb
  (custom-set-variables
    ;; activer fichier en lecture seule dans "ecb sources" (fichiers)
    '(ecb-sources-perform-read-only-check t)

    ;; l'arborescence en ascii dans "ecb directories" (repertoires)
    '(ecb-tree-buffer-style (quote ascii-guides))
    ;;
    ;; le symbole/image des dossiers developpe ou non (avec '+' ou '-')
    ;; est apres le nom
    '(ecb-tree-expand-symbol-before nil)
    ;;
    ;; indentation de 2 escpaces de l'arborescence dans "ecb directories"
    '(ecb-tree-indent 2)
    ;;
    ;; la premiere ligne affichee dans "ecb directories" (repertoires) est
    ;; le dossier parent du fichier/repertoires si cache
    '(ecb-tree-make-parent-node-sticky t)
    ;;
    ;; l'arborescence en ascii dans "ecb methods" (fonctions)
    ;;'(ecb-change-layout leftright-analyse)
    '(ecb-display-image-icons-for-semantic-tags nil)

    ;; desactive le pop-up de l'astuce du jour a chaque demarrage
    '(ecb-tip-of-the-day nil)

    ;;;; largeur de la fenetre de ecb (ici 10% de la largeur total d'emacs)
    ;;'(ecb-windows-width 0.1) ;see mystart-up in functions.el
    ;;
    ;;;; hauteur de la fenetre de compilation/grep/... geree par ecb
    ;;'(ecb-compile-window-height 25) ;see mystart-up in functions.el
    ;;
    ;; hauteur max de la fenetre de compilation/grep/... geree par ecb
    ;;  - value > 1 -> size in character
    ;;  - 0.0 < value < 1.0 -> size in percent of height
    '(ecb-enlarged-compilation-window-max-height 0.25)

    ;; affiche le plus de donnees possible dans tags-apropos
    '(tags-apropos-verbose t)

    ;; defini le nom du buffer de l'analyse
    '(ecb-analyse-buffer-name "*ECB Analyse*")

    ;; surligne dans la window methods
    '(ecb-highlight-tag-with-point (quote highlight-scroll))

    ;; augmente le delai avant le rafraichissement du surlignage
    ;; (pour eviter de ralentir la navigation)
    '(ecb-highlight-tag-with-point-delay 0.5)
    )
  ) ; when section-mode-cedet-ecb

;;; display-ecb.el ends here

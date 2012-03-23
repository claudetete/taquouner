;;; display-color.el --- a config file for color displayed setting

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

;; Keywords: config, display, color, mode, ecb, grep
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.3
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-display-color'
;;              mode    `color-theme'
;;              var     `section-environment-terminal-vs-graphics'
;;              var     `section-display-color-theme'

;;; Change Log:
;; 2012-03-20 (1.3)
;;    modify color for grep/occur matches
;; 2011-12-02 (1.2)
;;    add condition about theme to not use them
;; 2011-07-09 (1.1)
;;    add color-theme
;; 2011-04-21 (1.1)
;;    add test about running in graphics for background color
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
(if section-display-color-theme
;;
;;; COLOR-THEME
  (progn
    ;; Choose what theme you want
    (when (try-require 'color-theme)
      ;; DARK
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-clt-mm.el"))
      (color-theme-clt)
      ;;;; blanc sur noir, clair, string en orange, select en bleu clair
      ;;(color-theme-clarity)
      ;;;; fond turquoise sombre
      ;;(color-theme-classic)
      ;;;; ok sombre
      ;;(color-theme-dark-laptop)
      ;;;; psycheldelique
      ;;(color-theme-euphoria)
      ;;;; gnome color (fond turqoise sombtre)
      ;;(color-theme-gnome2)
      ;;;; tres tres sombre
      ;;(color-theme-late-night)
      ;;;; matrix
      ;;(color-theme-lawrence)
      ;;(color-theme-matrix)
      ;;;; retro
      ;;(color-theme-retro-green)
      ;;(color-theme-retro-orange)
      ;;;;
      ;;(color-theme-subtle-hacker)

      ;; LIGHT
      ;; default
      ;;(color-theme-emacs-21)
      ;;(color-theme-high-contrast)
      ;;(color-theme-rotor)
      ;;(color-theme-scintilla)
      ;;(color-theme-sitaramv-nt)
      ;;(color-theme-snapshot)
      ;;(color-theme-vim-colors)
      ;;(color-theme-xp)
      )
    )

  (progn
;;
;;; MISC
    ;; REQUIREMENT:     var     `section-display-color-misc'
    (when section-display-color-misc (message "    6.5.1 Color Misc...")
      ;; Ligne courante en surbrillance
      (global-hl-line-mode t)

      ;; coloration syntaxique
      (global-font-lock-mode t)
      (setq font-lock-maximum-decoration t)

      ;; Toutes les couleurs seulement en mode graphique
      (when running-in-graphical
        (set-face-background 'default "black")
        (set-face-foreground 'default "white"))

      (custom-set-faces
        ;; coloration en gris au lieu de souligner
        '(underline ((((supports :underline t)) (:background "grey20"))))

        ;; pas de style 3d dans la barre d'etat (mode-line)
        '(mode-line ((((class color) (min-colors 88)) (:background "grey75" :foreground "black"))))
        '(mode-line-highlight ((((class color) (min-colors 88)) nil))))
      (message "    6.5.1 Color Misc... Done"))

;;
;;; PARENTHESES MODE
    ;; REQUIREMENT:     var     `section-display-color-parentheses-mode'
    ;;                  mode    `paren'
    (when section-display-color-parentheses-mode (message "    6.5.2 Parentheses Mode...")
      ;; highlight parentheses at point
      (when (try-require 'paren)
        (show-paren-mode t)
        (setq show-paren-ring-bell-on-mismatch t))
      (message "    6.5.2 Parentheses Mode... Done"))

;;
;;; PARENTHESES MINIBUFFER
    ;; REQUIREMENT:     var     `section-display-color-parentheses-visible'
    ;;                  mode    `mic-paren'
    (when section-display-color-parentheses-visible (message "    6.5.3 Matched Parentheses display in Minibuffer ...")
      ;; affiche dans la barre d'etat la parenthese compl non visible
      (when (try-require 'mic-paren)
        (paren-activate))
      (message "    6.5.3 Matched Parentheses display in Minibuffer... Done"))

;;
;;; PARENTHESES HIGHLIGHT
    ;; REQUIREMENT:     var     `section-display-color-parentheses-highlight'
    ;;                  mode    `highlight-parentheses'
    (when section-display-color-parentheses-highlight (message "    6.5.4 Parentheses Highlight Mode ...")
      ;; display parentheses in same color when they match else in and other
      ;; color
      (try-require 'highlight-parentheses)
      (message "    6.5.4 Parentheses Highlight Mode... Done"))

;;
;;; MODE
    ;; REQUIREMENT:     var     `section-display-color-mode'
    (when section-display-color-mode (message "    6.5.5 Color Mode...")
      ;; style et couleur des strings (gras et vert)
      ;;(copy-face 'bold 'font-lock-string-face)
      (set-face-foreground 'font-lock-string-face "chartreuse3")
      ;;
      ;; style et couleur des types (gras et violet clair)
      ;;(copy-face 'bold 'font-lock-type-face)
      (set-face-foreground 'font-lock-type-face "orchid2")
      ;;
      ;; style et couleur des keyword (gras et cyan)
      (copy-face 'bold 'font-lock-keyword-face)
      (set-face-foreground 'font-lock-keyword-face "cyan1")
      ;;
      ;;        style et couleur des preprocessor (gris)
      ;;(copy-face 'bold 'font-lock-preprocessor-face)
      (set-face-foreground 'font-lock-preprocessor-face "ivory4")
      ;;
      ;; style et couleur des function (vert pale)
      ;;(copy-face 'bold 'font-lock-function-name-face)
      (set-face-foreground 'font-lock-function-name-face "PaleGreen1")
      ;;
      ;; style et couleur des function (jaune clair)
      ;;(copy-face 'bold 'font-lock-variable-name-face)
      (set-face-foreground 'font-lock-variable-name-face "LightGoldenrod1")
      ;;
      ;; style et couleur des commentaires (orange sanguine)
      ;;(copy-face 'bold 'font-lock-comment-face)
      (set-face-foreground 'font-lock-comment-face "OrangeRed2")
      ;;
      ;; style et couleur des de la ligne courante (gris fonce)
      (set-face-background 'hl-line "gray20")
      ;;
      ;; style et couleur du curseur (rose)
      (set-face-background 'cursor "HotPink")

      (custom-set-faces
        '(hide-ifdef-shadow
           ((t (:inherit mode-line-inactive
                 :background "#101E1E"
                 :foreground "grey40"
                 :box nil)))))
      (message "    6.5.5 Color Mode... Done"))

;;
;;; GREP
    ;; REQUIREMENT:     var     `section-display-color-grep'
    (when section-display-color-grep (message "    6.5.6 Color Grep...")
      ;;;; style et couleur pour 'grep hits'
      ;;(set-face-foreground 'compilation-info "magenta")
      ;;(set-face-underline  'compilation-info nil)
      ;;
      ;; style et couleur pour 'grep matches'
      (set-face-background 'match "red3")
      (set-face-foreground 'match "white")
      ;;
      ;;;; style et couleur pour 'grep error messages'
      ;;;;(set-face-foreground 'compilation-error "yellow")
      ;;
      ;; style et couleur pour 'grep context lines'
      (set-face-foreground 'shadow "yellow")
      ;;
      ;;;; style et couleur pour 'grep numero de ligne'
      ;;(set-face-foreground 'compilation-line-number "chartreuse3")
      ;;(set-face-underline  'compilation-line-number nil)
      ;;
      ;;;; style des liens
      ;;(set-face-foreground 'link "chartreuse3")
      ;;
      (custom-set-faces
      ;; couleur des liens 'grep nom et chemin de fichiers'
        '(link
           ((((class color)
               (min-colors 88)
               (background dark)) (:foreground "chartreuse3"))))
        ;; style et couleur pour 'grep hits'
        '(compilation-info
           ((((class color)
               (background dark))
              (:background "black"
                :foreground "magenta"))))
        ;; style et couleur pour 'grep error messages'
        '(compilation-error
           ((((class color)
               (background dark))
              (:background "black"
                :foreground "yellow"))))
        ;; style et couleur pour 'grep numero de ligne'
        '(compilation-line-number
           ((((class color)
               (background dark))
              (:background "black"
                :foreground "chartreuse3"))))
        )
      (message "    6.5.6 Color Grep... Done"))

;;
;;; ECB
    ;; REQUIREMENT:     var     `section-display-color-ecb'
    (when section-mode-cedet-ecb
      (when section-display-color-ecb (message "    6.5.7 Color Ecb...")
        (custom-set-variables
          ;; style utilise pour la surbrillance de la fonction dans le buffer
          '(ecb-method-face (quote ecb-method-face))
          ;; style du fichier en lecture seule dans "ecb sources" (fichiers)
          '(ecb-source-read-only-face (quote ecb-source-read-only-face))
          ;;
          ;; style de l'analyse
          '(ecb-analyse-face (quote ecb-analyse-face))
          '(ecb-analyse-general-face (quote ecb-analyse-general-face))
          ;;
          ;; style du root (defini dans ecb-directory) affiche dans ecb-history;;
          '(ecb-history-bucket-node-dir-soure-path-face (quote dired-header))
          ;;
          '(ecb-method-non-semantic-face (quote font-lock-function-name-face))
          '(ecb-methods-general-face (quote font-lock-function-name-face))
          )

        ;; style
        (custom-set-faces
          ;; style de ??
          '(ecb-analyse-face
             ((((class color)
                 (background dark))
                (:inherit ecb-default-highlight-face))))
          ;;
          ;; style de highlight
          '(ecb-default-highlight-face
             ((((class color)
                 (background dark))
                (:background "firebrick3"))))
          ;;
          ;; style de directory
          '(ecb-directory-face
             ((((class color)
                 (background dark))
                (:inherit ecb-default-highlight-face))))
          ;;
          ;; style de history
          '(ecb-history-face
             ((((class color)
                 (background dark))
                (:inherit ecb-default-highlight-face))))
          ;;
          ;; style de methode
          '(ecb-method-face
             ((((class color)
                 (background dark))
                (:inherit ecb-default-highlight-face))))
          ;;
          ;; style de source
          '(ecb-source-face
             ((((class color)
                 (background dark))
                (:inherit ecb-default-highlight-face))))
          ;;
          ;; style des fichiers en lecture seul
          '(ecb-source-read-only-face
             ((((class color)
                 (background dark))
                (:inherit ecb-default-general-face
                  :background "grey15"
                  :foreground "MistyRose3"))))
          ;;
          ;; style des tags header
          '(ecb-tag-header-face
             ((((class color)
                 (background dark))
                (:background "SeaGreen"))))
          )
        (message "    6.5.7 Color Ecb... Done"))
      ) ; when section-mode-cedet-ecb
    ) ; (progn
  ) ; if section-display-color-theme

;;; display-color.el ends here

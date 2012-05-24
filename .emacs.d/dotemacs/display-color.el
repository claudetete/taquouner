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
;; Version: 1.9
;; Created: October 2006
;; Last-Updated: May 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-display-color'
;;              mode    `color-theme'
;;              var     `section-environment-terminal-vs-graphics'
;;              var     `section-display-color-theme'

;;; Change Log:
;; 2012-05-11 (1.9)
;;    use color theme with terminal + renamed for MM
;; 2012-05-02 (1.8)
;;    move section for fixing bug fullscreen
;; 2012-04-27 (1.7)
;;    fix bugs about highlight current line + syntax coloration + parentheses
;;    + etc... if color-theme is used
;; 2012-04-26 (1.6)
;;    add a new beta theme
;; 2012-04-20 (1.5)
;;    add working environment for setting color
;; 2012-03-28 (1.4)
;;    translate comments in English
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

;; highlight current line in the buffer
(global-hl-line-mode t)

;; syntax color
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;;
;;; PARENTHESES MODE
;; REQUIREMENT: var     `section-display-color-parentheses-mode'
;;              mode    `paren'
(when section-display-color-parentheses-mode (message "    5.4.1 Parentheses Mode...")
  ;; highlight parentheses at point
  (when (try-require 'paren "      ")
    (show-paren-mode t)
    (setq show-paren-ring-bell-on-mismatch t))
  (message "    5.4.1 Parentheses Mode... Done"))

;;
;;; PARENTHESES MINIBUFFER
;; REQUIREMENT: var     `section-display-color-parentheses-visible'
;;              mode    `mic-paren'
(when section-display-color-parentheses-visible (message "    5.4.2 Matched Parentheses display in Minibuffer ...")
  ;; show complementary parenthesis (if not displayed) in mode-line
  (when (try-require 'mic-paren "      ")
    (paren-activate))
  (message "    5.4.2 Matched Parentheses display in Minibuffer... Done"))

;;
;;; PARENTHESES HIGHLIGHT
;; REQUIREMENT: var     `section-display-color-parentheses-highlight'
;;              mode    `highlight-parentheses'
(when section-display-color-parentheses-highlight (message "    5.4.3 Parentheses Highlight Mode ...")
  ;; display parentheses in same color when they match else in and other
  ;; color
  (try-require 'highlight-parentheses "      ")
  (message "    5.4.3 Parentheses Highlight Mode... Done"))

;;
;;; COLOR THEME
;; REQUIREMENT: var     `section-display-color-theme'
(if section-display-color-theme
;;
;;; COLOR-THEME
  (progn
    (message "    5.4.4 Color Theme...")
    (when (try-require 'color-theme)
      ;; color theme is applied everywhere
      (setq color-theme-is-global t)
      ;; color theme can be cumulated
      (setq color-theme-is-cumulative t)
      ;; load all color theme
      (setq color-theme-load-all-themes t)

      ;; load custom color theme
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-billc.el"))
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-blackboard.el"))
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-desert.el"))
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-empty-void.el"))
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-hober2.el"))
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-inkpot.el"))
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-mac-classic.el"))
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-tango.el"))
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-tangotango.el")) ; ok
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-zenburn.el")) ; ok
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-solarized.el"))
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-clt.el"))
      (load-file (concat dotemacs-path "/plugins/themes/color-theme-sweet.el"))

      ;; Choose what theme you want with "Global Menu"->"Tools"->"Color Theme"
      (cond
        ;; Magneti Marelli -----------------------------------------------------
        ((string= clt-working-environment "Magneti Marelli")
          (color-theme-clt)
          ) ; Magneti Marelli

        ;; Alstom Transport ----------------------------------------------------
        ((string= clt-working-environment "Alstom Transport")
            (color-theme-sweet)
          ) ; Alstom Transport

        ;; default -------------------------------------------------------------
        ((string= clt-working-environment "default")
          ;;FIXME working environment default
          (color-theme-zenburn)
          ) ; default

        ) ; cond ---------------------------------------------------------------
      ) ; when try-require color-theme
    (message "    5.4.4 Color Theme... Done")
    ) ; if section-display-color-theme

  (progn
;;
;;; MISC
    ;; REQUIREMENT:     var     `section-display-color-misc'
    (when section-display-color-misc (message "    5.4.4 Color Misc...")
      ;; put all colors only when run graphical
      (when running-in-graphical
        (set-face-background 'default "black")
        (set-face-foreground 'default "white")
        (custom-set-faces
          ;; no 3d style in state bar (mode-line)
          '(mode-line ((((class color) (min-colors 88)) (:background "#5E4545" :foreground "grey78"))))
          '(mode-line-highlight ((((class color) (min-colors 88)) nil))))
        )

      (custom-set-faces
        ;; grey highlight instead of underline
        '(underline ((((supports :underline t)) (:background "grey20")))))
      (message "    5.4.4 Color Misc... Done"))

;;
;;; MODE
    ;; REQUIREMENT:     var     `section-display-color-mode'
    (when section-display-color-mode (message "    5.4.5 Color Mode...")
      ;; style and color of strings (bold and green)
      ;;(copy-face 'bold 'font-lock-string-face)
      (set-face-foreground 'font-lock-string-face "chartreuse3")
      ;;
      ;; style and color of types (bold and light purple)
      ;;(copy-face 'bold 'font-lock-type-face)
      (set-face-foreground 'font-lock-type-face "orchid2")
      ;;
      ;; style and color of keyword (bold and cyan)
      (copy-face 'bold 'font-lock-keyword-face)
      (set-face-foreground 'font-lock-keyword-face "cyan1")
      ;;
      ;; style and color of preprocessor (grey)
      ;;(copy-face 'bold 'font-lock-preprocessor-face)
      (set-face-foreground 'font-lock-preprocessor-face "ivory4")
      ;;
      ;; style and color of function (pale green)
      ;;(copy-face 'bold 'font-lock-function-name-face)
      (set-face-foreground 'font-lock-function-name-face "PaleGreen1")
      ;;
      ;; style and color of variable (light yellow)
      ;;(copy-face 'bold 'font-lock-variable-name-face)
      (set-face-foreground 'font-lock-variable-name-face "LightGoldenrod1")
      ;;
      ;; style and color of comments (blood orange)
      ;;(copy-face 'bold 'font-lock-comment-face)
      (set-face-foreground 'font-lock-comment-face "OrangeRed2")
      ;;
      ;; style and color of current line (dark grey)
      (when running-in-graphical
        (set-face-background 'hl-line "gray20"))
      ;;
      ;; style and color of cursor (pink)
      (set-face-background 'cursor "HotPink")

      (custom-set-faces
        '(hide-ifdef-shadow
           ((t (:inherit mode-line-inactive
                 :background "#101E1E"
                 :foreground "grey40"
                 :box nil)))))
      (message "    5.4.5 Color Mode... Done"))

;;
;;; GREP
    ;; REQUIREMENT:     var     `section-display-color-grep'
    (when section-display-color-grep (message "    5.4.6 Color Grep...")
      ;;;; style and color of 'grep hits'
      ;;(set-face-foreground 'compilation-info "magenta")
      ;;(set-face-underline  'compilation-info nil)
      ;;
      ;; style and color of 'grep matches'
      (set-face-background 'match "red3")
      (set-face-foreground 'match "white")
      ;;
      ;;;; style and color of 'grep error messages'
      ;;;;(set-face-foreground 'compilation-error "yellow")
      ;;
      ;; style and color of 'grep context lines'
      (set-face-foreground 'shadow "yellow")
      ;;
      ;;;; style and color of 'grep line number'
      ;;(set-face-foreground 'compilation-line-number "chartreuse3")
      ;;(set-face-underline  'compilation-line-number nil)
      ;;
      ;;;; style of link
      ;;(set-face-foreground 'link "chartreuse3")
      ;;
      (custom-set-faces
      ;; color of links 'grep name et path of file'
        '(link
           ((((class color)
               (min-colors 88)
               (background dark)) (:foreground "chartreuse3"))))
        ;; style and color of 'grep hits'
        '(compilation-info
           ((((class color)
               (background dark))
              (:background "black"
                :foreground "magenta"))))
        ;; style and color of 'grep error messages'
        '(compilation-error
           ((((class color)
               (background dark))
              (:background "black"
                :foreground "yellow"))))
        ;; style and color of 'grep line number'
        '(compilation-line-number
           ((((class color)
               (background dark))
              (:background "black"
                :foreground "chartreuse3"))))
        )
      (message "    5.4.6 Color Grep... Done"))

;;
;;; ECB
    ;; REQUIREMENT:     var     `section-display-color-ecb'
    (when section-mode-cedet-ecb
      (when section-display-color-ecb (message "    5.4.7 Color Ecb...")
        (custom-set-variables
          ;; style used for function name (method)
          '(ecb-method-face (quote ecb-method-face))
          ;; style of read only file in "ecb sources"
          '(ecb-source-read-only-face (quote ecb-source-read-only-face))
          ;;
          ;; style of analyze
          '(ecb-analyse-face (quote ecb-analyse-face))
          '(ecb-analyse-general-face (quote ecb-analyse-general-face))
          ;;
          ;; style of root (defined in ecb-directory) show in ecb-history
          '(ecb-history-bucket-node-dir-soure-path-face (quote dired-header))
          ;;
          '(ecb-method-non-semantic-face (quote font-lock-function-name-face))
          '(ecb-methods-general-face (quote font-lock-function-name-face))
          )

        ;; style
        (custom-set-faces
          ;; style of ??
          '(ecb-analyse-face
             ((((class color)
                 (background dark))
                (:inherit ecb-default-highlight-face))))
          ;;
          ;; style of highlight
          '(ecb-default-highlight-face
             ((((class color)
                 (background dark))
                (:background "firebrick3"))))
          ;;
          ;; style of directory
          '(ecb-directory-face
             ((((class color)
                 (background dark))
                (:inherit ecb-default-highlight-face))))
          ;;
          ;; style of history
          '(ecb-history-face
             ((((class color)
                 (background dark))
                (:inherit ecb-default-highlight-face))))
          ;;
          ;; style of method
          '(ecb-method-face
             ((((class color)
                 (background dark))
                (:inherit ecb-default-highlight-face))))
          ;;
          ;; style of source
          '(ecb-source-face
             ((((class color)
                 (background dark))
                (:inherit ecb-default-highlight-face))))
          ;;
          ;; style of read only file
          '(ecb-source-read-only-face
             ((((class color)
                 (background dark))
                (:inherit ecb-default-general-face
                  :background "grey15"
                  :foreground "MistyRose3"))))
          ;;
          ;; style of tags header
          '(ecb-tag-header-face
             ((((class color)
                 (background dark))
                (:background "SeaGreen"))))
          )
        (message "    5.4.7 Color Ecb... Done"))
      ) ; when section-mode-cedet-ecb
    ) ; (progn
  ) ; if section-display-color-theme

;;; display-color.el ends here

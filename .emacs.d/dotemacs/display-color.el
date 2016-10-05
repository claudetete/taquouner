;;; display-color.el --- a config file for color displayed setting

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

;; Keywords: config, display, color, mode, ecb, grep
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 2.7
;; Created: October 2006
;; Last-Updated: September 2016

;;; Commentary:
;;
;; load by `dotemacs/display.el'
;; REQUIREMENT: var     `section-display-color'
;;              mode    `color-theme'
;;              var     `section-environment-terminal-vs-graphics'
;;              var     `section-display-color-theme'

;;; Change Log:
;; 2016-09-28 (2.7)
;;    replace condition about emacs version
;; 2015-08-21 (2.6)
;;    remove old colorization + add custom highlight current line
;; 2014-03-26 (2.5)
;;    add solarized theme for Emacs 24
;; 2013-09-10 (2.4)
;;    add ansi color for compile window
;; 2012-12-27 (2.3)
;;    add/update hash for theme + add solarized + add setting color for powerline + update dot emacs path
;; 2012-07-09 (2.2)
;;    robustness + try disable highlight line when marked region
;; 2012-06-12 (2.1)
;;    separate color theme (Emacs 23) and themes (Emacs 24)
;; 2012-06-05 (2.0)
;;    add all themes to be set by profile value
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
(when section-display-color-theme
;;
;;; COLOR-THEME
  (message "    5.4.4 Color Theme...")
  (if (and section-environment-version-recognition (not running-on-emacs-23))
    (progn
      ;; add path of theme
      (add-to-list 'custom-theme-load-path (concat (file-name-as-directory dotemacs-path) "plugins/themes"))
      (add-to-list 'custom-theme-load-path (concat (file-name-as-directory dotemacs-path) "plugins/themes/solarized"))
      (custom-set-variables
        ;; add secure hash
        '(custom-safe-themes
           (quote
             (
               ;; SHA-1 hash
               "9e82ddb6aec1178072266e7824fabe2e2cfbcef5ce62a09f98ad74e76adce201" ; sweet
               "7606cceeaa8251c5997f9ab71bf3b701a7ac4050db3ac232dc8feb9d338fa1de" ; zenburn
               default
               ))))
      (cond
        ;; load theme
        ((string= profile-color-theme "adwaita")     (load-theme 'adwaita     t))
        ((string= profile-color-theme "deeper-blue") (load-theme 'deeper-blue t))
        ((string= profile-color-theme "dichromacy")  (load-theme 'dichromacy  t))
        ((string= profile-color-theme "light-blue")  (load-theme 'light-blue  t))
        ((string= profile-color-theme "manoj-dark")  (load-theme 'manoj-dark  t))
        ((string= profile-color-theme "misterioso")  (load-theme 'misterioso  t))
        ((string= profile-color-theme "tango-dark")  (load-theme 'tango-dark  t))
        ((string= profile-color-theme "tango")       (load-theme 'tango       t))
        ((string= profile-color-theme "tsdh-dark")   (load-theme 'tsdh-dark   t))
        ((string= profile-color-theme "tsdh-light")  (load-theme 'tsdh-light  t))
        ((string= profile-color-theme "wheatgrass")  (load-theme 'wheatgrass  t))
        ((string= profile-color-theme "whiteboard")  (load-theme 'whiteboard  t))
        ((string= profile-color-theme "wombat")      (load-theme 'wombat      t))
        ;; custom
        ((string= profile-color-theme "sweet") (load-theme 'sweet t))
        ((string= profile-color-theme "zenburn") (load-theme 'zenburn t))
        ((string= profile-color-theme "solarized-light") (load-theme 'solarized-light t))
        ) ; (cond
      )
    ;; else running-on-emacs-23
    (when (try-require 'color-theme "      ")
      ;; color theme is applied everywhere
      (setq color-theme-is-global t)
      ;; color theme can be cumulated
      (setq color-theme-is-cumulative t)
      ;; load all color theme
      (setq color-theme-load-all-themes t)

      ;; add to load path the profile directory
      (add-to-list 'load-path (concat (file-name-as-directory dotemacs-path) "plugins/themes"))

      ;; choose theme
      (cond
        ((string= profile-color-theme "aalto-dark") (color-theme-aalto-dark))
        ((string= profile-color-theme "aalto-light") (color-theme-aalto-light))
        ((string= profile-color-theme "aliceblue") (color-theme-aliceblue))
        ((string= profile-color-theme "andreas") (color-theme-andreas))
        ((string= profile-color-theme "arjen") (color-theme-arjen))
        ((string= profile-color-theme "beige-diff") (color-theme-beige-diff))
        ((string= profile-color-theme "bharadwaj") (color-theme-bharadwaj))
        ((string= profile-color-theme "bharadwaj-slate") (color-theme-bharadwaj-slate))
        ((string= profile-color-theme "billw") (color-theme-billw))
        ((string= profile-color-theme "black-on-gray") (color-theme-black-on-gray))
        ((string= profile-color-theme "blippblopp") (color-theme-blippblopp))
        ((string= profile-color-theme "simple-1") (color-theme-simple-1))
        ((string= profile-color-theme "blue-erc") (color-theme-blue-erc))
        ((string= profile-color-theme "blue-gnus") (color-theme-blue-gnus))
        ((string= profile-color-theme "blue-mood") (color-theme-blue-mood))
        ((string= profile-color-theme "blue-sea") (color-theme-blue-sea))
        ((string= profile-color-theme "calm-forest") (color-theme-calm-forest))
        ((string= profile-color-theme "charcoal-black") (color-theme-charcoal-black))
        ((string= profile-color-theme "goldenrod ") (color-theme-goldenrod))
        ((string= profile-color-theme "clarity") (color-theme-clarity))
        ((string= profile-color-theme "classic") (color-theme-classic))
        ((string= profile-color-theme "comidia") (color-theme-comidia))
        ((string= profile-color-theme "jsc-dark") (color-theme-jsc-dark))
        ((string= profile-color-theme "jsc-light") (color-theme-jsc-light))
        ((string= profile-color-theme "jsc-light2") (color-theme-jsc-light2))
        ((string= profile-color-theme "dark-blue") (color-theme-dark-blue))
        ((string= profile-color-theme "dark-blue2") (color-theme-dark-blue2))
        ((string= profile-color-theme "dark-green") (color-theme-dark-green))
        ((string= profile-color-theme "dark-laptop") (color-theme-dark-laptop))
        ((string= profile-color-theme "deep-blue") (color-theme-deep-blue))
        ((string= profile-color-theme "digital-ofs1") (color-theme-digital-ofs1))
        ((string= profile-color-theme "euphoria") (color-theme-euphoria))
        ((string= profile-color-theme "feng-shui") (color-theme-feng-shui))
        ((string= profile-color-theme "fischmeister") (color-theme-fischmeister))
        ((string= profile-color-theme "gnome") (color-theme-gnome))
        ((string= profile-color-theme "gnome2") (color-theme-gnome2))
        ((string= profile-color-theme "gray1") (color-theme-gray1))
        ((string= profile-color-theme "gray30") (color-theme-gray30))
        ((string= profile-color-theme "kingsajz") (color-theme-kingsajz))
        ((string= profile-color-theme "greiner") (color-theme-greiner))
        ((string= profile-color-theme "gtk-ide") (color-theme-gtk-ide))
        ((string= profile-color-theme "high-contrast") (color-theme-high-contrast))
        ((string= profile-color-theme "hober") (color-theme-hober))
        ((string= profile-color-theme "infodoc") (color-theme-infodoc))
        ((string= profile-color-theme "jb-simple") (color-theme-jb-simple))
        ((string= profile-color-theme "jedit-grey") (color-theme-jedit-grey))
        ((string= profile-color-theme "jonadabian") (color-theme-jonadabian))
        ((string= profile-color-theme "jonadabian-slate") (color-theme-jonadabian-slate))
        ((string= profile-color-theme "katester") (color-theme-katester))
        ((string= profile-color-theme "late-night") (color-theme-late-night))
        ((string= profile-color-theme "lawrence") (color-theme-lawrence))
        ((string= profile-color-theme "lethe") (color-theme-lethe))
        ((string= profile-color-theme "ld-dark") (color-theme-ld-dark))
        ((string= profile-color-theme "marine") (color-theme-marine))
        ((string= profile-color-theme "matrix") (color-theme-matrix))
        ((string= profile-color-theme "marquardt") (color-theme-marquardt))
        ((string= profile-color-theme "midnight") (color-theme-midnight))
        ((string= profile-color-theme "mistyday") (color-theme-mistyday))
        ((string= profile-color-theme "montz") (color-theme-montz))
        ((string= profile-color-theme "oswald") (color-theme-oswald))
        ((string= profile-color-theme "parus") (color-theme-parus))
        ((string= profile-color-theme "pierson") (color-theme-pierson))
        ((string= profile-color-theme "ramangalahy") (color-theme-ramangalahy))
        ((string= profile-color-theme "raspopovic") (color-theme-raspopovic))
        ((string= profile-color-theme "renegade") (color-theme-renegade))
        ((string= profile-color-theme "resolve") (color-theme-resolve))
        ((string= profile-color-theme "retro-green") (color-theme-retro-green))
        ((string= profile-color-theme "retro-orange") (color-theme-retro-orange))
        ((string= profile-color-theme "robin-hood") (color-theme-robin-hood))
        ((string= profile-color-theme "rotor") (color-theme-rotor))
        ((string= profile-color-theme "ryerson") (color-theme-ryerson))
        ((string= profile-color-theme "salmon-diff") (color-theme-salmon-diff))
        ((string= profile-color-theme "salmon-font-lock") (color-theme-salmon-font-lock))
        ((string= profile-color-theme "scintilla") (color-theme-scintilla))
        ((string= profile-color-theme "shaman") (color-theme-shaman))
        ((string= profile-color-theme "sitaramv-nt") (color-theme-sitaramv-nt))
        ((string= profile-color-theme "sitaramv-solaris") (color-theme-sitaramv-solaris))
        ((string= profile-color-theme "snow") (color-theme-snow))
        ((string= profile-color-theme "snowish") (color-theme-snowish))
        ((string= profile-color-theme "standard-ediff") (color-theme-standard-ediff))
        ((string= profile-color-theme "standard") (color-theme-standard))
        ((string= profile-color-theme "emacs-21") (color-theme-emacs-21))
        ((string= profile-color-theme "emacs-nw") (color-theme-emacs-nw))
        ((string= profile-color-theme "xemacs") (color-theme-xemacs))
        ((string= profile-color-theme "subtle-blue") (color-theme-subtle-blue))
        ((string= profile-color-theme "subtle-hacker") (color-theme-subtle-hacker))
        ((string= profile-color-theme "taming-mr-arneson") (color-theme-taming-mr-arneson))
        ((string= profile-color-theme "taylor") (color-theme-taylor))
        ((string= profile-color-theme "tty-dark") (color-theme-tty-dark))
        ((string= profile-color-theme "vim-colors") (color-theme-vim-colors))
        ((string= profile-color-theme "whateveryouwant") (color-theme-whateveryouwant))
        ((string= profile-color-theme "wheat") (color-theme-wheat))
        ((string= profile-color-theme "pok-wob") (color-theme-pok-wob))
        ((string= profile-color-theme "pok-wog") (color-theme-pok-wog))
        ((string= profile-color-theme "word-perfect") (color-theme-word-perfect))
        ((string= profile-color-theme "xp") (color-theme-xp))

        ;; load custom color theme
        ((string= profile-color-theme "billc") (try-require 'color-theme-billc "      ")
          (color-theme-billc))
        ((string= profile-color-theme "blackboard") (try-require 'color-theme-blackboard "      ")
          (color-theme-blackboard))
        ((string= profile-color-theme "desert") (try-require 'color-theme-desert "      ")
          (color-theme-desert))
        ((string= profile-color-theme "empty-void") (try-require 'color-theme-empty-void "      ")
          (color-theme-empty-void))
        ((string= profile-color-theme "hober2") (try-require 'color-theme-hober2 "      ")
          (color-theme-hober2))
        ((string= profile-color-theme "inkpot") (try-require 'color-theme-inkpot "      ")
          (color-theme-inkpot))
        ((string= profile-color-theme "mac-classic") (try-require 'color-theme-mac-classic "      ")
          (color-theme-mac-classic))
        ((string= profile-color-theme "tango") (try-require 'color-theme-tango "      ")
          (color-theme-tango))
        ((string= profile-color-theme "tangotango") (try-require 'color-theme-tangotango "      ")
          (color-theme-tangotango))
        ((string= profile-color-theme "zenburn") (try-require 'color-theme-zenburn "      ")
          (color-theme-zenburn)) ; ok
        ((string= profile-color-theme "solarized") (try-require 'color-theme-solarized "      ")
          (color-theme-solarized))
        ((string= profile-color-theme "clt") (try-require 'color-theme-clt "      ")
          (color-theme-clt))
        ((string= profile-color-theme "sweet") (try-require 'color-theme-sweet "      ")
          (color-theme-sweet)) ; ok
        ) ; (cond
      ;; Choose what theme you want with "Global Menu"->"Tools"->"Color Theme"
      ) ; when try-require color-theme
    )
  (when section-mode-powerline
    (setq powerline-color1 (face-foreground 'default))
    (setq powerline-color2 (face-foreground 'shadow)))
  (message "    5.4.4 Color Theme... Done")
  ) ; if section-display-color-theme

;;
;;; ANSI COLOR COMPILE WINDOW
(when section-display-color-ansi-color-compile (message "    5.4.5 Ansi color for Compile...")
  ;; thanks to http://stackoverflow.com/a/3072831
  (try-require 'ansi-color "      ")
  ;; define a function to colorize compile buffer
  (defun colorize-compilation-buffer ()
    ;; set to write to add color
    (toggle-read-only)
    ;; apply color on all buffer
    (ansi-color-apply-on-region (point-min) (point-max))
    ;; set read only
    (toggle-read-only))
  ;; add this function to compile buffer filter hook
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  ;; put nice color from my .Xdefaults
  ;; black red green yellow blue magenta cyan white
  (setq ansi-color-names-vector ["#757575" "#FF7272" "#ABCB8D" "yellow" "#67CDE9" "#E2BAF1" "#36FFFC" "#F1F1F1"])
  (setq ansi-color-map (ansi-color-make-color-map))
  (message "    5.4.5 Ansi color for Compile... Done"))


;;
;;; HIGHLIGHT CURRENT LINE
;; REQUIREMENT: var     `section-display-color-parentheses-highlight'
;;              mode    `highlight-parentheses'
(when section-display-color-highlight-line (message "    5.4.6 HighLigth Current Line...")
  (if (not running-on-ms-windows)
    ;; highlight current line in the buffer
    (global-hl-line-mode t) ;; too slow with MS Windows
    )
  (message "    5.4.6 HighLigth Current Line... Done"))


(provide 'display-color)

;;; display-color.el ends here

;;; 05-display-03-color.el --- a config file for color displayed setting

;; Copyright (c) 2006-2019 Claude Tete
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

;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 2.8
;; Created: October 2006
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.set color in emacs]
;; [SUBDEFAULT.t]


;;; Code:

;; syntax color
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; [VARCOMMENT.PARENTHESES MODE: matched parentheses are highlight]
;; [VARIABLE.tqnr-section-display-color-parentheses-mode t]
(when tqnr-section-display-color-parentheses-mode (message "      Parentheses Mode...")
  ;; highlight parentheses at point
  (when (try-require 'paren "      ")
    (show-paren-mode t)
    (setq show-paren-ring-bell-on-mismatch t))
  (message "      Parentheses Mode... Done"))

;;
;; [[VARCOMMENT.PARENTHESES MINIBUFFER: matched parentheses are highlight and if not
;; visible show it in the Minibuffer
;; ]]
;; [VARIABLE.tqnr-section-display-color-parentheses-visible t]
(when tqnr-section-display-color-parentheses-visible (message "      Matched Parentheses display in Minibuffer...")
  ;; show complementary parenthesis (if not displayed) in mode-line
  (when (try-require 'mic-paren "      ")
    (paren-activate))
  (message "      Matched Parentheses display in Minibuffer... Done"))

;;
;; [VARCOMMENT.PARENTHESES HIGHLIGHT: matched parentheses are highlight in rainbow color]
;; [VARIABLE.tqnr-section-display-color-parentheses-highlight nil]
(when tqnr-section-display-color-parentheses-highlight (message "      Parentheses Highlight Mode...")
  ;; display parentheses in same color when they match else in and other
  ;; color
  (try-require 'highlight-parentheses "      ")
  (message "      Parentheses Highlight Mode... Done"))

;;
;; [VARCOMMENT.COLOR THEME: set color by color-theme mode (or manual settings nil)]
;; [VARIABLE.tqnr-section-display-color-theme t]
(when tqnr-section-display-color-theme (message "      Color Theme...")
  ;; [VARCOMMENT.theme to be used, do not use it with terminal]
  ;; [VARIABLE.tqnr-profile-color-theme "zenburn"]
  (if (and tqnr-section-environment-version-recognition (not tqnr-running-on-emacs-23))
    (progn
      ;; add path of theme
      (add-to-list 'custom-theme-load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/themes"))
      (add-to-list 'custom-theme-load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/themes/solarized"))
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
        ((string= tqnr-profile-color-theme "adwaita")     (load-theme 'adwaita     t))
        ((string= tqnr-profile-color-theme "deeper-blue") (load-theme 'deeper-blue t))
        ((string= tqnr-profile-color-theme "dichromacy")  (load-theme 'dichromacy  t))
        ((string= tqnr-profile-color-theme "light-blue")  (load-theme 'light-blue  t))
        ((string= tqnr-profile-color-theme "manoj-dark")  (load-theme 'manoj-dark  t))
        ((string= tqnr-profile-color-theme "misterioso")  (load-theme 'misterioso  t))
        ((string= tqnr-profile-color-theme "tango-dark")  (load-theme 'tango-dark  t))
        ((string= tqnr-profile-color-theme "tango")       (load-theme 'tango       t))
        ((string= tqnr-profile-color-theme "tsdh-dark")   (load-theme 'tsdh-dark   t))
        ((string= tqnr-profile-color-theme "tsdh-light")  (load-theme 'tsdh-light  t))
        ((string= tqnr-profile-color-theme "wheatgrass")  (load-theme 'wheatgrass  t))
        ((string= tqnr-profile-color-theme "whiteboard")  (load-theme 'whiteboard  t))
        ((string= tqnr-profile-color-theme "wombat")      (load-theme 'wombat      t))
        ;; custom
        ((string= tqnr-profile-color-theme "sweet") (load-theme 'sweet t))
        ((string= tqnr-profile-color-theme "zenburn") (load-theme 'zenburn t))
        ((string= tqnr-profile-color-theme "solarized-light") (load-theme 'solarized-light t))
        ) ; (cond
      )
    ;; else tqnr-running-on-emacs-23
    (when (try-require 'color-theme "      ")
      ;; color theme is applied everywhere
      (setq color-theme-is-global t)
      ;; color theme can be cumulated
      (setq color-theme-is-cumulative t)
      ;; load all color theme
      (setq color-theme-load-all-themes t)

      ;; add to load path the profile directory
      (add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/themes"))

      ;; choose theme
      (cond
        ((string= tqnr-profile-color-theme "aalto-dark") (color-theme-aalto-dark))
        ((string= tqnr-profile-color-theme "aalto-light") (color-theme-aalto-light))
        ((string= tqnr-profile-color-theme "aliceblue") (color-theme-aliceblue))
        ((string= tqnr-profile-color-theme "andreas") (color-theme-andreas))
        ((string= tqnr-profile-color-theme "arjen") (color-theme-arjen))
        ((string= tqnr-profile-color-theme "beige-diff") (color-theme-beige-diff))
        ((string= tqnr-profile-color-theme "bharadwaj") (color-theme-bharadwaj))
        ((string= tqnr-profile-color-theme "bharadwaj-slate") (color-theme-bharadwaj-slate))
        ((string= tqnr-profile-color-theme "billw") (color-theme-billw))
        ((string= tqnr-profile-color-theme "black-on-gray") (color-theme-black-on-gray))
        ((string= tqnr-profile-color-theme "blippblopp") (color-theme-blippblopp))
        ((string= tqnr-profile-color-theme "simple-1") (color-theme-simple-1))
        ((string= tqnr-profile-color-theme "blue-erc") (color-theme-blue-erc))
        ((string= tqnr-profile-color-theme "blue-gnus") (color-theme-blue-gnus))
        ((string= tqnr-profile-color-theme "blue-mood") (color-theme-blue-mood))
        ((string= tqnr-profile-color-theme "blue-sea") (color-theme-blue-sea))
        ((string= tqnr-profile-color-theme "calm-forest") (color-theme-calm-forest))
        ((string= tqnr-profile-color-theme "charcoal-black") (color-theme-charcoal-black))
        ((string= tqnr-profile-color-theme "goldenrod ") (color-theme-goldenrod))
        ((string= tqnr-profile-color-theme "clarity") (color-theme-clarity))
        ((string= tqnr-profile-color-theme "classic") (color-theme-classic))
        ((string= tqnr-profile-color-theme "comidia") (color-theme-comidia))
        ((string= tqnr-profile-color-theme "jsc-dark") (color-theme-jsc-dark))
        ((string= tqnr-profile-color-theme "jsc-light") (color-theme-jsc-light))
        ((string= tqnr-profile-color-theme "jsc-light2") (color-theme-jsc-light2))
        ((string= tqnr-profile-color-theme "dark-blue") (color-theme-dark-blue))
        ((string= tqnr-profile-color-theme "dark-blue2") (color-theme-dark-blue2))
        ((string= tqnr-profile-color-theme "dark-green") (color-theme-dark-green))
        ((string= tqnr-profile-color-theme "dark-laptop") (color-theme-dark-laptop))
        ((string= tqnr-profile-color-theme "deep-blue") (color-theme-deep-blue))
        ((string= tqnr-profile-color-theme "digital-ofs1") (color-theme-digital-ofs1))
        ((string= tqnr-profile-color-theme "euphoria") (color-theme-euphoria))
        ((string= tqnr-profile-color-theme "feng-shui") (color-theme-feng-shui))
        ((string= tqnr-profile-color-theme "fischmeister") (color-theme-fischmeister))
        ((string= tqnr-profile-color-theme "gnome") (color-theme-gnome))
        ((string= tqnr-profile-color-theme "gnome2") (color-theme-gnome2))
        ((string= tqnr-profile-color-theme "gray1") (color-theme-gray1))
        ((string= tqnr-profile-color-theme "gray30") (color-theme-gray30))
        ((string= tqnr-profile-color-theme "kingsajz") (color-theme-kingsajz))
        ((string= tqnr-profile-color-theme "greiner") (color-theme-greiner))
        ((string= tqnr-profile-color-theme "gtk-ide") (color-theme-gtk-ide))
        ((string= tqnr-profile-color-theme "high-contrast") (color-theme-high-contrast))
        ((string= tqnr-profile-color-theme "hober") (color-theme-hober))
        ((string= tqnr-profile-color-theme "infodoc") (color-theme-infodoc))
        ((string= tqnr-profile-color-theme "jb-simple") (color-theme-jb-simple))
        ((string= tqnr-profile-color-theme "jedit-grey") (color-theme-jedit-grey))
        ((string= tqnr-profile-color-theme "jonadabian") (color-theme-jonadabian))
        ((string= tqnr-profile-color-theme "jonadabian-slate") (color-theme-jonadabian-slate))
        ((string= tqnr-profile-color-theme "katester") (color-theme-katester))
        ((string= tqnr-profile-color-theme "late-night") (color-theme-late-night))
        ((string= tqnr-profile-color-theme "lawrence") (color-theme-lawrence))
        ((string= tqnr-profile-color-theme "lethe") (color-theme-lethe))
        ((string= tqnr-profile-color-theme "ld-dark") (color-theme-ld-dark))
        ((string= tqnr-profile-color-theme "marine") (color-theme-marine))
        ((string= tqnr-profile-color-theme "matrix") (color-theme-matrix))
        ((string= tqnr-profile-color-theme "marquardt") (color-theme-marquardt))
        ((string= tqnr-profile-color-theme "midnight") (color-theme-midnight))
        ((string= tqnr-profile-color-theme "mistyday") (color-theme-mistyday))
        ((string= tqnr-profile-color-theme "montz") (color-theme-montz))
        ((string= tqnr-profile-color-theme "oswald") (color-theme-oswald))
        ((string= tqnr-profile-color-theme "parus") (color-theme-parus))
        ((string= tqnr-profile-color-theme "pierson") (color-theme-pierson))
        ((string= tqnr-profile-color-theme "ramangalahy") (color-theme-ramangalahy))
        ((string= tqnr-profile-color-theme "raspopovic") (color-theme-raspopovic))
        ((string= tqnr-profile-color-theme "renegade") (color-theme-renegade))
        ((string= tqnr-profile-color-theme "resolve") (color-theme-resolve))
        ((string= tqnr-profile-color-theme "retro-green") (color-theme-retro-green))
        ((string= tqnr-profile-color-theme "retro-orange") (color-theme-retro-orange))
        ((string= tqnr-profile-color-theme "robin-hood") (color-theme-robin-hood))
        ((string= tqnr-profile-color-theme "rotor") (color-theme-rotor))
        ((string= tqnr-profile-color-theme "ryerson") (color-theme-ryerson))
        ((string= tqnr-profile-color-theme "salmon-diff") (color-theme-salmon-diff))
        ((string= tqnr-profile-color-theme "salmon-font-lock") (color-theme-salmon-font-lock))
        ((string= tqnr-profile-color-theme "scintilla") (color-theme-scintilla))
        ((string= tqnr-profile-color-theme "shaman") (color-theme-shaman))
        ((string= tqnr-profile-color-theme "sitaramv-nt") (color-theme-sitaramv-nt))
        ((string= tqnr-profile-color-theme "sitaramv-solaris") (color-theme-sitaramv-solaris))
        ((string= tqnr-profile-color-theme "snow") (color-theme-snow))
        ((string= tqnr-profile-color-theme "snowish") (color-theme-snowish))
        ((string= tqnr-profile-color-theme "standard-ediff") (color-theme-standard-ediff))
        ((string= tqnr-profile-color-theme "standard") (color-theme-standard))
        ((string= tqnr-profile-color-theme "emacs-21") (color-theme-emacs-21))
        ((string= tqnr-profile-color-theme "emacs-nw") (color-theme-emacs-nw))
        ((string= tqnr-profile-color-theme "xemacs") (color-theme-xemacs))
        ((string= tqnr-profile-color-theme "subtle-blue") (color-theme-subtle-blue))
        ((string= tqnr-profile-color-theme "subtle-hacker") (color-theme-subtle-hacker))
        ((string= tqnr-profile-color-theme "taming-mr-arneson") (color-theme-taming-mr-arneson))
        ((string= tqnr-profile-color-theme "taylor") (color-theme-taylor))
        ((string= tqnr-profile-color-theme "tty-dark") (color-theme-tty-dark))
        ((string= tqnr-profile-color-theme "vim-colors") (color-theme-vim-colors))
        ((string= tqnr-profile-color-theme "whateveryouwant") (color-theme-whateveryouwant))
        ((string= tqnr-profile-color-theme "wheat") (color-theme-wheat))
        ((string= tqnr-profile-color-theme "pok-wob") (color-theme-pok-wob))
        ((string= tqnr-profile-color-theme "pok-wog") (color-theme-pok-wog))
        ((string= tqnr-profile-color-theme "word-perfect") (color-theme-word-perfect))
        ((string= tqnr-profile-color-theme "xp") (color-theme-xp))

        ;; load custom color theme
        ((string= tqnr-profile-color-theme "billc") (try-require 'color-theme-billc "      ")
          (color-theme-billc))
        ((string= tqnr-profile-color-theme "blackboard") (try-require 'color-theme-blackboard "      ")
          (color-theme-blackboard))
        ((string= tqnr-profile-color-theme "desert") (try-require 'color-theme-desert "      ")
          (color-theme-desert))
        ((string= tqnr-profile-color-theme "empty-void") (try-require 'color-theme-empty-void "      ")
          (color-theme-empty-void))
        ((string= tqnr-profile-color-theme "hober2") (try-require 'color-theme-hober2 "      ")
          (color-theme-hober2))
        ((string= tqnr-profile-color-theme "inkpot") (try-require 'color-theme-inkpot "      ")
          (color-theme-inkpot))
        ((string= tqnr-profile-color-theme "mac-classic") (try-require 'color-theme-mac-classic "      ")
          (color-theme-mac-classic))
        ((string= tqnr-profile-color-theme "tango") (try-require 'color-theme-tango "      ")
          (color-theme-tango))
        ((string= tqnr-profile-color-theme "tangotango") (try-require 'color-theme-tangotango "      ")
          (color-theme-tangotango))
        ((string= tqnr-profile-color-theme "zenburn") (try-require 'color-theme-zenburn "      ")
          (color-theme-zenburn)) ; ok
        ((string= tqnr-profile-color-theme "solarized") (try-require 'color-theme-solarized "      ")
          (color-theme-solarized))
        ((string= tqnr-profile-color-theme "clt") (try-require 'color-theme-clt "      ")
          (color-theme-clt))
        ((string= tqnr-profile-color-theme "sweet") (try-require 'color-theme-sweet "      ")
          (color-theme-sweet)) ; ok
        ) ; (cond
      ;; Choose what theme you want with "Global Menu"->"Tools"->"Color Theme"
      ) ; when try-require color-theme
    )

  (when tqnr-section-mode-powerline
    (setq powerline-color1 (face-foreground 'default))
    (setq powerline-color2 (face-foreground 'shadow)))
  (message "      Color Theme... Done")
  ) ; if section-display-color-theme

;;
;; [VARCOMMENT.ANSI COLOR COMPILE WINDOW: have color and no more junk like this ^[[32m]
;; [VARIABLE.tqnr-section-display-color-ansi-color-compile t]
(when tqnr-section-display-color-ansi-color-compile (message "      Ansi color for Compile...")
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
  (message "      Ansi color for Compile... Done"))


;;
;; [VARCOMMENT.HIGHLIGHT CURRENT LINE: have current line highlighted]
;; [VARIABLE.tqnr-section-display-color-highlight-line nil]
(when tqnr-section-display-color-highlight-line (message "      HighLigth Current Line...")
  (if (not tqnr-running-on-ms-windows)
    ;; highlight current line in the buffer
    (global-hl-line-mode t) ;; too slow with MS Windows
    )
  (message "      HighLigth Current Line... Done"))


(provide '05-display-03-color)

;;; 05-display-03-color.el ends here

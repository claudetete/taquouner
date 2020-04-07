;;; 05-display-03-color.el --- a config file for color displayed setting

;; Copyright (c) 2006-2019 Claude Tete
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;

;; Author: Claude Tete <claude.tete@gmail.com>
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
  (use-package paren
    :ensure nil
    :init
    (show-paren-mode t)
    :config
    (setq show-paren-ring-bell-on-mismatch t))
  (message "      Parentheses Mode... Done"))

;;
;; [[VARCOMMENT.PARENTHESES MINIBUFFER: matched parentheses are highlight and if not
;; visible show it in the Minibuffer
;; ]]
;; [VARIABLE.tqnr-section-display-color-parentheses-visible t]
(when tqnr-section-display-color-parentheses-visible (message "      Matched Parentheses display in Minibuffer...")
  ;; show complementary parenthesis (if not displayed) in mode-line
  (use-package mic-paren
    :init
    (paren-activate))
  (message "      Matched Parentheses display in Minibuffer... Done"))

;;
;; [VARCOMMENT.PARENTHESES HIGHLIGHT: matched parentheses are highlight in rainbow color]
;; [VARIABLE.tqnr-section-display-color-parentheses-highlight nil]
(when tqnr-section-display-color-parentheses-highlight (message "      Parentheses Highlight Mode...")
  ;; display parentheses in same color when they match else in and other
  ;; color
  (use-package highlight-parentheses)
  (message "      Parentheses Highlight Mode... Done"))

;;
;; [VARCOMMENT.COLOR THEME: set color by color-theme mode (or manual settings nil)]
;; [VARIABLE.tqnr-section-display-color-theme t]
(when tqnr-section-display-color-theme (message "      Color Theme...")
  (when tqnr-profile-color-theme
    ;; [VARCOMMENT.theme to be used, do not use it with terminal]
    ;; [VARIABLE.tqnr-profile-color-theme "zenburn"]
    ;; add path of theme
    (add-to-list 'custom-theme-load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/themes"))
    (add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/themes"))
    (add-to-list 'custom-theme-load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/themes/solarized"))
    (add-to-list 'custom-theme-load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/themes/base16"))
    (add-to-list 'custom-theme-load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/themes/doom"))
    ;; load theme
    ;; use (intern "string") to access to variable using its name from string
    (load-theme (intern tqnr-profile-color-theme) t))
  (message "      Color Theme... Done")
  ) ; if section-display-color-theme

;;
;; [VARCOMMENT.ANSI COLOR COMPILE WINDOW: have color and no more junk like this ^[[32m]
;; [VARIABLE.tqnr-section-display-color-ansi-color-compile t]
(when tqnr-section-display-color-ansi-color-compile (message "      Ansi color for Compile...")
  ;; thanks to http://stackoverflow.com/a/3072831
  (use-package ansi-color
    :config
    ;; put nice color from my .Xdefaults
    ;; black red green yellow blue magenta cyan white
    (setq ansi-color-names-vector ["#757575" "#FF7272" "#ABCB8D" "yellow" "#67CDE9" "#E2BAF1" "#36FFFC" "#F1F1F1"])
    (setq ansi-color-map (ansi-color-make-color-map))
    ;; define a function to colorize compile buffer
    (defun colorize-compilation-buffer ()
      ;; set to write to add color
      (toggle-read-only)
      ;; apply color on all buffer
      (ansi-color-apply-on-region (point-min) (point-max))
      ;; set read only
      (toggle-read-only))
    ;; add this function to compile buffer filter hook
    :hook (compilation-filter-hook . colorize-compilation-buffer))
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

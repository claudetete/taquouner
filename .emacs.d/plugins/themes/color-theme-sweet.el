;;; color-theme-sweet.el --- config file for Emacs

;; Copyright (c) 2012 Claude Tete
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

;; Keywords: config, emacs, color, theme
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.2
;; Created: April 2012
;; Last-Updated: June 2012

;;; Commentary:
;; from the custom color I was manually set, with a lot of polish to have less
;; aggressive contrast between color but still colorful
;;
;; TODO change Auto Highlight Symbol + finish dired+

;;; Change Log:
;; 2012-06-13 (1.2)
;;    remove box for modeline + add sml modeline
;; 2012-05-24 (1.1)
;;    change isearch background to see in comments
;; 2012-05-04 (1.0)
;;    add dired+ color + change highlight default for ecb and cursor

;;; Code:
(require 'color-theme)

;; color definitions
(defvar sweet-fg+1 "#D0B8A2")
(defvar sweet-fg   "#FFF1E3")

(defvar sweet-bg+2 "#000000") ; black
(defvar sweet-bg+1 "#191919") ; grey8
(defvar sweet-bg   "#262626") ; grey15
(defvar sweet-bg-1 "#282828")
(defvar sweet-bg-2 "#363636") ; grey20
(defvar sweet-bg-3 "#5B5B5B") ; grey35

(defvar sweet-red+2 "#6F2716")
(defvar sweet-red+1 "#BD4C31")
(defvar sweet-red   "#FF3100")
(defvar sweet-red-1 "#FF7555")
(defvar sweet-red-2 "#FFAC98")
(defvar sweet-red-3 "#5F4741")

(defvar sweet-orange+2 "#A34D00")
(defvar sweet-orange+1 "#BD7331")
(defvar sweet-orange   "#FF7800")
(defvar sweet-orange-1 "#FFA555")
(defvar sweet-orange-2 "#FFC998")
(defvar sweet-orange-3 "#FFE1C5")

(defvar sweet-green+2 "#00752F")
(defvar sweet-green+1 "#23884C")
(defvar sweet-green   "#00B74A")
(defvar sweet-green-1 "#4DE78B")
(defvar sweet-green-2 "#8AE7B0")

(defvar sweet-cyan+2 "#015B63")
(defvar sweet-cyan+1 "#1F6C73")
(defvar sweet-cyan   "#028E9B")
(defvar sweet-cyan-1 "#4BD1DD")
(defvar sweet-cyan-2 "#86D6DD")

(defvar sweet-pink+2 "#793758")
(defvar sweet-pink+1 "#B15382")
(defvar sweet-pink   "#FF69B4") ; HotPink
(defvar sweet-pink-1 "#CC6EC8") ; orchid2
(defvar sweet-pink-2 "#AD6B8C")
(defvar sweet-pink-3 "#CQB5B3") ; MistyRose3

(defvar sweet-grey+4 "#1E1E1E") ; grey10
(defvar sweet-grey+3 "#363636") ; grey20
(defvar sweet-grey+2 "#4F4F4F") ; grey30
(defvar sweet-grey+1 "#676767") ; grey40
(defvar sweet-grey   "#7B7B7B") ; gray50
(defvar sweet-grey-3 "#C9C9C9") ; grey80
(defvar sweet-grey-4 "#E1E1E1") ; grey90
(defvar sweet-grey-5 "#FAFAFA") ; grey100

(defun color-theme-sweet ()
  "Color theme Sweet by Claude TETE."
  (interactive)
  (color-theme-install
    `(color-theme-sweet
       (
         (foreground-color . ,sweet-fg)
         (background-color . ,sweet-bg)
         (background-mode . dark)
         (cursor-color . ,sweet-pink-1)
         (border-color . ,sweet-bg))

       ;; basic
       (default ((t (:foreground ,sweet-fg))))
       (cursor ((t (:background ,sweet-pink-1))))
       (escape-glyph ((t (:foreground ,sweet-cyan))))
       (fringe ((t (:background ,sweet-bg-2))))
       (header-line ((t (:background ,sweet-bg-2 :foreground ,sweet-fg+1))))
       (highlight ((t (:background ,sweet-bg-3))))
       (hl-line ((t (:background ,sweet-bg-2))))
       (lazy-highlight ((t (:background ,sweet-red+2))))
       (link ((t (:foreground ,sweet-green))))
       (link-visited ((t (:foreground ,sweet-pink+2))))
       (shadow ((t (:foreground ,sweet-orange))))
       (underline ((t (:background ,sweet-bg-2))))
       (vertical-border ((t (nil))))
       (region ((t (:background ,sweet-bg+2))))

       ;; font lock
       (font-lock-builtin-face ((t (:foreground ,sweet-cyan))))
       (font-lock-comment-delimiter-face ((t (:foreground ,sweet-red+1))))
       (font-lock-comment-face ((t (:foreground ,sweet-red+1))))
       (font-lock-constant-face ((t (:foreground ,sweet-cyan-2))))
       (font-lock-doc-face ((t (:foreground ,sweet-green))))
       (font-lock-function-name-face ((t (:foreground ,sweet-green-1))))
       (font-lock-keyword-face ((t (:foreground ,sweet-cyan-1))))
       (font-lock-preprocessor-face ((t (:foreground ,sweet-grey-3))))
       (font-lock-string-face ((t (:foreground ,sweet-green))))
       (font-lock-type-face ((t (:foreground ,sweet-pink-1))))
       (font-lock-variable-name-face ((t (:foreground ,sweet-orange-2))))
       (font-lock-warning-face ((t (:bold t :foreground ,sweet-pink))))

       ;; mode line
       (mode-line ((t (:background ,sweet-bg+1 :foreground ,sweet-grey-3 :box nil))))
       (mode-line-buffer-id ((t (:foreground ,sweet-cyan-1 :bold t :weight bold :box nil))))
       (mode-line-emphasis ((t (:foreground ,sweet-green-1 :bold t :weight bold :box nil))))
       (mode-line-inactive ((t (:background ,sweet-grey+2 :foreground ,sweet-grey-3 :box nil))))
       (which-func ((t (:foreground ,sweet-green-1))))

       ;; compilation
       (compilation-column-number ((t (:foreground ,sweet-pink-3))))
       (compilation-error ((t (:foreground ,sweet-orange-1))))
       (compilation-info ((t (:foreground ,sweet-pink+1))))
       (compilation-line-number ((t (:foreground ,sweet-green))))
       (compilation-warning ((t (:bold t :foreground ,sweet-orange :weight bold))))
       (match ((t (:background ,sweet-red :foreground ,sweet-fg))))
       (next-error ((t (:background ,sweet-cyan+2))))

       ;; minibuffer
       (minibuffer-prompt ((t (:foreground ,sweet-cyan))))

       ;; isearch
       (isearch ((t (:background ,sweet-pink+2))))
       (isearch-fail ((t (:background ,sweet-red))))

       ;; edit
       (show-paren-match ((t (:background ,sweet-cyan))))
       (show-paren-mismatch ((t (:background ,sweet-pink+2))))
       (trailing-whitespace ((t (:background ,sweet-red-1))))

       ;; Auto Highligh Symbol
       (ahs-definition-face ((t (:background "CadetBlue" :foreground "moccasin" :underline t))))
       (ahs-edit-mode-face ((t (:background "Coral3" :foreground "White"))))
       (ahs-face ((t (:background "LightYellow4" :foreground "GhostWhite"))))
       (ahs-plugin-bod-face ((t (:background "DodgerBlue" :foreground "Black"))))
       (ahs-plugin-defalt-face ((t (:background "Orange1" :foreground "Black"))))
       (ahs-plugin-whole-buffer-face ((t (:background "GreenYellow" :foreground "Black"))))
       (ahs-warning-face ((t (:bold t :foreground "Red" :weight bold))))

       ;; ECB
       (ecb-analyse-face ((t (:background ,sweet-grey+3))))
       (ecb-default-highlight-face ((t (:background ,sweet-pink-2))))
       (ecb-directory-face ((t (:inherit ecb-default-highlight-face))))
       (ecb-history-face ((t (:inherit ecb-default-highlight-face))))
       (ecb-method-face ((t (:inherit ecb-default-highlight-face))))
       (ecb-method-non-semantic-face ((t (:inherit font-lock-function-name-face))))
       (ecb-methods-general-face ((t (:inherit font-lock-function-name-face))))
       (ecb-source-face ((t (:inherit ecb-default-highlight-face))))
       (ecb-source-read-only-face ((t (:background ,sweet-grey+3 :foreground ,sweet-fg+1 :height 1.0))))
       (ecb-tag-header-face ((t (:background ,sweet-cyan+1))))
       (ecb-history-bucket-node-dir-soure-path-face ((t (:inherit dired-header))))

       ;; dired+
       (diredp-file-name ((t (:foreground ,sweet-orange-2))))
       (diredp-file-suffix ((t (:foreground ,sweet-green-1))))
       (diredp-dir-priv ((t (:foreground ,sweet-cyan))))
       (diredp-dir-heading ((t (:foreground ,sweet-orange-2))))
       (diredp-number ((t (:foreground ,sweet-cyan-2))))
       (diredp-read-priv ((t (:foreground ,sweet-orange-2))))
       (diredp-write-priv ((t (:foreground ,sweet-green))))
       (diredp-exec-priv ((t (:foreground ,sweet-red+1))))
       (diredp-ignored-file-name ((t (:foreground ,sweet-red-3))))
       (diredp-date-time ((t (:foreground ,sweet-pink+1))))

       ;; diff
       (diff-removed ((t (:foreground ,sweet-orange+2))))
       (diff-indicator-removed ((t (:background ,sweet-orange+2))))
       (diff-added ((t (:foreground ,sweet-green+1))))
       (diff-indicator-added ((t (:background ,sweet-green+1))))
       (diff-header ((t (:foreground ,sweet-cyan+1))))
       (diff-index ((t (:foreground ,sweet-pink+1))))
       (diff-file-header ((t (:foreground ,sweet-cyan :background ,sweet-bg+2))))
       (diff-hunk-header ((t (:foreground ,sweet-bg+2 :background ,sweet-cyan))))

       ;; ediff
       (ediff-even-diff-A ((t (:background ,sweet-bg-1))))
       (ediff-even-diff-B ((t (:background ,sweet-bg-1))))
       (ediff-even-diff-C ((t (:background ,sweet-bg-1))))
       (ediff-current-diff-A ((t (:background ,sweet-red-3))))
       (ediff-current-diff-B ((t (:background ,sweet-red-3))))
       (ediff-current-diff-C ((t (:background ,sweet-red-3))))
       (ediff-fine-diff-A ((t (:foreground ,sweet-fg :background ,sweet-red))))
       (ediff-fine-diff-B ((t (:foreground ,sweet-fg :background ,sweet-red))))
       (ediff-fine-diff-C ((t (:foreground ,sweet-fg :background ,sweet-red))))
       (ediff-odd-diff-A ((t (:background ,sweet-bg-1))))
       (ediff-odd-diff-B ((t (:background ,sweet-bg-1))))
       (ediff-odd-diff-C ((t (:background ,sweet-bg-1))))

       ;; hide-ifdef
       (hide-ifdef-shadow ((t (:background ,sweet-grey+4 :foreground ,sweet-grey-4 :box nil))))

       ;; sml-modeline
       (sml-modeline-end-face ((t (:background ,sweet-bg :box (:line-width 1 :color ,sweet-grey)))))
       (sml-modeline-vis-face ((t (:background ,sweet-bg-3 :box (:line-width 1 :color ,sweet-grey)))))
       )
    )
  )
(add-to-list 'color-themes '(color-theme-sweet
                              "Sweet"
                              "Claude TETE <claude.tete@gmail.com"))

(provide 'color-theme-sweet)

;;; color-theme-sweet.el ends here.

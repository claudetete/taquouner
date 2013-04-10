;;; sweet-theme.el --- A sweet theme with a lot of color

;; Copyright (c) 2012-2013 Claude Tete

;; This file is NOT part of GNU Emacs.
;;
;; This file is free software: you can redistribute it and/or modify
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
;; Version: 1.4
;; Created: April 2012
;; Last-Updated: April 2013

;;; Commentary:
;; from the custom color I was manually set, with a lot of polish to have less
;; aggressive contrast between color but still colorful (port from
;; color-theme-sweet)
;;

;;; Change Log:
;; 2013-04-10 (1.4)
;;    add helm mode
;; 2012-11-27 (1.3)
;;    change compile color
;; 2012-06-18 (1.2)
;;    add foreground color for isearch highlight to more readability
;; 2012-06-13 (1.1)
;;    remove box around modeline (for powerline) + add sml modeline
;; 2012-06-11 (1.0)
;;    from color-theme with Emacs 23.4

;;; Code:
(deftheme sweet
  "Less aggressive contrast but still colorful.")

(let ((class '((class color) (min-colors 89)))
       ;; color definitions
       (sweet-fg+1 "#D0B8A2")
       (sweet-fg   "#FFF1E3")

       (sweet-bg+2 "#000000") ; black
       (sweet-bg+1 "#191919") ; grey8
       (sweet-bg   "#262626") ; grey15
       (sweet-bg-1 "#282828")
       (sweet-bg-2 "#363636") ; grey20
       (sweet-bg-3 "#5B5B5B") ; grey35

       (sweet-red+3 "#6F0000")
       (sweet-red+2 "#6F2716")
       (sweet-red+1 "#BD4C31")
       (sweet-red   "#FF3100")
       (sweet-red-1 "#FF7555")
       (sweet-red-2 "#FFAC98")
       (sweet-red-3 "#5F4741")

       (sweet-orange+2 "#A34D00")
       (sweet-orange+1 "#BD7331")
       (sweet-orange   "#FF7800")
       (sweet-orange-1 "#FFA555")
       (sweet-orange-2 "#FFC998")
       (sweet-orange-3 "#FFE1C5")

       (sweet-green+2 "#00752F")
       (sweet-green+1 "#23884C")
       (sweet-green   "#00B74A")
       (sweet-green-1 "#4DE78B")
       (sweet-green-2 "#8AE7B0")

       (sweet-cyan+2 "#015B63")
       (sweet-cyan+1 "#1F6C73")
       (sweet-cyan   "#028E9B")
       (sweet-cyan-1 "#4BD1DD")
       (sweet-cyan-2 "#86D6DD")

       (sweet-pink+2 "#793758")
       (sweet-pink+1 "#B15382")
       (sweet-pink   "#FF69B4") ; HotPink
       (sweet-pink-1 "#CC6EC8") ; orchid2
       (sweet-pink-2 "#AD6B8C")
       (sweet-pink-3 "#CAB5B3") ; MistyRose3

       (sweet-grey+4 "#1E1E1E") ; grey10
       (sweet-grey+3 "#363636") ; grey20
       (sweet-grey+2 "#4F4F4F") ; grey30
       (sweet-grey+1 "#676767") ; grey40
       (sweet-grey   "#7B7B7B") ; grey50
       (sweet-grey-3 "#C9C9C9") ; grey80
       (sweet-grey-4 "#E1E1E1") ; grey90
       (sweet-grey-5 "#FAFAFA") ; grey100
       )

  (custom-theme-set-faces
    'sweet

    ;; basic
    `(default         ((,class (:foreground ,sweet-fg :background ,sweet-bg))))
    `(cursor          ((,class (:background ,sweet-pink-1))))
    `(escape-glyph    ((,class (:foreground ,sweet-cyan))))
    `(fringe          ((,class (:background ,sweet-bg-2))))
    `(header-line     ((,class (:background ,sweet-bg-2 :foreground ,sweet-fg+1))))
    `(highlight       ((,class (:background ,sweet-bg-3))))
    `(hl-line         ((,class (:background ,sweet-bg-2))))
    `(lazy-highlight  ((,class (:background ,sweet-red+2))))
    `(link            ((,class (:foreground ,sweet-green))))
    `(link-visited    ((,class (:foreground ,sweet-pink+2))))
    `(shadow          ((,class (:foreground ,sweet-orange))))
    `(underline       ((,class (:background ,sweet-bg-2))))
    `(vertical-border ((,class (nil))))
    `(region          ((,class (:background ,sweet-bg+2))))

    ;; font lock
    `(font-lock-builtin-face           ((,class (:foreground ,sweet-cyan))))
    `(font-lock-comment-delimiter-face ((,class (:foreground ,sweet-red+1))))
    `(font-lock-comment-face           ((,class (:foreground ,sweet-red+1))))
    `(font-lock-constant-face          ((,class (:foreground ,sweet-cyan-2))))
    `(font-lock-doc-face               ((,class (:foreground ,sweet-green))))
    `(font-lock-function-name-face     ((,class (:foreground ,sweet-green-1))))
    `(font-lock-keyword-face           ((,class (:foreground ,sweet-cyan-1))))
    `(font-lock-preprocessor-face      ((,class (:foreground ,sweet-grey-3))))
    `(font-lock-string-face            ((,class (:foreground ,sweet-green))))
    `(font-lock-type-face              ((,class (:foreground ,sweet-pink-1))))
    `(font-lock-variable-name-face     ((,class (:foreground ,sweet-orange-2))))
    `(font-lock-warning-face           ((,class (:bold t :foreground ,sweet-pink))))

    ;; mode line
    `(mode-line           ((,class (:background ,sweet-bg+1 :foreground ,sweet-grey-3 :box nil))))
    `(mode-line-buffer-id ((,class (:foreground ,sweet-cyan-1 :bold t :weight bold :box nil))))
    `(mode-line-emphasis  ((,class (:foreground ,sweet-green-1 :bold t :weight bold :box nil))))
    `(mode-line-inactive  ((,class (:background ,sweet-grey+2 :foreground ,sweet-grey-3 :box nil))))
    `(which-func          ((,class (:foreground ,sweet-green-1))))

    ;; compilation
    `(compilation-column-number ((,class (:foreground ,sweet-pink-3))))
    `(compilation-error         ((,class (:foreground ,sweet-pink+1))))
    `(compilation-info          ((,class (:foreground ,sweet-pink-1))))
    `(compilation-line-number   ((,class (:foreground ,sweet-green))))
    `(compilation-warning       ((,class (:bold t :foreground ,sweet-orange :weight bold))))
    `(match                     ((,class (:background ,sweet-red+2 :foreground ,sweet-fg))))
    `(next-error                ((,class (:background ,sweet-cyan+2))))

    ;; minibuffer
    `(minibuffer-prompt ((,class (:foreground ,sweet-cyan))))

    ;; isearch
    `(isearch      ((,class (:foreground ,sweet-fg :background ,sweet-pink+2))))
    `(isearch-fail ((,class (:background ,sweet-red))))

    ;; edit
    `(show-paren-match    ((,class (:background ,sweet-cyan))))
    `(show-paren-mismatch ((,class (:background ,sweet-pink+2))))
    `(trailing-whitespace ((,class (:background ,sweet-red-1))))

    ;; Auto Highligh Symbol
    `(ahs-definition-face          ((,class (:background "CadetBlue" :foreground "moccasin" :underline t))))
    `(ahs-edit-mode-face           ((,class (:background "Coral3" :foreground "White"))))
    `(ahs-face                     ((,class (:background "LightYellow4" :foreground "GhostWhite"))))
    `(ahs-plugin-bod-face          ((,class (:background "DodgerBlue" :foreground "Black"))))
    `(ahs-plugin-defalt-face       ((,class (:background "Orange1" :foreground "Black"))))
    `(ahs-plugin-whole-buffer-face ((,class (:background "GreenYellow" :foreground "Black"))))
    `(ahs-warning-face             ((,class (:bold t :foreground "Red" :weight bold))))

    ;; ECB
    `(ecb-analyse-face                            ((,class (:background ,sweet-grey+3))))
    `(ecb-default-highlight-face                  ((,class (:background ,sweet-pink-2))))
    `(ecb-directory-face                          ((,class (:inherit ecb-default-highlight-face))))
    `(ecb-history-face                            ((,class (:inherit ecb-default-highlight-face))))
    `(ecb-method-face                             ((,class (:inherit ecb-default-highlight-face))))
    `(ecb-method-non-semantic-face                ((,class (:inherit font-lock-function-name-face))))
    `(ecb-methods-general-face                    ((,class (:inherit font-lock-function-name-face))))
    `(ecb-source-face                             ((,class (:inherit ecb-default-highlight-face))))
    `(ecb-source-read-only-face                   ((,class (:background ,sweet-grey+3 :foreground ,sweet-fg+1 :height 1.0))))
    `(ecb-tag-header-face                         ((,class (:background ,sweet-cyan+1))))
    `(ecb-history-bucket-node-dir-soure-path-face ((,class (:inherit dired-header))))

    ;; dired+
    `(diredp-file-name         ((,class (:foreground ,sweet-orange-2))))
    `(diredp-file-suffix       ((,class (:foreground ,sweet-green-1))))
    `(diredp-dir-priv          ((,class (:foreground ,sweet-cyan))))
    `(diredp-dir-heading       ((,class (:foreground ,sweet-orange-2))))
    `(diredp-number            ((,class (:foreground ,sweet-cyan-2))))
    `(diredp-read-priv         ((,class (:foreground ,sweet-orange-2))))
    `(diredp-write-priv        ((,class (:foreground ,sweet-green))))
    `(diredp-exec-priv         ((,class (:foreground ,sweet-red+1))))
    `(diredp-ignored-file-name ((,class (:foreground ,sweet-red-3))))
    `(diredp-date-time         ((,class (:foreground ,sweet-pink+1))))

    ;; diff
    `(diff-removed           ((,class (:foreground ,sweet-orange+2))))
    `(diff-indicator-removed ((,class (:background ,sweet-orange+2))))
    `(diff-added             ((,class (:foreground ,sweet-green+1))))
    `(diff-indicator-added   ((,class (:background ,sweet-green+1))))
    `(diff-header            ((,class (:foreground ,sweet-cyan+1 :background ,sweet-bg))))
    `(diff-index             ((,class (:foreground ,sweet-pink+1))))
    `(diff-file-header       ((,class (:foreground ,sweet-cyan :background ,sweet-bg+2))))
    `(diff-hunk-header       ((,class (:foreground ,sweet-bg+2 :background ,sweet-cyan))))
    `(diff-context           ((,class (:foreground ,sweet-fg+1))))

    ;; ediff
    `(ediff-even-diff-A    ((,class (:foreground ,sweet-fg+1 :background ,sweet-red-3))))
    `(ediff-even-diff-B    ((,class (:foreground ,sweet-fg+1 :background ,sweet-red-3))))
    `(ediff-even-diff-C    ((,class (:foreground ,sweet-fg+1 :background ,sweet-red-3))))
    `(ediff-current-diff-A ((,class (:foreground ,sweet-bg+1 :background ,sweet-red-2))))
    `(ediff-current-diff-B ((,class (:foreground ,sweet-bg+1 :background ,sweet-red-2))))
    `(ediff-current-diff-C ((,class (:foreground ,sweet-bg+1 :background ,sweet-red-2))))
    `(ediff-fine-diff-A    ((,class (:foreground ,sweet-fg :background ,sweet-red+3))))
    `(ediff-fine-diff-B    ((,class (:foreground ,sweet-fg :background ,sweet-red+3))))
    `(ediff-fine-diff-C    ((,class (:foreground ,sweet-fg :background ,sweet-red+3))))
    `(ediff-odd-diff-A     ((,class (:foreground ,sweet-fg+1 :background ,sweet-red-3))))
    `(ediff-odd-diff-B     ((,class (:foreground ,sweet-fg+1 :background ,sweet-red-3))))
    `(ediff-odd-diff-C     ((,class (:foreground ,sweet-fg+1 :background ,sweet-red-3))))

    ;; helm
    `(helm-header           ((,class (:foreground ,sweet-fg+1 :background ,sweet-bg+2))))
    `(helm-selection        ((,class (:background ,sweet-bg-2 :underline nil))))
    `(helm-separator        ((,class (:foreground ,sweet-pink+2 :background ,sweet-fg))))
    `(helm-selection-line   ((,class (:foreground ,sweet-bg-2))))
    `(helm-candidate-number ((,class (:foreground ,sweet-orange-1 :background ,sweet-bg+2))))
    `(helm-ff-directory     ((,class (:foreground ,sweet-cyan-2 :background ,sweet-bg-2))))
    `(helm-ff-executable    ((,class (:foreground ,sweet-red-2))))
    `(helm-ff-file          ((,class (:foreground ,sweet-fg))))

    ;; hide-ifdef
    `(hide-ifdef-shadow ((,class (:background ,sweet-grey+4 :foreground ,sweet-grey-4 :box nil))))

    ;; sml-modeline
    `(sml-modeline-end-face ((,class (:background ,sweet-bg :box (:line-width 1 :color ,sweet-grey)))))
    `(sml-modeline-vis-face ((,class (:background ,sweet-bg-3 :box (:line-width 1 :color ,sweet-grey)))))
    )
  )


;;(custom-set-faces
;; '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
;; '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
;; '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
;; '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
;; '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
;; '(cfw:face-grid ((t :foreground "DarkGrey")))
;; '(cfw:face-default-content ((t :foreground "#bfebbf")))
;; '(cfw:face-periods ((t :foreground "cyan")))
;; '(cfw:face-day-title ((t :background "grey10")))
;; '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
;; '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
;; '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
;; '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
;; '(cfw:face-today ((t :background: "grey10" :weight bold)))
;; '(cfw:face-select ((t :background "#2f2f2f")))
;; '(cfw:face-toolbar ((t :foreground "Steelblue4" :background "Steelblue4")))
;; '(cfw:face-toolbar-button-off ((t :foreground "Gray10" :weight bold)))
;; '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold))))

(provide-theme 'sweet)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; sweet-theme.el ends here.

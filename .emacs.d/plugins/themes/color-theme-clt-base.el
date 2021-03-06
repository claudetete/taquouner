(eval-when-compile    (require 'color-theme))
(defun my-color-theme ()
  "Color theme by Claude TETE, created 2012-04-24."
  (interactive)
  (color-theme-install
   '(my-color-theme
     ((background-color . "black")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "HotPink")
      (foreground-color . "white")
      (mouse-color . "black"))
     ((compilation-message-face . underline)
      (cua-global-mark-cursor-color . "cyan")
      (cua-normal-cursor-color . "black")
      (cua-overwrite-cursor-color . "yellow")
      (cua-read-only-cursor-color . "darkgreen")
      (diary-face . diary)
      (ecb-analyse-bucket-element-face . ecb-analyse-bucket-element-face)
      (ecb-analyse-bucket-node-face . ecb-analyse-bucket-node-face)
      (ecb-analyse-face . ecb-analyse-face)
      (ecb-analyse-general-face . ecb-analyse-general-face)
      (ecb-directories-general-face . ecb-default-general-face)
      (ecb-directory-face . ecb-default-highlight-face)
      (ecb-directory-not-accessible-face . ecb-directory-not-accessible-face)
      (ecb-history-bucket-node-dir-soure-path-face . dired-header)
      (ecb-history-bucket-node-face . ecb-history-bucket-node-face)
      (ecb-history-dead-buffer-face . ecb-history-dead-buffer-face)
      (ecb-history-face . ecb-default-highlight-face)
      (ecb-history-general-face . ecb-default-general-face)
      (ecb-history-indirect-buffer-face . ecb-history-indirect-buffer-face)
      (ecb-method-face . ecb-method-face)
      (ecb-method-non-semantic-face . font-lock-function-name-face)
      (ecb-methods-general-face . font-lock-function-name-face)
      (ecb-mode-line-data-face . ecb-mode-line-data-face)
      (ecb-mode-line-prefix-face . ecb-mode-line-prefix-face)
      (ecb-mode-line-win-nr-face . ecb-mode-line-win-nr-face)
      (ecb-source-face . ecb-default-highlight-face)
      (ecb-source-in-directories-buffer-face . ecb-source-in-directories-buffer-face)
      (ecb-source-read-only-face . ecb-source-read-only-face)
      (ecb-sources-general-face . ecb-default-general-face)
      (ecb-symboldef-symbol-face . use-font-lock-face)
      (ecb-tag-header-face . ecb-tag-header-face)
      (ecb-tree-guide-line-face . ecb-tree-guide-line-face)
      (goto-address-mail-face . italic)
      (goto-address-mail-mouse-face . secondary-selection)
      (goto-address-url-face . link)
      (goto-address-url-mouse-face . highlight)
      (hl-line-face . hl-line)
      (ispell-highlight-face . isearch)
      (list-matching-lines-buffer-name-face . underline)
      (list-matching-lines-face . match)
      (paren-match-face . paren-face-match)
      (paren-mismatch-face . paren-face-mismatch)
      (paren-no-match-face . paren-face-no-match)
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     (default ((t (:stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 75 :width normal :foundry "raster" :family "ProggyTinySZ"))))
     (ahs-definition-face ((t (:background "CadetBlue" :foreground "moccasin" :underline t))))
     (ahs-edit-mode-face ((t (:background "Coral3" :foreground "White"))))
     (ahs-face ((t (:background "LightYellow4" :foreground "GhostWhite"))))
     (ahs-plugin-bod-face ((t (:background "DodgerBlue" :foreground "Black"))))
     (ahs-plugin-defalt-face ((t (:background "Orange1" :foreground "Black"))))
     (ahs-plugin-whole-buffer-face ((t (:background "GreenYellow" :foreground "Black"))))
     (ahs-warning-face ((t (:bold t :foreground "Red" :weight bold))))
     (bold ((t (:bold t :weight bold))))
     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (border ((t (nil))))
     (buffer-menu-buffer ((t (:bold t :weight bold))))
     (button ((t (:underline t))))
     (calendar-today ((t (:underline t))))
     (change-log-acknowledgement ((t (:foreground "OrangeRed2"))))
     (change-log-conditionals ((t (:foreground "LightGoldenrod1"))))
     (change-log-date ((t (:foreground "chartreuse3"))))
     (change-log-email ((t (:foreground "LightGoldenrod1"))))
     (change-log-file ((t (:foreground "PaleGreen1"))))
     (change-log-function ((t (:foreground "LightGoldenrod1"))))
     (change-log-list ((t (:foreground "cyan1"))))
     (change-log-name ((t (:foreground "Aquamarine"))))
     (comint-highlight-input ((t (:bold t :weight bold))))
     (comint-highlight-prompt ((t (:foreground "cyan1"))))
     (compilation-column-number ((t (:foreground "orchid2"))))
     (compilation-error ((t (:background "black" :foreground "yellow"))))
     (compilation-info ((t (:background "black" :foreground "magenta"))))
     (compilation-line-number ((t (:background "black" :foreground "chartreuse3"))))
     (compilation-warning ((t (:bold t :foreground "Orange" :weight bold))))
     (completions-annotations ((t (:underline t))))
     (completions-common-part ((t (:family "ProggyTinySZ" :foundry "raster" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "white" :background "black" :stipple nil :height 75))))
     (completions-first-difference ((t (:bold t :weight bold))))
     (cua-global-mark ((t (:background "yellow1" :foreground "black"))))
     (cua-rectangle ((t (:background "maroon" :foreground "white"))))
     (cua-rectangle-noselect ((t (:background "dimgray" :foreground "white"))))
     (cursor ((t (:background "HotPink"))))
     (custom-button ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))
     (custom-button-mouse ((t (:background "grey90" :foreground "black" :box (:line-width 2 :style released-button)))))
     (custom-button-pressed ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))
     (custom-button-pressed-unraised ((t (:background "grey20" :foreground "violet"))))
     (custom-button-unraised ((t (:background "grey20"))))
     (custom-changed ((t (:background "blue1" :foreground "white"))))
     (custom-comment ((t (:background "dim gray"))))
     (custom-comment-tag ((t (:foreground "gray80"))))
     (custom-documentation ((t (nil))))
     (custom-face-tag ((t (:bold t :weight bold :foreground "light blue"))))
     (custom-group-tag ((t (:bold t :family "Sans Serif" :foreground "light blue" :weight bold :height 1.2))))
     (custom-group-tag-1 ((t (:bold t :family "Sans Serif" :foreground "pink" :weight bold :height 1.2))))
     (custom-invalid ((t (:background "red1" :foreground "yellow1"))))
     (custom-link ((t (:foreground "chartreuse3"))))
     (custom-modified ((t (:background "blue1" :foreground "white"))))
     (custom-rogue ((t (:background "black" :foreground "pink"))))
     (custom-saved ((t (:underline t))))
     (custom-set ((t (:background "white" :foreground "blue1"))))
     (custom-state ((t (:foreground "lime green"))))
     (custom-themed ((t (:background "blue1" :foreground "white"))))
     (custom-variable-button ((t (:bold t :underline t :weight bold))))
     (custom-variable-tag ((t (:bold t :foreground "light blue" :weight bold))))
     (custom-visibility ((t (:foreground "chartreuse3" :height 0.8))))
     (diary ((t (:foreground "yellow1"))))
     (diary-anniversary ((t (:foreground "cyan1"))))
     (diary-button ((t (nil))))
     (diary-time ((t (:foreground "LightGoldenrod1"))))
     (dired-directory ((t (:foreground "PaleGreen1"))))
     (dired-flagged ((t (:bold t :weight bold :foreground "Pink"))))
     (dired-header ((t (:foreground "orchid2"))))
     (dired-ignored ((t (:foreground "yellow"))))
     (dired-mark ((t (:foreground "Aquamarine"))))
     (dired-marked ((t (:bold t :weight bold :foreground "Pink"))))
     (dired-perm-write ((t (:family "ProggyTinySZ" :foundry "raster" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "white" :background "black" :stipple nil :height 75))))
     (dired-symlink ((t (:foreground "cyan1"))))
     (dired-warning ((t (:bold t :weight bold :foreground "Pink"))))
     (diredp-compressed-file-suffix ((t (:foreground "Blue"))))
     (diredp-date-time ((t (:foreground "#74749A9AF7F7"))))
     (diredp-deletion ((t (:background "Red" :foreground "Yellow"))))
     (diredp-deletion-file-name ((t (:foreground "Red"))))
     (diredp-dir-heading ((t (:background "#00003F3F3434" :foreground "Yellow"))))
     (diredp-dir-priv ((t (:background "#2C2C2C2C2C2C" :foreground "#7474FFFFFFFF"))))
     (diredp-display-msg ((t (:foreground "Yellow"))))
     (diredp-exec-priv ((t (:background "#4F4F3B3B2121"))))
     (diredp-executable-tag ((t (:foreground "Red"))))
     (diredp-file-name ((t (:foreground "Yellow"))))
     (diredp-file-suffix ((t (:foreground "#7474FFFF7474"))))
     (diredp-flag-mark ((t (:background "#7575D4D41D1D" :foreground "Blue"))))
     (diredp-flag-mark-line ((t (:background "#787831311414"))))
     (diredp-ignored-file-name ((t (:foreground "#C29D6F156F15"))))
     (diredp-link-priv ((t (:foreground "#00007373FFFF"))))
     (diredp-no-priv ((t (:background "#2C2C2C2C2C2C"))))
     (diredp-number ((t (:foreground "#FFFFFFFF7474"))))
     (diredp-other-priv ((t (:background "#111117175555"))))
     (diredp-rare-priv ((t (:background "#FFFF00008080" :foreground "Green"))))
     (diredp-read-priv ((t (:background "#999932325555"))))
     (diredp-symlink ((t (:foreground "#00007373FFFF"))))
     (diredp-write-priv ((t (:background "#25258F8F2929"))))
     (ecb-analyse-bucket-element-face ((t (:height 1.0))))
     (ecb-analyse-bucket-node-face ((t (:bold t :weight bold :height 1.0))))
     (ecb-analyse-face ((t (:background "firebrick3"))))
     (ecb-analyse-general-face ((t (:height 1.0))))
     (ecb-bucket-node-face ((t (:bold t :weight bold :height 1.0))))
     (ecb-default-general-face ((t (:height 1.0))))
     (ecb-default-highlight-face ((t (:background "firebrick3"))))
     (ecb-directories-general-face ((t (:height 1.0))))
     (ecb-directory-face ((t (:background "firebrick3"))))
     (ecb-directory-not-accessible-face ((t (:foreground "gray60" :height 1.0))))
     (ecb-history-bucket-node-dir-soure-path-face ((t (:bold t :weight bold :foreground "DarkMagenta" :height 1.0))))
     (ecb-history-bucket-node-face ((t (:bold t :weight bold :height 1.0))))
     (ecb-history-dead-buffer-face ((t (:foreground "gray60" :height 1.0))))
     (ecb-history-face ((t (:background "firebrick3"))))
     (ecb-history-general-face ((t (:height 1.0))))
     (ecb-history-indirect-buffer-face ((t (:italic t :slant italic :height 1.0))))
     (ecb-method-face ((t (:background "firebrick3"))))
     (ecb-method-non-semantic-face ((t (:foreground "brown" :height 1.0))))
     (ecb-methods-general-face ((t (:height 1.0))))
     (ecb-mode-line-data-face ((t (nil))))
     (ecb-mode-line-prefix-face ((t (:foreground "forestgreen"))))
     (ecb-mode-line-win-nr-face ((t (:bold t :weight bold))))
     (ecb-source-face ((t (:background "firebrick3"))))
     (ecb-source-in-directories-buffer-face ((t (:foreground "LightBlue1" :height 1.0))))
     (ecb-source-read-only-face ((t (:background "grey15" :foreground "MistyRose3" :height 1.0))))
     (ecb-sources-general-face ((t (:height 1.0))))
     (ecb-symboldef-prototype-face ((t (:bold t :weight bold :height 1.0))))
     (ecb-symboldef-symbol-face ((t (:bold t :weight bold :height 1.0))))
     (ecb-tag-header-face ((t (:background "SeaGreen"))))
     (ecb-tree-guide-line-face ((t (:foreground "gray" :height 1.0))))
     (ecb-type-tag-class-face ((t (:bold t :weight bold))))
     (ecb-type-tag-enum-face ((t (:bold t :weight bold))))
     (ecb-type-tag-group-face ((t (:bold t :foreground "dim gray" :weight bold))))
     (ecb-type-tag-interface-face ((t (:bold t :weight bold))))
     (ecb-type-tag-struct-face ((t (:bold t :weight bold))))
     (ecb-type-tag-typedef-face ((t (:bold t :weight bold))))
     (ecb-type-tag-union-face ((t (:bold t :weight bold))))
     (ediff-current-diff-A ((t (:background "pale green" :foreground "firebrick"))))
     (ediff-current-diff-Ancestor ((t (:background "VioletRed" :foreground "Black"))))
     (ediff-current-diff-B ((t (:background "Yellow" :foreground "DarkOrchid"))))
     (ediff-current-diff-C ((t (:background "Pink" :foreground "Navy"))))
     (ediff-even-diff-A ((t (:background "light grey" :foreground "Black"))))
     (ediff-even-diff-Ancestor ((t (:background "Grey" :foreground "White"))))
     (ediff-even-diff-B ((t (:background "Grey" :foreground "White"))))
     (ediff-even-diff-C ((t (:background "light grey" :foreground "Black"))))
     (ediff-fine-diff-A ((t (:background "sky blue" :foreground "Navy"))))
     (ediff-fine-diff-Ancestor ((t (:background "Green" :foreground "Black"))))
     (ediff-fine-diff-B ((t (:background "cyan" :foreground "Black"))))
     (ediff-fine-diff-C ((t (:background "Turquoise" :foreground "Black"))))
     (ediff-odd-diff-A ((t (:background "Grey" :foreground "White"))))
     (ediff-odd-diff-Ancestor ((t (:background "gray40" :foreground "cyan3"))))
     (ediff-odd-diff-B ((t (:background "light grey" :foreground "Black"))))
     (ediff-odd-diff-C ((t (:background "Grey" :foreground "White"))))
     (eieio-custom-slot-tag-face ((t (:foreground "light blue"))))
     (eldoc-highlight-function-argument ((t (:bold t :weight bold))))
     (escape-glyph ((t (:foreground "cyan"))))
     (file-name-shadow ((t (:foreground "yellow"))))
     (fixed-pitch ((t (:family "Monospace"))))
     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-comment-delimiter-face ((t (:foreground "OrangeRed2"))))
     (font-lock-comment-face ((t (:foreground "OrangeRed2"))))
     (font-lock-constant-face ((t (:foreground "Aquamarine"))))
     (font-lock-doc-face ((t (:foreground "chartreuse3"))))
     (font-lock-function-name-face ((t (:foreground "PaleGreen1"))))
     (font-lock-keyword-face ((t (:foreground "cyan1"))))
     (font-lock-negation-char-face ((t (nil))))
     (font-lock-preprocessor-face ((t (:foreground "ivory4"))))
     (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
     (font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
     (font-lock-string-face ((t (:foreground "chartreuse3"))))
     (font-lock-type-face ((t (:foreground "orchid2"))))
     (font-lock-variable-name-face ((t (:foreground "LightGoldenrod1"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink" :weight bold))))
     (fringe ((t (:background "grey10"))))
     (header-line ((t (:background "grey20" :foreground "grey90" :box nil))))
     (help-argument-name ((t (nil))))
     (highlight ((t (:background "darkolivegreen"))))
     (hl-line ((t (:background "gray20"))))
     (holiday ((t (:background "chocolate4"))))
     (isearch ((t (:background "palevioletred2" :foreground "brown4"))))
     (isearch-fail ((t (:background "red4"))))
     (italic ((t (:underline t))))
     (lazy-highlight ((t (:background "paleturquoise4"))))
     (link ((t (:foreground "chartreuse3"))))
     (link-visited ((t (:foreground "violet"))))
     (match ((t (:background "red3" :foreground "white"))))
     (menu ((t (:foreground "systemmenu" :background "systemmenutext"))))
     (message-cited-text ((t (:foreground "LightPink1"))))
     (message-header-cc ((t (:bold t :foreground "chartreuse1" :weight bold))))
     (message-header-name ((t (:foreground "green"))))
     (message-header-newsgroups ((t (:italic t :bold t :foreground "yellow" :slant italic :weight bold))))
     (message-header-other ((t (:foreground "VioletRed1"))))
     (message-header-subject ((t (:foreground "OliveDrab1"))))
     (message-header-to ((t (:bold t :foreground "DarkOliveGreen1" :weight bold))))
     (message-header-xheader ((t (:foreground "DeepSkyBlue1"))))
     (message-mml ((t (:foreground "MediumSpringGreen"))))
     (message-separator ((t (:foreground "LightSkyBlue1"))))
     (minibuffer-prompt ((t (:foreground "cyan"))))
     (mode-line ((t (:background "#5E4545" :foreground "grey78"))))
     (mode-line-buffer-id ((t (:bold t :weight bold))))
     (mode-line-emphasis ((t (:bold t :weight bold))))
     (mode-line-highlight ((t (nil))))
     (mode-line-inactive ((t (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))
     (mouse ((t (nil))))
     (next-error ((t (:background "blue3"))))
     (nobreak-space ((t (:foreground "cyan" :underline t))))
     (outline-1 ((t (:foreground "PaleGreen1"))))
     (outline-2 ((t (:foreground "LightGoldenrod1"))))
     (outline-3 ((t (:foreground "cyan1"))))
     (outline-4 ((t (:foreground "OrangeRed2"))))
     (outline-5 ((t (:foreground "orchid2"))))
     (outline-6 ((t (:foreground "Aquamarine"))))
     (outline-7 ((t (:foreground "LightSteelBlue"))))
     (outline-8 ((t (:foreground "chartreuse3"))))
     (paren-face-match ((t (:background "turquoise"))))
     (paren-face-mismatch ((t (:background "purple" :foreground "white"))))
     (paren-face-no-match ((t (:background "yellow" :foreground "black"))))
     (pulse-highlight-face ((t (:background "#AAAA33"))))
     (pulse-highlight-start-face ((t (:background "#AAAA33"))))
     (query-replace ((t (:foreground "brown4" :background "palevioletred2"))))
     (region ((t (:background "blue3"))))
     (rtrt-script-subl-face ((t (:foreground "green"))))
     (rtrt-script-tab-face ((t (:background "hotpink"))))
     (scroll-bar ((t (:foreground "systemscrollbar"))))
     (secondary-selection ((t (:background "SkyBlue4"))))
     (semantic-decoration-on-fileless-includes ((t (:background "#009000"))))
     (semantic-decoration-on-includes ((t (nil))))
     (semantic-decoration-on-private-members-face ((t (:background "#200000"))))
     (semantic-decoration-on-protected-members-face ((t (:background "#000020"))))
     (semantic-decoration-on-unknown-includes ((t (:background "#900000"))))
     (semantic-decoration-on-unparsed-includes ((t (:background "#555500"))))
     (semantic-highlight-edits-face ((t (:background "gray20"))))
     (semantic-highlight-func-current-tag-face ((t (:background "gray20"))))
     (semantic-tag-boundary-face ((t (:overline "cyan"))))
     (semantic-unmatched-syntax-face ((t (:underline "red"))))
     (senator-intangible-face ((t (:foreground "gray75"))))
     (senator-momentary-highlight-face ((t (:background "gray30"))))
     (senator-read-only-face ((t (:background "#664444"))))
     (shadow ((t (:foreground "yellow"))))
     (show-paren-match ((t (:background "steelblue3"))))
     (show-paren-mismatch ((t (:background "purple" :foreground "white"))))
     (speedbar-button-face ((t (:foreground "green3"))))
     (speedbar-directory-face ((t (:foreground "light blue"))))
     (speedbar-file-face ((t (:foreground "cyan"))))
     (speedbar-highlight-face ((t (:background "sea green"))))
     (speedbar-selected-face ((t (:foreground "red" :underline t))))
     (speedbar-separator-face ((t (:background "blue" :foreground "white" :overline "gray"))))
     (speedbar-tag-face ((t (:foreground "yellow"))))
     (tool-bar ((t (:background "systembuttonface" :foreground "systembuttontext" :box (:line-width 1 :style released-button)))))
     (tooltip ((t (:family "Sans Serif" :background "systeminfowindow" :foreground "systeminfotext"))))
     (trailing-whitespace ((t (:background "red1"))))
     (underline ((t (:background "grey20"))))
     (undo-tree-visualizer-active-branch-face ((t (:bold t :foreground "white" :weight bold))))
     (undo-tree-visualizer-current-face ((t (:foreground "red"))))
     (undo-tree-visualizer-default-face ((t (:foreground "gray"))))
     (undo-tree-visualizer-register-face ((t (:foreground "yellow"))))
     (variable-pitch ((t (:family "Sans Serif"))))
     (vertical-border ((t (nil))))
     (which-func ((t (:foreground "Blue1"))))
     (widget-button ((t (:bold t :weight bold))))
     (widget-button-pressed ((t (:foreground "red1"))))
     (widget-documentation ((t (:foreground "lime green"))))
     (widget-field ((t (:background "dim gray"))))
     (widget-inactive ((t (:foreground "yellow"))))
     (widget-single-line-field ((t (:background "dim gray")))))))
(add-to-list 'color-themes '(my-color-theme  "THEME NAME" "YOUR NAME"))

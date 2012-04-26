
(require 'color-theme)


;; toujours du tres fonce au plus clair avec au milieu le petant

;; Primary Color: Orange Sanguine
;; A31F00 *BD4C31 FF3100 FF7555 FFAC98
;;
;; Secondary Color A: jaune orange
;; FF7800 BD7331 A34D00 FFA555 FFC998 FFE1C5
;;
;; Secondary Color B: bleu cyan
;; 028E9B 1F6C73 015B63 *4BD1DD *86D6DD
;;
;; Complementary Color: vert sapin
;; 00B74A 23884C 00752F 4DE78B 8AE7B0 *00240E *013E19

;; color definitions
(defvar sweet-fg "#FFE1C5")
(defvar sweet-fg+1 "#D0B8A2")
(defvar sweet-bg "grey10")
(defvar sweet-bg-1 "grey20")
(defvar sweet-bg-2 "grey35")

;(defvar sweet-bg "#080D0A")
;(defvar sweet-bg-1 "#060F0A")
;(defvar sweet-bg-2 "#071F10")

(defvar sweet-red+2 "#A31F00")
(defvar sweet-red+1 "#BD4C31")
(defvar sweet-red "#FF3100")
(defvar sweet-red-1 "#FF7555")
(defvar sweet-red-2 "#FFAC98")
(defvar sweet-red-3 "#5F4741")

(defvar sweet-orange+2 "#A34D00")
(defvar sweet-orange+1 "#BD7331")
(defvar sweet-orange "#FF7800")
(defvar sweet-orange-1 "#FFA555")
(defvar sweet-orange-2 "#FFC998")
(defvar sweet-orange-3 "#FFE1C5")

(defvar sweet-green+2 "#00752F")
(defvar sweet-green+1 "#23884C")
(defvar sweet-green "#00B74A")
(defvar sweet-green-1 "4DE78B")
(defvar sweet-green-2 "#8AE7B0")

(defvar sweet-cyan+2 "#015B63")
(defvar sweet-cyan+1 "#1F6C73")
(defvar sweet-cyan "#028E9B")
(defvar sweet-cyan-1 "#4BD1DD")
(defvar sweet-cyan-2 "#86D6DD")

(defvar sweet-pink+2 "#793758")
(defvar sweet-pink+1 "#B15382")
(defvar sweet-pink "HotPink")
(defvar sweet-pink-1 "orchid2")
(defvar sweet-pink-2 "MistyRose3")

(defvar sweet-grey+4 "grey10")
(defvar sweet-grey+3 "grey20")
(defvar sweet-grey+2 "grey30")
(defvar sweet-grey+1 "grey40")
(defvar sweet-grey-3 "grey80")
(defvar sweet-grey-4 "grey90")
(defvar sweet-grey-5 "grey100")

(defun color-theme-sweet ()
  "Color theme by Claude TETE, created 2012-04-24."
  (interactive)
  (color-theme-install
    `(color-theme-sweet
       (
         (foreground-color . ,sweet-fg)
         (background-color . ,sweet-bg)
         (background-mode . dark)
         (cursor-color . ,sweet-pink)
         (border-color . ,sweet-bg))

       ;; basic
       (default ((t (:foreground ,sweet-fg))))
       (cursor ((t (:background ,sweet-pink))))
       (escape-glyph ((t (:foreground ,sweet-cyan))))
       (fringe ((t (:background ,sweet-bg-2))))
       (header-line ((t (:background ,sweet-bg-1 :foreground ,sweet-fg+1))))
       (highlight ((t (:background ,sweet-bg-2))))
       (hl-line ((t (:background ,sweet-bg-1))))
       (lazy-highlight ((t (:background ,sweet-cyan-1))))
       (link ((t (:foreground ,sweet-green))))
       (link-visited ((t (:foreground ,sweet-pink+2))))
       (shadow ((t (:foreground ,sweet-orange))))
       (underline ((t (:background ,sweet-bg-1))))
       (vertical-border ((t (nil))))
       (region ((t (:background ,sweet-bg-1))))

       ;; font lock
       (font-lock-builtin-face ((t (:foreground ,sweet-cyan))))
       (font-lock-comment-delimiter-face ((t (:foreground ,sweet-red+1))))
       (font-lock-comment-face ((t (:foreground ,sweet-red+1))))
       (font-lock-constant-face ((t (:foreground ,sweet-cyan+1))))
       (font-lock-doc-face ((t (:foreground ,sweet-green))))
       (font-lock-function-name-face ((t (:foreground ,sweet-green-1))))
       (font-lock-keyword-face ((t (:foreground ,sweet-cyan-1))))
       (font-lock-preprocessor-face ((t (:foreground ,sweet-orange-1))))
       (font-lock-string-face ((t (:foreground ,sweet-green))))
       (font-lock-type-face ((t (:foreground ,sweet-pink-2))))
       (font-lock-variable-name-face ((t (:foreground ,sweet-orange-1))))
       (font-lock-warning-face ((t (:bold t :foreground ,sweet-pink))))

       ;; mode line
       (mode-line ((t (:background ,sweet-red-3 :foreground ,sweet-grey-3))))
       (mode-line-buffer-id ((t (:foreground ,sweet-cyan-1 :bold t :weight bold))))
       (mode-line-emphasis ((t (:foreground ,sweet-green-1 :bold t :weight bold))))
       (mode-line-inactive ((t (:background ,sweet-grey+2 :foreground ,sweet-grey-3 :box (:line-width -1 :color ,sweet-grey+1 :style nil) :weight light))))
       (which-func ((t (:foreground ,sweet-green-1))))

       ;; compilation
       (compilation-column-number ((t (:foreground ,sweet-pink-2))))
       (compilation-error ((t (:foreground ,sweet-orange-1))))
       (compilation-info ((t (:foreground ,sweet-pink+1))))
       (compilation-line-number ((t (:foreground ,sweet-green))))
       (compilation-warning ((t (:bold t :foreground ,sweet-orange :weight bold))))
       (match ((t (:background ,sweet-red :foreground ,sweet-fg))))
       (next-error ((t (:background ,sweet-cyan+2))))

       ;; minibuffer
       (minibuffer-prompt ((t (:foreground ,sweet-cyan))))

       ;; isearch
       (isearch ((t (:background ,sweet-red+1 :foreground ,sweet-orange+2))))
       (isearch-fail ((t (:background ,sweet-red))))

       ;; edit
       (show-paren-match ((t (:background ,sweet-cyan+2))))
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
       (ecb-default-highlight-face ((t (:background ,sweet-orange+2))))
       (ecb-directory-face ((t (:inherit ecb-default-highlight-face))))
       (ecb-history-face ((t (:inherit ecb-default-highlight-face))))
       (ecb-method-face ((t (:inherit ecb-default-highlight-face))))
       (ecb-method-non-semantic-face ((t (:inherit font-lock-function-name-face))))
       (ecb-methods-general-face ((t (:inherit font-lock-function-name-face))))
       (ecb-source-face ((t (:inherit ecb-default-highlight-face))))
       (ecb-source-read-only-face ((t (:background ,sweet-grey+3 :foreground ,sweet-pink+2 :height 1.0))))
       (ecb-tag-header-face ((t (:background ,sweet-cyan+1))))
       (ecb-history-bucket-node-dir-soure-path-face ((t (:inherit dired-header))))

       ;; hide-ifdef
       (hide-ifdef-shadow ((t (:background ,sweet-grey+4 :foreground ,sweet-grey-4 :box nil))))

       )
    )
  )
(add-to-list 'color-themes '(color-theme-sweet
                              "Sweet"
                              "Claude TETE <claude.tete@gmail.com"))

(provide 'color-theme-sweet)

;;; color-theme-sweet.el ends here.


(require 'color-theme)


;; toujours du tres fonce au plus clair avec au milieu le petant

;; Primary Color: Orange Sanguine
;; A31F00 BD4C31 FF3100 FF7555 FFAC98
;;
;; Secondary Color A: jaune orange
;; FF7800 BD7331 A34D00 FFA555 FFC998
;;
;; Secondary Color B: bleu cyan
;; 028E9B 1F6C73 015B63 4BD1DD 86D6DD
;;
;; Complementary Color: vert sapin
;; 00B74A 23884C 00752F 4DE78B 8AE7B0

;; color definitions
(defvar clt-fg "white")
(defvar clt-bg "black")
(defvar clt-red+2 "red4")
(defvar clt-red+1 "red3")
(defvar clt-red "firebrick3")
(defvar clt-red-1 "#5E4545")
(defvar clt-orange+2 "OrangeRed4")
(defvar clt-orange+1 "OrangeRed2")
(defvar clt-orange "Orange")
(defvar clt-orange-1 "LightGoldenrod1")
(defvar clt-brown "brown4")
(defvar clt-yellow "yellow")
(defvar clt-yellow-1 "ivory4")
(defvar clt-green+1 "darkolivegreen")
(defvar clt-green "chartreuse3")
(defvar clt-green-1 "PaleGreen1")
(defvar clt-green-2 "#A0C3A0")
(defvar clt-cyan+1 "SeaGreen")
(defvar clt-cyan "cyan")
(defvar clt-cyan-1 "paleturquoise4")
(defvar clt-blue+2 "Aquamarine")
(defvar clt-blue+3 "steelblue3")
(defvar clt-blue "blue3")
(defvar clt-blue-5 "LightSteelBlue")
(defvar clt-magenta+4 "violet")
(defvar clt-magenta "magenta")
(defvar clt-magenta-4 "palevioletred2")
(defvar clt-pink+2 "MistyRose3")
(defvar clt-pink "HotPink")
(defvar clt-pink-2 "orchid2")
(defvar clt-grey+4 "grey10")
(defvar clt-grey+3 "grey20")
(defvar clt-grey+2 "grey30")
(defvar clt-grey+1 "grey40")
(defvar clt-grey-3 "grey80")
(defvar clt-grey-4 "grey90")
(defvar clt-grey-5 "grey100")

(defun color-theme-clt ()
  "Color theme by Claude TETE, created 2012-04-24."
  (interactive)
  (color-theme-install
    `(color-theme-clt
       (
         (foreground-color . ,clt-fg)
         (background-color . ,clt-bg)
         (background-mode . dark)
         (cursor-color . ,clt-pink)
         (border-color . ,clt-bg))

       ;; basic
       (default ((t (:foreground ,clt-fg))))
       (cursor ((t (:background ,clt-pink))))
       (escape-glyph ((t (:foreground ,clt-cyan))))
       (fringe ((t (:background ,clt-grey+4))))
       (header-line ((t (:background ,clt-grey+3 :foreground ,clt-grey-4))))
       (highlight ((t (:background ,clt-green+1))))
       (hl-line ((t (:background ,clt-grey+3))))
       (lazy-highlight ((t (:background ,clt-cyan-1))))
       (link ((t (:foreground ,clt-green))))
       (link-visited ((t (:foreground ,clt-magenta+4))))
       (shadow ((t (:foreground ,clt-yellow))))
       (underline ((t (:background ,clt-grey+3))))
       (vertical-border ((t (nil))))
       (region ((t (:background ,clt-grey+3))))

       ;; font lock
       (font-lock-builtin-face ((t (:foreground ,clt-blue-5))))
       (font-lock-comment-delimiter-face ((t (:foreground ,clt-orange+1))))
       (font-lock-comment-face ((t (:foreground ,clt-orange+1))))
       (font-lock-constant-face ((t (:foreground ,clt-blue+2))))
       (font-lock-doc-face ((t (:foreground ,clt-green))))
       (font-lock-function-name-face ((t (:foreground ,clt-green-1))))
       (font-lock-keyword-face ((t (:foreground ,clt-cyan))))
       (font-lock-preprocessor-face ((t (:foreground ,clt-yellow-1))))
       (font-lock-string-face ((t (:foreground ,clt-green))))
       (font-lock-type-face ((t (:foreground ,clt-pink-2))))
       (font-lock-variable-name-face ((t (:foreground ,clt-orange-1))))
       (font-lock-warning-face ((t (:bold t :foreground ,clt-pink))))

       ;; mode line
       (mode-line ((t (:background ,clt-red-1 :foreground ,clt-grey-3))))
       (mode-line-buffer-id ((t (:foreground ,clt-cyan-1 :bold t :weight bold))))
       (mode-line-emphasis ((t (:foreground ,clt-green-1 :bold t :weight bold))))
       (mode-line-inactive ((t (:background ,clt-grey+2 :foreground ,clt-grey-3 :box (:line-width -1 :color ,clt-grey+1 :style nil) :weight light))))
       (which-func ((t (:foreground ,clt-green-1))))

       ;; compilation
       (compilation-column-number ((t (:foreground ,clt-pink-2))))
       (compilation-error ((t (:foreground ,clt-yellow))))
       (compilation-info ((t (:foreground ,clt-magenta))))
       (compilation-line-number ((t (:foreground ,clt-green))))
       (compilation-warning ((t (:bold t :foreground ,clt-orange :weight bold))))
       (match ((t (:background ,clt-red+1 :foreground ,clt-fg))))
       (next-error ((t (:background ,clt-blue))))

       ;; minibuffer
       (minibuffer-prompt ((t (:foreground ,clt-cyan))))

       ;; isearch
       (isearch ((t (:background ,clt-magenta-4 :foreground ,clt-brown))))
       (isearch-fail ((t (:background ,clt-red+2))))

       ;; edit
       (show-paren-match ((t (:background ,clt-blue+3))))
       (show-paren-mismatch ((t (:background ,clt-magenta+4))))
       (trailing-whitespace ((t (:background ,clt-red-1))))

       ;; Auto Highligh Symbol
       (ahs-definition-face ((t (:background "CadetBlue" :foreground "moccasin" :underline t))))
       (ahs-edit-mode-face ((t (:background "Coral3" :foreground "White"))))
       (ahs-face ((t (:background "LightYellow4" :foreground "GhostWhite"))))
       (ahs-plugin-bod-face ((t (:background "DodgerBlue" :foreground "Black"))))
       (ahs-plugin-defalt-face ((t (:background "Orange1" :foreground "Black"))))
       (ahs-plugin-whole-buffer-face ((t (:background "GreenYellow" :foreground "Black"))))
       (ahs-warning-face ((t (:bold t :foreground "Red" :weight bold))))

       ;; ECB
       (ecb-analyse-face ((t (:background ,clt-grey+3))))
       (ecb-default-highlight-face ((t (:background ,clt-orange+2))))
       (ecb-directory-face ((t (:inherit ecb-default-highlight-face))))
       (ecb-history-face ((t (:inherit ecb-default-highlight-face))))
       (ecb-method-face ((t (:inherit ecb-default-highlight-face))))
       (ecb-method-non-semantic-face ((t (:inherit font-lock-function-name-face))))
       (ecb-methods-general-face ((t (:inherit font-lock-function-name-face))))
       (ecb-source-face ((t (:inherit ecb-default-highlight-face))))
       (ecb-source-read-only-face ((t (:background ,clt-grey+3 :foreground ,clt-pink+2 :height 1.0))))
       (ecb-tag-header-face ((t (:background ,clt-cyan+1))))
       (ecb-history-bucket-node-dir-soure-path-face ((t (:inherit dired-header))))

       ;; hide-ifdef
       (hide-ifdef-shadow ((t (:background ,clt-grey+4 :foreground ,clt-grey-4 :box nil))))

       )
    )
  )
(add-to-list 'color-themes '(color-theme-clt
                              "Clt-contrast"
                              "Claude TETE <claude.tete@gmail.com"))

(provide 'color-theme-clt)

;;; color-theme-clt.el ends here.

;; need to maintain but lazy

;font
;(progn (set-face-font 'default "-raster-Courier-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1"))

;color
(if (window-system)
  (progn
    (set-face-background 'default "white")
    (set-face-foreground 'default "black")))

;;        coloration en gris au lieu de souligner                             ;;
(custom-set-faces
  '(underline ((((supports :underline t)) (:background "grey80")))))

;;        style et couleur des de la ligne courante (gris fonce)              ;;
  (set-face-background 'hl-line "gray80")

; style et couleur pour 'grep matches'                                ;;
  (set-face-background 'match "white")
  (set-face-foreground 'match "tomato")

;;        style et couleur des strings (gras et vert)                         ;;
;          (copy-face 'bold 'font-lock-string-face)
          (set-face-foreground 'font-lock-string-face "OliveDrab")

;;        style et couleur des keyword (gras et cyan)                         ;;
          (copy-face 'bold 'font-lock-keyword-face)
          (set-face-foreground 'font-lock-keyword-face "LightSeaGreen")

;;        style et couleur des function (vert pale)                           ;;
;          (copy-face 'bold 'font-lock-function-name-face)
          (set-face-foreground 'font-lock-function-name-face "DarkGreen")

;;        style et couleur des function (jaune clair)                         ;;
;          (copy-face 'bold 'font-lock-variable-name-face)
          (set-face-foreground 'font-lock-variable-name-face "DarkGoldenrod")

;;        couleur des liens 'grep nom et chemin de fichiers'                  ;;
          (custom-set-faces
            '(link
               ((((class color)
                   (min-colors 88)
                   (background light)) (:foreground "OliveDrab"))))
;;          style et couleur pour 'grep hits'                                 ;;
            '(compilation-info
               ((((class color)
                   (background light))
                  (:background "white"
                    :foreground "magenta"))))
;;                                                                            ;;
;;          style et couleur pour 'grep error messages'                       ;;
            '(compilation-error
               ((((class color)
                   (background light))
                  (:background "white"
                    :foreground "yellow"))))
;;                                                                            ;;
;;          style et couleur pour 'grep numero de ligne'                      ;;
            '(compilation-line-number
               ((((class color)
                   (background light))
                  (:background "white"
                    :foreground "OliveDrab"))))
            )

;;          style                                                             ;;
          (custom-set-faces
;;                                                                            ;;
;;          style de ??                                                       ;;
            '(ecb-analyse-face
               ((((class color)
                   (background light))
                  (:inherit ecb-default-highlight-face))))
;;                                                                            ;;
;;          style de highlight                                                ;;
            '(ecb-default-highlight-face
               ((((class color)
                   (background light))
                  (:background "firebrick3"))))
;;                                                                            ;;
;;          style de directory                                                ;;
            '(ecb-directory-face
               ((((class color)
                   (background light))
                  (:inherit ecb-default-highlight-face))))
;;                                                                            ;;
;;          style de history                                                  ;;
            '(ecb-history-face
               ((((class color)
                   (background light))
                  (:inherit ecb-default-highlight-face))))
;;                                                                            ;;
;;          style de methode                                                  ;;
            '(ecb-method-face
               ((((class color)
                   (background light))
                  (:inherit ecb-default-highlight-face))))
;;                                                                            ;;
;;          style de source                                                   ;;
            '(ecb-source-face
               ((((class color)
                   (background light))
                  (:inherit ecb-default-highlight-face))))
;;                                                                            ;;
;;          style des fichiers en lecture seul                                ;;
            '(ecb-source-read-only-face
               ((((class color)
                   (background light))
                  (:inherit ecb-default-general-face
                    :background "grey85"
                    :foreground "MistyRose3"))))
;;                                                                            ;;
;;          style des tags header                                             ;;
            '(ecb-tag-header-face
               ((((class color)
                   (background light))
                  (:background "SeaGreen"))))
                  )

;;      pour ceux qui ne veulent pas avoir les retours a la ligne             ;;
;;      1 ligne n'est visible que sur une ligne mais pas entierment visible   ;;
        (setq truncate-lines t)

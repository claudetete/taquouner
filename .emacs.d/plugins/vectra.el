;;##########################################################
;; definition des fonctions personnels.
;;

;; Entourer un mot par des guillemets
(fset 'vectra-comment-region
      'comment-region)

(fset 'vectra-uncomment-region
      "\C-u\M-xcomment-region")

(defun vectra-indent-hilit-buffer ()
  "Indente et colorise le buffer courant"
  (interactive)
  (indent-region (point-min) (point-max) 'nil)
  (untabify  (point-min) (point-max))
  (font-lock-fontify-buffer)
  )

(defun vectra-hilit-buffer ()
  "Colorise le buffer courant"
  (interactive)
  (font-lock-fontify-buffer))


(defun vectra-color-test ()
  "Fixe comme couleur de fonte principale celle dont le nom est pointé par le curseur"
  (interactive)
  (set-foreground-color (current-word)))

(defun vectra-man-on-word ()
  "Appelle le man sur le mot pointé par le curseur"
  (interactive)
  (manual-entry (current-word)))

(defun vectra-apropos-on-word ()
  "Appelle le apropos sur le mot pointé par le curseur"
  (interactive)
  (apropos (current-word)))

(defun suspend-emacs-old ()
  (interactive)
  (message "Pour stopper emacs, taper C-x C-z"))

(provide 'vectra)

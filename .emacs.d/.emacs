(defvar tqnr-dotemacs-path "c:/path/to/.emacs.d") ; should be your HOME path
(defvar tqnr-profile-name "my-profile")

; !! have to be the last one
(load-file (concat (file-name-as-directory tqnr-dotemacs-path) "init.el"))

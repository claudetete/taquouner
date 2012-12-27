(defvar dotemacs-path "c:/path/to/.emacs.d") ; should be your HOME path
(defvar profile-name "my-profile")

; !! have t obe the last one
(load-file (concat (file-name-as-directory dotemacs-path) "emacs.el"))

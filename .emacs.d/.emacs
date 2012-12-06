(defvar dotemacs-path "c:/path/to/.emacs.d") ; !! be careful do not add a '/' at the end !!
(defvar profile-name "my-profile") ; !! have to be put before (load-file (concat dotemacs-path "/emacs.el")) !!
(load-file (concat dotemacs-path "/emacs.el"))

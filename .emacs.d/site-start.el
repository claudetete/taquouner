;; this file must be put in emacs-24.2/site-lisp/ directory to give a relative
;; HOME path

;; C-h v data-directory ENTER :
;; h:/emacs_portable/emacs-24.2/etc/
;; so to get h:/emacs_portable/ we must remove 15 characters
(defvar program-dir (substring data-directory 0 -15))

;; set the HOME environment variable
(setenv "HOME" program-dir)

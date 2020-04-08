;;; init.el --- init config file for GNU Emacs

;; Copyright (c) 2017 Claude Tete
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
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

;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 0.1
;; Created: July 2017
;; Last-Updated: July 2017

;;; Commentary:
;;
;; load default profile, then all file started by number in dotemacs directory
;;
;; function `try-require' to not crash when require is lost
;;

;;; Change Log:
;; 2017-07-04 (0.1)
;;    create from old emacs.el


;;; Code:
;;
;;;  INIT
(message "#######################################")
(message "# Taquouner, GNU Emacs configuration  #")
(message "#######################################\n")


;; increase the size of the log *Messages*
(custom-set-variables
  '(message-log-max 1000))


;;
;; TRY-REQUIRE DEFINITION
;;; attempt to load a feature/library, failing silently (by Fabrice Niessen)
;; patched by Claude Tete to add string before message (used to have indentation)
(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

(defun try-require (feature &optional indent)
  "Attempt to load a library or module (FEATURE).  Return true if the library \
given as argument is successfully loaded.  If not, instead of an error, just \
add the package to a list of missing packages. INDENT contains string to add \
before message."
  (let ()
    (if (eq indent nil) (setq indent ""))
  (condition-case err
      ;; protected form
      (progn
        (message "%sChecking for library `%s'..." indent feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "%sChecking for library `%s'... Found" indent feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "%sChecking for library `%s'... Missing" indent feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil))))


;; add to load current path
(add-to-list 'load-path tqnr-dotemacs-path)
;; add to load path the dotemacs directory
(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "dotemacs"))


(message "Load Profile...")
;;
;;; LOAD PROFILE
;; add to load path the profile directory
(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "profile"))
(try-require 'init-profile "  ")

;; load profile given in tqnr-profile-name set by ~/.emacs
(let ((profile-file-path (concat (file-name-as-directory tqnr-dotemacs-path) "profile/profile-" tqnr-profile-name ".el"))
       (profile-file (intern (concat "profile-" tqnr-profile-name))))
  ;; load only if the file exists
  (when (not (and (file-exists-p profile-file-path) (try-require profile-file "  ")))
    (display-warning 'tqnr-profile-name (concat tqnr-profile-name " profile do not exist") :warning)
    (display-warning 'tqnr-profile-name (concat "file not found: " profile-file) :warning)
    (error "Missing profile")))

;; show name of profile
(message (concat "* Profile is \"" tqnr-profile-name "\""))
(message "Load Profile...Done\n")


(message "Load Configuration...")
;;
;; LOAD ALL CONFIG FILE
;; load function useful for init
(when (not (try-require 'init-function "  "))
  (error "cannot found init-function.el"))

(message " ")

;; load each files in dotemacs directory
(require-files (concat (file-name-as-directory tqnr-dotemacs-path) "dotemacs/"))


;;
;; HOOK AFTER LOADING CONF (not used anymore ?)
(function-to-call-after-loading-conf)


;;
;; WARNINGS
;; show warning buffer if exist (means there was error)
(when (get-buffer "*Warnings*")
  (switch-to-buffer "*Warnings*"))

;;; init.el ends here

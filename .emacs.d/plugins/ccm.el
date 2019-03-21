;;; ccm-el               --- Continuus integration.
;;;
;;;
;;; Copyright (C) 2000 Henrik Joensson.
;;;
;;; Author:   Henrik Joensson <henrik7205@hotmail.com>
;;; Site:     http://www.ccm.f2s.com/emacs/
;;; Version:  0.6
;;; Keywords: Continuus, ccm
;;;
;;; Version history
;;; Ver  Sign      Date        Comment
;;; 0.1  henrik    2000-01-26  Created.
;;; 0.2  henrik    2000-02-09  Show version in modeline
;;;                            Show status in modeline
;;;                            Sets the cursor last in the log buffer
;;;                            Show history for a file
;;;                            Set CCM_ADDR inside XEmacs
;;;                            Read only flag correctly set
;;; 0.3  n. koch   2000-03-14  Default task
;;;                            Some lisp enhancements
;;;      henrik                Some task additions
;;; 0.4  henrik    2000-10-03  Create task
;;;                            Customize the size of the work buffer
;;;                            Change menu between top and tools
;;; 0.5  henrik    2001-04-12  Change settings with the Customize menu
;;;                            Better menu support on GNU Emacs
;;;                            Toolbar icons for check in and check out
;;;                            Show properties for a object
;;; 0.6  ot        2001-07-01  GNU Emacs compatibility
;;;                            GNU Emacs menus rearranged
;;;                            ccm-update-modeline now with 'ccm ls' call
;;;                            Undo checkout command
;;;      henrik    2001-07-02  Added the create command
;;;                            Some lisp cleanups
;;; 0.7  c. tete   2011-04-11  Added the set default task command,
;;;                            the query of assigned task
;;;
;;; Installation:
;;;  (load "ccm")
;;;  or
;;;  (require 'ccm)
;;;  (ccm-mode) ;; if not launch
;;;
;;; This file is not part of XEmacs.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'ccm)

(defvar ccm-running-gnuemacs
  (string-match "GNU Emacs" (emacs-version))
  "If the current Emacs is GNU Emacs, then, this is non-nil")
(defvar ccm-running-xemacs
  (string-match "XEmacs\\|Lucid" (emacs-version))
  "If the current Emacs is XEmacs/Lucid, then, this is non-nil")

(add-hook 'find-file-hooks 'ccm-add-menubar-entry)

;;; emacs compatibility issues
(if (not (fboundp `exec-to-string))
    (defalias `exec-to-string `shell-command-to-string))

(if (not (fboundp `redraw-modeline))
    (defalias `redraw-modeline `force-mode-line-update))

(defun ccm-add-menubar-entry ()
  (interactive)
  (save-window-excursion (ccm-menu)))


  (defgroup continuus nil
    "Continuus CM interface"
    :group 'programming
    :group 'tools)

  (defcustom ccm-buffer-name "*ccm*"
    "Buffer in which to log all CCM actions."
    :type 'function
    :group 'continuus)

  (defcustom ccm-exe-name "ccm"
    "The Continuus executable name"
    :type 'function
    :group 'continuus)

  (defcustom ccm-buffer-size 10
    "The size of the work buffer"
    :type 'function
    :group 'continuus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XEmacs
;;Customisation interface
(cond
 (ccm-running-xemacs
  (require 'easymenu)

  (defcustom ccm-toolbar t
    "Display Continuus icons in the toolbar"
    :type 'bool
    :group 'continuus)

;; (defcustom ccm-icon-checkout "~/emacs/icons/checkout.xpm"
;;   "Image for the checkout icon"
;;   :type 'function
;;   :group 'continuus)

;; (defcustom ccm-icon-checkin "~/emacs/icons/checkin.xpm"
;;   "Image for the checkout icon"
;;   :type 'function
;;   :group 'continuus)

 ;; Toolbar
 (setq ccm-toolbar-list
       '((ccm-toolbar-co "~/emacs/icons/checkout.xpm")
   (ccm-toolbar-ci "~/emacs/icons/checkin.xpm")))

 (mapcar
  (lambda (cons)
    (setf (symbol-value (car cons))
    (toolbar-make-button-list (cdr cons))))
  ccm-toolbar-list)

 (set-specifier default-toolbar
    '([toolbar-file-icon toolbar-open t "Open a file"]
      [toolbar-disk-icon toolbar-save t "Save buffer"]
      [toolbar-printer-icon toolbar-print t "Print buffer"]
      [:style 3D]
      [toolbar-cut-icon toolbar-cut t "Kill region"]
      [toolbar-copy-icon toolbar-copy t "Copy region"]
      [toolbar-paste-icon toolbar-paste t "Paste from clipboard"]
      [toolbar-undo-icon toolbar-undo t "Undo edit"]
      [:style 3D]
      [toolbar-replace-icon toolbar-replace t "Replace text"]
      [:style 3D]
      [ccm-toolbar-co ccm-co t "Check Out File"]
      [ccm-toolbar-ci ccm-ci t "Check In File"]
      [:style 3D]
      [toolbar-compile-icon toolbar-compile t "Compile"]
      [toolbar-debug-icon toolbar-debug t "Debug"]))
 ))

(defvar ccm-modeline-string nil
  "Display Continuus information in the modeline.")

(defvar ccm-menu-at-top t
  "Display the menu bar in the top menu bar")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
(defconst ccm-int-version-string "0.7" "Version String")
(defconst ccm-int-date-string "2011-04-11" "Date String")
(defconst ccm-int-author-string "Claude Tete" "Author String")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
(defun ccm-co ()
  "Check out a file"
  (interactive)
  (let ((filename (buffer-name)))
    (ccm-show-buffer)
    (cond(ccm-running-xemacs
           (insert-string "\nStarting check out...\n" ccm-buffer-name)))

    (setq comint-output-filter-functions
      (push 'shell-strip-ctrl-m
        comint-output-filter-functions))
    (start-process-shell-command "ccm" ccm-buffer-name
      ccm-exe-name "co" filename)
;    (set-process-sentinel (get-process "ccm") 'ccm-sentinel-co)
;    (set-process-filter (get-process "ccm") 'ccm-process-filter))
    ))

(defun ccm-undo-co ()
  "Undo file checkout"
  (interactive)
  (let ((filename (buffer-name)))
    (ccm-show-buffer)
    (cond(ccm-running-xemacs
      (insert-string "\nUndoing check out...\n" ccm-buffer-name)))

    (setq comint-output-filter-functions
          (push 'shell-strip-ctrl-m
                comint-output-filter-functions))
    (start-process-shell-command "ccm" ccm-buffer-name
                                 ccm-exe-name "unuse -replace -delete" filename)
    (set-process-sentinel (get-process "ccm") 'ccm-sentinel-ci)))



(defun ccm-ci ()
  "Check in a file"
  (interactive)
  (let ((filename (buffer-name)))
    (ccm-show-buffer)
    (cond(ccm-running-xemacs
    (insert-string "\nStarting check in...\n" ccm-buffer-name)))

    (setq comint-output-filter-functions
          (push 'shell-strip-ctrl-m
                comint-output-filter-functions))
    (start-process-shell-command "ccm" ccm-buffer-name
                                 ccm-exe-name "ci -nc" filename)
    (set-process-sentinel (get-process "ccm") 'ccm-sentinel-ci)))


(defun ccm-history ()
  "Display file history"
  (interactive)
  (let* ((filename (buffer-name))
   (command (format "history %s" filename)))
    (ccm-show-buffer)
    (ccm-run-command "\nStarting history...\n" command)))


(defun ccm-default-task ()
  "Display default task in the work buffer"
  (interactive)
  (ccm-show-buffer)
  (ccm-run-command "\nCurrent Default Task\n" "task -default"))

(defun ccm-properties ()
  "Display the properties of the file in the work buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
   (prop-command (format "prop %s" filename)))
    (ccm-show-buffer)
    (ccm-run-command "\nProperties...\n" prop-command)))

(defun ccm-create-task ()
  "Create a new task"
  (interactive)
  (ccm-show-buffer)
  (let ((proc (start-process-shell-command "ccm" ccm-buffer-name
                                           ccm-exe-name "create_task" "-g")))
    ))

(defun ccm-query-assigned ()
  "Query assigned and owned task."
  (interactive)
  (ccm-show-buffer)
  (let ((proc (start-process-shell-command "ccm" ccm-buffer-name
                                           ccm-exe-name "task" "-query -task_scope all_my_assigned")))
    ))

(defun ccm-set-default-task ()
  "Set default task number."
  (interactive)
  (let* ((number (read-string "Number of the desired default task? " ""))
          (task-default-number (format "task -default %s" number)))
    (ccm-show-buffer)
    (ccm-run-command "\nSetting default task...\n" task-default-number)
    ))

(defun ccm-delete-and-replace ()
  "Delete a file"
  (interactive)
  (let ((filename (buffer-name))
         (delete-replace (format "delete -replace %s" filename)))
    (ccm-show-buffer)
    (start-process-shell-command "ccm" ccm-buffer-name
            ccm-exe-name "ccm" delete-replace)
      ))

(defun ccm-set-ccmAddr ()
  "Read ccm-add from the user"
  (interactive)
  (let ((ccm-addr (read-string "Enter CCM_ADDR: " "")))
    (setenv "CCM_ADDR" ccm-addr)))

(defun ccm-create ()
  "Creates the current file in Continuus, asks the user for a type and version"
  (interactive)
  (let* ((type (read-string "Type of the file: " ""))
          (version (read-string "Version of the file: " ""))
          (filename (buffer-name))
          (create-command (format "create -type %s %s" type filename))
          (attr-command (format "attr -modify version -value %s %s" version filename)))
    (ccm-show-buffer)
    (ccm-run-command "\nCreating file...\n" create-command)
    (ccm-run-command "\n Setting version...\n" attr-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions
(defun ccm-run-command (info command)
  (cond(ccm-running-xemacs
	(insert-string info ccm-buffer-name)))

  (setq comint-output-filter-functions
	(push 'shell-strip-ctrl-m
	      comint-output-filter-functions))

  (start-process-shell-command "ccm" ccm-buffer-name
			       ccm-exe-name command)
  (set-process-sentinel (get-process "ccm") 'ccm-sentinel)
)

(defun ccm-show-buffer ()
  "Creates and displays the ccm output buffer"
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create ccm-buffer-name))
  (shrink-window ccm-buffer-size)
  (other-window -1))

(defun ccm-update-modeline ()
  "Show file name, version and status in the modeline"
  (interactive)
    (setq comint-output-filter-functions
          (push 'shell-strip-ctrl-m
                comint-output-filter-functions))
  (let* ((filename (buffer-name))
         (ccm-version (exec-to-string
                      (format "ccm ls -f \"%%version:%%status\" %s" filename))))
; strip \n from version string
  (setq ccm-version (substring ccm-version 0 (-(length ccm-version) 1)))

  (cond(ccm-running-xemacs
    (setq ccm-modeline-string
          (format "%s-%s" filename ccm-version)
          modeline-buffer-identification 'ccm-modeline-string))
    (ccm-running-gnuemacs
    (setq ccm-modeline-string
          (format "%s-%s" filename ccm-version)
          mode-line-buffer-identification 'ccm-modeline-string)))

 (redraw-modeline)))

(defun ccm-sentinel (process event)
  (ccm-update-modeline)
  (switch-to-buffer-other-window ccm-buffer-name)
  (goto-char (point-max))
  (dos2unix)
;;  (redraw-modeline)
)

(defun ccm-sentinel-co (process event)
  (ccm-sentinel process event)
  (other-window -1)
  (cond(ccm-running-xemacs
	(insert-string "Command completed!\n")))

  (message buffer-name)
;;  (if buffer-read-only
;;      (toggle-read-only))

  ; Ask the user if we should reload the file
  (find-file (buffer-name)))

(defun ccm-sentinel-ci (process event)
  (ccm-sentinel process event)
  (other-window -1)

  (cond(ccm-running-xemacs
	(insert-string "Command completed!\n")))

  (if (not buffer-read-only)
      (toggle-read-only)))

(defun ccm-process-filter (process output)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
        (let (moving)
          (set-buffer (process-buffer process))
          (setq moving (= (point) (process-mark process)))
          (save-excursion
            (goto-char (process-mark process))
            (insert output)
            (set-marker (process-mark process) (point)))
          (if moving (goto-char (process-mark process))))
      (set-buffer old-buffer))))

(defun dos2unix ()
  "Convert this entire buffer from MS-DOS text file format to UNIX."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\r$" nil t)
      (replace-match "" nil nil))
    (goto-char (1- (point-max)))
    (if (looking-at "\C-z")
        (delete-char 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Information
(defun ccm-integration-version ()
  "Display the version of Continuus Integration"
  (interactive)
  (message ccm-int-version-string))

(defun ccm-integration-date ()
  "Display the date of Continuus Integration"
  (interactive)
  (message ccm-int-date-string))

(defun ccm-integration-author ()
  "Display the author of Continuus Integration"
  (interactive)
  (message ccm-int-author-string))

(defun ccm-about ()
  "Display version, date, author"
  (interactive)
  (message (format "Continuus XEmacs Integration v%s, %s, %s"
		   ccm-int-version-string
		   ccm-int-date-string
		   ccm-int-author-string)))


(defun ccm-change-menu ()
  "Change the location of the ccm-mode menu"
  (interactive)
  (if (not ccm-menu-at-top)
      (ccm-menu-top)
    (ccm-menu-tools))
  )

(defun ccm-menu-top ()
  (delete-menu-item '("Tools" "Continuus"))
  (easy-menu-add ccm-mode-menu)
  (setq ccm-menu-at-top t)
  (message "Setting menu to top menu bar")
  )

(defun ccm-menu-tools ()
  (easy-menu-remove ccm-mode-menu)
  (add-menu-button '("Tools") ccm-mode-menu "Continuus")
  (setq ccm-menu-at-top nil)
  (message "Setting menu to Tools menu bar")
  )

;; Key Bindings
(defvar ccm-mode-map
  (let ((ccm-mode-map (make-sparse-keymap)))
    (define-key ccm-mode-map [(control c) (control m) o] 'ccm-co)
    (define-key ccm-mode-map [(control c) (control m) u] 'ccm-undo-co)
    (define-key ccm-mode-map [(control c) (control m) i] 'ccm-ci)
    (define-key ccm-mode-map [(control c) (control m) c] 'ccm-create)
    (define-key ccm-mode-map [(control c) (control m) h] 'ccm-history)
    (define-key ccm-mode-map [(control c) (control m) d] 'ccm-default-task)
    (define-key ccm-mode-map [(control c) (control m) t] 'ccm-create-task)
    (define-key ccm-mode-map [(control c) (control m) a] 'ccm-about)
    ccm-mode-map))

; Minor Mode
(defvar ccm-mode nil
  "Minor mode for editing Continuus controlled files")

(defun ccm-mode (&optional arg)
  "Minor mode for editing Continuus controlled files"
  (interactive)
  (setq ccm-mode
	(if (null arg) (not ccm-mode)
	  (> (prefix-numeric-value arg) 0)))
)


(defun ccm-menu ()
  (cond
   (ccm-running-xemacs
    ;; Menu Support for XEmacs
    (require 'easymenu)
    (easy-menu-define ccm-mode-menu
		      ccm-mode-map
		      "Menu used for Continuus Integration"
		      (list "Continuus"
			    ["Check Out File" ccm-co t]
			    ["Undo Check Out" ccm-undo-co t]
			    ["Check In File" ccm-ci t]
			    ["Create File" ccm-create t]
			    ["---" nil nil]
			    ["Show Default Task" ccm-default-task t]
			    ["Create New Task" ccm-create-task t]
			    ["---" nil nil]
			    ["Properties" ccm-properties t]
			    ["---" nil nil]
			    ["Update Modeline" ccm-update-modeline t]
			    ["Show History" ccm-history t]
			    ["Set CCM_ADDR" ccm-set-ccmAddr t]
			    ["---" nil nil]
			    ["Change Menu Bar Location" ccm-change-menu t]
			    ["About Continuus Mode" ccm-about t]))

    (easy-menu-add ccm-mode-menu "Continuus"))

   (ccm-running-gnuemacs
    ;; Menu support for GNU Emacs
    (or (lookup-key global-map [menu-bar])
	(define-key global-map [menu-bar] (make-sparse-keymap "menu-bar")))
    (defvar menu-bar-ccm-menu (make-sparse-keymap "Continuus"))
    (setq menu-bar-final-items (cons 'ccm-menu menu-bar-final-items))
    (define-key global-map [menu-bar ccm-menu]
      (cons "Continuus" menu-bar-ccm-menu))
    (define-key menu-bar-ccm-menu [ccm-about]
      '("About Continuus mode" . ccm-about))
    (define-key menu-bar-ccm-menu [separator-about] '("--"))
    (define-key menu-bar-ccm-menu [ccm-set-ccmAddr]
      '("Set CCM_ADDR" . ccm-set-ccmAddr))
    (define-key menu-bar-ccm-menu [ccm-history]
      '("Show History" . ccm-history))
    (define-key menu-bar-ccm-menu [ccm-update-modeline]
      '("Update Modeline" . ccm-update-modeline))
    (define-key menu-bar-ccm-menu [separator-history] '("--"))
    (define-key menu-bar-ccm-menu [ccm-properties]
      '("Properties" . ccm-properties))
    (define-key menu-bar-ccm-menu [separator-properties] '("--"))
    (define-key menu-bar-ccm-menu [ccm-create-task]
      '("Create New Task" . ccm-create-task))
    (define-key menu-bar-ccm-menu [ccm-default-task]
      '("Show Default Task" . ccm-default-task))
    (define-key menu-bar-ccm-menu [separator-task] '("--"))
    (define-key menu-bar-ccm-menu [ccm-ci]
      '("Check In File" . ccm-ci))
    (define-key menu-bar-ccm-menu [ccm-undo-co]
      '("Undo Check Out" . ccm-undo-co))
    (define-key menu-bar-ccm-menu [ccm-co]
      '("Check Out File" . ccm-co))
    (define-key menu-bar-ccm-menu [ccm-create]
      '("Create File" . ccm-create))

    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (assoc 'ccm-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(ccm-mode " Ccm")
		minor-mode-alist)))

(or (not (boundp 'minor-mode-map-alist))
    (assq 'ccm-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'ccm-mode ccm-mode-map)
		minor-mode-map-alist)))

; invoke the mode
(ccm-mode)

; ccm.el ends here


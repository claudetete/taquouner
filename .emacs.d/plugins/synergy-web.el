;;; synergy-web.el --- a mode to integrate synergy web mode with new java client
;;;                    CLI (with history and diff mode)

;; Copyright (c) 2013-2014 Claude Tete
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

;; Keywords: synergy, version control,
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 0.7
;; Created: August 2013
;; Last-Updated: August 2014

;;; Commentary:
;;
;; Use and test with Synergy 7.1
;;
;; TODO diff with ediff
;; TODO tree for history (like in graphical) (or run java interface ?)
;; TODO reconcile/sync conflicts mode with color
;; TODO display list of orphan file (mode with color)
;; TODO display list of associated file with a task (mode with color)
;; TODO common mode for list of file/objects
;; TODO add uncontrolled file in synergy


;;; Change Log:
;; 2014-08-11 (0.7)
;;    change creation of task with nconc to build list of parameters
;; 2014-03-26 (0.6)
;;    add new service to override shell-command-to-string and use a variable to
;;    test when synergy is connected
;; 2013-10-15 (0.5)
;;    add dis/association of objects with task
;; 2013-10-01 (0.4)
;;    add history mode + diff with external tool
;; 2013-09-10 (0.3)
;;    transform to a standalone mode
;; 2013-08-19 (0.2)
;;    add synchronize/reconcile, show conflict and create task
;; 2013-08-13 (0.1)
;;    from function-clearcase.el to checkout


;;; Code:

(require 'diff-mode)

;;
;;;
;;;; CUSTOM
(defgroup synergy-web nil
  "Synergy integration to manage connection, task, checkout, history and diff."
  :group 'tools
  :prefix "synergy-web")

(defcustom synergy-username nil
  "Username to connect."
  :type 'string
  :group 'synergy-web)

(defcustom synergy-database nil
  "Database to connect."
  :type 'string
  :group 'synergy-web)

(defcustom synergy-server nil
  "Server to connect."
  :type 'string
  :group 'synergy-web)

(defcustom synergy-ccm-command "ccm"
  "Synergy executable for CLI Web."
  :type 'string
  :group 'synergy-web)

(defcustom synergy-command-buffer "*synergy*"
  "Name of buffer to show output of command."
  :type 'string
  :group 'synergy-web)

(defcustom synergy-checkout-set-current-task nil
  "Set checkout used task to current task."
  :type 'boolean
  :group 'synergy-web)

(defcustom synergy-ignore-uncontrolled-file "GTAGS;GRTAGS;GPATH;*.el;*.files;.svn;.git"
  "List of file to ignore to reconcile."
  :type 'string
  :group 'synergy-web)

(defcustom synergy-mark-character ?*
  "Character at beginning of line when a line is marked."
  :type 'character
  :group 'synergy-web)

(defcustom synergy-diff-buffer "*synergy-diff*"
  "Name of buffer to show diff mode."
  :type 'string
  :group 'synergy-web)

;;; History mode
(defcustom synergy-history-buffer "*synergy-history*"
  "Name of buffer to show history mode."
  :type 'string
  :group 'synergy-web)

(defcustom synergy-history-filter nil
  "End of command line of history to filter/replace results."
  :type 'string
  :group 'synergy-web)

;;; Diff External
(defcustom synergy-diff-external-command nil
  "Command to run an external tool to run a diff."
  :type 'string
  :group 'synergy-web)

(defcustom synergy-diff-external-parameter-list nil
  "Parameter of external tool to run a diff."
  :type 'string
  :group 'synergy-web)

(defcustom synergy-diff-external-swap-file nil
  "Swap position of file right/left."
  :type 'boolean
  :group 'synergy-web)

;;
;;;
;;;; VARIABLES
(defvar synergy-is-started nil
  "Synergy is not connected.")
(defvar synergy-task-minibuffer-history nil
  "History of task in minibuffer.")

(defvar synergy-release-minibuffer-history nil
  "History of release in minibuffer.")

(defvar synergy-project-minibuffer-history nil
  "History of project in minibuffer.")

(defvar synergy-file-minibuffer-history nil
  "History of file in minibuffer.")

(defvar synergy-history-parent-buffer nil
  "Previous buffer before history.")

(defvar synergy-show-conflict-parent-buffer nil
  "Previous buffer before a show conflict.")

(defvar synergy-diff-parent-buffer nil
  "Previous buffer before history.")

(defvar synergy-history-file nil
  "History if from this file path.")

(defvar synergy-history-marked-list nil
  "List of version marked.")


;;
;;;
;;;; FUNCTION (interactive)
;;; Start Synergy
;;;###autoload
(defun synergy-start ()
  "Start a session on synergy-server and synergy-database with synergy-username."
  (interactive)
  (let ((my-password) (ret))
    ;; ask password
    (setq my-password (read-passwd (concat "Password for " synergy-username ": ")))
    ;; start ccm
    (setq ret (shell-command-to-string
                (combine-and-quote-strings
                  (list synergy-ccm-command "start" "-q"
                    "-d"  synergy-database
                    "-s"  synergy-server
                    "-n"  synergy-username
                    "-pw" my-password))))
    (message ret)))

;;; Checkout file from a synergy project
;;;###autoload
(defun synergy-checkout ()
  "Checkout the current buffer file."
  (interactive)
  (synergy-checkout-with-option))

;;;###autoload
(defun synergy-checkout-with-current-task ()
  "Checkout the current buffer file with current task."
  (interactive)
  (synergy-checkout-with-option t))

;;; Unckeckout/delete file from a synergy project
;;;###autoload
(defun synergy-uncheckout ()
  "Uncheckout the current buffer file."
  (interactive)
  ;; get buffer name
  (let ((my-filename (synergy-get-buffer-file-name))
         (my-flag))
    (when my-filename
      ;; ask about really unckeckout file ?
      (setq my-flag (y-or-n-p (concat "Are you sure to delete working file ? (" my-filename ") ? ")))
      (when my-flag
        ;; delete the checkout file
        (synergy-run-async-command (combine-and-quote-strings (list synergy-ccm-command "delete" "-recurse" my-filename)))
        (message "%s %s %s" "Synergy:" my-filename "is unckeckout.")))))
(defalias 'synergy-delete 'synergy-unckeckout)

;;; Associate with a task
;;;###autoload
(defun synergy-associate-with-task ()
  "Associate current buffer with a task."
  (interactive)
  ;; get buffer name
  (let ((my-filename (synergy-get-buffer-file-name))
         ;; get current task
         (my-task (synergy-get-current-task))
         ;; get all assigned task
         (my-tasks (synergy-get-assigned-task)))
    (when my-filename
      (if my-task
        ;; ask task with default when current task is set
        (setq my-task (synergy-get-task-number
                        (completing-read (concat "Associate with task (default: " my-task "): ")
                          my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task)))
        ;; ask task without default
        (setq my-task (synergy-get-task-number
                        (completing-read "Associate with task: "
                          my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task))))
      ;; associate buffer file with task
      (synergy-associate-object-with-task my-filename my-task))))

;;; Disassociate with a task
;;;###autoload
(defun synergy-disassociate-from-task ()
  "Disassociate current buffer from an associated task."
  (interactive)
  ;; get buffer name
  (let ((my-filename (synergy-get-buffer-file-name))
         (my-tasks) (my-task))
    (when my-filename
      ;; get all associated task
      (setq my-tasks (synergy-get-associated-task my-filename))
      ;; ask task without default
      (setq my-task (synergy-get-task-number
                      (completing-read "Disassociate from task: "
                        my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task)))
      ;; disassociate buffer file with task
      (synergy-disassociate-file-from-task my-filename my-task))
      ))

;;; Associate orphan object with a task
;;;###autoload
(defun synergy-associate-orphan-object ()
  "Associate an orphan file with an assigned task."
  (interactive)
  (let ((my-tasks (synergy-get-assigned-task))
         (my-files (synergy-get-orphan-object))
         (my-task (synergy-get-current-task))
         (my-file))
    (when (and my-tasks my-files)
      (setq my-file (completing-read "Orphan file: "
                      my-files nil t nil synergy-file-minibuffer-history my-file))
      (if my-task
        (setq my-task (completing-read (concat "Associate with task (default: " my-task "): ")
                        my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task))
        (setq my-task (completing-read "Associate with task: "
                        my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task)))
      (setq my-task (synergy-get-task-number my-task))
      (when my-task
        (synergy-associate-object-with-task my-file my-task)))))

;;; Reconcile/Sync Work Area
;;;###autoload
(defun synergy-sync-with-work-area ()
  "Synchronize server file with local file."
  (interactive)
  ;; get file name
  (let ((my-filename (synergy-get-buffer-file-name)))
    (when my-filename
      ;; synchronize server file with local file
      (synergy-synchronize-server-file my-filename))))
;;; alias with synonym for synchronize (from classic interface)
(defalias 'synergy-reconcile-to-server 'synergy-sync-with-work-area)

;;; Reconcile/Sync DataBase
;;;###autoload
(defun synergy-sync-with-database ()
  "Synchronize local file with sever file."
  (interactive)
  ;; get file name
  (let ((my-filename (synergy-get-buffer-file-name)))
    (when my-filename
      ;; synchronize local file with server file
      (synergy-synchronize-local-file my-filename))))
;;; alias with synonym for synchronize (from classic interface)
(defalias 'synergy-reconcile-to-local 'synergy-sync-with-database)

;;; Show Conflicts during a reconcile
;;;###autoload
(defun synergy-show-conflict ()
  "Show conflicts from synchronizing."
  (interactive)
  ;; get file name, show conflict buffer name
  (let ((ret) (my-filename (synergy-get-buffer-file-name)) (ignore-flag)
         (my-sc-buffer-name synergy-command-buffer))
    (if synergy-ignore-uncontrolled-file
      ;; set ignore file list only when the list is not empty
      (setq ignore-flag (combine-and-quote-strings (list "-ignore_types" synergy-ignore-uncontrolled-file)))
      (setq ignore-flag ""))
    (when my-filename
      ;; get show conflict output
      (setq ret (synergy-shell-command-to-string
                  (combine-and-quote-strings
                    (list synergy-ccm-command "reconcile"
                      "-show"
                      "-recurse"
                      "-consider_uncontrolled"
                      ignore-flag
                      my-filename))))
      (when ret
        ;; keep current buffer to restore cursor
        (setq synergy-show-conflict-parent-buffer (current-buffer))
        ;; remove old buffer
        (kill-buffer (get-buffer-create my-sc-buffer-name))
        ;; create a new buffer and go in
        (pop-to-buffer (get-buffer-create my-sc-buffer-name))
        ;; set auto-save mode in default mode
        (auto-save-mode auto-save-default)
        ;; empty buffer
        (erase-buffer)
        ;; insert output of show conflict
        (insert ret)
        ;; go to first character of buffer
        (goto-char (point-min))
        ;; go at end of line "Path        Conflict Message"
        (re-search-forward "^Path\\s-*Conflict Message" nil 'end)
        ;; go to next character (so next line)
        (forward-char)
        ;; go to start of current line
        (beginning-of-line)
        ))))

;;; Create task
;;;###autoload
(defun synergy-create-task ()
  "Create a new task in synergy."
  (interactive)
  ;; get all active releases
  (let ((my-releases (synergy-get-all-active-releases))
         ;; get release of current file or directory
         (my-release (synergy-get-current-release))
         (my-synopsis)
         (my-synopsis-command)
         (my-task)
         (default-task-flag))
    ;; only when a list of releases is returned
    (when my-releases
      (if my-release
        ;; use current release as default value and ask for confirmation or to choose other release
        (setq my-release (completing-read (concat "Create task with release (default: " my-release "): ")
                           my-releases nil 'confirm nil synergy-release-minibuffer-history my-release))
        ;; no default value and ask to choose a release
        (setq my-release (completing-read "Create task with release: "
                           my-releases nil 'confirm nil synergy-release-minibuffer-history my-release)))
      ;; ask for a synopsis
      (setq my-synopsis (read-string "Synopsis: "))
      ;; ask for a CR number
      (setq my-change-request (completing-read "CR: " nil))
      ;; init list of my command + parameters
      (setq my-command-list (list synergy-ccm-command "task" "-create"))
      (when (not (string-equal my-synopsis ""))
        ;; when synopsis exists add to list of my command + parameters
        (setq my-command-list (nconc my-command-list (list "-synopsis" my-synopsis))))
      (when (not (string-equal my-change-request ""))
        ;; when change request exists add to list of my command + parameters
        (setq my-command-list (nconc my-command-list (list "-change_request" my-change-request))))
      ;; add to list of my command + parameters, release and resolver
      (setq my-command-list (nconc my-command-list (list "-release" my-release "-resolver" synergy-username)))
      ;; run command + parameters
      (setq my-task (shell-command-to-string (combine-and-quote-strings my-command-list)))
      ;; create task with all asked parameter
      (setq my-task (replace-regexp-in-string " created.\n$" (concat ": " my-synopsis) my-task))
      ;; ask to set by default
      (setq default-task-flag (y-or-n-p (concat "Set as default task (" my-task ") ? ")))
      ;; set default when answer is yes
      (if default-task-flag
        (progn
          ;; set as current task
          (synergy-set-task-as-current my-task)
          ;; display task created and set as current task
          (message (concat my-task " created and set as default.")))
        ;; display task created
        (message (concat my-task " created."))))))

;;; Set current task
;;;###autoload
(defun synergy-set-current-task ()
  "Set current task."
  (interactive)
  ;; get current task
  (let ((my-task (synergy-get-current-task))
         ;; get all assigned task
         (my-tasks (synergy-get-assigned-task)))
    (when synergy-is-started
      (if my-task
        ;; ask task with default when current task is set
        (setq my-task (completing-read (concat "Set current task (default: " my-task "): ")
                        my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task))
        ;; ask task without default
        (setq my-task (completing-read "Set current task: "
                        my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task)))
      ;; set current task
      (synergy-set-task-as-current my-task))))

;;; Change release of a task
;;;###autoload
(defun synergy-set-task-release ()
  "Modify the release of a task."
  (interactive)
  ;; get current task
  (let ((my-task (synergy-get-current-task))
         ;; get all assigned task
         (my-tasks (synergy-get-assigned-task))
         ;; get all active releases
         (my-releases (synergy-get-all-active-releases))
         (my-release))
    ;; only when a list of releases is returned
    (when my-releases
      ;; when there is a current task
      (if my-task
        (progn
          ;; get release of current task
          (setq my-release (synergy-get-task-release my-task))
          ;; ask for task to change release (default is current task)
          (setq my-task (completing-read (concat "Task (default: " my-task "): ")
                          my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task))
          ;; ask for release (default is release of task)
          (setq my-release (completing-read (concat "Set release of task (" my-task ") (default: " my-release "): ")
                             my-releases nil 'confirm nil synergy-release-minibuffer-history my-release)))
        (progn
          ;; ask for task to change release
          (setq my-task (completing-read "Task: "
                          my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task))
          ;; ask for release (default is release of task)
          (setq my-release (completing-read (concat "Set release of task (" my-task "): ")
                             my-releases nil 'confirm nil synergy-release-minibuffer-history my-release))))
      ;; get number of chosen task
      (setq my-task (synergy-get-task-number my-task))
      ;; set release of task
      (synergy-run-async-command (combine-and-quote-strings
                                   (list synergy-ccm-command "task"
                                     "-modify"
                                     "-release" my-release
                                     my-task))))))

;;; List of object in task
;;;###autoload
(defun synergy-disassociate-object-from-task ()
  "List associated object with a task to disassociated it."
  (interactive)
  ;; get all assigned tasks
  (let ((my-tasks (synergy-get-assigned-task))
         ;; get current task
         (my-task (synergy-get-current-task))
         (my-files) (my-file))
    ;; only when there are assigned tasks
    (when my-tasks
      (if my-task
        ;; ask which task with current as default
        (setq my-task (synergy-get-task-number
                        (completing-read (concat "Task to list associated object (default: " my-task "): ")
                          my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task)))
        ; ;ask which task
        (setq my-task (synergy-get-task-number
                        (completing-read "Task to list associated object: "
                        my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task))))
      ;; get list of associated file
      (setq my-files (synergy-list-object-from-a-task my-task))
      ;; when there are associated files
      (when my-files
        ;; ask which file to disassociate
        (setq my-file (completing-read "File to disassociate: "
                        my-files nil t nil synergy-file-minibuffer-history my-file))
        ;; disassociate file from task
        (synergy-disassociate-file-from-task my-file my-task)))))

;;; Complete/CheckIn a task
;;;###autoload
(defun synergy-complete-task ()
  "Complete a task."
  (interactive)
  ;; get current task
  (let ((my-task (synergy-get-current-task))
         ;; get all assigned tasks
         (my-tasks (synergy-get-assigned-task)))
    ;; when current task is set
    (if my-task
      ;; ask for task to complete (default is current task)
      (setq my-task (completing-read (concat "Task to complete (default: " my-task "): ")
                      my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task))
      ;; ask for task to complete
      (setq my-task (completing-read "Task to complete:"
                      my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task)))
    ;; get number of task
    (setq my-task (synergy-get-task-number my-task))
    ;; complete/checkin the task
    (synergy-run-async-command (combine-and-quote-strings
                                 (list synergy-ccm-command "task"
                                   "-complete" my-task) t))))
;;; alias with synonym for complete (from classic interface)
(defalias 'synergy-checkin-task 'synergy-complete-task)

;;; Update/Reconfigure a project
;;;###autoload
(defun synergy-update ()
  "Update a project."
  (interactive)
  ;; get current project
  (let ((my-project (synergy-get-current-project))
         ;; get all working project
         (my-projects (synergy-get-working-project)))
    (if my-project
      ;; ask which project to update/reconfigure
      (setq my-project (completing-read (concat "Update project (default: " my-project "): ")
                         my-projects nil 'confirm nil synergy-project-minibuffer-history my-project))
      (setq my-project (completing-read "Update project: "
                         my-projects nil 'confirm nil synergy-project-minibuffer-history my-project)))
    ;; run update
    (synergy-update-project my-project)))
;;; alias with synonym for update (from classic interface)
(defalias 'synergy-reconfigure 'synergy-update)

;;; Update/Reconfigure current project
;;;###autoload
(defun synergy-update-current-project ()
  "Update current project."
  (interactive)
  ;; get current project
  (let ((my-project (synergy-get-current-project)))
    ;; run update
    (synergy-update-project my-project)))
;;; alias with synonym for update (from classic interface)
(defalias 'synergy-reconfigure-current-project 'synergy-update-current-project)

;;; Compare with previous
;;;###autoload
(defun synergy-diff-with-previous ()
  "Compare current file or directory with his previous."
  (interactive)
  ;; get file name
  (let ((my-filename (synergy-get-buffer-file-name))
         (my-version))
    ;; when file is a synergy file
    (when my-filename
      ;; get version
      (setq my-version (synergy-get-file-version my-filename))
      ;; concat filename and version
      (setq my-filename (concat my-filename "~" my-version))
      ;; run diff
      (synergy-run-diff my-filename))))

;;; Compare with previous by using external tool
;;;###autoload
(defun synergy-diff-external-with-previous ()
  "Compare current file or directory with his previous by using an external
  tool."
  (interactive)
  ;; get current buffer path
  (let ((my-file (synergy-get-buffer-file-name)))
    (when my-file
      ;; get current version
      (setq my-file (concat my-file "~" (synergy-get-file-version my-file)))
      ;; run diff external with previous
      (synergy-prepare-diff-external my-file))))

;;; History of current file or directory
;;;###autoload
(defun synergy-history ()
  "History of current file or directory."
  (interactive)
  ;; get file name
  (let ((my-filename (synergy-get-buffer-file-name))
         ;; set history buffer name
         (my-history-buffer synergy-history-buffer)
         (history-string))
    ;; when it is a synergy file
    (when my-filename
      ;; get history about this file
      (setq history-string (shell-command-to-string
                             (combine-and-quote-strings
                               (append (list synergy-ccm-command "history"
                                         "-column_header"
                                         "-format" "  %modify_time | %name~%version | %owner | %task: %task_synopsis | %status |"
                                         my-filename) synergy-history-filter))))
      ;; when no error (line start with Modify_time)
      (when (string-match "^\\s-*Modify_time.*" history-string)
        ;; keep file name for other function
        (setq synergy-history-file my-filename)
        ;; keep current buffer to restore cursor
        (setq synergy-history-parent-buffer (current-buffer))
        ;; remove old buffer
        (kill-buffer (get-buffer-create my-history-buffer))
        ;; create a new buffer and go in
        (pop-to-buffer (get-buffer-create my-history-buffer))
        ;; set auto-save mode in default mode
        ;(auto-save-mode auto-save-default)
        ;; empty buffer
        (erase-buffer)
        ;; insert output of show conflict
        (insert history-string)
        ;; start history mode
        (synergy-history-mode)
        ;; go to first character of buffer
        (goto-char (point-min))
        ;; go at end of line "Path        Conflict Message"
        (re-search-forward "^\\s-*Modify_time\\s-*Name\\s-*Version\\s-*Owner\\s-*Task\\s-*Task_synopsis\\s-*Status\\s-*$" nil 'end)
        ;; go to next character (so next line)
        (forward-char)
        ;; go to start of current line
        (beginning-of-line)
        ))))

;;;###autoload
;;; Toogle mark on current line (only for history mode)
(defun synergy-history-toogle-mark ()
  "Un/mark the file on the current line."
  (interactive)
  ;; check major mode
  (if (synergy-history-check-major-mode)
    ;; inhib read only
    (let ((inhibit-read-only t)
           (keep-pos)
           (line))
      ;; store current position
      (setq keep-pos (point))
      ;; get line number
      (setq line (count-lines 1 (point)))
      ;; when line is the first do not mark it
      (if (equal line 1)
        (message "Cannot toggle mark on this line.")
        (progn
          ;; go to start of line
          (beginning-of-line)
          (if (looking-at "\\*")
            (progn
              ;; unmark the current line
              ;; marked line so replace first character by space
              (delete-char 1)
              (insert " "))
            (progn
              ;; mark the current line
              ;; line not marked so replace first character by mark-character
              (delete-char 1)
              (insert synergy-mark-character)))
          ;; restore position
          (goto-char keep-pos))))
    (message "Only in synergy history mode.")))

;;;###autoload
;;; open current version in a new buffer
(defun synergy-history-open ()
  "Edit/Open the current version."
  (interactive)
  ;; check major mode
  (if (synergy-history-check-major-mode)
    ;; get version file on the current line
    (let ((my-version-file (synergy-history-current-line-file))
           (my-tmp-file) (my-file) (my-version))
      ;; get file path and version separately
      (when (and (string-match "^\\(.*\\)~\\([^\\s-]+\\)" my-version-file)
              (setq my-file (match-string 1 my-version-file))
              (setq my-version (match-string 2 my-version-file)))
        (progn
          ;; get version file in a tmp file
          (setq my-tmp-file (synergy-cat-version-file my-file my-version))
          (if my-tmp-file
            ;; open the tmp file in a buffer
            (find-file my-tmp-file)))))
    (message "Only in synergy history mode.")))

;;; diff in history mode
;;;###autoload
(defun synergy-history-diff ()
  "Run diff between two marked version, or one marked and current or current
with its previous."
  (interactive)
  ;; check history mode
  (if (synergy-history-check-major-mode)
    ;; get list of marked file
    (let ((my-list (synergy-history-list-of-marked-version))
           (my-file))
      (cond
        ;; too much marked
        ((> (length my-list) 2)
          (message "More than 2 version is marked. Use the youngest and the oldest.")
          (synergy-run-diff (car (last my-list)) (car my-list)))
        ;; two marked
        ((= (length my-list) 2)
          ;; run diff on two marked file
          (synergy-run-diff (car (last my-list)) (car my-list)))
        ;; one marked
        ((= (length my-list) 1)
          ;; run diff on marked file and current
          (setq my-file (synergy-history-current-line-file))
          (synergy-run-diff (car my-list) my-file))
        ;; zero marked
        ((= (length my-list) 0)
          ;; run diff on current and its previous
          (setq my-file (synergy-history-current-line-file))
          (synergy-run-diff my-file))))))

;;; diff with external tool in history mode
;;;###autoload
(defun synergy-history-diff-external ()
  "Run diff by using an external tool between two marked version, or one marked
and current or current with its previous."
  (interactive)
  ;; check history mode
  (if (synergy-history-check-major-mode)
    ;; get list of marked file
    (let ((my-list (synergy-history-list-of-marked-version))
           (my-file))
      (cond
        ;; too much marked
        ((> (length my-list) 2)
          (message "More than 2 version is marked. Use the youngest and the oldest.")
          (synergy-prepare-diff-external (car my-list) (car (last my-list))))
        ;; two marked
        ((= (length my-list) 2)
          ;; run diff on two marked file
          (synergy-prepare-diff-external (car my-list) (car (last my-list))))
        ;; one marked
        ((= (length my-list) 1)
          ;; run diff on marked file and current
          (setq my-file (synergy-history-current-line-file))
          (synergy-prepare-diff-external my-file (car my-list)))
        ;; zero marked
        ((= (length my-list) 0)
          ;; run diff on current and its previous
          (setq my-file (synergy-history-current-line-file))
          (synergy-prepare-diff-external my-file))))))

;;
;;;
;;;; HISTORY MODE
(defvar synergy-history-mode-hook nil
  "Run after loading synergy-history-mode.")

(defvar synergy-history-quit-hook nil
  "Run after quit synergy-history-mode.")

(defvar synergy-history-map nil
  "Bind map for history mode.")
(if synergy-history-map
  nil
  (progn
    (setq synergy-history-map (make-sparse-keymap))
    (define-key synergy-history-map (kbd "<return>") 'synergy-history-open)
    (define-key synergy-history-map (kbd "M-=") 'synergy-history-diff)
    (define-key synergy-history-map (kbd "=") 'synergy-history-diff-external)
    (define-key synergy-history-map (kbd "<SPC>") 'synergy-history-toogle-mark)
    ))

(defconst synergy-history-font-lock-keyword
  (list
    ;; mark
    '("^\\(\\*\\)"
       (1 font-lock-comment-face nil t))
    ;; day/month/year hour:minute
    '("^.\\s-*\\([0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\s-*[0-9][0-9]:[0-9][0-9]\\)"
       (1 font-lock-type-face nil t))
    ;; name ~ version
    '("|\\s-*\\([\\.a-zA-Z0-9_]+\\)\\s-*~\\s-*\\([0-9\\.]+\\)\\s-*|"
       (1 font-lock-variable-name-face nil t) (2 font-lock-function-name-face nil t))
    ;; task: task_description
    '("|\\s-*\\([0-9]+\\):\\s-*[^|]+\\s-*|"
       (1 font-lock-keyword-face nil t))
    ;; status (integrate)
    '("|\\s-*\\(integrate\\)\\s-*|\\s-*$"
       (1 font-lock-builtin-face nil t))
    ;; status (working) color all separation
    '(".*\\(|\\).*\\(|\\).*\\(|\\).*\\(|\\)\\s-*\\(working\\)\\s-*\\(|\\)\\s-*$"
       (1 font-lock-string-face nil t) (2 font-lock-string-face nil t)
       (3 font-lock-string-face nil t) (4 font-lock-string-face nil t)
       (5 font-lock-string-face nil t) (6 font-lock-string-face nil t))
    ;; status (prep)
    '("|\\s-*\\(prep\\)\\s-*|\\s-*$"
       (1 font-lock-variable-name-face nil t))
    ;; status (shared)
    '("|\\s-*\\(shared\\|visible\\|sqa\\|collaborative\\)\\s-*|\\s-*$"
       (1 font-lock-preprocessor-face nil t))
    )
  "Keyword for synergy-history-mode.")

(defvar synergy-history-syntax-table nil
  "Syntax table for synergy-history-mode.")

(defun synergy-history-create-syntax-table ()
  (if synergy-history-syntax-table
    ()
    (setq synergy-history-syntax-table (make-syntax-table))
    ;; This is added so entity names with underscores can be more easily parsed
    (modify-syntax-entry ?_ "w" synergy-history-syntax-table))
  (set-syntax-table synergy-history-syntax-table)
  )

;;;###autoload
(defun synergy-history-mode ()
  "History mode."
  (interactive)
  ;; empty environment
  (kill-all-local-variables)
  ;; set key binding
  (use-local-map synergy-history-map)
  ;; syntax
  (synergy-history-create-syntax-table)
  ;; Set up font-lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(synergy-history-font-lock-keyword nil t))
  ;; set major mode
  (setq major-mode 'synergy-history-mode)
  ;; set name mode
  (setq mode-name "Synergy-history")
  ;; cannot modify buffer
  (read-only-mode t)
  ;; ?
  ;(make-variable-buffer-local 'synergy-parent-buffer)
  ;; set to unmodified
  (set-buffer-modified-p nil)
  ;; highlight username in history
  (highlight-regexp synergy-username 'font-lock-warning-face)
  ;; run history mode hook
  (run-hooks 'text-mode-hook 'synergy-history-mode-hook))


;;
;;;
;;;; FUNCTION (elisp)
;;; checkout file with option
(defun synergy-checkout-with-option (&optional with-current-task)
  "Checkout the current buffer file, with current task when WITH-CURRENT-TASK is
true otherwise choose one."
  ;; get list of assigned tasks
  (let ((my-tasks (synergy-get-assigned-task))
         ;; get current task
         (my-task)
         ;; get buffer name
         (my-filename (synergy-get-buffer-file-name))
         (my-ret-checkout))
    (when my-filename
      (if with-current-task
        ;; checkout file
        (setq my-ret-checkout (shell-command-to-string
                                (combine-and-quote-strings
                                  (list synergy-ccm-command "checkout" my-filename))))
        (progn
          (setq my-task (synergy-get-current-task))
            ;; current task is not null
          (if my-task
            ;; ask for task with list of assigned task and current by default
            (setq my-task (completing-read (concat "Checkout with task (default: " my-task "): ")
                            my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task))
            ;; ask for task with list of assigned task
            (setq my-task (completing-read "Checkout with task: "
                            my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task)))
          ;; get number of chosen task
          (setq my-task (synergy-get-task-number my-task))
          ;; checkout file
          (setq my-ret-checkout (shell-command-to-string
                                  (combine-and-quote-strings
                                    (list synergy-ccm-command "checkout" "-task" my-task my-filename))))))
      ;; display in minibuffer the outputs
      (message my-ret-checkout)
      ;; set current task
      (when synergy-checkout-set-current-task
        (synergy-set-task-as-current my-task)))))

;;; update/reconfigure a project
(defun synergy-update-project (project)
  "Update PROJECT."
  (when my-project
    (synergy-run-async-command (combine-and-quote-strings
                                 (list synergy-ccm-command "update"
                                   "-project"
                                   "-recurse" my-project) t))))

;;; refresh server file from local file
(defun synergy-synchronize-server-file (file)
  "Synchronize server file with local FILE."
  (synergy-run-async-command (combine-and-quote-strings
                               (list synergy-ccm-command "reconcile"
                                 "-update_db"
                                 "-recurse"
                                 file) t)))

;;; refresh local file from server file
(defun synergy-synchronize-local-file (path)
  "Synchronize local FILE with server file."
  (synergy-run-async-command (combine-and-quote-strings
                               (list synergy-ccm-command "reconcile"
                                 "-update_wa"
                                 "-recurse"
                                 file) t)))

;;; set current task
(defun synergy-set-task-as-current (task)
  "Set the current task with TASK-NUMBER."
  ;; get number
  (setq task (synergy-get-task-number task))
  (when task
    ;; set current task
    (synergy-run-async-command (combine-and-quote-strings
                                 (list synergy-ccm-command "task"
                                   "-current" task)))))

;;; get current task
(defun synergy-get-current-task ()
  "Return the current task."
  (let* (ret (task) (number) (release) (synopsis))
    ;; get current task
    (setq task (synergy-shell-command-to-string
                 (combine-and-quote-strings
                   (list synergy-ccm-command "task" "-current"))))
    (when task
      (if (string-match "^\\([0-9]+\\)\\s-*:\\s-*\\(.*\\)$" task)
        (progn
          ;; get number of current task
          (setq number (match-string 1 task))
          ;; get synopsis of current task
          (setq synopsis (match-string 2 task))
          (if (string-match "^\\(.\\{0,27\\}\\)....+$" synopsis)
            ;; truncate synopsis to 30 character
            (setq synopsis (concat (match-string 1 synopsis) "...")))
          ;; get release of current task
          (setq release (synergy-get-task-release number))
          ;; return a concat of number, release and synopsis
          (concat number ": [" release "] " synopsis))
        nil))
      nil))

;;; associate a object with a task
(defun synergy-associate-object-with-task (file task)
  "Associate an object with a task."
  (let ((ret))
    ;; associate with task
    (setq ret (synergy-shell-command-to-string
                (combine-and-quote-strings
                  (list synergy-ccm-command "task"
                    "-associate" task
                    "-object" file))))
    ;; when no return
    (if (string-match "^\\s-*$" ret)
      ;; object is associated
      (message "%s is associated with task #%s" file task)
      ;; return error
      (message ret))))

;;; disassociate a object with a task
(defun synergy-disassociate-file-from-task (file task)
  "Disassociate an object FILE with TASK. FILE can use the format c:/myfile or
myfile~version:type:instance"
  (let ((ret))
    ;; associate with task
    (setq ret (synergy-shell-command-to-string
                (combine-and-quote-strings
                  (list synergy-ccm-command "task"
                    "-disassociate" task
                    "-object" file))))
    ;; when no return
    (if (string-match "^\\s-*$" ret)
      ;; object is associated
      (message "%s is disassociated from task #%s" file task)
      ;; return error
      (message ret))))

;;; get all associated tasks in a list
(defun synergy-get-associated-task (file-version)
  "Get all associated tasks in a list of string."
  (let* (ret (my-tasks) (task))
    ;; get all assigned task from synergy
    (setq my-tasks (synergy-shell-command-to-string
                     (combine-and-quote-strings
                       (list synergy-ccm-command "properties"
                         "-format" "%task" file-version))))
    ;; remove new line character
    (setq my-tasks (replace-regexp-in-string "\n" "" my-tasks))
    ;; create a list of each task
    (setq my-tasks (split-string my-tasks "," t))
    ;; for each task in list of tasks
    (while my-tasks
      ;; get first task
      (setq task (pop my-tasks))
      ;; get all info on number task
      (setq task (shell-command-to-string
                   (combine-and-quote-strings
                     (list synergy-ccm-command "task"
                       "-show" "information"
                       "-format" "%displayname: [%release] %{task_synopsis[truncate=30]}"
                       task))))
      ;; remove new line character
      (setq task (replace-regexp-in-string "\n" "" task))
      ;; add in a list for return
      (setq ret (cons task ret)))
    ;; reverse the list to set in original sort
    (setq ret (nreverse ret))
    ret))

;;; get only the number from a task string
(defun synergy-get-task-number (task-string)
  "Return the task number from task format string TASK-STRING (task_number:
task_synopsis)."
  (let* (ret)
    ;; try to match task format "#: task synopsis"
    (if (string-match "\\([0-9]+\\)\\(\\s-*:\\{0,1\\}.*\\)\\{0,1\\}" task-string)
      ;; get digit matched
      (setq ret (match-string 1 task-string))
      (progn
        ;; when there is only a number
        (if (string-match "\\s-*\\([0-9]+\\)\\s-*" task-string)
          ;; return it
          (setq ret (match-string 1 task-string))
          (progn
            ;; not matched a task format
            (setq ret nil)
            (message (concat "Wrong task format [number: synopsis]: " task-string))))))
    ret))

;;; get all assigned tasks in a list
(defun synergy-get-assigned-task ()
  "Get all assigned tasks in a list of string."
  (let* (ret)
    ;; get all assigned task from synergy
    (setq ret (synergy-shell-command-to-string
                (combine-and-quote-strings
                  (list synergy-ccm-command "task"
                    "-query"
                    "-task_scope" "all_my_assigned"
                    "-format" "%displayname: [%release] %{task_synopsis[truncate=30]}"))))
    (if ret
      (progn
        ;; remove #) which start each line
        (setq ret (replace-regexp-in-string "^[0-9]+)\\s-*" "" ret))
        ;; create a list of each task
        (setq ret (split-string ret "\n" t))
        ret)
      nil)))

;;; get list of file associated to a task
(defun synergy-list-object-from-a-task (task)
  "Return list of string of file associated with TASK."
  (let* (ret)
    ;; get list of associated file
    (setq ret (synergy-shell-command-to-string
                (combine-and-quote-strings
                  (list synergy-ccm-command "task"
                    "-show" "objects" task
                    "-format" "%displayname:%type:%instance"
                    "-unnumbered"))))
    ;; create a list of each file
    (setq ret (split-string ret "\n" t))
    ;; remove first line "Task #:"
    (pop ret)
    ;; when there is no associated file
    (when (not ret)
      (setq ret nil)
      (message "No associated objects with this task: %s" task))
    ret))

;;; get current release from current file or directory
(defun synergy-get-current-release ()
  "Return the release of the current file or directory."
  ;; get file name
  (let* ((ret nil) (release) (my-filename (synergy-get-buffer-file-name)))
    (when my-filename
      ;; when file name exist in synergy get release
      (setq release (shell-command-to-string
                      (combine-and-quote-strings
                        (list synergy-ccm-command "properties"
                          "-format" "\"%release\""
                          my-filename))))
      (if (string-match "^[^/]+/[^/]+$" release)
        ;; when synergy return a release
        (setq ret (replace-regexp-in-string "\n$" "" release))
        ;; otherwise nil
        (setq ret nil)))
    ret))

;;; get release from current task
(defun synergy-get-current-task-release ()
  "Return the release of the current task."
  ;; get file name
  (let* (ret (release) (my-task (synergy-get-current-task)))
    (when my-task
      ;; when current task is set
      (setq release (shell-command-to-string
                      (combine-and-quote-strings
                        (list synergy-ccm-command "properties"
                        "-format" "\"%release\""
                        my-filename))))
      (if (string-match "^[^/]+/[^/]+$" release)
        ;; when synergy return a release
        (setq ret (replace-regexp-in-string "\n$" "" release))
        ;; otherwise nil
        (setq ret nil)))
    ret))

;;; get all active releases
(defun synergy-get-all-active-releases ()
  "Get all active releases in a list of string."
  (let* (ret)
    ;; get all active releases
    (setq ret (synergy-shell-command-to-string
                (combine-and-quote-strings
                  (list synergy-ccm-command "release"
                    "-list" "-active"))))
    ;; when cannot connect to synergy server
    (if (string-match "[cC]annot connect" ret)
      (progn
        ;; start a connection before
        (setq ret nil)
        (message "Synergy is not started"))
      (progn
        ;; remove #) which start each line
        (setq ret (replace-regexp-in-string "^[0-9]+)\\s-*" "" ret))
        ;; create a list of each task
        (setq ret (split-string ret "\n" t))))
    ret))

;;; get release of a task
(defun synergy-get-task-release (task)
  "Get release of a task."
  (let* (ret (release))
    ; ;get task number
    (setq task (synergy-get-task-number task))
    ;; get release of task from synergy
    (setq release (synergy-shell-command-to-string
                    (combine-and-quote-strings
                      (list synergy-ccm-command "task"
                        "-show" "release"
                        task))))
    (if release
      ;; check right return value
      (if (string-match "^Task [0-9]+\\s-*:\\s-*\\(.*\\)$" release)
        ;; get release
        (setq ret (match-string 1 release))
        (progn
          ;; return nil
          (setq ret nil)
          ;; display error message
          (message (concat "Error to get release from task: " task))))
      ;; synergy not started
      (setq ret nil))
    ret))

;;; test when file/directory is from a synergy project
(defun synergy-in-a-project (path)
  "Return the PATH of the object when it is in a synergy project otherwise nil."
  ;; get the default directory
  (let* (ret (my-properties))
    (setq my-properties (synergy-shell-command-to-string
                          (combine-and-quote-strings
                            (list synergy-ccm-command "properties"
                              "-format" "\"%displayname\""
                              path))))
    (if my-properties
      ;; when synergy return "Invalid value"
      (if (string-match "^Invalid value" my-properties)
        (progn
          ;;  file is not from synergy
          (setq ret nil)
          (message (concat "This file or directory is not part of a synergy project: " path)))
        ;; otherwise the file is from synergy project
        (setq ret t))
      ;; synergy not started
      (setq ret nil))
    ret))

;;; return list of orphan object
(defun synergy-get-orphan-object ()
  "Return a string list of orphan object in synergy."
  (let* (ret (my-files))
    ;; get all orphan file
    (setq my-files (synergy-shell-command-to-string
                     (combine-and-quote-strings
                       ;; query command
                       (list synergy-ccm-command "query"
                         ;; query string to have all objects with following properties
                         (concat
                           ;; current user is the owner
                           "(owner='" synergy-username "') and "
                           ;; in working state
                           "(status='working') and "
                           ;; with no associated task,
                           "has_no_assoicated_task() and "
                           ;; is not a project
                           "(type!='project') and "
                           ;; is not a project grouping
                           "(type!='project_grouping')")
                         ;; with format unnumbered
                         "-format" "%displayname:%type:%instance" "-unnumbered"))))
    ;; when cannot connect ot synergy server
    (if my-files
      ;; create a list of files
      (setq ret (split-string my-files "\n" t))
      ;; synergy not started
      (setq ret nil))
    ret))

;;; get the buffer file name and check it
(defun synergy-get-buffer-file-name ()
  "Return the file name or nil if current buffer is not in synergy project or if
it is an untracked file."
  (let* (ret (my-buffer buffer-file-name))
    ;; get filename at point in dired mode
    (when (equal major-mode 'dired-mode)
      (setq my-buffer (dired-get-filename)))
    ;; the path is from a mounted view
    (if (synergy-in-a-project my-buffer)
      (setq ret my-buffer)
      ;; the current file is not from synergy
      (setq ret nil))
    ret)) ; return value

;;; get the version of a file
(defun synergy-get-file-version (file)
  "Return the version of FILE."
  (let* (ret)
    ;; get version with synergy
    (setq ret (synergy-shell-command-to-string
                (combine-and-quote-strings
                  (list synergy-ccm-command "properties"
                    "-nocolumn_header"
                    "-format" "VERSION %version"
                    file))))
    (when ret
      ;; test value return by synergy
      (if (string-match "^VERSION\\s-*\\(.*\\)$" ret)
        ;; return version
        (setq ret (match-string 1 ret))
        ;; display error message
        (message (concat "Error to get version of file: " file))))
    ret))

;;; get previous file
(defun synergy-get-previous-version (file)
  "Return a string which contains the previous version of FILE."
  ;; init variable
  (let* (ret (my-previous))
    ;; get previous version of FILE
    (setq my-previous (synergy-shell-command-to-string
                        (combine-and-quote-strings
                          (list synergy-ccm-command "relate"
                            "-show"
                            "-name" "successor"
                            "-to" file))))
    (if my-previous
      ;; when synergy return a predecessor
      (if (string-match "^\\s-*.*~\\([^/]*\\):.*:.*\\s-*successor\\s-*\\(.*~[^/]*\\):.*:.*$" my-previous)
        ;; get previous version
        (setq ret (match-string 1 my-previous))
        (progn
          ;; return nil
          (setq ret nil)
          ;; display error message
          (message (concat "The file do not have a predecessor: " file))))
      (setq ret nil))
    ret))

;;; get temporary version file
(defun synergy-cat-version-file (file version)
  "Return a temporary file path for a version of file."
  (let* (my-temp-file (my-string-buffer))
    ;; get file name only and build a unique temp name
    (setq my-temp-file (concat (file-name-as-directory  temporary-file-directory)
                         "__synergy__" version "~" (file-name-nondirectory file)))
    ;; delete the tmp file without warning
    (condition-case nil
      (delete-file my-temp-file)
      (error nil))
    ;; get the whole version file
    (setq my-string-buffer (synergy-shell-command-to-string
                             (combine-and-quote-strings
                               (list synergy-ccm-command "cat"
                                 (concat file "~" version)))))
    (if my-string-buffer
      ;; write version file into temp file
      (append-to-file my-string-buffer nil my-temp-file)
      (setq my-temp-file nil))
    ;; return the path to temp version file
    my-temp-file))

;;; return t if file is the local version
(defun synergy-is-local-version (file-version)
  "Return filepath without version when FILE-VERSION is the local version, otherwise nil."
  ;; split file and version
  (let* (ret (my-file-version (synergy-split-file-and-version file-version))
          (my-version))
    ;; get version of local file
    (setq my-version (synergy-get-file-version (car my-file-version)))
    ;; when version are same
    (if (string-equal (car (last my-file-version)) my-version)
      ;; return file path
      (setq ret (car my-file-version))
      ;; return nil
      (setq ret nil))
    ret))

;; return the path with version from history line
(defun synergy-history-current-line-file ()
  "Return the file version of the current line."
  (let* (my-version-file (my-line) (my-version))
    ;; get current line
    (setq my-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
    ;; must be a history line
    (if (string-match "^.*|\\s-*.*~\\([^\\s-]+\\)\\s-*|.*|.*|.*|\\s-*$" my-line)
      (progn
        ;; get version
        (setq my-version (match-string 1 my-line))
        ;; set with full path
        (setq my-version-file (concat synergy-history-file "~" my-version)))
      (progn
        ;; point not on a line with a file
        (setq my-version-file nil)
        (message "Not on a history line.")))
    ;; return file path with version
    my-version-file))

(defun synergy-history-list-of-marked-version ()
  "Return a list of marked version."
  (let* (my-list (my-line)
          (my-keep-point))
    ;; store current position
    (setq my-keep-point (point))
    ;; beginning of buffer
    (goto-char 1)
    ;; until end of buffer
    (while (not (= (point) (point-max)))
      ;; when line start with *
      (when (looking-at "\\*")
        ;; add file version in list
        (setq my-list (cons (synergy-history-current-line-file) my-list)))
      ;; go to next beginning of line
      (goto-char (+ (line-end-position) 1)))
    ;; restore current position
    (goto-char my-keep-point)
    ;; return list of marked file
    my-list))

;;; get status of a file
(defun synergy-get-object-status (file)
  "Return the status of an object."
  ;; init variable
  (let* (ret (my-status))
    ;; get status of object
    (setq my-status (synergy-shell-command-to-string
                      (combine-and-quote-strings
                        (list synergy-ccm-command "properties"
                          "-format" "\"%status\""
                          file))))
    (if my-status
      ;; check if the status exists
      (if (string-match "working\\|prep\\|shared\\|visible\\|integrate\\|sqa\\|collaborative")
        ;; return the status
        (setq ret my-status)
        (progn
          ;; return nil
          (setq ret nil)
          ;; display error message
          (message "%s %s" "The object do not have a status:" file)))
      (setq ret nil))
    ret))

;;; check if object is in working state
(defun synergy-is-object-in-working (file)
  "Return t when object is in working status otherwise nil."
  ;; get status of object
  (let* (ret (my-status (synergy-get-object-status file)))
    (if (string-equal my-status "working")
      ;; when status is working return true
      (setq ret t)
      ;; otherwise return nil
      (setq ret nil))
    ret))

;;; get all working project
(defun synergy-get-working-project ()
  "Return list of working project of `synergy-username'."
  (let* (ret)
    (setq ret (synergy-shell-command-to-string
                (combine-and-quote-strings
                  (list synergy-ccm-command "query"
                  "-format" "\"%displayname\""
                    "-type" "project"
                    "-state" "working"
                    "-owner" synergy-username))))
    (when ret
      ;; remove #) which start each line
      (setq ret (replace-regexp-in-string "^[0-9]+)\\s-*" "" ret))
      ;; create a list of each task
      (setq ret (split-string ret "\n" t)))
    ret))

;;; get project ofcurrent file or directory
(defun synergy-get-current-project ()
  "Return the current project of file or directory."
  ;; get current filename
  (let* (ret (my-filename (synergy-get-buffer-file-name)))
    (when my-filename
      ;; when filename exist get project name
      (setq ret (shell-command-to-string
                  (combine-and-quote-strings
                    (list synergy-ccm-command "project" my-filename))))
      ;; remove new line character
      (setq ret (replace-regexp-in-string "\n$" "" ret)))
    ret))

;;; check history major mode
(defun synergy-history-check-major-mode ()
  "Return TRUE when major mode is Synergy history mode otherwise nil."
  (let* (ret)
    ;; check major mode
    (if (string-equal major-mode "synergy-history-mode")
      (setq ret t)
      (setq ret nil))
    ret))

;;; split filename and version from synergy fullname (myfile.c~1)
(defun synergy-split-file-and-version (file-version)
  "Return list of string with filename and version."
  (let* (ret (my-file) (my-version))
    ;; match file.ext~version
    (if (string-match "^\\(.*\\)~\\([^/]*\\)$" file-version)
      (progn
        ;; get filename
        (setq my-file (match-string 1 file-version))
        ;; get version
        (setq my-version (match-string 2 file-version))
        ;; return list of file and version
        (setq ret (list my-file my-version)))
      (progn
        ;; do not match
        (setq ret nil)
        (message "%s is not a file with version." file-version)))
    ret))

;;; run synergy diff
(defun synergy-run-diff (file1 &optional file2)
  "Run a synergy diff in CLI."
  (let ((ret-diff)
         (my-diff-buffer synergy-diff-buffer))
    ;; get result of diff command
    (if (not file2)
      (setq ret-diff (synergy-shell-command-to-string
                       (combine-and-quote-strings (list synergy-ccm-command "diff" file1))))
      (setq ret-diff (synergy-shell-command-to-string
                       (combine-and-quote-strings (list synergy-ccm-command "diff" file1 file2)))))
    (when ret-diff
      ;; keep current buffer to restore cursor
      (setq synergy-diff-parent-buffer (current-buffer))
      ;; remove old buffer
      (kill-buffer (get-buffer-create my-diff-buffer))
      ;; create a new buffer and go in
      (pop-to-buffer (get-buffer-create my-diff-buffer))
      ;; set auto-save mode in default mode
      (auto-save-mode auto-save-default)
      ;; empty buffer
      (erase-buffer)
      ;; insert output of show conflict
      (insert ret-diff)
      ;; run in diff mode
      (diff-mode)
      ;; go to first character of buffer
      (goto-char (point-min)))))

;;; prepare a diff for external tool
(defun synergy-prepare-diff-external (file1 &optional file2)
  "Prepare the files to use a diff external, when FILE2 is nil take the previous
of FILE1."
  ;; init variable
  (let ((my-file-version1) (my-file-version2) (my-version2)
         (my-file1) (my-file2))
    ;; get current version of file1
    (setq my-file-version1 (synergy-split-file-and-version file1))
    ;; when file2 is given
    (if file2
      ;; get current version of file2
      (setq my-file-version2 (synergy-split-file-and-version file2))
      (progn
        ;; otherwise get version of previous
        (setq my-version2 (synergy-get-previous-version file1))
        ;; when previous is present
        (when my-version2
          ;; set file path and version
          (setq my-file-version2 (list (car my-file-version1) my-version2))
          ;; set file path with version
          (setq file2 (concat (car my-file-version1) "~" my-version2)))))
    ;; only when file2 exist (useful for previous)
    (when (= (length my-file-version2) 2)
      (if (synergy-is-local-version file1)
        ;; return local path
        (setq my-file1 (car my-file-version1))
        ;; get contains of file1
        (setq my-file1 (synergy-cat-version-file (car my-file-version1) (car (last my-file-version1)))))
      (if (synergy-is-local-version file2)
        ; ;return local path
        (setq my-file2 (car my-file-version2))
        ;; get contains of file2
        (setq my-file2 (synergy-cat-version-file (car my-file-version2) (car (last my-file-version2)))))
      ;; run diff external
      (if synergy-diff-external-swap-file
        (synergy-run-diff-external my-file2 my-file1)
        (synergy-run-diff-external my-file1 my-file2)))))

;;; run a diff with an external tool
(defun synergy-run-diff-external (file1 file2)
  "Run a diff in external tool between FILE1 and FILE2."
  (if synergy-diff-external-command
    ;; run external tool
    (synergy-run-async-command (combine-and-quote-strings
                                 (append (list synergy-diff-external-command)
                                   synergy-diff-external-parameter-list
                                   (list file1 file2))))
    (message "Set an external diff tool with synergy-diff-external-command.")))

;;; return nil if command return "synergy not connected"
(defun synergy-shell-command-to-string (command)
  (let* (ret)
    (setq ret (shell-command-to-string command))
    (if (or (string-match "[cC]annot connect" ret)
          (string-match "^\\s-*Warning:\\s-No sessions found.\\s-*$" ret))
      (progn
        (setq synergy-is-started nil)
        (setq ret nil)
        (message "Synergy is not started"))
      (setq synergy-is-started t))
    ret))

;;; run an asynchronous command to call synergy command
(defun synergy-run-async-command (command &optional keep-buffer)
  "Call the COMMAND in an asynchronous process."
  ;; do not show a new window
  (save-window-excursion
    (kill-buffer (get-buffer-create synergy-command-buffer))
    ;; get current buffer
    (let ((my-async-buffer (generate-new-buffer synergy-command-buffer)))
      ;; call command
      (async-shell-command command my-async-buffer)
      ;; do not ask about process running with async buffer
      (set-process-query-on-exit-flag (get-buffer-process my-async-buffer) nil)
      (if keep-buffer
        ;; pop async buffer (after 1s: to wait async process start)
        (run-with-timer 1 nil (lambda (my-async-buffer) (pop-to-buffer synergy-command-buffer)) my-async-buffer)
        ;; kill async buffer (after 1s: to wait async process start)
        (run-with-timer 1 nil (lambda (my-async-buffer) (kill-buffer my-async-buffer)) my-async-buffer))
    )))

;;; key map example
;; start
(global-set-key         (kbd "C-c s s")         'synergy-start)
;; checkout
(global-set-key         (kbd "C-c s c")         'synergy-checkout)
;; checkout with current task
(global-set-key         (kbd "C-c s x")         'synergy-checkout-with-current-task)
;; uncheckout
(global-set-key         (kbd "C-c s d")         'synergy-uncheckout)
;; diff
(global-set-key         (kbd "C-c s =")         'synergy-diff-with-previous)
;; history
(global-set-key         (kbd "C-c s h")         'synergy-history)
;; reconcile to server
(global-set-key         (kbd "C-c s r")         'synergy-reconcile-to-server)
;; reconcile from server
(global-set-key         (kbd "C-c s w")         'synergy-reconcile-to-local)
;; show conflict
(global-set-key         (kbd "C-c s f")         'synergy-show-conflict)
;; create task
(global-set-key         (kbd "C-c s t c")       'synergy-create-task)
;; set current task
(global-set-key         (kbd "C-c s t d")       'synergy-set-current-task)
;; set task release
(global-set-key         (kbd "C-c s t r")       'synergy-set-task-as-current)
;; complete/checkin task
(global-set-key         (kbd "C-c s t i")       'synergy-checkin-task)
;; update/reconfigure
(global-set-key         (kbd "C-c s u")         'synergy-reconfigure)
;; update/reconfigure current
(global-set-key         (kbd "C-c s p")         'synergy-reconfigure-current-project)

(provide 'synergy-web)

;;; synergy-web.el ends here

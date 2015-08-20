;;; function-synergy.el --- some function for synergy integration

;; Copyright (c) 2013 Claude Tete
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

;; Keywords: config, function, synergy
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 0.1
;; Created: August 2013
;; Last-Updated: August 2013

;;; Commentary:
;;
;; load by `functions.el'
;;
;; under MS Windows can be used with AutoHotKey script SynergyShortcut.ahk to
;; keep homogeneous bind for synergy functions
;;
;; Use and test with Synergy 7.1
;;
;; TODO compare with previous (only done for diff in cli) (must get temp file of old file to have gui diff)
;; TODO compare with (ccm diff ?) (can be done in history mode ?)
;; TODO history mode with color
;; TODO list of objects (remove from task)
;; TODO dis/associate with task from file
;; TODO reconcile/sync conflicts mode with color


;;; Change Log:
;; 2013-08-19 (0.2)
;;    add synchronize/reconcile, show conflict and create task
;; 2013-08-13 (0.1)
;;    from function-clearcase.el to checkout


;;; Code:

(require 'diff-mode)


(defvar synergy-username nil
  "Username to connect.")

(defvar synergy-database nil
  "Database to connect.")

(defvar synergy-server nil
  "Server to connect.")


(defvar synergy-ccm-command "ccm"
  "Synergy executable for CLI Web.")

(defvar synergy-command-buffer "*synergy*"
  "Name of buffer to show output of command.")


(defvar synergy-task-minibuffer-history nil
  "History of task in minibuffer.")

(defvar synergy-release-minibuffer-history nil
  "History of release in minibuffer.")

(defvar synergy-project-minibuffer-history nil
  "History of project in minibuffer.")

(defvar synergy-checkout-set-current-task nil
  "Set checkout used task to current task.")

(defvar synergy-ignore-uncontrolled-file "GTAGS;GRTAGS;GPATH;*.el;*.files;.svn;.git"
  "List of file to ignore to reconcile.")

(defvar synergy-show-conflict-parent-buffer nil
  "Previous buffer before a show conflict.")

(defvar synergy-history-parent-buffer nil
  "Previous buffer before history.")

(defvar synergy-history-filter nil
  "End of command line of history to filter/replace results.")

(defvar synergy-diff-parent-buffer nil
  "Previous buffer befor history.")


;;
;;;
;;;; FUNCTION (interactive)
;;; Start Synergy
(defun synergy-start ()
  "Start a session on synergy-server and synergy-database with synergy-username."
  (interactive)
  (let ((my-password) (ret))
    (setq my-password (read-passwd (concat "Password for " synergy-username ": ")))
    (setq ret (shell-command-to-string (combine-and-quote-strings
                                         (list synergy-ccm-command "start" "-q"
                                           "-d"  synergy-database
                                           "-s"  synergy-server
                                           "-n"  synergy-username
                                           "-pw" my-password))))
    (message ret)))

;;; Checkout file from a synergy project
(defun synergy-checkout ()
  "Checkout the current buffer file."
  (interactive)
  ;; get list of assigned tasks
  (let ((my-tasks (synergy-get-assigned-task))
         ;; get current task
         (my-task (synergy-get-current-task))
         ;; get buffer name
         (my-filename (synergy-get-buffer-file-name)))
    (when my-filename
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
      (synergy-run-async-command (combine-and-quote-strings (list synergy-ccm-command "checkout" "-task" my-task my-filename)) t)
      ;; set current task
      (when synergy-checkout-set-current-task
        (synergy-set-task-as-current my-task)))))

;;; Unckeckout/delete file from a synergy project
(defun synergy-uncheckout ()
  "Uncheckout the current buffer file."
  (interactive)
  ;; get buffer name
  (let ((my-filename (synergy-get-buffer-file-name)))
    (when my-filename
      ;; delete the checkout file
      (synergy-run-async-command (combine-and-quote-strings (list synergy-ccm-command "delete" "-recurse" my-filename)))
      (message "%s %s %s" "Synergy:" my-filename "is unckeckout."))))
(defalias 'synergy-delete 'synergy-unckeckout)

;;; Reconcile/Sync Work Area
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
(defun synergy-show-conflict ()
  "Show conflicts from synchronizing."
  (interactive)
  ;; get file name, show conflict buffer name
  (let ((ret) (my-filename (synergy-get-buffer-file-name)) (ignore-flag)
         (my-sc-buffer-name synergy-command-buffer))
    (if synergy-ignore-uncontrolled-file
      ;; set ignore file list only when the list is not empty
      (setq ignore-flag (concat "-ignore_types \"" synergy-ignore-uncontrolled-file "\" "))
      (setq ignore-flag ""))
    (when my-filename
      ;; get show conflict output
      (setq ret (shell-command-to-string (concat synergy-ccm-command " reconcile -show -recurse -consider_uncontrolled " ignore-flag my-filename)))
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
    (if (not (string-equal my-synopsis ""))
      ;; when non nil set synopsis parameter
      (setq my-synopsis-command (concat "-synopsis \"" my-synopsis "\" "))
      (setq my-synopsis-command ""))
    (if (not (string-equal my-change-request ""))
      ;; when non nil set CR parameter
      (setq my-change-request (concat "-change_request " my-change-request " "))
      (setq my-change-request ""))
    ;; create task with all asked parameter
    (setq my-task (shell-command-to-string (concat synergy-ccm-command " task -create " my-synopsis-command my-change-request "-release " my-release " -resolver " synergy-username)))
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
      (message (concat my-task " created.")))))

;;; Set current task
(defun synergy-set-current-task ()
  "Set current task."
  (interactive)
  ;; get current task
  (let ((my-task (synergy-get-current-task))
         ; ;get all assigned task
         (my-tasks (synergy-get-assigned-task)))
    (if my-task
      ;; ask task with default when current task is set
      (setq my-task (completing-read (concat "Set current task (default: " my-task "): ")
                      my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task))
      ;; ask task without default
      (setq my-task (completing-read "Set current task: "
                      my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task)))
    ;; set current task
    (synergy-set-task-as-current my-task)))

;;; Change release of a task
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
    ;; when there is a current task
    (if my-task
      (progn
        ;; get release of current task
        (setq my-release (synergy-get-task-release my-task))
        ;; ask for task to change release (default is current task)
        (setq my-task (completing-read (concat "Task (default: " my-task "): ")
                        my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task))
        ;; ask for release (default is release of task)
        (setq my-release (completing-read (concat "Release of task (" my-task ") (default: " my-release "): ")
                           my-releases nil 'confirm nil synergy-release-minibuffer-history my-release)))
      (progn
        ;; ask for task to change release
        (setq my-task (completing-read "Task: "
                        my-tasks nil 'confirm nil synergy-task-minibuffer-history my-task))
        ;; ask for release (default is release of task)
        (setq my-release (completing-read (concat "Release of task (" my-task "): ")
                           my-releases nil 'confirm nil synergy-release-minibuffer-history my-release))))
    ;; get number of chosen task
    (setq my-task (synergy-get-task-number my-task))
    ;; set release of task
    (synergy-run-async-command (concat synergy-ccm-command " task -modify -release \"" my-release "\" " my-task))))

;;; Complete/CheckIn a task
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
    (synergy-run-async-command (concat synergy-ccm-command " task -complete " my-task) t)))
;;; alias with synonym for complete (from classic interface)
(defalias 'synergy-checkin-task 'synergy-complete-task)

;;; Update/Reconfigure a project
(defun synergy-update ()
  "Update a project."
  (interactive)
  (let ((my-project (synergy-get-current-project))
         (my-projects (synergy-get-working-project)))
    (if my-project
      (setq my-project (completing-read (concat "Update project (default: " my-project "): ")
                         my-projects nil 'confirm nil synergy-project-minibuffer-history my-project))
      (setq my-project (completing-read "Update project: "
                         my-projects nil 'confirm nil synergy-project-minibuffer-history my-project)))
    (synergy-update-project my-project)))
;;; alias with synonym for update (from classic interface)
(defalias 'synergy-reconfigure 'synergy-update)

;;; Update/Reconfigure current project
(defun synergy-update-current-project ()
  "Update current project."
  (interactive)
  (let ((my-project (synergy-get-current-project)))
    (synergy-update-project my-project)))
;;; alias with synonym for update (from classic interface)
(defalias 'synergy-reconfigure-current-project 'synergy-update-current-project)

;;; History of current file or directory
(defun synergy-history ()
  "History of current file or directory."
  (interactive)
  (let ((my-filename (synergy-get-buffer-file-name))
         (my-history-buffer synergy-command-buffer)
         (ret))
    (when my-filename
      (setq ret (shell-command-to-string
                  (combine-and-quote-strings
                    (append (list synergy-ccm-command "history" "-column_header"
                              "-format" "%modify_time | %name ~ %version | %owner | %task: %task_synopsis | %status |"
                              my-filename) synergy-history-filter))))
      (when ret
        ;; keep current buffer to restore cursor
        (setq synergy-history-parent-buffer (current-buffer))
        ;; remove old buffer
        (kill-buffer (get-buffer-create my-history-buffer))
        ;; create a new buffer and go in
        (pop-to-buffer (get-buffer-create my-history-buffer))
        ;; set auto-save mode in default mode
        (auto-save-mode auto-save-default)
        ;; empty buffer
        (erase-buffer)
        ;; insert output of show conflict
        (insert ret)
        ;; go to first character of buffer
        (goto-char (point-min))
        ;; go at end of line "Path        Conflict Message"
        (re-search-forward "^Modify_time\\s-*Name\\s-*Version\\s-*Owner\\s-*Task\\s-*Task\\s-*Status\\s-*$" nil 'end)
        ;; go to next character (so next line)
        (forward-char)
        ;; go to start of current line
        (beginning-of-line)
        ))))

;;; Compare with previous
(defun synergy-diff-with-previous ()
  "Compare current file or directory with his previous."
  (interactive)
  (let ((my-filename (synergy-get-buffer-file-name))
         (my-diff-buffer synergy-command-buffer)
         (my-version)
         (ret-diff))
    (when my-filename
      (setq my-version (synergy-get-file-version my-filename))
      (setq my-filename (concat my-filename "~" my-version))
      (setq ret-diff (shell-command-to-string (combine-and-quote-strings (list synergy-ccm-command "diff" my-filename))))
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
      (diff-mode)
      ;; go to first character of buffer
      (goto-char (point-min))
      )))

(defun synergy-diff ()
  ""
  (interactive)
  (let ((my-filename (synergy-get-buffer-file-name))
         (my-tmp-file)
         (my-string))
    (when my-filename
      (setq my-filename (synergy-get-previous-version my-filename))
      (message my-filename)
      (setq my-string (shell-command-to-string (combine-and-quote-strings (list synergy-ccm-command "cat" my-filename))))
;      (setq my-tmp-file (make-temp-file (file-name-nondirectory my-filename)))
;      (append-to-file )
      )
    ))

;;
;;;
;;;; FUNCTION (elisp)
;;; update/reconfigure a project
(defun synergy-update-project (project)
  "Update PROJECT."
  (when my-project
    (synergy-run-async-command (concat synergy-ccm-command " update -project -recurse " my-project) t)))

;;; refresh server file from local file
(defun synergy-synchronize-server-file (file)
  "Synchronize server file with local FILE."
  (synergy-run-async-command (concat synergy-ccm-command " reconcile -update_db -recurse " file) t))

;;; refresh local file from server file
(defun synergy-synchronize-local-file (path)
  "Synchronize local FILE with server file."
  (synergy-run-async-command (concat synergy-ccm-command " reconcile -update_wa -recurse " file) t))

;;; set current task
(defun synergy-set-task-as-current (task)
  "Set the current task with TASK-NUMBER."
  ;; get number
  (setq task (synergy-get-task-number task))
  (when task
    ;; set current task
    (synergy-run-async-command (concat synergy-ccm-command " task -current " task))))

;;; get current task
(defun synergy-get-current-task ()
  "Return the current task."
  (let* (ret (task) (number) (release) (synopsis))
    ;; get current task
    (setq task (shell-command-to-string (concat synergy-ccm-command " task -current")))
    (if (and
          (string-match "^\\([0-9]+\\)\\s-*:\\s-*\\(.*\\)$" task)
          ;; get number of current task
          (setq number (match-string 1 task))
          ;; get synopsis of current task
          (setq synopsis (match-string 2 task)))
      (progn
        (if (string-match "^\\(.\\{0,27\\}\\)....+$" synopsis)
          ;; truncate synopsis to 30 character
          (setq synopsis (concat (match-string 1 synopsis) "...")))
        ;; get release of current task
        (setq release (synergy-get-task-release number))
        ;; return a concat of number, release and synopsis
        (concat number ": [" release "] " synopsis))
      (progn
        (message "Set a current task with `synergy-set-current-task'.")
        nil))))

;;; get only the number from a task string
(defun synergy-get-task-number (task-string)
  "Return the task number from task format string TASK-STRING (task_number:
task_synopsis)."
  (let* (ret)
    ;; try to match task format "#: task synopsis"
    (if (and (string-match "\\([0-9]+\\)\\(\\s-*:\\{0,1\\}.*\\)\\{0,1\\}" task-string)
          ;; get digit matched
          (setq ret (match-string 1 task-string)))
      () ; nothing to do
      (progn
        ;; not matched a task format
        (setq ret nil)
        (message (concat "Wrong task format [number: synopsis]: " task-string))))
    ret))

;;; get all assigned tasks in a list
(defun synergy-get-assigned-task ()
  "Get all assigned tasks in a list of string."
  (let* (ret)
    ;; get all assigned task from synergy
    (setq ret (shell-command-to-string (concat synergy-ccm-command " task -query -task_scope all_my_assigned"
                                         " -format \"%displayname: [%release] %{task_synopsis[truncate=30]}\"")))
    ;; remove #) which start each line
    (setq ret (replace-regexp-in-string "^[0-9]+)\\s-*" "" ret))
    ;; create a list of each task
    (setq ret (split-string ret "\n" t))
    ret))

;;; get current release from current file or directory
(defun synergy-get-current-release ()
  "Return the release of the current file or directory."
  ;; get file name
  (let* ((ret nil) (release) (my-filename (synergy-get-buffer-file-name)))
    (when my-filename
      ;; when file name exist in synergy get release
      (setq release (shell-command-to-string (concat synergy-ccm-command " properties -format \"%release\" " my-filename)))
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
      (setq release (shell-command-to-string (concat synergy-ccm-command " properties -format \"%release\" " my-filename)))
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
    (setq ret (shell-command-to-string (concat synergy-ccm-command " release -list -active")))
    ;; remove #) which start each line
    (setq ret (replace-regexp-in-string "^[0-9]+)\\s-*" "" ret))
    ;; create a list of each task
    (setq ret (split-string ret "\n" t))
    ret))

;;; get release of a task
(defun synergy-get-task-release (task)
  "Get release of a task."
  (let* (ret (release))
    ; ;get task number
    (setq task (synergy-get-task-number task))
    ;; get release of task from synergy
    (setq release (shell-command-to-string (concat synergy-ccm-command " task -show release " task)))
    ;; check right return value
    (if (string-match "^Task [0-9]+\\s-*:\\s-*\\(.*\\)$" release)
      ;; get release
      (setq ret (match-string 1 release))
      (progn
        ;; return nil
        (setq ret nil)
        ;; display error message
        (message (concat "Error to get release from task: " task))))
    ret))

;;; test when file/directory is from a synergy project
(defun synergy-in-a-project (path)
  "Return the PATH of the object when it is in a synergy project otherwise nil."
  ;; get the default directory
  (let* (ret)
    ;; test when synergy return "Invalid value"
    (if (string-match "^Invalid value" (shell-command-to-string (concat synergy-ccm-command " properties -format \"%displayname\" " path)))
      nil
      t)))

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
      (progn
        (setq ret my-buffer)
        ;; set in windows path when in MS Windows
        ;;(when (string-equal system-type "windows-nt")
        ;;  (setq my-buffer (replace-regexp-in-string "/" "\\\\" my-buffer)))
        )
      ;; the current file is not from synergy
      (progn
        (message (concat "This file or directory is not part of a synergy project: " buffer-file-name))
        (setq ret nil)))
    ret)) ; return value

;;; get the version of a file
(defun synergy-get-file-version (file)
  "Return the version of FILE."
  (let* (ret)
    ;; get version with synergy
    (setq ret (shell-command-to-string (combine-and-quote-strings (append (list synergy-ccm-command "properties" "-nocolumn_header" "-format" "VERSION %version" my-filename)))))
    ;; test value return by synergy
    (if (string-match "^VERSION\\s-*\\(.*\\)$" ret)
      ;; return version
      (setq ret (match-string 1 ret))
      ;; display error message
      (message (concat "Error to get version of file: " my-filename))
    ret)))

;;; get previous file
(defun synergy-get-previous-version (file)
  "Return a string which contains the previous of FILE."
  ;; init variable
  (let* (ret (my-previous))
    ;; get previous version of FILE
    (setq my-previous (shell-command-to-string
                        (combine-and-quote-strings
                          (list synergy-ccm-command "relate" "-show" "-name" "successor" "-to" file))))
    ;; when synergy return a predecessor
    (if (string-match "^.*~\\([^/]*\\):.*:.*\\s-*successor\\s-*.*~\\([^/]*\\):.*:.*$" my-previous)
      ;; get previous version
      (setq ret (match-string 1 my-previous))
      (progn
        ;; return nil
        (setq ret nil)
        ;; display error message
        (message (concat "The file do not have a predecessor: " file))))
    ret))

;;; get status of a file
(defun synergy-get-object-status (file)
  "Return the status of an object."
  ;; init variable
  (let* (ret (my-status))
    ;; get status of object
    (setq my-status (shell-command-to-string
                      (combine-and-quote-strings
                        (list synergy-ccm-command "properties" "-format" "\"%status\"" file))))
    ;; check if the status exists
    (if (string-match "working\\|prep\\|shared\\|visible\\|integrate\\|sqa\\|collaborative")
      ;; return the status
      (setq ret my-status)
      (progn
        ;; return nil
        (setq ret nil)
        ;; display error message
        (message "%s %s" "The object do not have a status:" file)))
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
    (setq ret (shell-command-to-string (concat synergy-ccm-command " query "
                                         "-format \"%displayname\" -type project -state working -owner " synergy-username)))
    ;; remove #) which start each line
    (setq ret (replace-regexp-in-string "^[0-9]+)\\s-*" "" ret))
    ;; create a list of each task
    (setq ret (split-string ret "\n" t))
    ret))

;;; get project ofcurrent file or directory
(defun synergy-get-current-project ()
  "Return the current project of file or directory."
  ;; get current filename
  (let* (ret (my-filename (synergy-get-buffer-file-name)))
    (when my-filename
      ;; when filename exist get project name
      (setq ret (shell-command-to-string (concat synergy-ccm-command " project " my-filename)))
      ;; remove new line character
      (setq ret (replace-regexp-in-string "\n$" "" ret)))
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

(provide 'function-synergy)

;;; function-synergy.el ends here

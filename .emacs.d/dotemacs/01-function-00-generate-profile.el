;;; 01-function-00-generate-profile.el --- functions to generate profile from config file

;; Copyright (c) 2017-2019 Claude Tete
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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; section comment
;; [HEADER.custom functions]
;; [DEFAULT.t]
;;
;;
;; subsection comment
;; [[SUBHEADER.functions to generate profile from all config file
;;
;; interactive function `tqnr-generate-profile' will parse dotemacs directory to
;; generate a default profile file or with a parameter not nil init-profile.el file
;; ]]
;; [SUBDEFAULT.t]


;;; Code:
;;
;; current buffer to generate profile file
(defvar tqnr-profile-buffer "*Taquouner profile*")
;; current section when loading files
(defvar tqnr-profile-section nil)
(defvar tqnr-profile-is-init nil)
(defvar tqnr-profile-depth-counter 0)
(defvar tqnr-profile-depth-counter-prev 0)


;; function call by file in dotemacs directory
(defun tqnr-process-config-file (file)
  "Fill buffer which contains generated profile file with config from FILE."
  (let (
         ;; extract filename from file path without extension
         (filename (file-name-base file))
         (section nil)
         (section-default nil)
         (subsection nil)
         (subsection-default nil)
         (symbol-full nil)
         (symbol-section nil))
    ;; build section and subsection names
    (when filename
      (setq section (build-section-name filename))
      (setq subsection (build-subsection-name filename)))
    ;; only when filename has a section
    (when section
      ;; build full section symbol useful for check if enable in profile
      (setq symbol-full (build-section-symbol section subsection))
      ;; build only section symbol useful for check if enable in profile
      (setq symbol-section (build-section-symbol section))

      ;; if section is a newer section
      (when (not (string= symbol-section tqnr-profile-section))
        (setq tqnr-profile-depth-counter 0)
        ;; when older section exists
        (when (> tqnr-profile-depth-counter-prev 0)
          ;; print end of section
          (tqnr-print-profile-end tqnr-profile-section)
          )

        (setq section-default (tqnr-print-profile-init file symbol-section))
        (tqnr-print-profile-set (concat symbol-section " " section-default))
        )

      ;; when filename has a subsection
      (when subsection
        (progn
          (when (= tqnr-profile-depth-counter 0)
            (tqnr-print-profile-begin symbol-section)
            ;; indent
            (setq tqnr-profile-depth-counter (+ tqnr-profile-depth-counter 1)))


          (setq subsection-default (tqnr-print-profile-init-subsection file symbol-full))

          ;; print that subsection is started
          (tqnr-print-profile-set (concat symbol-full " " subsection-default))
          )
        )

      ;; in all cases keep current section
      (setq tqnr-profile-section symbol-section)

      ;; print everything found in file
      (tqnr-parse-config-file file symbol-full)

      ;; keep last depth counter
      (setq tqnr-profile-depth-counter-prev tqnr-profile-depth-counter)
      )))

(defun tqnr-parse-config-file (file symbol)
  "Parse config file to extract profile data to insert into profile buffer."
  (let ((substring-start nil)
         (current-subsection "")
         (is-first t))
    (when (= tqnr-profile-depth-counter 1)
      (setq current-subsection symbol))
    ;; use a temp buffer
    (with-temp-buffer
      ;; open file in temp buffer
      (insert-file-contents file)
      ;; go to beginning of file
      (goto-char 1)
      ;; search for a useful tag (one '[' = one line, two '[' = multiple line)
      (while (re-search-forward ";\\s-*\\(\\[?\\[\\(?:COMMENT\\|SUBCOMMENT\\|SUBSECTION\\|VARCOMMENT\\|VARIABLE\\)\\.\\)" nil t)
        (when is-first
          )
        ;;(message (match-string 1))
        (setq substring-start (point))
        ;; match string correspond to which tag
        (cond
          ;;
          ;; SINGLE LINE
          ;;
          ((string-equal (match-string 1) "[COMMENT.")
            ;; to move point to last closing bracket on line
            (tqnr-profile-move-to-closing-bracket)
            ;; print what found between found tag and last bracket on line
            (tqnr-profile-print (concat ";; " (buffer-substring substring-start (point)) "\n")))

          ((string-equal (match-string 1) "[SUBCOMMENT.")
            ;; to move point to last closing bracket on line
            (tqnr-profile-move-to-closing-bracket)
            ;; when subsection is different of older subsection
            (cond
              ((= tqnr-profile-depth-counter 2)
                (setq tqnr-profile-depth-counter 1)
                ;; print end of subsection
                (tqnr-print-profile-end current-subsection)
                (setq current-subsection nil))
              ((= tqnr-profile-depth-counter 0)
                (tqnr-print-profile-begin tqnr-profile-section)
                ;; indent
                (setq tqnr-profile-depth-counter 1)))
            ;; print what found between found tag and last bracket on line
            (tqnr-profile-print (concat ";; " (buffer-substring substring-start (point)) "\n")))

          ((string-equal (match-string 1) "[SUBSECTION.")
            ;; to move point to last closing bracket on line
            (tqnr-profile-move-to-closing-bracket)
            (cond
              ;; when not first subsection in a section
              ((and (= tqnr-profile-depth-counter 2) current-subsection (not (string= current-subsection "")))
                (setq tqnr-profile-depth-counter 1)
                ;; print end of subsection
                (tqnr-print-profile-end current-subsection)
                (setq current-subsection nil))
              ;; when first subsection in a section
              ((= tqnr-profile-depth-counter 0)
                (tqnr-print-profile-begin tqnr-profile-section)
                ;; indent
                (setq tqnr-profile-depth-counter 1)))
            ;; keep subsection name
            (setq current-subsection (buffer-substring substring-start (point)))
            ;; print what found between found tag and last bracket on line
            (tqnr-print-profile-set current-subsection))

          ((string-equal (match-string 1) "[VARCOMMENT.")
            ;; to move point to last closing bracket on line
            (tqnr-profile-move-to-closing-bracket)
            (cond
              ((and (= tqnr-profile-depth-counter 1) current-subsection (not (string= current-subsection "")))
                ;; indent
                (setq tqnr-profile-depth-counter-prev tqnr-profile-depth-counter)
                (setq tqnr-profile-depth-counter (+ tqnr-profile-depth-counter 1))
                (tqnr-print-profile-begin (car (split-string current-subsection))))
              ((= tqnr-profile-depth-counter 0)
                (tqnr-print-profile-begin tqnr-profile-section)
                ;; indent
                (setq tqnr-profile-depth-counter (+ tqnr-profile-depth-counter 1))))
            ;; print what found between found tag and last bracket on line
            (tqnr-profile-print (concat ";; " (buffer-substring substring-start (point)) "\n")))

          ((string-equal (match-string 1) "[VARIABLE.")
            ;; to move point to last closing bracket on line
            (tqnr-profile-move-to-closing-bracket)
            (cond
              ((and (= tqnr-profile-depth-counter 1) current-subsection (not (eq current-subsection "")))
                ;; indent
                (setq tqnr-profile-depth-counter-prev tqnr-profile-depth-counter)
                (setq tqnr-profile-depth-counter (+ tqnr-profile-depth-counter 1))
                (tqnr-print-profile-begin (car (split-string current-subsection))))
              ((= tqnr-profile-depth-counter 0)
                (tqnr-print-profile-begin tqnr-profile-section)
                ;; indent
                (setq tqnr-profile-depth-counter (+ tqnr-profile-depth-counter 1))))
            ;; print what found between found tag and last bracket on line
            (tqnr-print-profile-set (buffer-substring substring-start (point))))

          ;;
          ;; MULTIPLE LINE
          ;;
          ((string-equal (match-string 1) "[[COMMENT.")
            ;; set point to end of line
            (end-of-line)
            ;; print what found between found tag and last bracket on line as symbol
            (tqnr-profile-print (concat ";; " (buffer-substring substring-start (point))) "\n")
            ;; move to next line
            (forward-line 1)
            ;; print raw multiple line until ]]
            (tqnr-profile-print-multiline ";; ")
            )

          ((string-equal (match-string 1) "[[SUBCOMMENT.")
            ;; to move end of line
            (end-of-line)
            ;; when subsection is different of older subsection
            (cond
              ((= tqnr-profile-depth-counter 2)
                (setq tqnr-profile-depth-counter 1)
                ;; print end of section
                (tqnr-print-profile-end current-subsection)
                (setq current-subsection nil))
              ((= tqnr-profile-depth-counter 0)
                (tqnr-print-profile-begin tqnr-profile-section)
                ;; indent
                (setq tqnr-profile-depth-counter 1)))
            ;; print what found between found tag and last bracket on line
            (tqnr-profile-print (concat ";; " (buffer-substring substring-start (point)) "\n"))
            ;; set point to end of line
            (end-of-line)
            ;; move to next line
            (forward-char)
            (tqnr-profile-print-multiline ";; ")
            )

          ((string-equal (match-string 1) "[[SUBSECTION.")
            ;; to move point to end of line
            (end-of-line)
            (cond
              ;; when new subsection
              ((and (= tqnr-profile-depth-counter 2) current-subsection (not (string= current-subsection "")))
                (setq tqnr-profile-depth-counter 1)
                ;; print end of section
                (tqnr-print-profile-end current-subsection))
              ((= tqnr-profile-depth-counter 0)
                (tqnr-print-profile-begin tqnr-profile-section)
                ;; indent
                (setq tqnr-profile-depth-counter 1)))
            ;; keep subsection name
            (setq current-subsection (buffer-substring substring-start (point)))
            ;; print what found between found tag and last bracket on line
            (tqnr-print-profile-set current-subsection t)
            ;; move to next line
            (forward-line 1)
            ;; print all next lines until "]]"
            (tqnr-profile-print-multiline)
            (setq tqnr-profile-depth-counter-prev (+ tqnr-profile-depth-counter-prev 1))
            ;; close definition or set of subsection
            (tqnr-print-profile-end-unit current-subsection t)
            (setq tqnr-profile-depth-counter-prev (- tqnr-profile-depth-counter-prev 1))
            )

          ((string-equal (match-string 1) "[[VARCOMMENT.")
            ;; to move end of line
            (end-of-line)
            (cond
              ((and (= tqnr-profile-depth-counter 1) current-subsection (not (string= current-subsection "")))
                ;; indent
                (setq tqnr-profile-depth-counter-prev tqnr-profile-depth-counter)
                (setq tqnr-profile-depth-counter (+ tqnr-profile-depth-counter 1))
                (tqnr-print-profile-begin (car (split-string current-subsection))))
              ((= tqnr-profile-depth-counter 0)
                (tqnr-print-profile-begin tqnr-profile-section)
                ;; indent
                (setq tqnr-profile-depth-counter (+ tqnr-profile-depth-counter 1))))
            ;; print what found between found tag and last bracket on line
            (tqnr-profile-print (concat ";; " (buffer-substring substring-start (point)) "\n"))
            ;; move to next line
            (forward-line 1)
            ;; print raw comment multiple line until ]]
            (tqnr-profile-print-multiline ";; ")
            )

          ((string-equal (match-string 1) "[[VARIABLE.")
            ;; to move point to end of line
            (end-of-line)
            (cond
              ((and (= tqnr-profile-depth-counter 1) current-subsection (not (eq current-subsection "")))
                ;; indent
                (setq tqnr-profile-depth-counter-prev tqnr-profile-depth-counter)
                (setq tqnr-profile-depth-counter (+ tqnr-profile-depth-counter 1))
                (tqnr-print-profile-begin (car (split-string current-subsection))))
              ((= tqnr-profile-depth-counter 0)
                (tqnr-print-profile-begin tqnr-profile-section)
                ;; indent
                (setq tqnr-profile-depth-counter (+ tqnr-profile-depth-counter 1))))
            (let ((variable-name (buffer-substring substring-start (point))))
              ;; print what found between found tag and last bracket on line
              (tqnr-print-profile-set variable-name t)
              ;; move to next line
              (forward-line 1)
              ;; print raw multiple line until ]]
              (tqnr-profile-print-multiline)
              (setq tqnr-profile-depth-counter-prev (+ tqnr-profile-depth-counter-prev 1))
              ;; close definition or set of subsection
              (tqnr-print-profile-end-unit variable-name t)
              (setq tqnr-profile-depth-counter-prev (- tqnr-profile-depth-counter-prev 1)))
            )
          (t t))
        (setq tqnr-profile-depth-counter-prev tqnr-profile-depth-counter)
        )
      (when (> tqnr-profile-depth-counter 1)
        (setq tqnr-profile-depth-counter 1)
        (tqnr-print-profile-end current-subsection))
      )))

(defun tqnr-print-profile-init (file section)
  "Print init comment of config file to insert into profile buffer."
  (let* ((substring-start nil)
          (is-first-comment t)
          (default-value "nil"))
    ;; new empty line without indentation
    (tqnr-profile-print "\n" t)
    (tqnr-profile-print (concat ";; " (mapconcat 'upcase (cdr (cdr (delete "" (split-string section "\\-")))) " ")))
    ;; use a temp buffer
    (with-temp-buffer
      ;; open file in temp buffer
      (insert-file-contents file)
      ;; go to beginning of file
      (goto-char 1)
      ;; search for a useful tag (one '[' = one line, two '[' = multiple line)
      (while (re-search-forward ";\\s-*\\(\\[?\\[\\(?:HEADER\\|DEFAULT\\)\\.\\)" nil t)
        (setq substring-start (point))
        ;; match string correspond to which tag
        (cond
          ;;
          ;; SINGLE LINE
          ;;
          ((string-equal (match-string 1) "[HEADER.")
            ;; to move point to last closing bracket on line
            (tqnr-profile-move-to-closing-bracket)
            (if is-first-comment
              (progn
                ;; add separator ": " without indentation
                (tqnr-profile-print ": " t)
                (setq is-first-comment nil))
              (progn
                ;; new empty line without indentation
                (tqnr-profile-print "\n" t)
                (tqnr-profile-print ";; ")))
            ;; print what found between found tag and last bracket on line without indentation
            (tqnr-profile-print (concat (buffer-substring-no-properties substring-start (point)) "\n") t))

          ;;
          ;; MULTIPLE LINE
          ;;
          ((string-equal (match-string 1) "[[HEADER.")
            ;; set point to end of line
            (end-of-line)
            (when is-first-comment
              (tqnr-profile-print "\n" t)
              (setq is-first-comment nil))
            ;; print what found between found tag and last bracket on line as symbol
            (tqnr-profile-print (concat ";; " (buffer-substring-no-properties substring-start (point)) "\n"))
            ;; move to next line
            (forward-line 1)
            ;; print raw multiple line until ]]
            (tqnr-profile-print-multiline ";; ")
            )

          ((string-equal (match-string 1) "[DEFAULT.")
            ;; to move point to last closing bracket on line
            (tqnr-profile-move-to-closing-bracket)
            ;; return default value
            (setq default-value (buffer-substring-no-properties substring-start (point))))

          (t t)))
      (when is-first-comment
        (tqnr-profile-print "\n"))
      )
    default-value))

(defun tqnr-print-profile-init-subsection (file subsection)
  "Print init comment of config file subsection to insert into profile buffer."
  (let* ((substring-start nil)
          (is-first-comment t)
          (default-value "nil"))
    ;; new empty line without indentation
    (tqnr-profile-print "\n" t)
    (tqnr-profile-print (concat ";; " (mapconcat 'upcase (cdr (cdr (cdr (delete "" (split-string subsection "\\-"))))) " ")))
    ;; use a temp buffer
    (with-temp-buffer
      ;; open file in temp buffer
      (insert-file-contents file)
      ;; go to beginning of file
      (goto-char 1)
      ;; search for a useful tag (one '[' = one line, two '[' = multiple line)
      (while (re-search-forward ";\\s-*\\(\\[?\\[\\(?:SUBHEADER\\|SUBDEFAULT\\)\\.\\)" nil t)
        (setq substring-start (point))
        ;; match string correspond to which tag
        (cond
          ;;
          ;; SINGLE LINE
          ;;
          ((string-equal (match-string 1) "[SUBHEADER.")
            ;; to move point to last closing bracket on line
            (tqnr-profile-move-to-closing-bracket)
            (if is-first-comment
              (progn
                ;; add separator ": " without indentation
                (tqnr-profile-print ": " t)
                (setq is-first-comment nil))
              (progn
                ;; new empty line without indentation
                (tqnr-profile-print "\n" t)
                (tqnr-profile-print ";; ")))
            ;; print what found between found tag and last bracket on line without indentation
            (tqnr-profile-print (concat (buffer-substring-no-properties substring-start (point)) "\n") t))

          ;;
          ;; MULTIPLE LINE
          ;;
          ((string-equal (match-string 1) "[[SUBHEADER.")
            (message subsection)
            ;; set point to end of line
            (end-of-line)
            (when is-first-comment
              (tqnr-profile-print "\n" t)
              (setq is-first-comment nil))
            ;; print what found between found tag and last bracket on line as symbol
            (tqnr-profile-print (concat ";; " (buffer-substring-no-properties substring-start (point)) "\n"))
            ;; move to next line
            (forward-line 1)
            ;; print raw multiple line until ]]
            (tqnr-profile-print-multiline ";; ")
            )

          ((string-equal (match-string 1) "[SUBDEFAULT.")
            ;; to move point to last closing bracket on line
            (tqnr-profile-move-to-closing-bracket)
            ;; return default value
            (setq default-value (buffer-substring-no-properties substring-start (point))))

          (t t)))
      (when is-first-comment
        (tqnr-profile-print "\n"))
      )
    default-value))

(defun tqnr-profile-print-multiline (&optional prefix)
  "Insert in profile buffer raw multiline without ;; prefix of comment."
  (let ((is-next-line t))
    (when (not prefix) (setq prefix ""))
    (while is-next-line
      ;; move to first non space character in line
      (skip-chars-forward " \t")
      ;; move to first ; in line
      (skip-chars-forward ";")
      ;; remove one space after ;;
      (when (looking-at "[ \t]")
        (forward-char))
      (if (looking-at "\\s-*]]")
        (setq is-next-line nil)
        (progn
          ;; keep point position
          (setq substring-start (point))
          ;; set point to end of line
          (end-of-line)
          (when (or (not tqnr-profile-is-init) prefix)
            (tqnr-profile-print (concat prefix (buffer-substring substring-start (point)) "\n")))
          (setq is-next-line (= 0 (forward-line 1))))))))

(defun tqnr-profile-move-to-closing-bracket ()
  "Move point to closing bracket at end of line."
  (let ()
    ;; until there ] at end of line
    (while (not (looking-at "\\]\\s-*?\n"))
      ;; move to next closing bracket
      (skip-chars-forward "^]"))))

(defun tqnr-profile-print (string &optional no_indent)
  "Insert STRING directly in profile buffer only with indentation modification."
  (let ()
    (with-current-buffer tqnr-profile-buffer
      (if no_indent
        (insert string)
        (insert (concat (tqnr-profile-indent tqnr-profile-depth-counter) string))))))

(defun tqnr-profile-indent (depth)
  "Insert right indentation regarding DEPTH."
  (let ()
    (with-current-buffer tqnr-profile-buffer
      (while (> depth 0)
        (insert "  ")
        (setq depth (- depth 1))))))

(defun tqnr-print-profile-set (symbol &optional multiline)
  "Fill profile buffer for start of section/subsection/variable."
  (let ((string-end (if multiline "" ")")))
    ;; use profile buffer to insert everything
    (with-current-buffer tqnr-profile-buffer
      ;; add symbol definition or set
      (if tqnr-profile-is-init
        (insert (concat (tqnr-profile-indent tqnr-profile-depth-counter) "(defvar " (car (split-string symbol " ")) " nil)\n"))
        (insert (concat (tqnr-profile-indent tqnr-profile-depth-counter) "(setq " symbol string-end "\n")))
      )))

(defun tqnr-print-profile-begin (symbol &optional multiline)
  "Fill profile buffer for start of section/subsection/variable."
  (let ((string-end (if multiline "" ")")))
    ;; use profile buffer to insert everything
    (with-current-buffer tqnr-profile-buffer
      ;; add a indentation using progn or when
      (if tqnr-profile-is-init
        (insert (concat (tqnr-profile-indent tqnr-profile-depth-counter-prev) "(progn ;; " symbol "\n"))
        (insert (concat (tqnr-profile-indent tqnr-profile-depth-counter-prev) "(when " symbol "\n")))
      )))

(defun tqnr-print-profile-end (symbol)
  "Fill profile buffer for end of section/subsection/variable."
  (let ()
    ;; use profile buffer to insert everything
    (with-current-buffer tqnr-profile-buffer
      ;; previous depth is higher than current depth
      (when (< tqnr-profile-depth-counter tqnr-profile-depth-counter-prev)
        (while (not (eq tqnr-profile-depth-counter tqnr-profile-depth-counter-prev))
          (tqnr-print-profile-end-unit symbol)
          ;; end of indentation so decrement depth prev
          (setq tqnr-profile-depth-counter-prev (- tqnr-profile-depth-counter-prev 1)))))))

(defun tqnr-print-profile-end-unit (symbol &optional definition)
  "Print only one end of block about section/subsection/variable."
  (let ()
    ;; use profile buffer to insert everything
    (with-current-buffer tqnr-profile-buffer
      (if definition
        ;; add a indentation using progn or when
        (if tqnr-profile-is-init
          (insert (concat (tqnr-profile-indent tqnr-profile-depth-counter-prev) ") ;; (defvar " symbol "\n"))
          (insert (concat (tqnr-profile-indent tqnr-profile-depth-counter-prev) ") ;; (setq " symbol "\n")))
        ;; add a indentation using progn or when
        (if tqnr-profile-is-init
          (insert (concat (tqnr-profile-indent tqnr-profile-depth-counter-prev) ") ;; (progn ;; " (car (split-string symbol " ")) "\n"))
          (insert (concat (tqnr-profile-indent tqnr-profile-depth-counter-prev) ") ;; (when " (car (split-string symbol " ")) "\n")))))))

(defun tqnr-generate-profile (&optional is-init)
  "list of .el files in dotemacs directory"
  (interactive)
  (let ((dotemacs-files (directory-files
                          (concat (file-name-as-directory dotemacs-path) "dotemacs/")
                          t
                          ".el$")))
    (setq tqnr-profile-is-init is-init)
    ;; create buffer of generated profile
    (get-buffer-create tqnr-profile-buffer)
    ;; empty buffer if already exists
    (with-current-buffer tqnr-profile-buffer
      (funcall 'lisp-mode)
      (erase-buffer))

    ;; remove ".el" extension of all filepath
    (setq dotemacs-files (mapcar (lambda (file) (substring file 0 (- (length file) 3))) dotemacs-files))
    ;; sort using human sort
    (setq dotemacs-files (sort dotemacs-files 'dictionary-lessp))
    ;; add ".el" extension to all filepath
    (setq dotemacs-files (mapcar (lambda (file) (concat file ".el")) dotemacs-files))
    ;; call `tqnr-process-config-file' for each found files
    (mapc 'tqnr-process-config-file dotemacs-files)

    ;; if section is a newer section
    (when (not (eq tqnr-profile-section nil))
      (setq tqnr-profile-depth-counter 0)
      ;; when older section exists
      (when (> tqnr-profile-depth-counter-prev 0)
        ;; print end of section
        (tqnr-print-profile-end tqnr-profile-section))
      (setq tqnr-profile-section nil))

    ;; show temp buffer
    (switch-to-buffer-other-window tqnr-profile-buffer)
    )
  )


(provide '01-function-00-generate-profile)

;;; 01-function-00-generate-profile.el ends here

;;; init-function.el --- init config file for init function about GNU Emacs

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
;; Useful to define function which parse file in dotemacs to load it or not
;; regarding chosen profile
;;

;;; Change Log:
;; 2017-07-04 (0.1)
;;    create from old emacs.el


;;; Code:
;; useful to have seq-filter
(require 'seq)

;;
;; LOAD ALL DEFINITION
;; current section when loading files
(defvar require-file-section nil)


;; function call by file in dotemacs directory
(defun require-file (file)
  "Attempt to load file wrapped with message of section/subsection indicated in FILE name. \
00-toto.el will print \"00 Toto...\" load file and print \"00 Toto..Done\". \
02-toto-titi-00-tata will print \"02 Toto Titi...\" then \"  00 Tata...\", load file \
and print \"  00 Tata..Done\" then \"02 Toto Titi..Done\"."
  (let (
         ;; extract filename from file path without extension
         (filename (file-name-base file))
         (section nil)
         (subsection nil)
         (symbol-full nil)
         (symbol-section nil))
    ;; build section and subsection names
    (when filename
      (setq section (build-section-name filename))
      (setq subsection (build-subsection-name filename)))
    ;; only when filename start with decimal
    (when section
      ;; build full section symbol useful for check if enable in profile
      (setq symbol-full (build-section-symbol section subsection))
      ;; build only section symbol useful for check if enable in profile
      (setq symbol-section (build-section-symbol section))

      ;; if section is a newer section
      (when (not (string= (mapconcat 'identity section " ") (mapconcat 'identity require-file-section " ")))
        ;; when older section exists
        (when require-file-section
          ;; print that older section loading is done
          (require-print-end require-file-section)
          ;; print almost empty line
          (message " "))
        ;; print that newer section is started
        (require-print-begin section))

      ;; if subsection is set
      (if subsection
        (progn
          ;; print that subsection is started
          (require-print-begin subsection "  ")
          ;; try to load only if enabled
          ;; (intern-soft) returns variable names as symbol
          ;; (symbol-value) returns variable value name
          (if (and (symbol-value (intern-soft symbol-section)) (symbol-value (intern-soft symbol-full)))
            ;; try to load filename
            (try-require (intern filename) "  ")
            (message (concat "  ## " symbol-full " not set")))
          ;; print that subsection loading is done
          (require-print-end subsection "  "))
        ;; try to load only if enabled
        ;; (intern-soft) returns variable names as symbol
        ;; (symbol-value) returns variable value name
        (if (symbol-value (intern-soft symbol-full))
          ;; try to load filename
          (try-require (intern filename) "  ")
          (message (concat "  ## " symbol-full " not set"))))

      ;; in all cases keep current section
      (setq require-file-section section))))

(defun build-section-symbol (section &optional subsection)
  "Build symbol name from SECTION and SUBSECTION."
  (let ((section-name (seq-filter (lambda (element) (string-match "^[^0-9]+" element)) section))
         (subsection-name (seq-filter (lambda (element) (string-match "^[^0-9]+" element)) subsection)))
    (if subsection
      (mapconcat 'identity (append (list "tqnr" "section") section-name subsection-name) "-")
      (mapconcat 'identity (append (list "tqnr" "section") section-name) "-"))))

(defun build-section-name (filename)
  "Build list of section names including number extracted from FILENAME."
  (let ()
    ;; only when filename match by example "00-toto.el", "01-toto-tata.el", "02-toto-00-tata.el", etc
    (if (string-match "^\\([0-9]+\\)-\\([^0-9]+\\)" filename)
      ;; build string of filename to be print "00-toto" -> "00 Toto"
      (append (list (match-string 1 filename))
        (delete "" (split-string (match-string 2 filename) "[-]")))
      nil)))

(defun build-subsection-name (filename)
  "Build list of subsection names including number extracted from FILENAME."
  (let ()
    ;; only when filename match by example "02-toto-00-tata.el" but not "00-toto.el"
    (if (string-match "-\\([0-9]+\\)-\\(.+\\)$" filename)
      ;; build indented string of filename to be print "02-toto-00-tata" -> "  00 Tata"
      ;; "02 Toto" must be print before
      (append (list (match-string 1 filename))
        (split-string (match-string 2 filename) "[-]"))
      nil)))

(defun require-print-begin (list &optional indent)
  "Print on message when start loading"
  (let ()
    ;; default value of INDENT parameter is "" empty string
    (when (not indent) (setq indent ""))
    ;; concat each element seperated by a space
    (message (concat indent (mapconcat 'capitalize list " ") "..."))))

(defun require-print-end (list &optional indent)
  "Print on message when end loading"
  (let ()
    ;; default value of INDENT parameter is "" empty string
    (when (not indent) (setq indent ""))
    ;; concat each element seperated by a space
    (message (concat indent (mapconcat 'capitalize list " ") "... Done"))))

;; sort with natural/human order like a dictionary (by example "spell" is before "spelling")
;; take from https://stackoverflow.com/questions/1942045/natural-order-sort-for-emacs-lisp
;; usage: (setq my-list (sort my-list 'dictionary-lessp))
(defun dictionary-lessp (str1 str2)
  "return t if STR1 is < STR2 when doing a dictionary compare
(splitting the string at numbers and doing numeric compare with them)"
  (let ((str1-components (dict-split str1))
        (str2-components (dict-split str2)))
    (dict-lessp str1-components str2-components)))
(defun dict-lessp (slist1 slist2)
  "compare the two lists of strings & numbers"
  (cond
    ((null slist1)
      (not (null slist2)))
    ((null slist2)
      nil)
    ((and (numberp (car slist1))
       (stringp (car slist2)))
      t)
    ((and (numberp (car slist2))
       (stringp (car slist1)))
      nil)
    ((and (numberp (car slist1))
       (numberp (car slist2)))
      (or (< (car slist1) (car slist2))
        (and (= (car slist1) (car slist2))
          (dict-lessp (cdr slist1) (cdr slist2)))))
    (t
      (or (string-lessp (car slist1) (car slist2))
        (and (string-equal (car slist1) (car slist2))
          (dict-lessp (cdr slist1) (cdr slist2)))))))
(defun dict-split (str)
  "split a string into a list of number and non-number components"
  (save-match-data
    (let ((res nil))
      (while (and str (not (string-equal "" str)))
        (let ((p (string-match "[0-9]*\\.?[0-9]+" str)))
          (cond
            ((null p)
              (setq res (cons str res))
              (setq str nil))
            ((= p 0)
              (setq res (cons (string-to-number (match-string 0 str)) res))
              (setq str (substring str (match-end 0))))
            (t
              (setq res (cons (substring str 0 (match-beginning 0)) res))
              (setq str (substring str (match-beginning 0)))))))
      (reverse res))))

;; function called on dotemacs directory which call `require-file' function for each files in dotemacs directory
(defun require-files (dir)
  "Load all file started with decimal in DIR"
  ;; list of .el files in dotemacs directory
  (let ((dotemacs-files (directory-files dir t ".el$")))
    ;; remove ".el" extension of all filepath
    (setq dotemacs-files (mapcar (lambda (file) (substring file 0 (- (length file) 3))) dotemacs-files))
    ;; sort using human sort
    (setq dotemacs-files (sort dotemacs-files 'dictionary-lessp))
    ;; add ".el" extension to all filepath
    (setq dotemacs-files (mapcar (lambda (file) (concat file ".el")) dotemacs-files))

    ;; ;; comment (mapc) and uncomment it to measure time to load each files
    ;;  (require 'benchmark)
    ;;  (dolist (x dotemacs-files)
    ;;    (message "%f" (* 1000 (car (benchmark-run 1 (require-file x)))))
    ;;    )

    ;; call `require-file' for each found files
    (mapc 'require-file dotemacs-files)

    ;; when last section exists
    (when require-file-section
      ;; print that last section loading is done
      (require-print-end require-file-section)
      (setq require-file-section nil))))


(provide 'init-function)

;;; init-function.el ends here

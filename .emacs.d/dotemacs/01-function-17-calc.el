;;; 01-function-17-calc.el --- functions to calculate/convert units -*- lexical-binding: t -*-

;; Copyright (c) 2018-2019 Claude Tete
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;

;; Author: Claude Tete <claude.tete@gmail.com>
;; Version: 0.1
;; Created: January 2018
;; Last-Updated: March 2019

;;; Commentary:
;;
;; subsection comment
;; [SUBHEADER.functions to add type support to helm ag for ripgrep]
;; [SUBDEFAULT.nil]


;;; Code:
;;

;; BUILD
(defun calc-hms-to-deg (hms-string)
  "Convert Degre-Hour-Minute-Second format into degres decimal."
  (interactive)

  (let ((hms (split-string hms-str "[°\'\"NEWS ]" t)))
    (flet ((to-deg ()
             (string-to-number
               (calc-eval (format "deg(%s@ %s' %s\")" (pop hms) (pop hms) (pop hms)))
               )
             )
            )
      (list (to-deg) (to-deg))
      )
    )
  )

(defun calc-deg-hm-toggle (input)
  (interactive)
  (let ((without_comma (s-replace "," "." input))
         (deg (if (s-match "[NEWS°@ \.]")
                (car (s-split "[NEWS°@ \.]" without_comma t))
                ()
        )
           )
         (min) (sec) (N-S) (E-W))
    (if (s-match "[°\'\"NEWS @]" without_comma)
      (if (s-match "[°@]")
        (setq deg)
        )
           ())
  ))

(defun calc-get-direction (string)
  (cond
    ((s-contains "N" string) "N")
    ((s-contains "E" string) "E")
    ((s-contains "W" string) "W")
    ((s-contains "S" string) "S")
    ((s-contains "" string) "S")
    )
  )

(defun calc-get-deg (string)
  (if (s-match "[NEWS]" string)
    (car (s-split "[NEWS°@ \.]" string t))
    ()
    )
  )

(defun calc-deg-to-hm (deg-string)
  "Convert Degree Decimal to Degre-Hour-Minute format."
  (interactive)
  (let (degree)
    (setq degree (string-to-number (calc-eval (format "hms(%s)"))))
    )
  )

(calc-eval "46.42 % 1")
(calc-eval (math-convert-units (calc-eval "46.42" 'raw)
                               (calc-eval "hms" 'raw)))
(calc-eval "hms(46.42)")
(message (calc-eval 1 'top))

(message (calc-eval "deg(46@ 30\' 30\")"))
(message (calc-eval "hms(46.1833332)"))

;; in Calc ("u c" to convert units)
;; Nautical mile:    nmi
;; Meter:            m
;; Foot:             ft
;;
;; example:
;; - Nautical mile to meter: 1 RET u c nmi m RET        => 1852
;; - Meter to nautical mile: 1852 RET u c m nmi RET     => 1

;; in Calc ("c d" to convert HMS format to degree)
;; example:
;; - HMS to degree:  45@ 32' 45.12' RET c d             => 45.5458666667

;; in Calc
;; C-M-w to copy current whole line
;;

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '01-function-17-calc)

;;; 01-function-17-calc.el ends here

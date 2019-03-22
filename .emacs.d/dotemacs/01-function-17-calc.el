;;; 01-function-17-calc.el --- functions to calculate/convert units

;; Copyright (c) 2018-2019 Claude Tete
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
  (let ((hms (split-string hms-str "[°′″NW ]" t)))
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

(defun calc-deg-to-hm (deg-string)
  "Convert Degree Decimal to Degre-Hour-Minute format."
  (interactive)
  (let (degree)
    (setq degree (string-to-number (calc-eval (format "hms(%s)"))))
    )
  )

(message (calc-eval "deg(46@ 10.99\")"))
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

;;; project.el --- a config file for ede project settings

;; Copyright (c) 2011, 2012 Claude Tete
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

;; Keywords: config, ede, cedet, project, ide
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.2
;; Created: July 2011
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `mode-semantic.el' (all requirements are defined in `emacs.el')
;; REQUIREMENT: var     `section-mode-cedet-semantic'
;;              var     `section-mode-cedet'

;;; Change Log:
;; 2012-03-02 (1.2)
;;    add project for AT
;; 2011-09-02 (1.1)
;;    add project for MM
;; 2011-08-13 (1.0)
;;    use external file for define project
;; 2011-07-10 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; the order is important: display in reverse order (first->last)

(cond
  ;; Magneti Marelli -----------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
    ;; XL1
    (load-file "d:/ccm_wa/XL1/XL1.ede.el")

    ;; NSF
    (load-file "d:/ccm_wa/NSF/NSF/NSF_CLIENT.ede.el")

    ;; NBNF_LL
    (load-file "d:/ccm_wa/NBNF/NBNF_LL/NBNFLL.ede.el")

    ;; NBNF_HL
    (load-file "d:/ccm_wa/NBNF/NBNF_HL/NBNF_HL.ede.el")

    ;; ECAR
    (load-file "d:/ccm_wa/ECAR/ENSF.ede.el")

    ;; PQ36 HL
    (load-file "d:/ccm_wa/PQ36/PQ36_HL/PQ35GPHL.ede.el")
    ) ; Magneti Marelli

  ;; Alstom Transport ----------------------------------------------------------
  ((string= clt-working-environment "Alstom Transport")
    ;; PM4S
    (load-file "m:/e_ctete/a2kc/soft/ccn4/ccn4_pm4s/PM4S.ede.el")

    ;;;; PM4S
    ;;(load-file "d:/Users/ctete/PM4S/src/1.2.0_RC01/PM4S.ede.el")
    ) ; Alstom Transport

  ;; default -------------------------------------------------------------------
  ((string= clt-working-environment "default")
    ) ; default

  ) ; cond ---------------------------------------------------------------------

;;; project.el ends here

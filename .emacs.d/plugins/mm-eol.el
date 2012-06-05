;;; mm-eol.el --- generic mode to coloration of End Of Line file of Magneti
;;; Marelli tool

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

;; Keywords: mode, coloration, dbc, vector, canoe
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.1
;; Created: March 2011
;; Last-Updated: June 2012

;;; Commentary:
;; only colors, do not syntax

;;; Change Log:
;; 2012-06-05 (1.1)
;;    update header + add provide
;; 2011-04-06 (1.0)
;;    fix some bug
;; 2011-03-04 (0.1)
;;    creation from scratch

(require 'generic-x) ;; we need this

(define-generic-mode
  ;; name
  'mm-eol-mode

  ;; comments start with '//'
  '("//")

  ;; keywords
  '("MESSAGERIE DIAGNOSTIC" "INFORMATION"
     "RESEAU" "REQUETE_ID" "REPONSE_ID"
     "SERVICE"
     "SESSION" "OUVRIR" "MAINTENIR" "FERMER")

  '(
     ;; '=' is an operator
     ("=" . 'font-lock-operator)

     ;; ';' is a a built-in
     (";" . 'font-lock-builtin)

     ;; number decimal and hex
     ("\\<\\(0x[0-9ABCDEF]+\\|[0-9]+\\)\\>" . 'font-lock-constant-face)

     ;; function
     ("FONCTION" . 'font-lock-function-name-face)

     ;; variable in service|information|reseau
     ("TYPE\\|REFERENCE\\|DATE\\|MNEMONIC\\|LOCAL_ID\\|DONNEES_RE[QP]" . 'font-lock-variable-name-face)

     ;; value of some variable
     ("CAN\\|STRBLID_UDS\\|RRRBLID_UDS\\|PREC_RE[PQ]\\|UNU\\|CST\\|ASC\\|BCD\\|BMP\\|HEX\\|UNM\\|SNM" . 'font-lock-type-face)

     )

  ;; files for which to activate this mode
  '("EOL_manual_Command\\.txt$")

  ;; other functions to call
  nil

  ;; doc string for this mode
  "A mode for Magneti Marelli EOL files"
  )


(provide 'mm-eol)

;;; mm-eol.el ends here

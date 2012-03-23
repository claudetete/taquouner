;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @NAME:               mm-eol.el                                             ;;
;; @CREATED_BY:         Claude TETE                                           ;;
;; @MAIL:               claude.tete@gmail.com                                 ;;
;; @ABOUT:              generic mode to coloration of End Of Line file of     ;;
;;                      Magneti Marelli tool                                  ;;
;; @VERSION:            1.0                                                   ;;
;; @DATE:               2011 April                                            ;;
;; @DATE_CREATION:      2011 March                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @CHANGELOG:          1.0 fix some bug                                      ;;
;;                      0.1 creation from scratch                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @EXPLANATION:                                                              ;;
;;                                                                            ;;
;;   This program is free software; you can redistribute it and/or modify it  ;;
;;   under the terms of  the GNU General Public License  as published by the  ;;
;;   Free Software Foundation ;  either version 3, or  (at your option)  any  ;;
;;   later version.                                                           ;;
;;                                                                            ;;
;;   This program  is distributed  in the hope  that it will be  useful,      ;;
;;   but  WITHOUT ANY WARRANTY ;  without even  the implied  warranty of      ;;
;;   MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR  PURPOSE.  See the      ;;
;;   GNU General Public License for more details.                             ;;
;;                                                                            ;;
;;   You should have received  a copy of the  GNU  General Public License     ;;
;;   along with this program; see the file COPYING.  If not, write to the     ;;
;;   Free Software Foundation,  Inc.,  51  Franklin Street,  Fifth Floor,     ;;
;;   Boston, MA 02110-1301, USA.                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

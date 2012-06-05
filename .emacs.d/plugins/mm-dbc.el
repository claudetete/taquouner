;;; mm-dbc.el --- generic mode to coloration of CAN dbc file

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
  'mm-dbc-mode

  ;; comments start with '//'
  '("//")

  ;; keywords
  '("VERSION" "NS_" "BS_" "BO_" "SG_" "BA_DEF_" "BA_DEF_DEF_" "BA_DEF_REL_" "BA_DEF_DEF_REL_" "BA_" "VAL_" "BA_REL_" "BU_SG_REL_" "NS_DESC_" "CAT_DEF_" "CAT_" "FILTER" "EV_DATA_" "ENVVAR_DATA_" "SGTYPE_" "SGTYPE_VAL_" "BA_DEF_SGTYPE_" "BA_SGTYPE_" "SIG_TYPE_REF_" "VAL_TABLE_" "SIG_GROUP_" "SIG_VALTYPE_" "SIGTYPE_VALTYPE_" "BO_TX_BU_" "BU_EV_REL_" "BU_BO_REL_" "SG_MUL_VAL_"
     )

  '(
     ;; '=' is an operator
     ("=" . 'font-lock-operator)

     ;; ';' is a a built-in
     (";" . 'font-lock-builtin)

     ;; function
     ("[[:alnum:]_]+:" . 'font-lock-function-name-face)

     ;; value of some variable
     ("[^_]\\<\\(ACC_Radar\\|AFS\\(_Skoda\\)\\{0,1\\}\\|Airbag\\|Allrad_RPU\\|Allrad_T5GP\\|BAP_Tester\\|BCM_Gateway\\|BCM_Max_Gateway\\|BMS_VW120_7\\|Bremse_ABS_82\\(_RKA\\)\\{0,1\\}\\|Bremse_ABS_VW120\\|Bremse_ESP_82i\\|Bremse_ESP_VW120\\|Bremse_MK25AESP\\|Bremse_MK25EABS\\|Bremse_MK60EC\\|Bremse_MK70MABS\\|CV_Sensor\\|Fahrtenschreiber\\|Gateway_separat\\|Getriebe_AQ250\\|Getriebe_DQ\\|Getriebe_ZF_8HPx\\|i_Booster_VW120_7\\|Lademanager_VW120_7\\|LEH_VW120_7\\|LH_CEPS\\|LH_EHPS\\|LH_EPS_ZF_3G\\|LPG_Interface_Box\\|LWS\\|Motor_Diesel\\|Motor_Otto\\|Motor_VW120_7\\|PDC_PLA\\|Quersperre_RPU\\|SAK\\|SWA\\|Waehlhebel\\|Zusatzanzeige\\|BCM_Max_4Gateway\\|BCM_Max_3Gateway\\|BFM\\|EBKV_VW120_7\\|Ladegeraet_VW120_7\\)\\>" . 'font-lock-variable-name-face)

     ;;
     ("[^_]\\<Kombi\\(_VW120_7\\|_VW120\\|_Highline\\|_alt\\|_NSF\\|_Roadmap\\)\\{0,1\\}\\>" . 'font-lock-comment-face)

     ("\\<Vector__XXX\\>" . 'default)

     ;; variable in service|information|reseau
;     ("\\<[A-Z0-9]\\{2,3\\}_[[:alnum:]_]+\\>\\( :\\)\\{,1\\}" . 'font-lock-type-face)
     ("\\(\\<[[:alnum:]]+_[[:alnum:]_]+\\>\\( :\\)\\{0,1\\}\\)" . 'font-lock-type-face)


     ;; number decimal and hex
     ("\\<\\(0x\\)\\{0,1\\}[0-9ABCDEF@]+\\>" . 'font-lock-constant-face)


     ("CM_" . 'font-lock-comment-face)

     )

  ;; files for which to activate this mode
  '("\\.dbc$")

  ;; other functions to call
  nil

  ;; doc string for this mode
  "A mode for Magneti Marelli CAN DBC files"
  )


(provide 'mm-dbc)

;;; mm-dbc.el ends here

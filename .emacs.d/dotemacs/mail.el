;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @NAME:               mail.el                                               ;;
;; @CREATED_BY:         Claude TETE                                           ;;
;; @MAIL:               claude.tete@gmail.com                                 ;;
;; @ABOUT:              config file for emacs                                 ;;
;; @VERSION:            1.0                                                   ;;
;; @DATE:               2012 March                                            ;;
;; @DATE_CREATION:      2012 March                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @CHANGELOG:          1.0 config for gmail account                          ;;
;;                      0.1 creation from scratch                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @LICENCE:                                                                  ;;
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  MAIL                                                                      ;;
;;  {                                                                         ;;
      ;; configure gmail as server
      ;; do not forget ~/.authinfo with:
      ;; machine imap.gmail.com login username@gmail.com password secret port 993
      (setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

      ;; to use smtp of gmail
      (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587 "personne146@gmail.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-local-domain "yourcompany.com")

      ;; only MS Windows to have ssl connection
      (setq tls-program '("d:/cygwin/bin/openssl.exe s_client -connect %h:%p -no_ssl2 -ign_eof"))
;;  } /* MAIL */                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

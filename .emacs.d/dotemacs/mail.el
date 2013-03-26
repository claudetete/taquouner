;; mail.el --- a config file for mail mode

;; Copyright (c) 2012-2013 Claude Tete

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

;; Keywords: config, mode
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.0
;; Created: March 2012
;; Last-Updated: March 2013

;;; Commentary:
;; Not used

;;; Change Log:
;; 2013-03-26 (1.0)
;;      config for gmail account
;; 2012-03-11 (0.1)
;;      creation from scratch

;;; MAIL
;;      ;; configure gmail as server
;;      ;; do not forget ~/.authinfo with:
;;      ;; machine imap.gmail.com login username@gmail.com password secret port 993
;;      (setq gnus-select-method '(nnimap "gmail"
;;                                  (nnimap-address "imap.gmail.com")
;;                                  (nnimap-server-port 993)
;;                                  (nnimap-stream ssl)))
;;
;;      ;; to use smtp of gmail
;;      (setq message-send-mail-function 'smtpmail-send-it
;;        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;        smtpmail-auth-credentials '(("smtp.gmail.com" 587 "personne146@gmail.com" nil))
;;        smtpmail-default-smtp-server "smtp.gmail.com"
;;        smtpmail-smtp-server "smtp.gmail.com"
;;        smtpmail-smtp-service 587
;;        smtpmail-local-domain "yourcompany.com")
;;
;;      ;; only MS Windows to have ssl connection
;;      (setq tls-program '("d:/cygwin/bin/openssl.exe s_client -connect %h:%p -no_ssl2 -ign_eof"))


(when section-mail-mew (message "  Mew...")
  (autoload 'mew "mew" nil t)
  (autoload 'mew-send "mew" nil t)

  ;; Optional setup (Read Mail menu):
  (setq read-mail-command 'mew)

  ;; Optional setup (e.g. C-xm for sending a message):
  (autoload 'mew-user-agent-compose "mew" nil t)
  (if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
  (if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))

  (setq mew-pop-size 0)
  ;;(setq mew-imap-prefix-list '("#mh/" "#mhinbox"))
  ;;(setq mew-auto-get t)
  (setq toolbar-mail-reader 'Mew)
  (setq mew-use-cached-passwd t)
  (setq mew-passwd-timer-unit 999)
  (setq mew-passwd-lifetime 999)
  (set-default 'mew-decode-quoted 't)
  (setq mew-prog-pgp "gpg")
  (setq mew-pop-delete nil)

;;  (setq mew-config-alist
;;  ;;Gmail
;;    '(("default"
;;        ("name"            . "Your Name")
;;        ("user"            . "gmail login name")
;;        ("mail-domain"     . "gmail.com")
;;        ("proto"           . "+")
;;        ("pop-ssl"         . t)
;;        ("pop-ssl-port"    . "995")
;;        ("prog-ssl"        . "stunnel")
;;        ("pop-auth"        . pass)
;;        ("pop-user"        . "gmail login name")
;;        ("pop-server"      . "pop.gmail.com")
;;        ("smtp-ssl"        . t)
;;        ("smtp-ssl-port"   . "465")
;;        ("smtp-auth-list"  . ("PLAIN" "LOGIN" "CRAM-MD5"))
;;        ("smtp-user"       . "gmail login name")
;;        ("smtp-server"     . "smtp.gmail.com")
;;        )
;;       ("IMAP"
;;         ("name"               . "Your Name")
;;         ("user"               . "login name")
;;         ("mail-domain"        . "server domain")
;;         ("proto"              . "%")
;;         ("imap-server"        . "server address")
;;         ("imap-ssh-server"    . "SSH server address")
;;         ("imap-user"          . "IMAP login name")
;;         ("imap-size"          . 0)
;;         ("smtp-ssl"           . t)
;;         ("smtp-ssl-port". "465")
;;         ("smtp-auth-list"     . ("PLAIN" "LOGIN" "CRAM-MD5"))
;;         ("smtp-user"          . "SMTP login name")
;;         ("smtp-server"        . "SMTP server address")
;;         ("imap-delete"        . t)
;;         ("imap-queue-folder"  . "%queue")
;;         ("imap-trash-folder"  . "%INBOX.Trash") ;; This must be in concile with your IMAP box setup
;;         )
;;       ))
  (setq mew-ssl-verify-level 0)
  (setq mew-prog-ssl "stunnel")

  (setq mew-config-alist
    '(
;;       ("gmail"
;;         ("name"           . "Claude TETE")
;;         ("user"           . "personne146")
;;         ("mail-domain"    . "gmail.com")
;;         ("pop-ssl"        . t)
;;         ("pop-ssl-port"   . "995")
;;         ("prog-ssl"       . "stunnel")
;;         ("pop-auth"       . pass)
;;         ("pop-user"       . "personne146@gmail.com")
;;         ("pop-server"     . "pop.gmail.com")
;;         ("smtp-ssl"       . t)
;;         ("smtp-ssl-port"  . "465")
;;         ("smtp-auth-list" . ("PLAIN" "LOGIN" "CRAM-MD5"))
;;         ("smtp-user"      . "personne146@gmail.com")
;;         ("smtp-server"    . "smtp.gmail.com"))
       ("IMAP"
         ("name"               . "Claude TETE")
         ("user"               . "personne146")
         ("mail-domain"        . "gmail.com")
         ("proto"              . "%")
         ("imap-server"        . "imap.gmail.com")
         ("imap-ssh-server"    . "SSH server address")
         ("imap-user"          . "personne146@gmail.com")
         ("imap-size"          . 0)
         ("smtp-ssl"           . t)
         ("smtp-ssl-port"      . "465")
         ("smtp-auth-list"     . ("PLAIN" "LOGIN" "CRAM-MD5"))
         ("smtp-user"          . "personne146@gmail.com")
         ("smtp-server"        . "smtp.mail.com")
         ("imap-delete"        . t)
         ("imap-queue-folder"  . "%queue")
         ("imap-trash-folder"  . "%INBOX.Trash")) ;; This must be in concile with your IMAP box setup
       ))

    (message "  Mew... Done"))

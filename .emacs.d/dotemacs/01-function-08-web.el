;;; 01-function-08-web.el --- add some function about web browser interface

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
;;
;; [SUBHEADER.custom function about web interface to browser]
;;

;;; Change Log:
;; 2017-07-21 (0.1)
;;    creation from split of old functions.el


;;; Code:

;;
;;;
;;; translate from French to English in generic browser
(defun translate-fren (beg end)
  "Translate the word at point using WordReference or Google Translate when a region is selected."
  (interactive (tqnr-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (if (use-region-p)
        (browse-url-generic
          (concat "http://translate.google.fr/?hl=&ie=UTF-8&text=&sl=fr&tl=en#fr|en|"
            (url-hexify-string string)))
        (browse-url-generic
          (concat "http://www.wordreference.com/fren/"
            (url-hexify-string string)))))))
;;; translate from English to French in generic browser
(defun translate-enfr (beg end)
  "Translate the word at point using WordReference or Google Translate when a region is selected."
  (interactive (tqnr-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (if (use-region-p)
        (browse-url-generic
          (concat "http://translate.google.fr/?hl=&ie=UTF-8&text=&sl=en&tl=fr#en|fr|"
            (url-hexify-string string)))
        (browse-url-generic
          (concat "http://www.wordreference.com/enfr/"
            (url-hexify-string string)))
        )
      (message "No word at point or no mark set"))))

;;; search in French Wikipedia
(defun wikipedia-fr (beg end)
  "Search the word at point or selected region using Wikipedia."
  (interactive (tqnr-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://fr.wikipedia.org/wiki/Special:Search?search=" (url-hexify-string string)))
      (message "No word at point or no mark set"))))
;;
;;; search in English Wikipedia
(defun wikipedia-en (beg end)
  "Search the word at point or selected region using Wikipedia in English."
  (interactive (tqnr-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://en.wikipedia.org/wiki/Special:Search?search=" (url-hexify-string string)))
      (message "No word at point or no mark set"))))

;;
;;; search synonym in French
(defun synonym-fr (beg end)
  "Search the word at point or selected region using Synonymes.com."
  (interactive (tqnr-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://www.synonymes.com/synonyme.php?mot=" (url-hexify-string string) "&x=0&y=0"))
      (message "No word at point or no mark set"))))
;;; search synonym in English
(defun synonym-en (beg end)
  "Search the word at point or selected region using Synonym.com."
  (interactive (tqnr-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://http://www.synonym.com/synonyms/" (url-hexify-string string) "&x=0&y=0"))
      (message "No word at point or no mark set"))))

;;
;;; search grammatical conjugation in French
(defun conjugation-fr (beg end)
  "Search the word at point or selected region using leconjugueur.com."
  (interactive (tqnr-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://www.leconjugueur.com/php5/index.php?v=" (url-hexify-string string)))
      (message "No word at point or no mark set"))))
;;; search grammatical conjugation in English
(defun conjugation-en (beg end)
  "Search the word at point or selected region using theconjugator.com."
  (interactive (tqnr-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://www.theconjugator.com/php5/index.php?verbe=" (url-hexify-string string)))
      (message "No word at point or no mark set"))))

;;
;;; search in French Google
(defun google-fr (beg end)
  "Search the word at point or selected region using google.fr."
  (interactive (tqnr-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://www.google.fr/search?hl=fr&hs=0HH&q=" (url-hexify-string string)
          "&btnG=Rechercher&meta=&num=100&as_qdr=a&as_occt=any"))
      (message "No word at point or no mark set"))))
;;; search in English Google
(defun google-en (beg end)
  "Search the word at point or selected region using google.com."
  (interactive (tqnr-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://www.google.fr/search?hl=en&hs=0HH&q=" (url-hexify-string string)
          "&btnG=Rechercher&meta=&num=100&as_qdr=a&as_occt=any"))
      (message "No word at point or no mark set"))))

;;
;;; search in DuckDuckGo
(defun duckduckgo (beg end)
  "Search the word at point or selected region using duckduckgo.com."
  (interactive (tqnr-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "https://duckduckgo.com/?q=" (url-hexify-string string)))
      (message "No word at point or no mark set"))))


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; translate word at point or region
    (defalias       'ef   'translate-enfr)
    (defalias       'fe   'translate-fren)
    ;; synonym
    (defalias       's      'synonym-fr)
    (defalias       'se     'synonym-en)
    ;; grammatical conjugation
    (defalias       'c      'conjugation-fr)
    ;; wikipedia
    (defalias       'we     'wikipedia-en)
    (defalias       'w      'wikipedia-fr)
    ;; google
    (defalias       'g      'google-fr)
    (defalias       'ge     'google-en)
    ;; duckduckgo
    (defalias       'dd     'duckduckgo)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '01-function-08-web)

;;; 01-function-08-web.el ends here

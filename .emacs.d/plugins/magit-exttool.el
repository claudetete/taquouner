(require 'magit)

(defgroup magit-exttool nil
  "Open difftool or mergetool from Magit"
  :prefix "magit-exttool"
  :group 'magit-extensions)

(defun magit-exttool-run-tool (list &rest args)
  (if (not (equal (length list) 0))
    (apply #'magit-run-git args list)))

(defun magit-exttool-run (&rest files)
  ""
  (let ((unmerged (magit-unmerged-files)))
    (let (mergelist difflist)
      (dolist (file files)
        (if (member file unmerged)
          (add-to-list 'mergelist file)
          (add-to-list 'difflist file)))
      (if (equal (magit-diff-type) 'staged)
        (magit-exttool-run-tool difflist "difftool" "--cached" "--no-prompt")
        (magit-exttool-run-tool difflist "difftool" "--no-prompt"))
      (magit-exttool-run-tool mergelist  "mergetool" "--no-prompt"))))

(defun magit-exttool-open  (&optional file)
  ""
  ;; Modified from `magit-stage'.
  (if file
      (magit-exttool-run "difftool" "--no-prompt" file)
    (--when-let (magit-current-section)
        (pcase (list (magit-diff-scope))
          (`(file)
           (magit-exttool-run (magit-section-value it)))
          (`(files)
           (magit-exttool-run (magit-region-values)))))))

(defun magit-exttool-open-current ()
  ""
  (interactive)
  (magit-exttool-open))

;;;###autoload
(eval-after-load 'magit
  '(progn
     (define-key magit-mode-map "X" 'magit-exttool-open-current)
     (magit-define-popup-action 'magit-dispatch-popup
       ?X "External Tool" 'magit-exttool-open-current)))

(provide 'magit-exttool)

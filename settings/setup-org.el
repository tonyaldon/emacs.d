(require 'kb)
(require 'org)

(setq org-edit-src-content-indentation 0)
(setq org-return-follows-link t)
(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t)))
(defun ta-org-confirm-babel-evaluate (lang body)
  (not (string= lang "emacs-lisp")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'ta-org-confirm-babel-evaluate)
(setq system-time-locale "C")
(setq org-export-backends '(ascii beamer html icalendar latex md))

(set-default 'org-link-frame-setup
             (quote
              ((vm . vm-visit-folder-other-frame)
               (vm-imap . vm-visit-imap-folder-other-frame)
               (gnus . org-gnus-no-new-news)
               (file . find-file)
               (wl . wl-other-frame))))

(add-to-list 'org-file-apps '(directory . emacs))

(defun ta-company-file-/ ()
  (interactive)
  (insert "/")
  (company-begin-backend 'company-files))

(defun ta-company-file-~ ()
  (interactive)
  (insert "~")
  (insert "/")
  (company-begin-backend 'company-files))

(define-key org-mode-map (kbd "C-c /") 'ta-company-file-/)
(define-key org-mode-map (kbd "C-c ~") 'ta-company-file-~)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'org-indent-mode)


(provide 'setup-org)

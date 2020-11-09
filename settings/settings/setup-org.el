;;; Packages
(require 'org)

;;; Global

(setq org-use-speed-commands t)
(setq org-return-follows-link t)
(setq org-export-backends '(ascii beamer html icalendar latex md))
(set-default 'org-link-frame-setup '((file . find-file)))
(add-to-list 'org-file-apps '(directory . emacs))

;;; time
(setq system-time-locale "C")
(setq org-log-done 'time)

;;; tags
(setq org-tags-column -77) ; default value

;;; src and babel

(require 'ob-js)
;; https://emacs.stackexchange.com/questions/55690/org-babel-javascript-error
(setq org-babel-js-function-wrapper
      "console.log(require('util').inspect(function(){\n%s\n}(), { depth: 100 }))")

(setq org-edit-src-content-indentation 0)

(org-babel-do-load-languages
 'org-babel-load-languages '((js . t)
														 (shell . t)
														 (python . t)
														 (dot . t)))

(defun ta-org-confirm-babel-evaluate (lang body)
  (and (not (string= lang "emacs-lisp"))
			 (not (string= lang "dot"))))  ; don't ask for ditaa

(setq org-confirm-babel-evaluate 'ta-org-confirm-babel-evaluate)

;;; Hooks

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'org-indent-mode)

;;; Footer

(provide 'setup-org)
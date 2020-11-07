;;; Packages
;;; Code

(setq refactor-wgrep-mode-list '(grep-mode ivy-occur-grep-mode rg-mode))

(defun refactor-write-mode ()
  "Toggle to the Writable variant of the current mode.

Call command `dired-toggle-read-only' if `major-mode' is equal
`dired-mode' and call command `wgrep-change-to-wgrep-mode' if
`major-mode' is in `refactor-wgrep-mode-list'."
  (interactive)
  (cond ((string-equal major-mode "dired-mode")
         (call-interactively 'dired-toggle-read-only))
				((string-equal major-mode "occur-mode")
         (call-interactively 'occur-edit-mode))
        ((memq major-mode refactor-wgrep-mode-list)
         (call-interactively 'wgrep-change-to-wgrep-mode))
        (t (message "In `%s' you can't toggle to `wgrep' like mode"
                    major-mode))))

(defun refactor-abort-changes ()
  "Abort changes and return to the appropiate mode.

Call command `wdired-abort-changes' if `major-mode' is
`wdired-mode' and call command `wgrep-abort-changes' if
`major-mode' is in `refactor-wgrep-mode-list'."
  (interactive)
  (cond ((string-equal major-mode "wdired-mode")
         (call-interactively 'wdired-abort-changes))
        ((memq major-mode refactor-wgrep-mode-list)
         (call-interactively 'wgrep-abort-changes))
        (t nil)))

(defun refactor-exit ()
  "Exit writable mode and return to the appropiate mode.

Call command `wdired-exit' if `major-mode' is
`wdired-mode' and call command `wgrep-exit' if
`major-mode' is in `refactor-wgrep-mode-list'."
  (interactive)
  (cond ((string-equal major-mode "wdired-mode")
         (call-interactively 'wdired-exit))
        ((memq major-mode refactor-wgrep-mode-list)
         (call-interactively 'wgrep-exit))
        (t nil)))

(defun refactor-finish-edit ()
  "Abort changes and return to the appropiate mode.

Call command `wdired-finish-edit' if `major-mode' is
`wdired-mode' and call command `wgrep-finish-edit' if
`major-mode' is in `refactor-wgrep-mode-list'."
  (interactive)
  (cond ((string-equal major-mode "wdired-mode")
         (call-interactively 'wdired-finish-edit))
        ((string-equal major-mode "occur-edit-mode")
         (call-interactively 'occur-cease-edit))
				((memq major-mode refactor-wgrep-mode-list)
         (call-interactively 'wgrep-finish-edit))
        (t nil)))

;;; Footer

(provide 'refactor)

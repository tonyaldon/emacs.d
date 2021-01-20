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

;;; Tables

(setq org-table-tab-jumps-over-hlines t)

(defun ta-org-table-previous-row ()
  "Go to the previous row (same column) in the current table.
Before doing so, re-align the table if necessary."
  (interactive)
  (unless (org-at-table-hline-p)
    (org-table-maybe-eval-formula)
    (org-table-maybe-recalculate-line))
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (when (and (org-at-table-p)
               (not (= (org-table-current-line) 1)))
      (previous-line)
      (unless (org-at-table-hline-p)
        (org-table-goto-column col)))))

(defun ta-org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item' or
`org-table-wrap-region', depending on context.

In table, `org-meta-return' calls `org-table-wrap-region' interactively
but it DOESN'T PASS the prefix arg.  So using `org-meta-return' in table
to run `org-table-wrap-region' does't work as expected.
Below you have a workaround to have full power of `org-table-wrap-region'
when calling `org-meta-return' in tables."
  (interactive "P")
  (org-check-before-invisible-edit 'insert)
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (if (org-at-table-p)
          (org-table-wrap-region arg)
        (call-interactively (cond (arg #'org-insert-heading)
                                  ((org-in-item-p) #'org-insert-item)
                                  (t #'org-insert-heading))))))

(defun ta-org-shiftmetadown (&optional _arg)
  "Drag the line at point down.
In a table, insert an empty row below the current line (this part
differs from the original `org-shiftmetadown' command).
On a clock timestamp, update the value of the timestamp like `S-<down>'
but also adjust the previous clocked item in the clock history.
Everywhere else, drag the line at point down."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetadown-hook))
   ((org-at-table-p) (org-table-insert-row 'below))
   ((org-at-clock-log-p) (let ((org-clock-adjust-closest t))
                           (call-interactively 'org-timestamp-down)))
   (t (call-interactively 'org-drag-line-forward))))

(defun org-self-insert-command (N)
  "Like `self-insert-command', use overwrite-mode for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment.

;; Tony Aldon (overwrite the original command `org-self-insert-command'.)
Add `ta-org-table-previous-row' to the commands after which we want
to blank table field if we start typing just after using it as `org-cycle',
`org-return', `org-shifttab', `org-ctrl-c-ctrl-c' commands."
  (interactive "p")
  (org-check-before-invisible-edit 'insert)
  (cond
   ((and org-use-speed-commands
         (let ((kv (this-command-keys-vector)))
           (setq org-speed-command
                 (run-hook-with-args-until-success
                  'org-speed-command-hook
                  (make-string 1 (aref kv (1- (length kv))))))))
    (cond
     ((commandp org-speed-command)
      (setq this-command org-speed-command)
      (call-interactively org-speed-command))
     ((functionp org-speed-command)
      (funcall org-speed-command))
     ((and org-speed-command (listp org-speed-command))
      (eval org-speed-command))
     (t (let (org-use-speed-commands)
          (call-interactively 'org-self-insert-command)))))
   ((and
     (= N 1)
     (not (org-region-active-p))
     (org-at-table-p)
     (progn
       ;; Check if we blank the field, and if that triggers align.
       (and (featurep 'org-table)
            org-table-auto-blank-field
            (memq last-command
                  '(ta-org-table-previous-row
                    org-cycle org-return org-shifttab org-ctrl-c-ctrl-c))
            (if (or (eq (char-after) ?\s) (looking-at "[^|\n]*  |"))
                ;; Got extra space, this field does not determine
                ;; column width.
                (let (org-table-may-need-update) (org-table-blank-field))
              ;; No extra space, this field may determine column
              ;; width.
              (org-table-blank-field)))
       t)
     (looking-at "[^|\n]*  |"))
    ;; There is room for insertion without re-aligning the table.
    (self-insert-command N)
    (org-table-with-shrunk-field
     (save-excursion
       (skip-chars-forward "^|")
       ;; Do not delete last space, which is
       ;; `org-table-separator-space', but the regular space before
       ;; it.
       (delete-region (- (point) 2) (1- (point))))))
   (t
    (setq org-table-may-need-update t)
    (self-insert-command N)
    (org-fix-tags-on-the-fly)
    (when org-self-insert-cluster-for-undo
      (if (not (eq last-command 'org-self-insert-command))
          (setq org-self-insert-command-undo-counter 1)
        (if (>= org-self-insert-command-undo-counter 20)
            (setq org-self-insert-command-undo-counter 1)
          (and (> org-self-insert-command-undo-counter 0)
               buffer-undo-list (listp buffer-undo-list)
               (not (cadr buffer-undo-list)) ; remove nil entry
               (setcdr buffer-undo-list (cddr buffer-undo-list)))
          (setq org-self-insert-command-undo-counter
                (1+ org-self-insert-command-undo-counter))))))))
;;; Hooks

(defun ta-org-whitspace-mode ()
  "Set `whitespace-style' for `org-mode'."
  (setq whitespace-style '(face tabs tab-mark)))

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'ta-org-whitspace-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Footer

(provide 'setup-org)

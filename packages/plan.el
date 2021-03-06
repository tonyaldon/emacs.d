;;; About

;; Some utilities and configuration to plan my work both in
;; rich-project and Jack Inside.
;;
;; I rely on `org-clock', `org-columns' and `org-agenda'.  So the
;; utilities and configurations use those packages.
;;
;; To plan my work, I use the following `org-mode' properties:
;; - ESTIMATED_TIME_TODAY (`org-effort-property')
;; - ESTIMATED_TIME_TOTAL
;; - PLANNED
;; - PLANNED_FOR_THE_FIRST_TIME_ON
;; - TASK_ID
;;
;; Each day, I work on tasks.  A task, has an entry (`org-mode' entry),
;; in my weekly-reports (in rich-project).  Long task (i.e. that
;; require more than a day to be done) have multiple entries in
;; weekly-reports, so to identify multiple entries in weekly-reports
;; that match the same task, we introduce the TASK_ID properties.
;; Each entry has a TASK_ID that uniquely identify a task.  So various
;; entries can have the same TASK_ID if they correspond to the same task
;; extended during severals days.

;;; Packages

(require 's)
(require 'time-stamp)

;;; Setup

(setq org-effort-property "ESTIMATED_TIME_TODAY")

;;; TASK_ID

(setq plan-task-id "TASK_ID")

(defun plan-generate-task-id (heading)
  "Generate `plan-task-id' string from HEADING."
  (sha1 (s-concat (time-stamp-string) heading)))

(defun plan-get-task-id ()
  "Get value of the `plan-task-id' property for the current entry."
  (org-entry-get nil plan-task-id))

(defun plan-set-task-id ()
  "In the current entry, set `plan-task-id' property."
  (interactive)
  (let* ((property plan-task-id)
         (heading (org-get-heading t t t t))
         (value (plan-generate-task-id heading)))
    (org-set-property property value)))

(defun plan-sparse-tree-task-id ()
  "Create a sparse tree according to the `plan-task-id' of the current entry."
  (interactive)
  (let ((kwd plan-task-id)
        (value (plan-get-task-id)))
    (unless (string-match "\\`{.*}\\'" value)
      (setq value (concat "\"" value "\"")))
    (org-match-sparse-tree nil (concat kwd "=" value))))

;;; Comments

(comment ; org-link-display-format, org-sparse-tree
 ;; /home/tony/work/learning/apps/emacs/lisp/org/ol.el 1195 (org-link-display-format)
 (org-link-display-format "[[https://github.com/tonyaldon/emacs.d][emacs.d]] uieuie uie") ; "emacs.d uieuie uie"

 ;; /home/tony/work/learning/apps/emacs/lisp/org/org.el 11394 (org-sparse-tree)
 )

(comment ; plan-generate-task-id
 (plan-generate-task-id "plan of the week") ; "4cda3931b68da2275e927600e519d6bac894cc61"
 )
;;; Footer

(provide 'plan)

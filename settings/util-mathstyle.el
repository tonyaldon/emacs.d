(require 'setup-dired)

(defun ms-find-org-file (filename)
  "Switch to a buffer visiting file FILNAME in `org-mode'.

Rotate the entire buffer through the state OVERVIEW."
	(find-file filename)
	(org-cycle-internal-global)
	(beginning-of-buffer))

(defun ms-dashboard ()
  "Set frame and buffers to produce a dashboard to manage MathStyle."
  (interactive)
	(ms-find-org-file "~/Documents/mathstyle/diary.org")
	(ta-dired-current-buffer-file-toggle)
	(windmove-right)
	(split-window-below)
	(split-window-below 8)
	(windmove-down)
	(ms-find-org-file "~/Documents/mathstyle/identity.org")
	(windmove-down)
	(ms-find-org-file "~/Documents/mathstyle/notes.org")
	(split-window-below 14)
	(windmove-down)
	(ms-find-org-file "~/Documents/mathstyle/team.org")
	(windmove-left))

(provide 'util-mathstyle)

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
	(ms-find-org-file "~/work/mathstyle/mathstyle/diary.org")
	(ta-sidebar)
	(windmove-right)
	(split-window-below)
	(split-window-below 8)
	(windmove-down)
	(ms-find-org-file "~/work/mathstyle/mathstyle/identity.org")
	(windmove-down)
	(ms-find-org-file "~/work/mathstyle/mathstyle/notes.org")
	(split-window-below 14)
	(windmove-down)
	(ms-find-org-file "~/work/mathstyle/mathstyle/team.org")
	(windmove-left))

(provide 'util-mathstyle)

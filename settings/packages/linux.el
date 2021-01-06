;;; About
;; In this file, we define commands that modify some behaviour
;; linux OS where emacs live in like (i3, xrandr, dpi rendering...)

;;; i3
;;;; Toggle laptop output visibility

(setq linux-i3-config-file "~/work/settings/i3/.config/i3/config")

(defun linux--laptop-output-visibility-config-position ()
	"Return buffer position of the config line responsible for the laptop output visibility.

Return nil, if no laptop output visibility configuration line found."
	(save-excursion
		(beginning-of-buffer)
		(when (search-forward "laptop monitor" nil t)
			(next-line)
			(beginning-of-line)
			(point))))

(defun linux-toggle-laptop-output-visibility ()
  "Toggle on/off the laptop output visibility.

This is done via the i3 configuration file `linux-i3-config-file'.
Note that you have to restart your linux session to see the changes.

This is useful when using only my laptop to make Zoom calls,
thought I've no external webcam."
  (interactive)
	(with-temp-buffer
		(insert-file-contents linux-i3-config-file)
		(let ((laptop-config (linux--laptop-output-visibility-config-position))
					output)
			(when laptop-config
				(goto-char laptop-config)
				(if (looking-at "# ")
						(progn (delete-char 2)
									 (setq output "off"))
					(insert "# ")
					(setq output "on"))))
		(write-region (point-min) (point-max) linux-i3-config-file))
	(if output (message (concat "Laptop output turned: " output))
		(message "No 'laptop monitor' config line found in `linux-i3-config-file'")))

;; (global-set-key (kbd "C-c o") 'linux-toggle-laptop-output-visibility)

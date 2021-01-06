;;; About
;; In this file, we define commands that modify some behaviour of
;; linux OS where emacs lives in, like (i3, xrandr, dpi rendering...)

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

;;;; Toggle i3bar visibility

(defun linux--i3bar-is-hidden-p ()
  "Return t if i3bar is hidden."
	(s-blank-p (shell-command-to-string "i3-msg -t get_tree | grep '\"class\":\"i3bar\"'")))

(defun linux-toggle-i3bar ()
  "Toggle visibility of i3bar."
  (interactive)
  (let ((inhibit-message t))
		(if (linux--i3bar-is-hidden-p)
				(shell-command "i3-msg bar mode dock")
			(shell-command "i3-msg bar mode invisible"))))

;; (global-set-key (kbd "C-c b") 'linux-toggle-i3bar)

;; COMMENTS
;; (s-blank-p "")
;; (shell-command-to-string "ls")
;; (shell-command-to-string "i3-msg -t get_tree | grep '\"class\":\"i3bar\"'")
;; (shell-command "i3-msg bar mode invisible")
;; (shell-command "i3-msg bar mode dock")

;;; Monitor DPI

(setq linux-xresources "~/work/settings/uconfig/.Xresources")

(defun linux-toggle-dpi ()
  "Toggle the DPI in the file ~/.Xresources between 216 and 96.

96 is the default DPI setting.
216 is the DPI setting for screencasting. Setting it that big has
for consequence to zoom in everything in the screen.

Note that you have to restart your linux session to see the changes."
  (interactive)
	(let (dpi)
		(with-temp-buffer
			(insert-file-contents linux-xresources)
			(beginning-of-buffer)
			(cond
			 ((search-forward "96" nil t)
				(delete-char -2)
				(insert "216")
				(setq dpi "216"))
			 ((search-forward "216" nil t)
				(delete-char -3)
				(insert "96")
				(setq dpi "96"))
			 (t nil))
			(write-region (point-min) (point-max) linux-xresources))
		(if dpi (message (concat "Xft.dpi: " dpi))
			(message "Neither 96 nor 216 is the DPI in ~/.Xresources file"))))

;; (global-set-key (kbd "C-c d") 'linux-toggle-dpi)

;;; Footer
(provide 'linux)

;;; About
;; In this file, we define commands that modify some behaviour of
;; linux OS where emacs lives in, like (i3, xrandr, dpi rendering...)

;;; i3

;;;; Toggle laptop output

(setq linux-i3-config-file "~/work/settings/i3/.config/i3/config")

(defun linux--laptop-output-config-position ()
  "Return buffer position of the config line responsible for the laptop output visibility.

Return nil, if no laptop output visibility configuration line found."
  (save-excursion
    (beginning-of-buffer)
    (when (search-forward "laptop monitor" nil t)
      (next-line)
      (beginning-of-line)
      (point))))

(defun linux-toggle-laptop-output ()
  "Toggle on/off the laptop output visibility.

This is done via the i3 configuration file `linux-i3-config-file'.
Note that you have to restart your linux session to see the changes.

This is useful when using only my laptop to make Zoom calls,
thought I've no external webcam."
  (interactive)
  (let (output)
    (with-temp-buffer
      (insert-file-contents linux-i3-config-file)
      (when-let ((laptop-config (linux--laptop-output-config-position)))
        (goto-char laptop-config)
        (if (looking-at "# ")
            (progn (delete-char 2)
                   (setq output "off"))
          (insert "# ")
          (setq output "on")))
      (write-region (point-min) (point-max) linux-i3-config-file))
    (if output (message (concat "Laptop output turned: " output))
      (message "No 'laptop monitor' config line found in `linux-i3-config-file'"))))

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

;;; Keyboard layout

(defun linux-switch-keyboard-layout ()
  "Switch keyboard layout variant between\"takbl\" and \"takbl fr\"."
  (interactive)
  (let ((variant (shell-command-to-string "setxkbmap -query | grep variant | awk -F' '  '{ print $2 }'")))
    (if (string= variant "fr\n")
        (progn
          (shell-command-to-string "setxkbmap -layout takbl")
          (message "takbl"))
      (shell-command-to-string "setxkbmap -layout takbl -variant fr")
      (message "takbl - fr"))))

;;; Git (format commit message)

(defun linux-toggle-git-commit-msg ()
  "Set the git hook \"prepare-commit-msg\".

If it already exist (ie: not ends with \".sample\") bypass it. It is mandatory when
you rebase/rewrite your git history.
If it doesn't exist, create it with the following content:
\"
#!/bin/bash
COMMIT_MSG_FILEPATH=$1
HINT=`cat $COMMIT_MSG_FILEPATH`

echo \"Subject line\" > $COMMIT_MSG_FILEPATH
echo \"\" >> $COMMIT_MSG_FILEPATH
for cached_file in `git diff --cached --name-only | sed 's/ /\n/g'`;do
    echo \"* $cached_file:\" >> $COMMIT_MSG_FILEPATH;
done
echo \"$HINT\" >> $COMMIT_MSG_FILEPATH
\""
  (interactive)
  (when-let* ((hooks (concat (cdr (project-current)) ".git/hooks/"))
              (prepare-commit-msg (concat hooks "prepare-commit-msg")))
    (if (file-exists-p prepare-commit-msg)
        (progn (delete-file prepare-commit-msg)
               (message "\"%s\" has been removed" (file-name-nondirectory prepare-commit-msg)))
      (with-temp-file prepare-commit-msg
        (insert
         "#!/bin/bash
COMMIT_MSG_FILEPATH=$1
HINT=`cat $COMMIT_MSG_FILEPATH`

echo \"Subject line\" > $COMMIT_MSG_FILEPATH
echo \"\" >> $COMMIT_MSG_FILEPATH
for cached_file in `git diff --cached --name-only | sed 's/ /\\n/g'`;do
    echo \"* $cached_file:\" >> $COMMIT_MSG_FILEPATH;
done
echo \"$HINT\" >> $COMMIT_MSG_FILEPATH"))
      (shell-command (concat "chmod +x " prepare-commit-msg))
      (message "\"%s\" has been created" (file-name-nondirectory prepare-commit-msg)))))

;; COMMENTS
;; (shell-command "ls")
;; (shell-command-to-string "ls")
;; (global-set-key (kbd "C-<f1>") 'linux-toggle-git-commit-msg)

;;; Footer
(provide 'linux)

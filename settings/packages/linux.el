;;; About
;;
;; In this file, we define commands that modify some behaviour of
;; linux OS where emacs lives in, like (xrandr, dpi rendering...)

;;; Packages

(require 'comment) ; https://github.com/tonyaldon/emacs.d/blob/master/settings/packages/comment.el

;;; Turn off laptop output

(defun linux-turn-off-laptop-output ()
  "Turn off the laptop output visibility.

This is not persistent across linux sessions."
  (interactive)
  (shell-command-to-string "xrandr --output eDP-1 --off"))

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
    (cond ((string= variant "")
           (shell-command-to-string "setxkbmap -layout takbl -variant fr")
           (message "takbl - fr"))
          ((string= variant "fr\n")
           (shell-command-to-string "setxkbmap -layout takbl -variant es")
           (message "takbl - es"))
          ((string= variant "es\n")
           (shell-command-to-string "setxkbmap -layout takbl")
           (message "takbl")))))

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

;;; Comments

(comment ; shell-command-to-string, shell-command
 (shell-command "ls")
 (shell-command-to-string "ls"))

;;; Footer

(provide 'linux)

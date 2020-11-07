(require 'outline)

;;; outline-self-insert-command
(defun outline-self-insert-command (N)
  "Like `self-insert-command' but allow speed commands

define in `outline-speed-commands-default' and `outline-speed-commands-user'
when the cursor is at the beginning of an outline headline.

This is a ligth adaptation of `org-self-insert-command'."
  (interactive "p")
  ;; (org-check-before-invisible-edit 'insert)
  (cond
   ((and outline-use-speed-commands
         (let ((kv (this-command-keys-vector)))
           (setq outline-speed-command
                 (run-hook-with-args-until-success
                  'outline-speed-command-hook
                  (make-string 1 (aref kv (1- (length kv))))))))
    (cond
     ((commandp outline-speed-command)
      (setq this-command outline-speed-command)
      (call-interactively outline-speed-command))
     ((functionp outline-speed-command)
      (funcall outline-speed-command))
     ((and outline-speed-command (listp outline-speed-command))
      (eval outline-speed-command))
     (t (let (outline-use-speed-commands)
          (call-interactively 'outline-self-insert-command)))))
   (t
    (setq this-command 'self-insert-command)
    (self-insert-command N))))


;;; Speed keys
(defvar outline-speed-commands-user nil
  "Alist of additional speed commands.
This list will be checked before `outline-speed-commands-default'.
when the cursor is at the beginning of a headline.
The car of each entry is a string with a single letter, which must
be assigned to `self-insert-command' in the global map.
The cdr is either a command to be called interactively, a function
to be called, or a form to be evaluated.
An entry that is just a list with a single string will be interpreted
as a descriptive headline that will be added when listing the speed
commands in the Help buffer using the `?' speed command.")

(defvar outline-speed-command-hook '(outline-speed-command-activate)
  "Hook for activating speed commands at strategic locations.
Hook functions are called in sequence until a valid handler is
found.

Each hook takes a single argument, a user-pressed command key
which is also a `self-insert-command' from the global map.

Within the hook, examine the cursor position and the command key
and return nil or a valid handler as appropriate.  Handler could
be one of an interactive command, a function, or a form.

Turn on `outline-speed-commands-mode' to enable this hook.")

(defconst outline-speed-commands-default
  '(("Outline Navigation")
    ("n" . (outline-speed-move-safe 'outline-next-visible-heading))
    ("p" . (outline-speed-move-safe 'outline-previous-visible-heading))
    ("f" . (outline-speed-move-safe 'outline-forward-same-level))
    ("b" . (outline-speed-move-safe 'outline-backward-same-level))
    ("u" . (outline-speed-move-safe 'outline-up-heading))
    ("Outline Visibility")
    ("i" . outline-show-children)
    ("s" . outline-show-subtree)
    ("d" . outline-hide-subtree)
    ("t" . outline-hide-body)
    ("a" . outline-show-all)
    ("c" . outline-hide-entry)
    ("e" . outline-show-entry)
    ("l" . outline-hide-leaves)
    ("k" . outline-show-branches)
    ("q" . outline-hide-sublevels)
    ("o" . outline-hide-other)
    ("Outline Structure Editing")
    ("@" . outline-mark-subtree)
		("^". outline-move-subtree-up)
    ("v". outline-move-subtree-down)
    ("<". outline-promote)
    (">". outline-demote)
    ("m". outline-insert-heading)
		"The default Outline speed commands."))

(defun outline-speed-move-safe (cmd)
  "Execute CMD, but make sure that the cursor always ends up in a headline.
If not, return to the original position and throw an error."
  (interactive)
  (let ((pos (point)))
    (call-interactively cmd)
    (unless (and (bolp) (outline-on-heading-p t))
      (goto-char pos)
      (error "Boundary reached while executing %s" cmd))))

(defun outline-speed-command-activate (keys)
  "Hook for activating single-letter speed commands.
`outline-speed-commands-default' specifies a minimal command set.
Use `outline-speed-commands-user' for further customization."
  (when (and (bolp) (looking-at outline-regexp))
    (cdr (assoc keys (append outline-speed-commands-user
                             outline-speed-commands-default)))))

;;; outline-speed-commands-mode
(define-minor-mode outline-speed-commands-mode
  "Toggle outline-speed-commands-mode mode on or off."
  :global nil
  (if outline-speed-commands-mode
      (progn
        (setq-local outline-use-speed-commands t)
        (local-set-key [remap self-insert-command] 'outline-self-insert-command))
    (makunbound 'outline-use-speed-commands)
    (local-set-key [remap self-insert-command] nil)))



(provide 'outline-speed-commands)

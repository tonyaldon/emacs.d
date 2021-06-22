;;; About

;; `outline-spc' (SPeed Commands) brings speed commands capability
;; to any major-mode with `outline-minor-mode' turned on.  To use
;; it, you just have to turn on the minor mode `outline-spc-mode'.
;; Then when you are at the beginning of an outline defined in
;; `outline-regexp', typing a single key where the mapping is
;; defined either in `outline-spc-user' or
;; `outline-spc-default'variables, trigger an outline
;; commands instead of inserting the character typed.  For instance,
;; by default, if you are at the beginning of an outline and you
;; type "n", this call `outline-next-visible-heading' command.

;;; Packages

(require 'outline)

;;; The self insert command

(defun outline-spc-self-insert-command (N)
  "Like `self-insert-command' but allow speed commands

define in `outline-spc-default' and `outline-spc-user'
when the cursor is at the beginning of an outline headline.

This is a ligth adaptation of `org-self-insert-command'."
  (interactive "p")
  (cond
   ((and outline-spc-mode
         (let ((kv (this-command-keys-vector)))
           (setq outline-spc-speed-command
                 (run-hook-with-args-until-success
                  'outline-spc-hook
                  (make-string 1 (aref kv (1- (length kv))))))))
    (cond
     ((commandp outline-spc-speed-command)
      (setq this-command outline-spc-speed-command)
      (call-interactively outline-spc-speed-command))
     ((functionp outline-spc-speed-command)
      (funcall outline-spc-speed-command))
     ((and outline-spc-speed-command (listp outline-spc-speed-command))
      (eval outline-spc-speed-command))
     (t (call-interactively 'outline-spc-self-insert-command))))
   (t
    (setq this-command 'self-insert-command)
    (self-insert-command N))))


;;; Speed keys

(defvar outline-spc-user nil
  "Alist of additional speed commands.
This list will be checked before `outline-spc-default'.
when the cursor is at the beginning of a headline.
The car of each entry is a string with a single letter, which must
be assigned to `self-insert-command' in the global map.
The cdr is either a command to be called interactively, a function
to be called, or a form to be evaluated.
An entry that is just a list with a single string will be interpreted
as a descriptive headline that will be added when listing the speed
commands in the Help buffer using the `?' speed command.")

(defvar outline-spc-hook '(outline-spc-activate)
  "Hook for activating speed commands at strategic locations.
Hook functions are called in sequence until a valid handler is
found.

Each hook takes a single argument, a user-pressed command key
which is also a `self-insert-command' from the global map.

Within the hook, examine the cursor position and the command key
and return nil or a valid handler as appropriate.  Handler could
be one of an interactive command, a function, or a form.

Turn on `outline-spc-mode' to enable this hook.")

(defconst outline-spc-default
  '(("Outline Navigation")
    ("n" . (outline-spc-move-safe 'outline-next-visible-heading))
    ("p" . (outline-spc-move-safe 'outline-previous-visible-heading))
    ("f" . (outline-spc-move-safe 'outline-forward-same-level))
    ("b" . (outline-spc-move-safe 'outline-backward-same-level))
    ("u" . (outline-spc-move-safe 'outline-up-heading))
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

(defun outline-spc-move-safe (cmd)
  "Execute CMD, but make sure that the cursor always ends up in a headline.
If not, return to the original position and throw an error."
  (interactive)
  (let ((pos (point)))
    (call-interactively cmd)
    (unless (and (bolp) (outline-on-heading-p t))
      (goto-char pos)
      (error "Boundary reached while executing %s" cmd))))

(defun outline-spc-activate (keys)
  "Hook for activating single-letter speed commands.
`outline-spc-default' specifies a minimal command set.
Use `outline-spc-user' for further customization."
  (when (and (bolp) (looking-at outline-regexp))
    (cdr (assoc keys (append outline-spc-user
                             outline-spc-default)))))

;;; outline-spc-mode

(define-minor-mode outline-spc-mode
  "Toggle `outline-spc-mode' mode on or off."
  :global nil
  (if outline-spc-mode
      (local-set-key [remap self-insert-command]
                     'outline-spc-self-insert-command)
    (local-set-key [remap self-insert-command] nil)))


;;; Footer

(provide 'outline-spc)

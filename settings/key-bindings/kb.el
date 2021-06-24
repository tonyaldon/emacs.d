;;;; Operate on lines

(require 'drag-stuff)
(load "~/.emacs.d/.cask/28.0/elpa/drag-stuff-20161108.749/drag-stuff.el")

(defvar handy-line-active nil)

(defun handy-line-active ()
  "Toggle status of `handy-line-active'"
  (if handy-line-active
      (setq handy-line-active nil)
    (setq handy-line-active t)))

(defhydra handy-line
  (:pre (progn (if insight-mode (insight-mode -1))
               (set-cursor-color "#fa87ce"))
   :post (progn (set-cursor-color "#26f9ad")
                (handy-line-active))
   :hint nil)
  ("M-l" recenter-top-bottom)
  ("t" handy-sexp/body :color blue)
  (";" handy-line-comment)
  ("DEL" delete-backward-char)
  ("~" set-mark-command)
  ("m" exchange-point-and-mark)
  ;; action on line(s)
  ("!" flush-lines)
  ("?" keep-lines)
  ;; current line
  ("k" kill-line)
  ("l" (kill-line 0))
  ("x" handy-line-kill)
  ("y" handy-line-copy-paste-below)
  ("r" join-line)
  ("o" open-line)
  ("O" delete-blank-lines)
  ("," handy-cycle-spacing)
  ;; to insert text
  ("u" handy-line-add-above :color blue)
  ("]" handy-line-add-below :color blue)
  ("_" handy-add-space :color blue)
  ;; quick motions
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)
  ("b" backward-char)
  ("i" back-to-indentation)
  ("a" move-beginning-of-line)
  ("e" move-end-of-line)
  ("M-f" forward-word)
  ("M-b" backward-word)
  ("M-e" org-forward-sentence)
  ("M-a" org-backward-sentence)
  ;; drag stuff
  ("d" drag-stuff-up)
  ("s" drag-stuff-down)
  ("<left>" drag-stuff-left)
  ("<right>" drag-stuff-right)
  ;; clean/undo/nil
  ("M--" undo)
  ("q" nil))

(defadvice move-beginning-of-line (before move-beginning-of-line-advice activate)
  (if (not mark-active) (push-mark)))

(defadvice move-end-of-line (before move-end-of-line-advice activate)
  (if (not mark-active) (push-mark)))

(defadvice handy-line/body (before handy-line-advice activate)
  (handy-line-active))

(key-chord-define-global "ld" 'handy-line/body)

;;;; transient key bindings interface

;;;;; transient interface

(declare-function transient-define-prefix "ext:transient")
(declare-function transient-define-suffix "ext:transient")

(defmacro ta-transient-define-suffix (command)
  "Create a command COMMAND--transient that is a transient suffix command
that call interactively COMMAND."
  (let ((func-name (concat (symbol-name command) "--transient")))
    `(transient-define-suffix ,(intern func-name) ()
       (interactive)
       (call-interactively (quote ,command)))))

;;;;; describe

(declare-function yas-describe-tables "ext:yasnippet")
(declare-function yas-new-snippet "ext:yasnippet")

(ta-transient-define-suffix describe-key)
(ta-transient-define-suffix describe-keymap)
(ta-transient-define-suffix describe-function)
(ta-transient-define-suffix describe-variable)
(ta-transient-define-suffix describe-mode)

;;;;; linux

(require 'linux)

(ta-transient-define-suffix linux-switch-keyboard-layout)
(ta-transient-define-suffix linux-turn-off-laptop-output)
(ta-transient-define-suffix linux-toggle-dpi)
(ta-transient-define-suffix linux-toggle-git-commit-msg)

;;;;; miscellaneous

(ta-transient-define-suffix yas-describe-tables)
(ta-transient-define-suffix yas-new-snippet)
(ta-transient-define-suffix yas-visit-snippet-file)

(ta-transient-define-suffix apropos)
(ta-transient-define-suffix info)
(ta-transient-define-suffix image-toggle-display)
(ta-transient-define-suffix display-line-numbers-mode)

;;;;; ta-remind-me

(transient-define-prefix ta-remind-me ()
  "Show menu buffer for miscellaneous commands I often need but do not remember."
  [["describe"
    ("dk" "describe-key" describe-key--transient)
    ("dp" "describe-keymap" describe-keymap--transient)
    ("df" "describe-function" describe-function--transient)
    ("dv" "describe-variable" describe-variable--transient)
    ("dm" "describe-mode" describe-mode--transient)]
   ["linux"
    ("lk" "linux-switch-keyboard-layout" linux-switch-keyboard-layout--transient)
    ("ll" "linux-turn-off-laptop-output" linux-turn-off-laptop-output--transient)
    ("ld" "linux-toggle-dpi" linux-toggle-dpi--transient)
    ("lg" "linux-toggle-git-commit-msg" linux-toggle-git-commit-msg--transient)]
   ["yasnippet"
    ("sd" "yas-describe-tables" yas-describe-tables--transient)
    ("sn" "yas-new-snippet" yas-new-snippet--transient)
    ("sf" "yas-visit-snippet-file" yas-visit-snippet-file--transient)]
   ["misc"
    ("a" "apropos" apropos--transient)
    ("i" "info" info--transient)
    ("t" "image-toggle-display" image-toggle-display--transient)
    ("n" "display-line-numbers-mode" display-line-numbers-mode--transient)]])

(global-set-key (kbd "C-M-i") 'ta-remind-me)

;;; Footer

(provide 'kb)

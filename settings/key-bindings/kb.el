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

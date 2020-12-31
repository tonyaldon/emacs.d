;;; Thing at point

(defun ta-describe-thing-at-point ()
  "Display the full documentation of the `thing-at-point'

that is either a function or a variable.
Return nil if the symbol of the `thing-at-point' is neither a function
nor a variable."
  (interactive)
  (let ((current-symbol (symbol-at-point)))
    (cond
     ((not current-symbol))
     ((boundp current-symbol) (describe-variable current-symbol))
     ((fboundp current-symbol) (describe-function current-symbol))
     (t (message "The symbol-at-point is neither a variable or a function")))))

;;; Get info

(declare-function transient-define-prefix "ext:transient")
(declare-function transient-define-suffix "ext:transient")
(declare-function yas-describe-tables "ext:yasnippet")

(defmacro ta-transient-define-suffix (command)
  "Create a command COMMAND--transient that is a transient suffix command
that call interactively COMMAND."
  (let ((func-name (concat (symbol-name command) "--transient")))
    `(transient-define-suffix ,(intern func-name) ()
       (interactive)
       (call-interactively (quote ,command)))))

(ta-transient-define-suffix describe-key)
(ta-transient-define-suffix describe-keymap)
(ta-transient-define-suffix describe-function)
(ta-transient-define-suffix describe-variable)
(ta-transient-define-suffix describe-mode)
(ta-transient-define-suffix yas-describe-tables)
(ta-transient-define-suffix apropos)
(ta-transient-define-suffix info)

(transient-define-prefix ta-get-info ()
  "Show menu buffer for describe commands, info commands,
apropos commands and other information about emacs."
  [["Describe"
    ("kk" "describe-key" describe-key--transient)
    ("kl" "describe-keymap" describe-keymap--transient)
		("f" "describe-function" describe-function--transient)
    ("v" "describe-variable" describe-variable--transient)
    ("m" "describe-mode" describe-mode--transient)]
   ["Other info"
    ("y" "yas-describe-tables" yas-describe-tables--transient)
    ("a" "apropos" apropos--transient)
    ("i" "info" info--transient)]])

;;; Key bindings

(global-set-key (kbd "C-d") 'ta-describe-thing-at-point)
(global-set-key (kbd "C-M-i") 'ta-get-info)


;;; Footer

(provide 'kb-describe)

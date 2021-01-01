;;; Packages
(require 'moody)
(require 'minions)
(require 'keycast)

;;; Mode line

;;;; Format

(defvar screencast-mode-line-format
  '("%e"
    mode-line-front-space
    mode-line-mule-info
    mode-line-client
    mode-line-modified
    mode-line-remote
    mode-line-frame-identification
    mode-line-buffer-identification
    " "
    minions-mode-line-modes)
  "`mode-line-format' to be used when `screencast-mode' is enable.")

;;;; minions

(defvar screencast-minions-direct '(insight-mode)
  "`minions-direct' to be used when `screencast-mode' is enable.")

(defvar screencast-minions-mode-line-lighter ""
  "`minions-mode-line-lighter' to be used when `screencast-mode' is enable.")

(defvar screencast-minions-mode-line-delimiters '("" . "")
  "`minions-mode-line-delimiters' to be used when `screencast-mode' is enable.")

(defvar screencast--minions-variables '(minions-direct
                                        minions-mode-line-lighter
                                        minions-mode-line-delimiters)
  "List of `minions' variables that are modified when `screencast-mode' is enable.")

(setq screencast--minions-variables-alist nil)

(defun screencast-set-minions ()
  "Set `minions' variables used by `screencast-mode'."
  (if screencast-mode
      (progn
        (setq screencast--minions-variables-alist
              (--map (cons it (symbol-value it)) screencast--minions-variables))
        (--each screencast--minions-variables
          (let ((item it)
                (item-value (symbol-value
                             (intern (s-concat "screencast-" (symbol-name it))))))
            (set item item-value))))
    (--each screencast--minions-variables
      (let ((item it)
            (item-value (cdr (assoc it screencast--minions-variables-alist))))
        (set item item-value)))))

;;;; keycast

(defvar screencast-keycast-separator-width 2
  "`keycast-separator-width' to be used when `screencast-mode' is enable.")

(defvar screencast-keycast-insert-after 'mode-line-buffer-identification
  "`keycast-insert-after' to be used when `screencast-mode' is enable.")

(defvar screencast-keycast-window-predicate  'moody-window-active-p
  "`keycast-window-predicate' to be used when `screencast-mode' is enable.")

(defvar screencast-keycast-remove-tail-elements nil
  "`keycast-remove-tail-elements' to be used when `screencast-mode' is enable.")

(defvar screencast-keycast-substitute-alist
  '((self-insert-command "." "self-insert-command")
    (org-self-insert-command "." "org-self-insert-command")
    (outline-self-insert-command "." "outline-self-insert-command"))
  "`keycast-substitute-alist' to be used when `screencast-mode' is enable.")

(defvar screencast--keycast-variables '(keycast-separator-width
                                        keycast-insert-after
                                        keycast-window-predicate
                                        keycast-remove-tail-elements
                                        keycast-substitute-alist)
  "List of `keycast' variables that are modified when `screencast-mode' is enable.")

(setq screencast--keycast-variables-alist nil)

(defun screencast-set-keycast ()
  "Set `keycast' variables used by `screencast-mode'."
  (if screencast-mode
      (progn
        (setq screencast--keycast-variables-alist
              (--map (cons it (symbol-value it)) screencast--keycast-variables))
        (--each screencast--keycast-variables
          (let ((item it)
                (item-value (symbol-value
                             (intern (s-concat "screencast-" (symbol-name it))))))
            (set item item-value))))
    (--each screencast--keycast-variables
      (let ((item it)
            (item-value (cdr (assoc it screencast--keycast-variables-alist))))
        (set item item-value)))))

;;; Other

(defvar screencast-display-buffer-alist '()
  "`display-buffer-alist' to be used when `screencast-mode' is enable.")

(defvar screencast-hook-to-remove-alist '()
  "A list of alist (HOOK . FUNCTION) to be removed when `screencast-mode'
is enable.

For instance a valid list is:
  '((window-configuration-change-hook . my-function-1)
    (buffer-list-update-hook . my-function-2))")

;;; Mode

(setq screencast--mode-line-format nil)
(setq screencast--display-buffer-alist nil)

(define-minor-mode screencast-mode
  "Toggle Screencast mode on or off.

Set emacs layout to record screencasts.

Turn on `keycast-mode', clean the `mode-line-format', adapt `display-buffer-alist'
to a smaller window frame than usual."
  :global t
  (if screencast-mode
      (progn
        (when mini-frame-mode (mini-frame-mode -1))
        (setq ivy-height 7)
        (--each screencast-hook-to-remove-alist (remove-hook (car it) (cdr it)))
        (setq screencast--display-buffer-alist display-buffer-alist)
        (setq display-buffer-alist screencast-display-buffer-alist)
        (screencast-set-minions)
        (screencast-set-keycast)
        (setq screencast--mode-line-format mode-line-format)
        (setq-default mode-line-format screencast-mode-line-format)
        (keycast-mode)
				(set-face-attribute 'mode-line-buffer-id nil :foreground "#151515"))
    (mini-frame-mode 1)
    (setq ivy-height 25)
    (--each screencast-hook-to-remove-alist (add-hook (car it) (cdr it)))
    (setq display-buffer-alist screencast--display-buffer-alist)
    (setq screencast--display-buffer-alist nil)
    (screencast-set-minions)
    (screencast-set-keycast)
    (keycast-mode -1) ; should be called before setting mode-line-format
    (setq-default mode-line-format screencast--mode-line-format)
    (setq screencast--mode-line-format nil)
		(set-face-attribute 'mode-line-buffer-id nil :foreground "#b3b3b3")))

;;; Footer

(provide 'screencast)

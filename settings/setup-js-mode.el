(require 'tern)
(require 'outline)
(require 'outline-magic)

(defun ta-tern-js-mode-hook ()
  "Turn `tern-mode' on in `js-mode'"
  (tern-mode t))

(defun ta-outline-cycle-entire-buffer ()
  "Call `outline-cycle' with universal argument."
  (interactive)
  (let ((current-prefix-arg '(4)))
		(call-interactively 'outline-cycle)))

(defun ta-outline-js-mode-hook ()
  "Set up `outline-mode' and `outline-magic'.  Specifically,

the variable `outline-regexp'."
  (outline-minor-mode t)
	(setq outline-regexp (concat "[[:space:]]*"
															 "\\(app\\|axios\\|const\\)"))
	(define-key outline-minor-mode-map (kbd "<tab>") 'outline-cycle)
	(define-key outline-minor-mode-map (kbd "C-<tab>") 'ta-outline-cycle-entire-buffer))

(add-hook 'js-mode-hook 'ta-tern-js-mode-hook)
(add-hook 'js-mode-hook 'ta-outline-js-mode-hook)

(provide 'setup-js-mode)

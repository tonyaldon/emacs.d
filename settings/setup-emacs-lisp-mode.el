(require 'outline)
(require 'bicycle)

(defun ta-outline-emacs-lisp-mode-hook ()
  "Set up `outline-mode' and `bicycle'.  Specifically,

the variable `outline-regexp'."
  (outline-minor-mode t)
	(define-key outline-minor-mode-map (kbd "<tab>") 'bicycle-cycle)
	(define-key outline-minor-mode-map (kbd "C-<tab>") 'bicycle-cycle-global))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'ta-outline-emacs-lisp-mode-hook)



(provide 'setup-emacs-lisp-mode)

(require 'outline)
(require 'bicycle)

(declare-function yas-expand "ext:yasnippet")

(defun ta-defun-above ()
  "Expand the `def' yasnippet above the current root node.

Before doing so, push `symbol-at-point' into the `kill-ring'.
See `yas-expand'. "
  (interactive)
  (if (symbol-at-point) (kill-new (symbol-name (symbol-at-point))))
  (re-search-backward "^[(]" nil t)
  (open-line 2)
  (insert "def")
  (call-interactively 'yas-expand))

(defun ta-outline-emacs-lisp-mode-hook ()
  "Hook to turn on `outline-minor-mode'."
  (outline-minor-mode t))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'ta-outline-emacs-lisp-mode-hook)

(define-key emacs-lisp-mode-map (kbd "C-c C-f") 'ta-defun-above)

(provide 'setup-emacs-lisp-mode)

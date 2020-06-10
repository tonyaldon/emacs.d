(require 'tern)
(require 'outline)

(defun ta-tern-js-mode-hook ()
  "Turn `tern-mode' on in `js-mode'"
  (tern-mode t))

(defun ta-outline-js-mode-hook ()
  "Set up `outline-mode' and `bicycle'.  Specifically,

the variable `outline-regexp'."
  (outline-minor-mode t)
  (setq outline-regexp (concat
                        "//\\|"
                        "const\\|"
                        "[[:space:]]*app\\|"
                        "[[:space:]]*axios\\|"
                        "[[:space:]]*const")))

(add-hook 'js-mode-hook 'ta-tern-js-mode-hook)
(add-hook 'js-mode-hook 'ta-outline-js-mode-hook)

(provide 'setup-js-mode)

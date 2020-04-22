(require 'tern)

(defun ta-tern-js-mode-hook ()
  "Turn `tern-mode' on in `js-mode'"
  (tern-mode t))

(add-hook 'js-mode-hook 'ta-tern-js-mode-hook)


(provide 'setup-js-mode)

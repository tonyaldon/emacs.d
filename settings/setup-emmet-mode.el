(require 'emmet-mode)

(setq emmet-move-cursor-between-quotes t)

(defun ta-emmet-css-mode-hook ()
  "Emmet with css-transform"
	(emmet-mode t)
  (setq emmet-use-css-transform t))

(add-hook 'css-mode-hook 'ta-emmet-css-mode-hook)
(add-hook 'html-mode-hook  'emmet-mode) 


(provide 'setup-emmet-mode)

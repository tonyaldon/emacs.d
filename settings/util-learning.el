(defun ta-start-learning-smartparens ()
  "Set frame and buffers to start learning smartparens keybinding."
  (interactive)
  (sp-cheat-sheet)
	(split-window-below)
	(describe-function 'hydra-sp-reshape/body)
	(ta-drag-window-left)
	(enlarge-window-horizontally 35)
	(split-window-right)
	(find-file "~/Documents/learning/key-bindings/mhtml-mode.el")
	(windmove-right)
	(find-file "~/Documents/learning/key-bindings/_mhtml-mode.el"))

(provide 'util-learning)

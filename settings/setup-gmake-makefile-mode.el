(require 'make-mode)

(defun ta-newline-smart ()
  "Perform `newline-and-indent' or `newline' depending of the context."
  (interactive)
  (if (looking-back "\t")
			(progn
				(delete-char -1)
				(newline))
		(if (looking-back "\n\n")
				(newline)
			(newline)
			(indent-for-tab-command))))

(defun ta-makefile-gmake-mode-hook ()
  "Hook for `makefile-gmake-mode'."
  (setq tab-width 8))

(defadvice makefile-next-dependency
    (after makefile-next-dependency-advice activate)
  "Enter in the `hydra-org/body' map."
  (hydra-org/body))

(defadvice makefile-previous-dependency
    (after makefile-previous-dependency-advice activate)
  "Enter in the `hydra-org/body' map."
  (hydra-org/body))

(add-hook 'makefile-gmake-mode-hook 'ta-makefile-gmake-mode-hook)

(define-key makefile-gmake-mode-map (kbd "RET") 'ta-newline-smart)
(define-key makefile-gmake-mode-map (kbd "M-n") 'windmove-down)
(define-key makefile-gmake-mode-map (kbd "M-p") 'windmove-up)



(provide 'setup-gmake-makefile-mode)

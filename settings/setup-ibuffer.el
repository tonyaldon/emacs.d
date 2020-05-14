(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) 
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(setq
 ibuffer-saved-filter-groups
 '(("home"
		("Org" (mode . org-mode))
    ("emacs-config"
     (or (filename . ".emacs.d")
         (filename . "init.el")
         (filename . "ta-abbrev-mode.el")
         (filename . "ta-dired.el")
         (filename . "ta-key-binding.el")
         (filename . "ta-miscellaneous.el")
         (filename . "ta-programming-environments.el")
         (filename . "ta-specific-modes.el")
         (filename . "ta-ui-preferences.el")
         (filename . "ta-utils.el")))
		("Web Dev" (or (mode . html-mode)
									 (mode . css-mode)))
		("Help" (or (name . "\*Help\*")
								(name . "\*Apropos\*")
								(name . "\*info\*"))))))

(add-hook
 'ibuffer-mode-hook
 '(lambda ()
		(ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "home")))


(provide 'setup-ibuffer)

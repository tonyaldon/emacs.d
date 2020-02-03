(require 'flycheck)

(setq flycheck-disabled-checkers '(python-flake8))
(setq flycheck-python-pylint-executable "~/.local/bin/pylint")
(add-hook 'python-mode-hook 'flycheck-mode)


(provide 'setup-flycheck)

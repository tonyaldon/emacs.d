;;; Packages

(require 'python)
(require 'pydoc)
(require 'py-autopep8)
(require 'anaconda-mode)
(require 'pyvenv)
(require 'flycheck)

;;; Global

(setq python-shell-interpreter "python3")
(setq pydoc-command "python3 -m pydoc")
(setq python-indent-offset 4)

;;; Flycheck

(setq flycheck-disabled-checkers '(python-flake8))
(setq flycheck-python-pylint-executable "~/.local/bin/pylint")

;;; Hooks

(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'pyvenv-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;;; Key bindings

(declare-function rg-dwim "ext:rg")

(define-key pydoc-mode-map (kbd "C-c C-o") 'org-open-at-point)
(define-key python-mode-map (kbd "C-c C-a") 'pyvenv-activate)
(define-key anaconda-mode-map (kbd "C-r") 'rg-dwim)

;;; Footer
(provide 'setup-python-mode)



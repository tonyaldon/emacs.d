(require 'python)
(require 'pydoc)
(require 'py-autopep8)
(require 'anaconda-mode)
(require 'pyvenv)

(setq python-shell-interpreter "python3")
(setq pydoc-command "python3 -m pydoc")

(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(define-key pydoc-mode-map (kbd "C-c C-o") 'org-open-at-point)
(define-key python-mode-map (kbd "C-c C-a") 'pyvenv-activate)
(define-key anaconda-mode-map (kbd "M-r") 'rgrep)

;; TODO
;; Can’t guess python-indent-offset, using defaults: 4


(add-hook 'python-mode-hook 'pyvenv-mode)

;; anaconda
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(provide 'setup-python-mode)

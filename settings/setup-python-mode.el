(require 'python)
(require 'pydoc)
(require 'py-autopep8)
(require 'anaconda-mode)

(setq python-shell-interpreter "python3")

(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(define-key pydoc-mode-map (kbd "C-c C-o") 'org-open-at-point)

;; TODO
;; Can’t guess python-indent-offset, using defaults: 4

;; anaconda
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(provide 'setup-python-mode)

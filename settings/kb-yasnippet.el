(require 'yasnippet)

(yas-global-mode 1)
(define-key yas-keymap (kbd "M-d") (yas-filtered-definition yas-maybe-skip-and-clear-field))


(provide 'kb-yasnippet)

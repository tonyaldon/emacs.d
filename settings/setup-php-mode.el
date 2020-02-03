(eval-after-load 'php-mode '(require 'php-extras))

;; for php-extras Generate the hash table containing the PHP functions:
;; M-x load-library RET php-extras-gen-eldoc RET
;; M-x php-extras-generate-eldoc RET

(add-hook 'php-mode-hook (lambda () (setq c-basic-offset 2)))


(provide 'setup-php-mode)

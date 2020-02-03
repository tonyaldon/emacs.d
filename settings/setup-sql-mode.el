(require 'sql)
(require 'sql-indent)
(require 'sqlup-mode)


(add-hook 'sql-mode-hook 'sqlind-minor-mode) 
(add-hook 'sql-mode-hook 'sqlup-mode)


(provide 'setup-sql-mode)

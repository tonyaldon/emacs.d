(require 'kb)
(require 'company)

(define-key company-active-map (kbd ">") 'company-filter-candidates)
(define-key company-active-map (kbd "]") 'company-other-backend)
(define-key company-active-map (kbd "<next>") 'company-next-page)
(define-key company-active-map (kbd "<prior>") 'company-previous-page)
(define-key company-active-map (kbd "<tab>") 'company-complete-common)
(define-key company-active-map (kbd "C-<tab>") 'company-complete-common)




(provide 'kb-company)

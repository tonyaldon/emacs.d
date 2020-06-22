(require 'kb)
(require 'setup-company)

(define-key company-active-map (kbd ">") 'company-filter-candidates)
(define-key company-active-map (kbd "]") 'ta-company-fuzzy-mode-toggle)

(define-key company-active-map (kbd "}") 'company-other-backend)
(define-key company-active-map (kbd "(") 'company-show-doc-buffer)
(define-key company-active-map (kbd "<left>") 'ta-company-previous-show-doc)
(define-key company-active-map (kbd "<right>") 'ta-company-next-show-doc)
(define-key company-active-map (kbd "<up>") 'company-select-previous-or-abort)
(define-key company-active-map (kbd "<down>") 'company-select-next-or-abort)
(define-key company-active-map (kbd "<prior>") 'company-previous-page)
(define-key company-active-map (kbd "<next>") 'company-next-page)
(define-key company-active-map (kbd "<tab>") 'company-complete-common)


(provide 'kb-company)

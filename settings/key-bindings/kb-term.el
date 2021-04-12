(require 'kb)
(require 'term)
(require 'eterm-256color)

(defun ta-term-disable-hl-line-mode ()
  "Disable `hl-line-mode' in `term-mode'."
  (setq global-hl-line-mode nil))

(add-hook 'term-mode-hook #'eterm-256color-mode)
(add-hook 'term-mode-hook 'ta-term-disable-hl-line-mode)

(define-key term-raw-map (kbd "M-p") nil)
(define-key term-raw-map (kbd "M-n") nil)
(define-key term-raw-map (kbd "M-u") nil)
(define-key term-raw-map (kbd "C-o") nil)
(define-key term-raw-map (kbd "<left>") nil)
(define-key term-raw-map (kbd "<right>") nil)
(define-key term-raw-map (kbd "C-x") nil)
(define-key term-raw-map (kbd "M-x") nil)
(define-key term-raw-map (kbd "C-y") nil)
(define-key term-raw-map (kbd "C-y") 'ta-term-yank)
(define-key term-raw-map (kbd "C-M-b") 'ta-windmove-left)
(define-key term-raw-map (kbd "C-M-f") 'ta-windmove-right)

(define-key term-raw-map (kbd "M-t") 'term-line-mode)
(define-key term-mode-map (kbd "M-t") 'term-char-mode)


(provide 'kb-term)

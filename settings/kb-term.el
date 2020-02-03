(require 'kb)
(require 'term)
(require 'eterm-256color)

(defun ta-term-bash ()
  (interactive)
  (split-window-below -12)
  (other-window 1)
  (term "/bin/bash"))

(defun ta-term-yank ()
  "Yank in `term-raw-map'."
  (interactive)
  (read-only-mode -1)
  (call-interactively 'yank)
  (read-only-mode t))


(defun ta-term-hl-line-mode ()
  "Disable `hl-line-mode' in `term-mode'."
  (setq global-hl-line-mode nil))

(add-hook 'term-mode-hook #'eterm-256color-mode)
(add-hook 'term-mode-hook 'ta-term-hl-line-mode)

(define-key term-raw-map (kbd "M-p") nil)
(define-key term-raw-map (kbd "M-n") nil)
(define-key term-raw-map (kbd "M-u") nil)
(define-key term-raw-map (kbd "C-o") nil)
(define-key term-raw-map (kbd "C-`") nil)
(define-key term-raw-map (kbd "C-`") nil)
(define-key term-raw-map (kbd "<left>") nil)
(define-key term-raw-map (kbd "<right>") nil)
(define-key term-raw-map (kbd "C-x") nil)
(define-key term-raw-map (kbd "M-x") nil)
(define-key term-raw-map (kbd "C-y") nil)
(define-key term-raw-map (kbd "C-y") 'ta-term-yank)

(define-key term-raw-map (kbd "M-`") 'term-line-mode)
(define-key term-mode-map (kbd "M-`") 'term-char-mode)


(provide 'kb-term)

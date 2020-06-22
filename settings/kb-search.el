(require 'setup-search)
(require 'iy-go-to-char)
(require 'ibuffer)

(defhydra hydra-toggle
  (:pre (hydra-color-pre)
   :post (hydra-color-post)
   :hint nil)
  ("n" ta-toggle-write-mode :color blue)
  ("i" ta-w-abort-changes :color blue)
  ("e" ta-w-exit :color blue)
  ("f" ta-w-finish-edit :color blue)
  ("M--" undo)
  ("q" nil))

(global-set-key (kbd "M-t") 'hydra-toggle/body)
(global-set-key (kbd "M-s") 'swiper)
(define-key ibuffer-mode-map (kbd "M-s") 'swiper)
(define-key dired-mode-map (kbd "M-s") 'swiper)
;; (global-set-key (kbd "M-r") 'rgrep)
(global-set-key (kbd "M-r") 'projectile-grep)
(global-set-key (kbd "M-.") 'swiper-thing-at-point)
(global-set-key (kbd "M-S") 'query-replace)
(global-set-key (kbd "M-R") 'query-replace-regexp)
(define-key grep-mode-map (kbd "M-p") 'windmove-up)
(define-key grep-mode-map (kbd "M-n") 'windmove-down)
(define-key ivy-minibuffer-map (kbd "<left>") 'ivy-previous-history-element)
(define-key ivy-minibuffer-map (kbd "<right>") 'ivy-next-history-element)
(define-key ivy-minibuffer-map (kbd "<up>") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "<down>") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "M-e") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "M-a") 'ivy-dispatching-done)
(define-key ivy-minibuffer-map (kbd "M-s") 'ivy-occur)
(define-key swiper-map (kbd "M-q") 'minibuffer-keyboard-quit)
(define-key swiper-map (kbd "M-c") 'swiper-mc)
(global-set-key (kbd "C-M-a") 'iy-go-to-char-backward)
(global-set-key (kbd "C-M-e") 'iy-go-to-char)


(provide 'kb-search)

(require 'kb)
(require 'setup-projects)
(require 'org)

(declare-function ibuffer "ibuffer")

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "<f5>") 'ta-find-file-notes)
(global-set-key (kbd "M->") 'counsel-quick-access)
(define-key org-mode-map (kbd "C-e") nil)
(define-key org-mode-map (kbd "M-e") nil)
(define-key org-mode-map (kbd "C-a") nil)
(define-key org-mode-map (kbd "M-a") nil)
(global-set-key (kbd "C-e") 'projectile-find-file)
(global-set-key (kbd "M-e") 'counsel-find-file)
(global-set-key (kbd "C-a") 'projectile-switch-to-buffer)
(global-set-key (kbd "M-a") 'ivy-switch-buffer-other-window)
(key-chord-define-global "::" 'ibuffer)
(global-set-key (kbd "M-<next>") 'projectile-command-map)

;; d - #'projectile-find-dir
;; D - #'projectile-dired
;; f - #'projectile-find-file
;; ???  projectile--find-file
;; g - #'projectile-find-file-dwim
;; F - #'projectile-find-file-in-known-projects
;; I - #'projectile-ibuffer
;; l - #'projectile-find-file-in-directory
;; p - #'projectile-switch-project
;; q - #'projectile-switch-open-project
;; s g - #'projectile-grep
;; s r - #'projectile-ripgrep
;; t - #'projectile-toggle-between-implementation-and-test
;; T - #'projectile-find-test-file
;; v - #'projectile-vc
;; x t - #'projectile-run-term
;; <left> - #'projectile-previous-project-buffer
;; <right> - #'projectile-next-project-buffer
;; ESC - #'projectile-project-buffers-other-buffer


(provide 'kb-projects)

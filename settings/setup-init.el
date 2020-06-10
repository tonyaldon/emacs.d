(setq browse-url-browser-function 'browse-url-chromium)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(setq-default tab-width 2)

(set-language-environment "UTF-8")
(pending-delete-mode t)

(setenv "PATH" (concat "~/.local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "~/.local/bin")

(add-to-list `auto-mode-alist '("\\.svg\\'" . fundamental-mode))

;; TODO: to dispatch in appropriate setup files
(require 'recentf)
(recentf-mode 1)
(setq recentf-save-file "~/.emacs.d/recentf")
(setq recentf-max-saved-items 25)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(defadvice magit-status (before ta-magit-status-advice activate)
	(unless (s-contains-p "magit" (buffer-name)) (delete-other-windows)))

(defadvice magit-commit-create (before ta-magit-commit-create-advice activate)
	(delete-other-windows))

(setq save-interprogram-paste-before-kill t)
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'setup-init)

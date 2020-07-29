(require 'quick-access)
(require 'recentf)
(require 'magit)
(require 'projectile)

(save-place-mode t)
(global-auto-revert-mode 1)
(recentf-mode 1)
(projectile-mode +1)

(setq save-place-file "~/emacs.d/places")
(setq recentf-save-file "~/.emacs.d/recentf")
(setq recentf-max-saved-items 25)

(setq quick-access-alist
      '(("notes" . "~/work/notes.org")
        ("extra" . "~/work/extra.org")
				("tmp" . "~/work/tmp/")
				("Jack Inside - private" . "~/work/jackinside/private/")
        ("i3 config" . "~/work/settings/i3/.config/i3/config")
        ("settings emacs" . "~/work/settings/emacs.d/.emacs.d/settings/")
        ("settings linux" . "~/work/settings/")
        ("learning" . "~/work/learning/")
				("tricks emacs" . "~/work/learning/tricks/org/emacs.org")
        ("tricks linux" . "~/work/learning/tricks/org/linux.org")
        ("tricks git" . "~/work/learning/tricks/org/git.org")
        ("tricks miscellaneous" . "~/work/learning/tricks/org/miscellaneous.org")
        ("tricks video" . "~/work/learning/tricks/org/video.org")
        ("tricks image" . "~/work/learning/tricks/org/image.org")
        ("csv - my videos" . "~/work/learning/videos/videos.csv")
        ("csv - my expenses" . "~/life/home/expenses/expenses.csv")
				("videos - programming-sessions" . "~/work/videos/programming-sessions/")
				("videos - youtube/README" . "~/work/videos/youtube/README.org")))

(projectile-add-known-project "~/work/learning/apps/emacs")
(projectile-add-known-project "~/work/learning/tricks/")
(projectile-add-known-project "~/work/settings/")
(projectile-add-known-project "~/work/settings/emacs.d/.emacs.d/")
(projectile-add-known-project "~/work/apps/tricks-app/")

(setq projectile-completion-system 'ivy)
(setq projectile-find-dir-includes-top-level t)
(setq projectile-indexing-method 'hybrid)
(setq projectile-enable-caching t)
(setq projectile-sort-order 'default)
(setq projectile-require-project-root 'prompt)
(setq projectile-current-project-on-switch 'remove)
(setq projectile-switch-project-action 'projectile-find-file)

(defun ta-find-file-notes ()
  (interactive)
  (find-file "~/work/notes.org"))

(defadvice magit-status (before ta-magit-status-advice activate)
  (unless (s-contains-p "magit" (buffer-name)) (delete-other-windows)))

(defadvice magit-commit-create (before ta-magit-commit-create-advice activate)
  (delete-other-windows))

(defun ta-pre-format-git-tag ()
  "Pre-Format git-tag. In a buffer with the output of \"git log\" up
to the last tag remove no relevant information and bad indentation."
  (interactive)
  (beginning-of-buffer)
	(flush-lines "^\\(commit\\|Author\\|Date\\)")
	(replace-regexp "^    " "" nil (point-min) (point-max))
	(replace-regexp "\n\n\n" "\n\n" nil (point-min) (point-max)))


(provide 'setup-projects)

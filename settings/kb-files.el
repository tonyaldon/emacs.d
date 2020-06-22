(require 'kb)
(require 'org)
(require 'util-mathstyle)
(require 'ibuffer)
(require 'quick-access)

(setq save-place-file "~/emacs.d/places")

(save-place-mode t)
(global-auto-revert-mode 1)

(setq quick-access-alist
			'(("notes" . "~/work/notes.org")
				("extra" . "~/work/extra.org")
				("i3 config" . "~/work/settings/i3/.config/i3/config")
				("settings emacs" . "~/work/settings/emacs.d/.emacs.d/settings/")
				("settings linux" . "~/work/settings/")
				("tricks emacs" . "~/work/learning/tricks/org/emacs.org")
				("tricks linux" . "~/work/learning/tricks/org/linux.org")
				("tricks git" . "~/work/learning/tricks/org/git.org")
				("tricks miscellaneous" . "~/work/learning/tricks/org/miscellaneous.org")
				("tricks video" . "~/work/learning/tricks/org/video.org")
				("tricks image" . "~/work/learning/tricks/org/image.org")
				("emacs source code" . "~/work/apps/emacs/")
				("csv - my videos" . "~/work/learning/videos/videos.csv")
				("csv - my expenses" . "~/life/home/expenses/expenses.csv")))

(defun ta-find-file-notes ()
  (interactive)
  (find-file "~/work/notes.org"))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting.

see: http://github.com/magnars"
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun ta-number-lines-whole-buffer ()
  "Number of lines in the whole buffer.
Printed in the message area"
  (interactive)
  (message "Buffer's number of lines: %s"
           (int-to-string (count-lines (point-min) (point-max)))))

(defun ta-copy-buffer-file-name ()
  "Push current `buffer-file-name' to the `kill-ring'."
  (interactive)
  (kill-new (buffer-file-name)))

(global-set-key (kbd "<f5>") 'ta-find-file-notes)
(global-set-key (kbd "<f6>") 'save-buffer)
(global-set-key (kbd "<left>") 'previous-buffer)
(global-set-key (kbd "<right>") 'next-buffer)

(define-key org-mode-map (kbd "C-e") nil)
(define-key org-mode-map (kbd "M-e") nil)
(define-key org-mode-map (kbd "C-a") nil)
(define-key org-mode-map (kbd "M-a") nil)

(global-set-key (kbd "M->") 'counsel-quick-access)
(global-set-key (kbd "C-e") 'counsel-find-file)
(global-set-key (kbd "M-e") 'find-file-other-window)
(global-set-key (kbd "C-a") 'ivy-switch-buffer)
(global-set-key (kbd "M-a") 'ivy-switch-buffer-other-window)
(key-chord-define-global "::" 'ibuffer)


(provide 'kb-files)

(require 'kb)
(require 'org)
(require 'util-mathstyle)

(setq save-place-file "~/emacs.d/places")

(save-place-mode t)
(global-auto-revert-mode 1)

(defun ta-find-file-notes ()
  (interactive)
  (find-file "~/work/notes.org"))

(defun ta-find-directory-emacs-settings ()
  (interactive)
  (dired "~/work/settings/emacs.d/.emacs.d/settings/"))

(defun ta-find-directory-settings ()
  (interactive)
  (dired "~/work/settings/"))

(defun ta-find-file-i3-config ()
  (interactive)
  (find-file "~/work/settings/i3/.config/i3/config"))

(defun ta-find-file-tricks ()
  (interactive)
  (find-file "~/work/learning/tricks/org/tricks.org"))

(defun ta-find-file-emacs-app ()
  (interactive)
  (find-file "~/work/apps/emacs/"))

(defun ta-find-file-videos ()
  (interactive)
  (find-file "~/work/learning/videos/videos.csv"))

(defun ta-find-file-expenses ()
  (interactive)
  (find-file "~/life/home/expenses/expenses.csv"))

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


(defhydra hydra-files
  (
   :pre (hydra-color-pre)
   :post (hydra-color-post)
   :hint nil)
	("i" ta-find-file-i3-config :color blue)
	("e" ta-find-file-emacs-app :color blue)
	("E" ta-find-file-expenses :color blue)
	("t" ta-find-file-tricks :color blue)
	("s" ta-find-directory-emacs-settings :color blue)
	("S" ta-find-directory-settings :color blue)
	("v" ta-find-file-videos :color blue)
	("r" rename-current-buffer-file :color blue)
	("l" ta-number-lines-whole-buffer :color blue)
	("d" ms-dashboard)
  ;; ---
  ("M--" undo)
  ("q" nil))

(global-set-key (kbd "M->") 'hydra-files/body)

(global-set-key (kbd "<f5>") 'ta-find-file-notes)
(global-set-key (kbd "<f6>") 'save-buffer)
(global-set-key (kbd "<left>") 'previous-buffer)
(global-set-key (kbd "<right>") 'next-buffer)

(define-key org-mode-map (kbd "C-e") nil)
(define-key org-mode-map (kbd "M-e") nil)
(define-key org-mode-map (kbd "C-a") nil)
(define-key org-mode-map (kbd "M-a") nil)

(global-set-key (kbd "C-e") 'counsel-find-file)
(global-set-key (kbd "M-e") 'find-file-other-window)
(global-set-key (kbd "C-a") 'ivy-switch-buffer)
(global-set-key (kbd "M-a") 'ivy-switch-buffer-other-window)
(key-chord-define-global "::" 'ibuffer)


(provide 'kb-files)

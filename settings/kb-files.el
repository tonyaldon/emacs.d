(require 'kb)
(require 'org)

(setq save-place-file "~/emacs.d/places")

(save-place-mode t)
(global-auto-revert-mode 1)

(defun ta-find-file-notes ()
  (interactive)
  (find-file "~/Documents/notes.org"))

(defun ta-find-directory-settings ()
  (interactive)
  (dired "~/Documents/settings/dotfiles/emacs/.emacs.d/settings/"))

(defun ta-find-file-variable-costs ()
  (interactive)
  (find-file "~/Documents/life/household/household-expenses/variable-costs.csv"))

(defun ta-find-file-i3-config ()
  (interactive)
  (find-file "~/Documents/settings/dotfiles/user-config-file/.config/i3/config"))

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
	("c" ta-find-file-variable-costs :color blue)
	("d" ta-find-file-i3-config :color blue)
	("s" ta-find-directory-settings :color blue)
	("r" rename-current-buffer-file :color blue)
	("l" ta-number-lines-whole-buffer :color blue)
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

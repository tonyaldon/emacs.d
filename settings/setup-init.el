(setq browse-url-browser-function 'browse-url-chromium)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq-default tab-width 2)
(set-language-environment "UTF-8")
(pending-delete-mode t)
(setenv "PATH" (concat "~/.local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "~/.local/bin")
(add-to-list `auto-mode-alist '("\\.svg\\'" . fundamental-mode))
(setq csv-separators '(","))
(setq save-interprogram-paste-before-kill t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; TODO: to dispatch in appropriate setup files
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

(provide 'setup-init)

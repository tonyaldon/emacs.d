(require 'dired)
(require 'dired-subtree)
(require 'dired-narrow)
(require 'dired-hide-dotfiles)
(require 'dired-open)
(require 's)
(require 'dash)
(require 'dired-hacks-utils)
(require 'dired-toggle-sudo)
(require 'dired-collapse)
(require 'wdired)
(require 'dired-x)

(setq dired-keep-marker-rename t)
(setq wdired-allow-to-change-permissions nil)
(setq wdired-create-parent-directories t)
(setq dired-narrow-exit-when-one-left t)
(setq dired-clean-up-buffers-too t)
(setq dired-clean-confirm-killing-deleted-buffers t)

(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq-default dired-listing-switches "-lhA --group-directories-first")
(setq dired-open-extensions
      '(("pdf" . "evince")
        ("jpg" . "eog")
        ("png" . "eog")))
(setq-default dired-subtree-line-prefix "  ")
(setq-default dired-subtree-use-backgrounds nil)

(dired-hide-dotfiles-mode)

(defun ta-size-bigger-file-or-directory-in-dired ()
  "Return the number of characters of the bigger FILE-OR-DIRECTORY in current dired buffer."
  (with-current-buffer (current-buffer)
	  (-max (--map (length (-last-item (s-split "/" it)))
								 (dired-utils-get-all-files)))))

(defun ta-sidebar ()
  "Pop a buffer on the left of the frame in `dired-mode'

with the parent directory of the current `buffer-file-name' if not `nil' and
if the frame contains any buffer in `dired-mode'. If the frame contains buffers
in `dired-mode', delete them.
"
  (interactive)
  ;; TODO: - check the case of buffer-file-name is nil
  ;; TODO: - check the case of the is only one buffer in dired-mode
  (setq current-window (car (avy-window-list)))
  (setq window-list (avy-window-list))
  (setq dired-buffer-into-frame-p nil)
  (while window-list
    (select-window (car window-list))
    (if (string-equal major-mode "dired-mode")
        (progn
          (setq dired-buffer-into-frame-p t)
          (delete-window)))
    (setq window-list (cdr window-list)))
  (if dired-buffer-into-frame-p
      nil
    (select-window current-window)
    (delete-other-windows)
    (let ((width (with-current-buffer
                     (dired-noselect (file-name-directory (buffer-file-name)))
                   (ta-size-bigger-file-or-directory-in-dired))))
      (split-window-right (+ 10 width))) ; 10 is arbitrary
    (dired (file-name-directory (buffer-file-name)))))

(define-minor-mode dired-header-line-mode
  "Show only the last two directories of the path to the current directory
that `dired-mode' is displaying."
  :init-value nil :group 'header-line :group 'dired
  (unless (derived-mode-p 'dired-mode)
    (error "You must be in Dired or a mode derived from it to use this command"))
  (if dired-header-line-mode
      (progn
        (setq path-len
              (length
               (s-split
                "/"
                (expand-file-name dired-directory))))
        (setq header-line-directories
              (car
               (last
                (s-split-up-to
                 "/"
                 (expand-file-name dired-directory)
                 (- path-len 3)))))
        (setq header-line-format (concat "..." header-line-directories)))
    (setq header-line-format  (default-value 'header-line-format))))

(defface ta-dired-header-face
  nil
  "Face for dired header, first line of buffer in `dired-mode'"
  :group 'dired)

(font-lock-add-keywords
 'dired-mode
 '(("\\(^.*:$\\)" . 'ta-dired-header-face)))

(font-lock-add-keywords
 'wdired-mode
 '(("\\(^.*:$\\)" . 'ta-dired-header-face)))

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
(add-hook 'dired-before-readin-hook 'dired-header-line-mode)

(define-key dired-mode-map (kbd "[") 'dired-hide-details-mode)
(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
(define-key dired-mode-map (kbd "<backspace>") 'dired-subtree-remove)
;; (define-key dired-mode-map (kbd "(") 'dired-subtree-previous-sibling)
;; (define-key dired-mode-map (kbd ")") 'dired-subtree-next-sibling)
(define-key dired-mode-map (kbd "<up>") 'dired-subtree-up)
(define-key dired-mode-map (kbd "/") 'dired-narrow)
(define-key dired-mode-map (kbd ".") 'dired-hide-dotfiles-mode)

(global-set-key (kbd "M-]") 'ta-sidebar)

;; new bindings
(define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
(define-key dired-mode-map (kbd ")") 'dired-subtree-cycle)
(define-key dired-mode-map (kbd "C-c C-c") 'dired-collapse-mode)







(provide 'setup-dired)

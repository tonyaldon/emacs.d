;;; Packages

(require 'dired)
(require 'dired-hacks-utils)
(require 'dired-subtree)
(require 'dired-narrow)
(require 'dired-hide-dotfiles)
(require 'dired-open)
(require 's)
(require 'dash)
(require 'dired-toggle-sudo)
(require 'dired-collapse)
(require 'wdired)
(require 'dired-x)
(require 'dired-rainbow)

;;; Global
(dired-hide-dotfiles-mode)

(setq dired-keep-marker-rename t)
(setq wdired-allow-to-change-permissions nil)
(setq wdired-create-parent-directories t)
(setq dired-narrow-exit-when-one-left t)
(setq dired-clean-up-buffers-too t)
(setq dired-clean-confirm-killing-deleted-buffers t)
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

(setq dired-open-extensions
      '(("pdf" . "evince")
        ("jpg" . "eog")
        ("png" . "eog")))
(setq-default dired-subtree-line-prefix "  ")
(setq-default dired-subtree-use-backgrounds nil)

;; dired-broken-symlink color??

;;; Dired listing

(setq-default dired-listing-switches "-lhAX --group-directories-first")

(defun ta-toggle-dired-sort ()
  "Toggle Dired listing between extension or name.

Directories are always listed first."
  (interactive)
  (let ((ls-by-extension "-lhAX --group-directories-first")
        (ls-by-name "-lhA --group-directories-first"))
    (if (equal dired-listing-switches ls-by-extension)
        (setq dired-listing-switches ls-by-name)
      (setq dired-listing-switches ls-by-extension)))
  (revert-buffer))
;;; auto-revert-mode

(defun ta-dired-auto-revert ()
  "Set `auto-revert-mode' in `dired-mode' buffers."
  (auto-revert-mode 1)
  (set (make-local-variable 'auto-revert-verbose) nil))

(add-hook 'dired-mode-hook 'ta-dired-auto-revert)

;;; Subtree

(defun ta-dired-subtree-toggle-all ()
  "Apply `dired-subtree-toggle' to all root directories

in the dired buffer"
  (interactive)
  (setq deactivate-mark t)
  (save-excursion
    (cond
     ((eq last-command 'dired-subtree-toggle-overview)
      (goto-char (point-min))
      (next-line)
      (while (not (eobp))
        (if (dired-utils-is-dir-p) (dired-subtree-toggle))
        (dired-subtree-next-sibling))
      (message "ALL"))
     ((eq last-command 'ta-dired-subtree-toggle-all)
      (goto-char (point-min))
      (next-line)
      (while (not (eobp))
        (if (dired-utils-is-dir-p) (dired-subtree-toggle))
        (dired-subtree-next-sibling))
      (message "ALL"))
     (t
      (goto-char (point-max))
      (previous-line)
      (setq number-line-before-remove (line-number-at-pos))
      (dired-subtree-remove)
      (while (not (bobp))
        (while (not (equal number-line-before-remove (line-number-at-pos)))
          (setq number-line-before-remove (line-number-at-pos))
          (dired-subtree-remove))
        (previous-line))
      (message "OVERVIEW")
      (setq this-command 'dired-subtree-toggle-overview)))))

;;; Header line

(define-minor-mode dired-header-line-mode
  "Show only the last two directories of the path to the current directory
that `dired-mode' is displaying."
  :init-value nil :group 'header-line :group 'dired
  (unless (derived-mode-p 'dired-mode)
    (error "You must be in Dired or a mode derived from it to use this command"))
  (if dired-header-line-mode
      (progn
        (setq path-len (length (s-split "/" (expand-file-name dired-directory))))
        (setq header-line-directories
              (car (last (s-split-up-to "/" (expand-file-name dired-directory)
                                        (- path-len 3)))))
        (setq header-line-format (concat " ↪[" header-line-directories "]")))
    (setq header-line-format  (default-value 'header-line-format))))

(defface ta-dired-header-face nil
  "Face for dired header, first line of buffer in `dired-mode'"
  :group 'dired)

(font-lock-add-keywords
 'dired-mode
 '(("\\(^.*:$\\)" . 'ta-dired-header-face)))

(font-lock-add-keywords
 'wdired-mode
 '(("\\(^.*:$\\)" . 'ta-dired-header-face)))

(add-hook 'dired-before-readin-hook 'dired-header-line-mode)


;;; dired-rainbow
(dired-rainbow-define-chmod executable-unix "#7fffd4" "-.*x.*")
(dired-rainbow-define media "#d4fbcb"
                      ("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg" "m4a"))
(dired-rainbow-define image "#a4c30d" ("jpg" "jpeg" "png"))
(dired-rainbow-define svg "#8a510f" ("svg"))
(dired-rainbow-define media-editing "#e421e8" ("kdenlive" "aup"))
(dired-rainbow-define plain-text "#ffd500" ("org" "md"))
(dired-rainbow-define emacs-lisp "#00bdd6" ("el"))
(dired-rainbow-define python "#d16500" ("py"))
(dired-rainbow-define js "#bb8415" ("js"))
(dired-rainbow-define shadow (:inherit shadow) "\\.git.*" )

;;; Hooks

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))


;;; Footer

(provide 'setup-dired)

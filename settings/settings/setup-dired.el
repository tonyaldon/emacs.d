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

;;; Hooks

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))


;;; Footer

(provide 'setup-dired)

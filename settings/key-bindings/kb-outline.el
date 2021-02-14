;;; Packages

(require 'bicycle)
;; (setq outline-regexp ";;;\\(;* [^]\\|###autoload\\)\\|(") ; default in emacs-lisp-mode

;;; Utility functions

(defun ta-outline-toggle-global ()
  "Toggle visibility of all outline (see `outline-mode') sections.

This command toggle between this following levels:
1. FOLDED:   Show only top level heading.
1. TREES:    Show all headings, treaing top-level code blocks
             as sections (i.e. their first line is treated as
             a heading).
2. ALL:      Show everything, except code blocks that have been
             collapsed individually (using a `hideshow' command
             or function).

This is a variant off (hack on) the `bicycle-cycle-global'."
  (interactive)
  (setq deactivate-mark t)
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward outline-regexp nil t)
      (user-error "Found no heading"))
    (cond
     ((eq last-command 'outline-cycle-folded)
      (outline-hide-sublevels (bicycle--level))
      (outline-map-region
       (lambda ()
         (when (bicycle--top-level-p)
           (outline-show-branches)))
       (point-min)
       (point-max))
      (bicycle--message "TREES")
      (setq this-command 'outline-cycle-trees))
     ((eq last-command 'outline-cycle-trees)
      (outline-show-all)
      (bicycle--message "ALL"))
     (t
      (outline-map-region
       (lambda () (when (bicycle--top-level-p)
                    (outline-hide-subtree)))
       (point-min)
       (point-max))
      (bicycle--message "FOLDED")
      (setq this-command 'outline-cycle-folded)
      ))))

;;; Key bindings

(define-key emacs-lisp-mode-map (kbd "TAB") 'bicycle-cycle)
(define-key clojure-mode-map (kbd "TAB") 'bicycle-cycle)
(global-set-key (kbd "C-M-b") 'outline-previous-visible-heading)
(global-set-key (kbd "C-M-f") 'outline-next-visible-heading)
(global-set-key (kbd "C-SPC") 'ta-outline-toggle-global)

;;; speed keys

(setq outline-spc-default nil)
(setq outline-spc-user
      '(("Outline Navigation")
        ("n" . (outline-spc-move-safe 'outline-next-visible-heading))
        ("p" . (outline-spc-move-safe 'outline-previous-visible-heading))
        ("f" . (outline-spc-move-safe 'outline-forward-same-level))
        ("b" . (outline-spc-move-safe 'outline-backward-same-level))
        ("i" . (outline-spc-move-safe 'outline-up-heading))
        ("Outline Structure Editing")
        ("." . org-toggle-narrow-to-subtree)
        ("@" . outline-mark-subtree)
        ("`". outline-move-subtree-up)
        (",". outline-move-subtree-down)
        ("+". outline-insert-heading)
        "The default Outline speed commands."))

;;; Footer

(provide 'kb-outline)

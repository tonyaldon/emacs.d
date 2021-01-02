;;; Global key bindings settings
;;;; Packages

(require 'avy)
(require 'hydra)
(require 'iso-transl)
(require 'key-chord)

;;;; avy

(setq avy-highlight-first t)
(setq avy-style 'at-full)
(setq avy-keys '(?a ?u ?i ?e ?y ?x ?o ?w ?b ?n ?f ?p ?k
										?t ?s ?r ?q ?d ?l ?j ?m ?c ?g ?h
										?( ?) ?[ ?] ?< ?> ?, ?\; ?. ?:))

;;;; hydra

(setq-default hydra-hint-display-type  'message)

;;;; key-chord

(key-chord-mode t)
(setq key-chord-two-keys-delay 0.1)
(setq key-chord-one-key-delay 0.2)

;;; Global key bindings
;;;; avy

(require 'avy)

(defun ta-avy-goto-end-of-line ()
  "Call `avy-goto-char' with \"\n\" as argument."
  (interactive)
  (avy-goto-char ?\n))

(global-set-key (kbd "M-)") 'ta-avy-goto-end-of-line)
(global-set-key (kbd "M-(") 'avy-goto-line)
(global-set-key (kbd "<down>") 'avy-goto-char)

;;;; buffers

(global-set-key [escape] 'kill-this-buffer)
(global-set-key (kbd "<f6>") 'save-buffer)
(global-set-key (kbd "<left>") 'previous-buffer)
(global-set-key (kbd "<right>") 'next-buffer)
(key-chord-define-global "::" 'ibuffer)

;;;; completion

(global-set-key (kbd "M->") 'counsel-quick-access)
(global-set-key (kbd "C->") 'counsel-outline)
(global-set-key (kbd "M-e") 'counsel-find-file)
(global-set-key (kbd "C-a") 'project-switch-to-buffer)
(global-set-key (kbd "M-a") 'ivy-switch-buffer-other-window)

(global-set-key (kbd "C-b") 'swiper)
(global-set-key (kbd "C-n") 'swiper-all)
(global-set-key (kbd "C-f") 'swiper-thing-at-point)
(global-set-key (kbd "C-M-n") 'counsel-rg)

(global-set-key (kbd "C-x C-e") 'ta-fzf-emacs-settings)
(global-set-key (kbd "C-e") 'ta-fzf-dwim)


;;;; dired-mode-map

(define-key dired-mode-map (kbd "C-M-p") nil)
(define-key dired-mode-map (kbd "C-M-n") nil)

(define-key dired-mode-map (kbd ",") 'dired-mark)
(define-key dired-mode-map (kbd "t") 'dired-toggle-marks)
(define-key dired-mode-map (kbd "u") 'dired-unmark)
(define-key dired-mode-map (kbd "k") 'dired-do-kill-lines)
(define-prefix-command 'ta-dired-mark-map)
(define-key ta-dired-mark-map (kbd "r") 'dired-mark-files-regexp)
(define-key ta-dired-mark-map (kbd "e") 'dired-mark-extension)
(define-key ta-dired-mark-map (kbd "d") 'dired-mark-directories)
(define-key ta-dired-mark-map (kbd "u") 'dired-unmark-all-marks)
(define-key dired-mode-map (kbd "]") 'ta-dired-mark-map)

(define-key dired-mode-map (kbd "v") 'dired-view-file)
(define-key dired-mode-map (kbd "y") 'dired-show-file-type)
(define-key dired-mode-map (kbd "!") 'dired-do-shell-command)
(define-key dired-mode-map (kbd "&") 'dired-do-async-shell-command)
(define-key dired-mode-map (kbd "=") 'dired-diff)
(define-key dired-mode-map (kbd "g") 'revert-buffer)
(define-key dired-mode-map (kbd "L") 'dired-do-load)

(define-key dired-mode-map (kbd "i") 'dired-up-directory)
(define-key dired-mode-map (kbd "p") 'dired-previous-line)
(define-key dired-mode-map (kbd "n") 'dired-next-line)
(define-key dired-mode-map (kbd "M-)") 'dired-next-marked-file)
(define-key dired-mode-map (kbd "M-(") 'dired-next-marked-file)

(define-key dired-mode-map (kbd "+") 'dired-create-directory)
(define-key dired-mode-map (kbd "r") 'dired-do-rename)
(define-key dired-mode-map (kbd "c") 'dired-do-copy)
(define-key dired-mode-map (kbd "x") 'dired-do-flagged-delete)
(define-key dired-mode-map (kbd "w") 'dired-copy-filename-as-kill)
(define-key dired-mode-map (kbd "S") 'dired-do-symlink)
(define-key dired-mode-map (kbd "R") 'dired-do-relsymlink)

(define-prefix-command 'ta-dired-find-file-map)
(define-key ta-dired-find-file-map (kbd "e") 'ace-hacks-dired-find-file)
(define-key ta-dired-find-file-map (kbd "p") 'ace-hacks-dired-find-file-split-up)
(define-key ta-dired-find-file-map (kbd "n") 'ace-hacks-dired-find-file-split-down)
(define-key ta-dired-find-file-map (kbd "b") 'ace-hacks-dired-find-file-split-left)
(define-key ta-dired-find-file-map (kbd "f") 'ace-hacks-dired-find-file-split-right)
(define-key ta-dired-find-file-map (kbd "'") 'dired-do-find-marked-files)
(define-key dired-mode-map (kbd "e") 'ta-dired-find-file-map)

(define-key dired-mode-map (kbd "C-SPC") 'ta-dired-subtree-toggle-all)
(define-key dired-mode-map (kbd "C-<tab>") 'dired-subtree-toggle)
(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-cycle)
(define-key dired-mode-map (kbd "<backspace>") 'dired-subtree-remove)
(define-key dired-mode-map (kbd ":") 'dired-subtree-up)
(define-key dired-mode-map (kbd "b") 'dired-subtree-previous-sibling)
(define-key dired-mode-map (kbd "f") 'dired-subtree-next-sibling)

(define-key dired-mode-map (kbd "[") 'dired-hide-details-mode)
(define-key dired-mode-map (kbd ".") 'dired-hide-dotfiles-mode)
(define-key dired-mode-map (kbd "/") 'dired-narrow)
(define-key dired-mode-map (kbd "%") 'dired-collapse-mode)
(define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
(define-key dired-mode-map (kbd ")") 'ta-aw-other-window-scroll-buffer)
(define-key dired-mode-map (kbd "<SPC>") 'peep-dired)
(define-key peep-dired-mode-map (kbd "_") 'peep-dired-ace-window)
(define-key peep-dired-mode-map (kbd "C-n") nil)
(define-key peep-dired-mode-map (kbd "C-p") nil)
(define-key peep-dired-mode-map (kbd "<up>") nil)
(define-key peep-dired-mode-map (kbd "<down>") nil)
(define-key peep-dired-mode-map (kbd "<SPC>") nil)
(define-key peep-dired-mode-map (kbd "<backspace>") nil)
(define-key peep-dired-mode-map (kbd "q") nil)
(define-key peep-dired-mode-map (kbd "p") 'peep-dired-prev-file)
(define-key peep-dired-mode-map (kbd "n") 'peep-dired-next-file)
(define-key peep-dired-mode-map (kbd "<next>") 'peep-dired-scroll-page-down)
(define-key peep-dired-mode-map (kbd "<prior>") 'peep-dired-scroll-page-up)

(define-key dired-mode-map (kbd "M-s") 'isearch-forward)
(define-key dired-mode-map (kbd "M-r") 'isearch-backward)
(define-key dired-mode-map (kbd "(") 'avy-goto-line)

(define-key dired-mode-map (kbd "C-t") 'ta-toggle-dired-sort)

;;;; eval

(global-set-key (kbd "<f1>") 'eval-defun)
(global-set-key (kbd "<f2>") 'eval-last-sexp)

;;;; insight-mode

(global-set-key (kbd "M-i") 'insight-mode)

;;;; iy-go-to-char

(require 'iy-go-to-char)

(global-set-key (kbd "C-M-a") 'iy-go-to-char-backward)
(global-set-key (kbd "C-M-e") 'iy-go-to-char)

(define-key iy-go-to-char-keymap (kbd "C-b") 'backward-char)
(define-key iy-go-to-char-keymap (kbd "C-f") 'forward-char)
(define-key iy-go-to-char-keymap (kbd "M-s") 'iy-go-to-char-isearch)
(define-key iy-go-to-char-keymap (kbd "M-r") 'iy-go-to-char-isearch-backward)
(define-key iy-go-to-char-keymap (kbd "C-w") 'iy-go-to-char-kill-region)
(define-key iy-go-to-char-keymap (kbd "M-w") 'iy-go-to-char-kill-ring-save)

;;;; keyboard-quit

(define-key key-translation-map (kbd "M-q") (kbd "C-g"))

;;;; miscellaneous

(global-set-key (kbd "<f5>") 'ta-find-file-notes)
(key-chord-define-global ";;" 'counsel-M-x)
(global-set-key (kbd "<C-backspace>") 'delete-char)
(global-set-key (kbd "M-l") 'recenter-top-bottom)
(global-set-key (kbd "C-v") 'visual-line-mode)
(global-set-key (kbd "<f3>") 'yank)
(global-set-key (kbd "<C-escape>") 'repeat)


(global-set-key (kbd "C-s") 'ta-switch-keyboard-layout)
(global-set-key (kbd "C-c f") 'ta-copy-buffer-file-name)

;;;; rg

(global-set-key (kbd "C-r") 'rg-dwim)
(global-set-key (kbd "C-M-p") 'ta-rg-ask)

;;;; refactor

(require 'refactor)

(defhydra hydra-refactor
  (:hint nil)
  ("M-t" refactor-write-mode :color blue)
  ("a" refactor-abort-changes :color blue)
  ("e" refactor-exit :color blue)
  ("f" refactor-finish-edit :color blue)
  ("M--" undo)
  ("q" nil))

(global-set-key (kbd "M-t") 'hydra-refactor/body)

;;;; sidebar

(require 'sidebar)

(global-set-key (kbd "M-]") 'sidebar)

;;;; undo

(global-set-key (kbd "M--") 'undo)
(global-set-key (kbd "M-+") 'undo-redo)

;;;; zap-up-to-char

(defun ta-zap-back-to-char (char)
  "As `zap-to-char' but backward."
  (interactive (list (read-char-from-minibuffer
											"Zap back to char: " nil 'read-char-history)))
	(zap-to-char -1 char))

(global-set-key (kbd "M-m") 'ta-zap-back-to-char)

;;; Per Mode
;;;; clojure & cider

(require 'cider)

;; cider-start-map  [[/home/tony/work/settings/emacs.d/.emacs.d/.cask/28.0/elpa/cider-20200903.1034/cider.el::960]]
;; cider-eval-commands-map [[/home/tony/work/settings/emacs.d/.emacs.d/.cask/28.0/elpa/cider-20200903.1034/cider-eval.el::1168]]

(define-key cider-mode-map (kbd "<f2>") 'cider-eval-last-sexp)
(define-key cider-mode-map (kbd "<f1>") 'cider-eval-defun-at-point)

(define-key cider-repl-mode-map (kbd "M-n") nil)
(define-key cider-repl-mode-map (kbd "M-p") nil)
(define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
(define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)

;;;; emacs-lisp-mod-map

(define-key emacs-lisp-mode-map (kbd "C-M-i") nil)

;;;; company-mode-map

(require 'company)

(define-key company-active-map (kbd "<up>") 'company-select-previous-or-abort)
(define-key company-active-map (kbd "<down>") 'company-select-next-or-abort)
(define-key company-active-map (kbd "<prior>") 'company-previous-page)
(define-key company-active-map (kbd "<next>") 'company-next-page)
(define-key company-active-map (kbd "<tab>") 'company-complete-common)

(define-key company-active-map (kbd "M-t") 'company-filter-candidates)
(define-key company-active-map (kbd "M-q") 'company-abort)

;;;; grep-mode-map, rg-mode-map

(require 'grep)
(require 'rg)


(define-key grep-mode-map (kbd "M-p") 'windmove-up)
(define-key grep-mode-map (kbd "M-n") 'windmove-down)

(define-key rg-mode-map (kbd "C-p") 'rg-prev-file)
(define-key rg-mode-map (kbd "C-n") 'rg-next-file)

;;;; ibuffer-mode-map

(define-key ibuffer-mode-map (kbd "M-p") nil)
(define-key ibuffer-mode-map (kbd "M-n") nil)
(define-key ibuffer-mode-map (kbd "M-o") nil)
(define-key ibuffer-mode-map (kbd "C-o") nil)

(define-key ibuffer-mode-map (kbd ",") 'ibuffer-toggle-sorting-mode)
(define-key ibuffer-mode-map (kbd "s i") 'ibuffer-invert-sorting)
(define-key ibuffer-mode-map (kbd "s a") 'ibuffer-do-sort-by-alphabetic)
(define-key ibuffer-mode-map (kbd "s v") 'ibuffer-do-sort-by-recency)
(define-key ibuffer-mode-map (kbd "s s") 'ibuffer-do-sort-by-size)
(define-key ibuffer-mode-map (kbd "s f") 'ibuffer-do-sort-by-filename/process)
(define-key ibuffer-mode-map (kbd "s m") 'ibuffer-do-sort-by-major-mode)
(define-key ibuffer-mode-map (kbd "s .") 'ibuffer-do-sort-by-vc-status)
(define-key ibuffer-mode-map (kbd "s d") 'ibuffer-do-sort-by-alphabetic-directory-first)

(define-prefix-command 'ta-ibuffer-visite-buffer-map)
(define-key ta-ibuffer-visite-buffer-map (kbd "e") 'ace-hacks-ibuffer-visit-buffer)
(define-key ta-ibuffer-visite-buffer-map (kbd "p") 'ace-hacks-ibuffer-visit-buffer-split-up)
(define-key ta-ibuffer-visite-buffer-map (kbd "n") 'ace-hacks-ibuffer-visit-buffer-split-down)
(define-key ta-ibuffer-visite-buffer-map (kbd "b") 'ace-hacks-ibuffer-visit-buffer-split-left)
(define-key ta-ibuffer-visite-buffer-map (kbd "f") 'ace-hacks-ibuffer-visit-buffer-split-right)

(define-key ibuffer-mode-map (kbd "e") 'ta-ibuffer-visite-buffer-map)
(define-key ibuffer-mode-map (kbd "RET") 'ibuffer-visit-buffer)

(define-key ibuffer-mode-map (kbd "d") 'ibuffer-mark-for-delete)
(define-key ibuffer-mode-map (kbd "C-d") 'ibuffer-mark-for-delete-backwards)
(define-key ibuffer-mode-map (kbd "x") 'ibuffer-do-kill-on-deletion-marks)
(define-key ibuffer-mode-map (kbd "U") 'ibuffer-unmark-all-marks)
(define-key ibuffer-mode-map (kbd "m") 'ibuffer-mark-forward)
(define-key ibuffer-mode-map (kbd "t") 'ibuffer-toggle-marks)
(define-key ibuffer-mode-map (kbd "u") 'ibuffer-unmark-forward)

(define-key ibuffer-mode-map (kbd "n") 'ibuffer-forward-line)
(define-key ibuffer-mode-map (kbd "p") 'ibuffer-backward-line)
(define-key ibuffer-mode-map (kbd "g") 'ibuffer-update)
(define-key ibuffer-mode-map (kbd "C-p") 'ibuffer-backward-filter-group)
(define-key ibuffer-mode-map (kbd "C-n") 'ibuffer-forward-filter-group)

(define-key ibuffer-mode-map (kbd "TAB") 'ibuffer-toggle-filter-group)
(define-key ibuffer-mode-map (kbd "C-b") 'ta-ibuffer-toggle-show-buffers-with-predicates)
(define-key ibuffer-mode-map (kbd "SPC") 'ta-ibuffer-switch-filter-groups)

(define-key ibuffer-mode-map (kbd "M-s") 'isearch-forward)

;;;; Info-mode-map and help-mode-map

(define-key Info-mode-map (kbd "d") 'insight-scroll-down-half-window)
(define-key Info-mode-map (kbd "s") 'insight-scroll-up-half-window)
(define-key help-mode-map (kbd "d") 'insight-scroll-down-half-window)
(define-key help-mode-map (kbd "s") 'insight-scroll-up-half-window)

;;;; isearch-mode

(require 'isearch)

(defun ta-isearch-yank-sexp-at-point ()
  "Pull sexp at point into search string."
  (interactive)
	(isearch-yank-string (thing-at-point 'sexp)))

(defun ta-isearch-yank-word-at-point ()
  "Pull word at point into search string."
  (interactive)
	(isearch-yank-string (thing-at-point 'word)))

(global-set-key (kbd "M-s") 'isearch-forward)
(global-set-key (kbd "M-r") 'isearch-backward)

(define-key isearch-mode-map (kbd "M-s") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-r") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<prior>") 'isearch-beginning-of-buffer)
(define-key isearch-mode-map (kbd "<next>") 'isearch-end-of-buffer)
(define-key isearch-mode-map (kbd "M-o") 'isearch-occur)

(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "M-.") 'ta-isearch-yank-sexp-at-point)
(define-key isearch-mode-map (kbd "M-i") 'ta-isearch-yank-word-at-point)

(define-key isearch-mode-map (kbd "M-c") 'isearch-toggle-case-fold)
(define-key isearch-mode-map (kbd "C-r") 'isearch-toggle-regexp)
(define-key isearch-mode-map (kbd "M-e") 'isearch-edit-string)

(define-key isearch-mode-map (kbd "M-t") 'swiper-isearch-toggle)


;;;; ivy-minibuffer-map and swiper-map

(require 'ivy)
(require 'swiper)

(define-key ivy-minibuffer-map (kbd "<left>") 'ivy-previous-history-element)
(define-key ivy-minibuffer-map (kbd "<right>") 'ivy-next-history-element)
(define-key ivy-minibuffer-map (kbd "<up>") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "<down>") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "M-e") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "M-a") 'ivy-dispatching-done)
(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "M-p") 'ivy-reverse-i-search)

(define-key ivy-minibuffer-map (kbd "C-e") 'ace-hacks-ivy-visit)
(define-key ivy-minibuffer-map (kbd "C-p") 'ace-hacks-ivy-visit-split-up)
(define-key ivy-minibuffer-map (kbd "C-n") 'ace-hacks-ivy-visit-split-down)
(define-key ivy-minibuffer-map (kbd "C-b") 'ace-hacks-ivy-visit-split-left)
(define-key ivy-minibuffer-map (kbd "C-f") 'ace-hacks-ivy-visit-split-right)
(define-key ivy-minibuffer-map (kbd "C-a") 'ta-ivy-switch-to-buffer)

(define-key ivy-minibuffer-map (kbd "C-M-n") 'ta-counsel-rg-ivy-command)

(define-key swiper-map (kbd "M-q") 'minibuffer-keyboard-quit)
(define-key swiper-map (kbd "M-c") 'swiper-mc)
(define-key swiper-map (kbd "M-t") 'swiper-isearch-toggle)



;;;; magit

(require 'magit)

(define-key magit-section-mode-map (kbd "C-i") 'magit-section-toggle)
(define-key magit-section-mode-map [C-tab]     'magit-section-cycle)
(define-key magit-section-mode-map (kbd "C-SPC") 'magit-section-cycle-global)
(define-key magit-section-mode-map (kbd   "p") 'magit-section-backward)
(define-key magit-section-mode-map (kbd   "n") 'magit-section-forward)
(define-key magit-section-mode-map (kbd "M-p") 'magit-section-backward-sibling)
(define-key magit-section-mode-map (kbd "M-n") 'magit-section-forward-sibling)
(define-key magit-mode-map (kbd "i") 'magit-section-up)

(define-key magit-status-mode-map (kbd "C-M-p") 'windmove-up)
(define-key magit-status-mode-map (kbd "C-M-n") 'windmove-down)
(define-key magit-status-mode-map (kbd "<prior>") 'insight-scroll-down-half-window)
(define-key magit-status-mode-map (kbd "<next>") 'insight-scroll-up-half-window)
(define-key magit-log-mode-map (kbd "C-M-p") 'windmove-up)
(define-key magit-log-mode-map (kbd "C-M-n") 'windmove-down)
(define-key magit-log-mode-map (kbd "<prior>") 'insight-scroll-down-half-window)
(define-key magit-log-mode-map (kbd "<next>") 'insight-scroll-up-half-window)


;;;; markdown-mode

(require 'markdown-mode)

(define-key markdown-mode-map (kbd "C-M-i") nil)

;;;; occur-mode

(require 'replace)

(define-key occur-mode-map (kbd "M-p") 'nil)
(define-key occur-mode-map (kbd "M-n") 'nil)

;;;; org-mode-map
;; Note that `org' package must be loaded before using org-mode-map

(define-key org-mode-map (kbd "C-e") nil)
(define-key org-mode-map (kbd "M-e") nil)
(define-key org-mode-map (kbd "C-a") nil)
(define-key org-mode-map (kbd "M-a") nil)

(define-key org-mode-map (kbd "M-m") 'ta-org-table-previous-row)
(define-key org-mode-map (kbd "C-<tab>") 'org-shifttab)
(define-key org-mode-map (kbd "<M-return>") 'ta-org-meta-return)
(define-key org-mode-map (kbd "M-S-<down>") 'ta-org-shiftmetadown)
(define-key org-mode-map (kbd "C-t") 'org-toggle-inline-images)

(setq org-speed-commands-default nil)
(setq org-speed-commands-user
			'(("Outline Navigation")
				("n" . (org-speed-move-safe 'org-next-visible-heading))
				("p" . (org-speed-move-safe 'org-previous-visible-heading))
				("f" . (org-speed-move-safe 'org-forward-heading-same-level))
				("b" . (org-speed-move-safe 'org-backward-heading-same-level))
				("i" . (org-speed-move-safe 'outline-up-heading))
				("Sparse tree navigation")
				("d" . previous-error)
				("s" . next-error)
				("Outline Structure Editing")
				("." . org-toggle-narrow-to-subtree)
				("@" . org-mark-subtree)
				("`". org-metaup)
				(",". org-metadown)
				("]". org-shiftmetaright)
				("[". org-shiftmetaleft)
				(")". org-metaright)
				("(". org-metaleft)
				("+". (progn (forward-char 1) (call-interactively
																			 'org-insert-heading-respect-content)))
				("Meta Data Editing")
				("t" . org-todo)
				(":" . org-set-tags-command)
				("c" . org-comment-dwim)
				("Agenda Views etc")
				("a" . org-agenda)
				("/" . org-sparse-tree)))



;;; Footer

(provide 'kb)

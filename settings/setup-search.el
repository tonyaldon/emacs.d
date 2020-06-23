(require 'counsel)
(require 'grep)
(require 'ivy)
(load "../packages/posframe/posframe.el")
;; (require 'ivy-posframe)
(load "../.cask/28.0/elpa/ivy-posframe-20200528.553/ivy-posframe.el")
(require 'ivy-rich)
(require 'swiper)
(require 'wgrep)

(counsel-mode 1)
(ivy-mode 1)
(ivy-rich-mode 1)
(ivy-posframe-mode 1)

(setq ivy-wrap t)
(setq ivy-extra-directories '("./"))
(setq ivy-use-virtual-buffers t)
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
(setq wgrep-auto-save-buffer t)
(setq counsel-outline-display-style 'path)
(setq counsel-outline-path-separator "/")
(setq counsel-outline-settings
      '((emacs-lisp-mode
         :outline-regexp ";;;\\(;* [^ \t\n]\\|###autoload\\)\\|("
         :outline-level lisp-outline-level
         :outline-title ta-counsel-outline-title-emacs-lisp-mode
         :display-style title)
        (org-mode
         :outline-title counsel-outline-title-org
         :action counsel-org-goto-action
         :history counsel-org-goto-history
         :caller counsel-org-goto
         :display-style path)))

(ivy-set-occur 'swiper 'swiper-occur)
(ivy-set-occur 'swiper-isearch 'swiper-occur)
(setq-local posframe--truncate-lines t)
(setq ivy-posframe-height-alist
      '((swiper . 8)
        (counsel-outline . 8)
        (counsel-org-goto . 8)
        (t . 10)))
(setq ivy-posframe-width 80)
(setq ivy-posframe-display-functions-alist
      '((swiper . ivy-display-function-fallback)
        (counsel-outline . ivy-posframe-display-at-point)
        (counsel-org-goto . ivy-posframe-display-at-point)
        (counsel-yank-pop . ivy-posframe-display-at-point)
        (t . ta-ivy-posframe-display-at-frame-below-top-center)))

(defadvice ivy-posframe-cleanup
    (after ta-ivy-posframe-width-default-advice activate)
  (setq ivy-posframe-width 80))

(defadvice counsel-outline (before ta-counsel-outline-posframe-width-advice activate)
  (setq-local ivy-posframe-width 50))

(defun ta-posframe-poshandler-frame-below-top-center (info)
  "Posframe's position handler."
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        180))

(defun ta-ivy-posframe-display-at-frame-below-top-center (str)
  (ivy-posframe--display str #'ta-posframe-poshandler-frame-below-top-center))

;; https://github.com/abo-abo/swiper/issues/649
(defun ta-ivy-resize--minibuffer-setup-hook ()
  "Minibuffer setup hook."
  (add-hook 'post-command-hook #'ta-ivy-resize--post-command-hook nil t))

(defun ta-ivy-resize--post-command-hook ()
  "Hook run every command in minibuffer."
  (when ivy-mode
    (shrink-window (1+ ivy-height))))  ; Plus 1 for the input field.

(defun ta-counsel-outline-title-emacs-lisp-mode ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun counsel-outline-candidates (&optional settings prefix)
  "Little changes in the function from the package.

Allow to define the text faces directly in the function
`:outline-title'. Very handy when it comes to get the text
properties of the current buffer.

Return an alist of outline heading completion candidates.
Each element is a pair (HEADING . MARKER), where the string
HEADING is located at the position of MARKER.  SETTINGS is a
plist entry from `counsel-outline-settings', which see.
PREFIX is a string prepended to all candidates."
  (let* ((bol-regex (concat "^\\(?:"
                            (or (plist-get settings :outline-regexp)
                                outline-regexp)
                            "\\)"))
         (outline-title-fn (or (plist-get settings :outline-title)
                               #'counsel-outline-title))
         (outline-level-fn (or (plist-get settings :outline-level)
                               outline-level))
         (display-style (or (plist-get settings :display-style)
                            counsel-outline-display-style))
         (path-separator (or (plist-get settings :path-separator)
                             counsel-outline-path-separator))
         (face-style (or (plist-get settings :face-style)
                         counsel-outline-face-style))
         (custom-faces (or (plist-get settings :custom-faces)
                           counsel-outline-custom-faces))
         (stack-level 0)
         (orig-point (point))
         (stack (and prefix (list (counsel-outline--add-face
                                   prefix 0 face-style custom-faces))))
         cands name level marker)
    (save-excursion
      (setq counsel-outline--preselect 0)
      (goto-char (point-min))
      (while (re-search-forward bol-regex nil t)
        (save-excursion
          (setq name (or (save-match-data
                           (funcall outline-title-fn))
                         ""))
          (goto-char (match-beginning 0))
          (setq marker (point-marker))
          (setq level (funcall outline-level-fn))
          (cond ((eq display-style 'path)
                 ;; Update stack.  The empty entry guards against incorrect
                 ;; headline hierarchies, e.g. a level 3 headline
                 ;; immediately following a level 1 entry.
                 (while (<= level stack-level)
                   (pop stack)
                   (cl-decf stack-level))
                 (while (> level stack-level)
                   (push "" stack)
                   (cl-incf stack-level))
                 (setf (car stack)
                       (counsel-outline--add-face
                        name level face-style custom-faces))
                 (setq name (mapconcat #'identity
                                       (reverse stack)
                                       path-separator)))
                (t
                 (when (eq display-style 'headline)
                   (setq name (concat (make-string level ?*) " " name)))
                 ;; COMMENT this part in order to define the face directly
                 ;; in the `outline-title' function (very handy to get the
                 ;; text property of the current buffer)
                 ;; (setq name (counsel-outline--add-face
                 ;;             name level face-style custom-faces))
                 ))
          (push (cons name marker) cands))
        (unless (or (string= name "")
                    (< orig-point marker))
          (cl-incf counsel-outline--preselect))))
    (nreverse cands)))

(defun ta-toggle-write-mode ()
  "Toggle to the Writable variant of the current mode.

Call command `dired-toggle-read-only' if `major-mode' is equal
`dired-mode' and call command `wgrep-change-to-wgrep-mode' if
`major-mode' is equal to `grep-mode'."
  (interactive)
  (cond ((string-equal major-mode "dired-mode")
         (call-interactively 'dired-toggle-read-only))
        ((memq major-mode '(grep-mode ivy-occur-grep-mode))
         (call-interactively 'wgrep-change-to-wgrep-mode))
        (t (message "You have to be in either in `dired-mode' or
`grep-mode' to execute this command"))))

(defun ta-w-abort-changes ()
  "Abort changes and return to the appropiate mode.

Call command `wdired-abort-changes' if `major-mode' is
`wdired-mode' and call command `wgrep-abort-changes' if
`major-mode' is `grep-mode'."
  (interactive)
  (cond ((string-equal major-mode "wdired-mode")
         (call-interactively 'wdired-abort-changes))
        ((memq major-mode '(grep-mode ivy-occur-grep-mode))
         (call-interactively 'wgrep-abort-changes))
        (t (message "You have to be in either in `wdired-mode' or
`grep-mode' to execute this command"))))

(defun ta-w-exit ()
  "Exit writable mode and return to the appropiate mode.

Call command `wdired-exit' if `major-mode' is
`wdired-mode' and call command `wgrep-exit' if
`major-mode' is `grep-mode'."
  (interactive)
  (cond ((string-equal major-mode "wdired-mode")
         (call-interactively 'wdired-exit))
        ((memq major-mode '(grep-mode ivy-occur-grep-mode))
         (call-interactively 'wgrep-exit))
        (t (message "You have to be in either in `wdired-mode' or
`grep-mode' to execute this command"))))

(defun ta-w-finish-edit ()
  "Abort changes and return to the appropiate mode.

Call command `wdired-finish-edit' if `major-mode' is
`wdired-mode' and call command `wgrep-finish-edit' if
`major-mode' is `grep-mode'."
  (interactive)
  (cond ((string-equal major-mode "wdired-mode")
         (call-interactively 'wdired-finish-edit))
        ((memq major-mode '(grep-mode ivy-occur-grep-mode))
         (call-interactively 'wgrep-finish-edit))
        (t (message "You have to be in either in `wdired-mode' or
`grep-mode' to execute this command"))))


(defvar ta-ivy-aw-caller
  '(ivy-switch-buffer counsel-find-file ta-counsel-quick-access projectile-completing-read)
  "List of ivy or counsel commands that \"open\" file, buffer or quick-access.")

(defun ta--ivy-aw-find (buffer-or-file caller)
  "Function to be used within ivy actions."
  (cond
   ((equal 'ta-counsel-quick-access caller)
    (find-file (ta-quick-access-get-filename buffer-or-file)))
   ((equal 'ivy-switch-buffer caller)
    (ivy--switch-buffer-action buffer-or-file))
   (t
    (find-file (expand-file-name buffer-or-file ivy--directory)))))

(defun ta--ivy-aw-find-action (buffer-or-file)
  "Action to be used in `ta-ivy-aw-find'."
  (let ((caller (ivy-state-caller ivy-last)))
    (if (not (member caller ta-ivy-aw-caller))
        (message "caller (%s) not listed in ta-ivy-aw-caller" caller)
      (call-interactively 'ace-window)
      (ta--ivy-aw-find buffer-or-file caller))))

(defun ta-ivy-aw-find ()
  "Ivy command that use ace-window to select a window and \"open\"

the selected thing. This command must be bind in ivy-minibuffer-map."
  (interactive)
  (ivy-set-action 'ta--ivy-aw-find-action)
  (ivy-done))

(defun ta--ivy-aw-find-split-up (buffer-or-file)
  "Action to be used in `ta-ivy-aw-find-split-up'."
  (let ((caller (ivy-state-caller ivy-last)))
    (if (not (member caller ta-ivy-aw-caller))
        (message "caller (%s) not listed in ta-ivy-aw-caller" caller)
      (call-interactively 'ace-window)
			(split-window-below)
      (ta--ivy-aw-find buffer-or-file caller))))

(defun ta-ivy-aw-find-split-up ()
  "Open ivy selection in the up part window selected with `ace-window'

after spliting it verticaly."
  (interactive)
  (ivy-set-action 'ta--ivy-aw-find-split-up)
  (ivy-done))

(defun ta--ivy-aw-find-split-down (buffer-or-file)
  "Action to be used in `ta-ivy-aw-find-split-down'."
  (let ((caller (ivy-state-caller ivy-last)))
    (if (not (member caller ta-ivy-aw-caller))
        (message "caller (%s) not listed in ta-ivy-aw-caller" caller)
      (call-interactively 'ace-window)
			(split-window-below)
			(windmove-down)
      (ta--ivy-aw-find buffer-or-file caller))))

(defun ta-ivy-aw-find-split-down ()
  "Open ivy selection in the down part window selected with `ace-window'

after spliting it verticaly."
  (interactive)
  (ivy-set-action 'ta--ivy-aw-find-split-down)
  (ivy-done))

(defun ta--ivy-aw-find-split-left (buffer-or-file)
  "Action to be used in `ta-ivy-aw-find-split-left'."
  (let ((caller (ivy-state-caller ivy-last)))
    (if (not (member caller ta-ivy-aw-caller))
        (message "caller (%s) not listed in ta-ivy-aw-caller" caller)
      (call-interactively 'ace-window)
			(split-window-right)
      (ta--ivy-aw-find buffer-or-file caller))))

(defun ta-ivy-aw-find-split-left ()
  "Open ivy selection in the left part window selected with `ace-window'

after spliting it horizontaly."
  (interactive)
  (ivy-set-action 'ta--ivy-aw-find-split-left)
  (ivy-done))

(defun ta--ivy-aw-find-split-right (buffer-or-file)
  "Action to be used in `ta-ivy-aw-find-split-right'."
  (let ((caller (ivy-state-caller ivy-last)))
    (if (not (member caller ta-ivy-aw-caller))
        (message "caller (%s) not listed in ta-ivy-aw-caller" caller)
      (call-interactively 'ace-window)
			(split-window-right)
			(windmove-right)
      (ta--ivy-aw-find buffer-or-file caller))))

(defun ta-ivy-aw-find-split-right ()
  "Open ivy selection in the righ part window selected with `ace-window'

after spliting it horizontaly."
  (interactive)
  (ivy-set-action 'ta--ivy-aw-find-split-right)
  (ivy-done))

(defun ta-ivy-switch-to-buffer ()
  "Wrapper on `switch-to-buffer' to be used in `ivy-minibuffer-map'."
  (interactive)
	(ivy-set-action 'switch-to-buffer)
	(ivy-done))

(define-key ivy-minibuffer-map (kbd "C-e") 'ta-ivy-aw-find)
(define-key ivy-minibuffer-map (kbd "C-p") 'ta-ivy-aw-find-split-up)
(define-key ivy-minibuffer-map (kbd "C-n") 'ta-ivy-aw-find-split-down)
(define-key ivy-minibuffer-map (kbd "C-b") 'ta-ivy-aw-find-split-left)
(define-key ivy-minibuffer-map (kbd "C-f") 'ta-ivy-aw-find-split-right)
(define-key ivy-minibuffer-map (kbd "C-a") 'ta-ivy-switch-to-buffer)

(add-hook 'ivy-mode-hook 'ivy-posframe-enable)
(add-hook 'minibuffer-setup-hook 'ta-ivy-resize--minibuffer-setup-hook)

(provide 'setup-search)

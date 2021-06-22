;;; Packages

(require 'counsel)
(require 'ivy)
(require 'swiper)
(require 'flx)

;;; ivy and counsel
(counsel-mode 1)
(ivy-mode 1)

(setq ivy-height 8)
(setq ivy-wrap t)
(setq ivy-extra-directories '("./"))
(setq ivy-use-virtual-buffers nil)
(setq ivy-count-format "(%d/%d) ")
;; (setq ivy-re-builders-alist '((swiper . ivy--regex-ignore-order)
;;                               (swiper-all . ivy--regex-ignore-order)
;;                               (counsel-rg . ivy--regex-ignore-order)
;;                               (counsel-outline . ivy--regex-ignore-order)
;;                               (t . ivy--regex-fuzzy)))

(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

(ivy-set-occur 'swiper 'swiper-occur)
(ivy-set-occur 'swiper-isearch 'swiper-occur)


;;; counsel-outline

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

(defun ta-counsel-outline-title-emacs-lisp-mode ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun counsel-outline-candidates (&optional settings prefix)
  "Little changes in the function from the package `counsel-outline'.

Text faces are directly defined in the function
`:outline-title'. Very handy when it comes to get the text
properties of the current buffer.

From `counsel-outline':
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




;;; fzf
;;;; Packages

(require 'fzf)

;;;; fzf options

(setq fzf/directory-start "~/work/")

(setq fzf/window-height 15) ; default
(setq fzf/executable "/home/tony/.linuxbrew/bin/fzf")
(setq fzf/git-grep-args "-i --line-number %s") ; default
(setq fzf/position-bottom nil) ; default

(setq fzf/with-ace-hacks t)
(setq fzf/ace-hacks-keys '(:left  ?b :right ?f :up ?p :down ?n :ace ?e))

;; fzf/args
;; 1. don't use "--reverse" argument, the way fzf/after-term-handle-exit
;;    handle the parsing to retrieve the selected file doesn't cover this case.
;; 2. "--print-query" is a mandatory argument, if you remove it, fzf/after-term-handle-exit
;;    doesn't do well its job.
(setq ta-fzf/bind
      (concat " --bind "
              "'"
              "ctrl-s:reload(fd),"                               ;; in ctrl-s 's' stands for Standard
              "ctrl-h:reload(fd --hidden --exclude \".git/*\")," ;; in ctrl-h 'h' stands for Hidden
              "ctrl-n:reload(fd --hidden --no-ignore),"          ;; in ctrl-n 'n' stands for No-ignore
              "ctrl-d:reload(fd --type d),"                      ;; in ctrl-d 'd' stands for Directory
              "ctrl-l:reload(fd --type d --hidden --no-ignore)," ;; in ctrl-l 'l' because can't take upcase 'D'
              "ctrl-f:reload(fd --type f),"                      ;; in ctrl-f 'f' stands for File
              "ctrl-b:reload(fd --type f --hidden --no-ignore)"  ;; in ctrl-b 'b' because can't take upcase 'F'
              "'"))
(setq fzf/args (concat "--print-query " ta-fzf/bind))

;;;; Custom fzf command using 'fd'

(defun ta-fzf-emacs-settings ()
  "fzf in my emacs settings directory."
  (interactive)
  (let ((process-environment (cons (concat "FZF_DEFAULT_COMMAND=fd")
                                   process-environment)))
    (fzf/start "~/work/settings/emacs.d/.emacs.d/")))

(defun ta-fzf-directory ()
  "Start a fzf session frome `fzf/directory-start'."
  (interactive)
  (let ((process-environment (cons (concat "FZF_DEFAULT_COMMAND=fd")
                                   process-environment)))
    (fzf/start fzf/directory-start)))

(defun ta-fzf-default-directory ()
  "Start a fzf session at the specified directory.

Select the start directory from `default-directory'."
  (interactive)
  (let ((process-environment (cons (concat "FZF_DEFAULT_COMMAND=fd")
                                   process-environment)))
    (fzf/start (read-directory-name "Directory:" default-directory))))

(defun ta-fzf-project ()
  "Start a fzf session in current project.

If there is no current project, use the `default-directory'."
  (interactive)
  (let ((process-environment (cons (concat "FZF_DEFAULT_COMMAND=fd")
                                   process-environment)))
    (if-let ((proj (cdr (project-current))))
        (fzf/start proj)
      (fzf/start default-directory))))

(defun ta-fzf-dwim (&optional arg)
  "Start a fzf session in current project.

If there is no current project, use the `default-directory'.

With \\[universal-argument] prefix, ask to choose a directory to start fzf.
With two \\[universal-argument] prefix, start fzf at from `fzf/directory-start'."
  (interactive "p")
  (cond
   ((equal arg 4) (call-interactively 'ta-fzf-default-directory))
   ((equal arg 16) (call-interactively 'ta-fzf-directory))
   (t (call-interactively 'ta-fzf-project))))

;;; ivy (to sort)

(defun ta-ivy-switch-to-buffer ()
  "Wrapper on `switch-to-buffer' to be used in `ivy-minibuffer-map'."
  (interactive)
  (ivy-set-action 'ivy--switch-buffer-action)
  (ivy-done))

(defadvice swiper (after ta-ivy-occur-delete-other-windows activate)
  (when (equal major-mode 'ivy-occur-grep-mode)
    (let ((bname (buffer-name)))
      (other-window 1)
      (delete-other-windows)
      (pop-to-buffer bname))))

(defadvice swiper-all (after ta-ivy-occur-delete-other-windows activate)
  (when (equal major-mode 'ivy-occur-mode)
    (let ((bname (buffer-name)))
      (other-window 1)
      (delete-other-windows)
      (pop-to-buffer bname))))

;; (ad-unadvise 'swiper) ; for debugging
;; (ad-unadvise 'swiper-all)



;;; company
;;;; Packages

(require 'company)

;;;; Global

(setq company-selection-wrap-around t)
(setq company-tooltip-limit 10)
(setq company-require-match nil)
(setq company-idle-delay 0)
(setq company-backends '(company-capf company-files))
(make-variable-buffer-local 'company-idle-delay)
(make-variable-buffer-local 'company-minimum-prefix-length)
(make-variable-buffer-local 'company-backends)


;;;; Per mode
;;;;; emacs-lisp-mode

(defun ta-company-emacs-lisp-mode ()
  "Setup `company-mode' for `emacs-lisp-mode-hook'"
  (company-mode 1)
  (setq company-backends '((company-capf company-files)))
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 1))

(add-hook 'emacs-lisp-mode-hook 'ta-company-emacs-lisp-mode)

;;;;; TODO: clojure



;;;;; sh-mode

(defun ta-company-sh-mode ()
  "Setup `company-mode' for `sh-mode-hook'"
  (company-mode 1)
  (setq company-backends '((company-capf company-files))))

(add-hook 'sh-mode-hook 'ta-company-sh-mode)

;;;;; org-mode

(defun ta-company-org-mode ()
  "Setup `company-mode' for `org-mode-hook'"
  (company-mode 1)
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)
  (setq company-minimum-prefix-length 4)
  (setq company-backends '(company-capf company-files company-dabbrev)))

(add-hook 'org-mode-hook 'ta-company-org-mode)

;;;;; TODO: python-mode

;; I don't know if I am using `company-jedi'
;; (require 'company-jedi)

;; (defun ta-company-python-mode ()
;;   "Setup `company-mode' for `python-mode-hook'"
;;   (company-mode 1)
;;  (set (make-local-variable 'company-backends)
;;        '((company-anaconda
;;           company-dabbrev-code
;;           company-files)
;;          company-capf)))

;; (add-hook 'python-mode-hook 'ta-company-python-mode)

;;;;; TODO: Web development

;; (require 'company-web)
;; (require 'company-web-html)
;; (require 'ac-html-csswatcher)

;; ;; (company-web-csswatcher-setup)

;; (defun ta-company-js-mode ()
;;   "Setup `company-mode' for `js-mode-hook'"
;;   (company-mode 1)
;;  (setq company-backends '((company-tide
;;                            company-dabbrev-code
;;                            company-web-html
;;                            company-files))))

;; (defun ta-company-html-mode ()
;;   "Setup `company-mode' for `html-mode'"
;;   (set (make-local-variable 'company-backends)
;;        '((company-dabbrev-code
;;           company-web-html
;;           company-yasnippet
;;           company-files)
;;          company-dabbrev
;;          company-capf)))

;; (defun ta-company-css-mode ()
;;   "Setup `company-mode' for `css-mode'"
;;   (set (make-local-variable 'company-backends)
;;        '((;; company-dabbrev-code
;;           company-css
;;          company-web-html
;;           ;; company-files
;;          )
;;          company-dabbrev
;;          company-capf)))

;; (add-hook 'mhtml-mode-hook 'ta-company-html-mode)
;; (add-hook 'html-mode-hook 'ta-company-html-mode)
;; (add-hook 'css-mode-hook 'ta-company-css-mode)
;; (add-hook 'js-mode-hook 'ta-company-js-mode)
;; (add-hook 'js-mode-hook 'ac-html-csswatcher+)
;; (add-hook 'js-jsx-mode-hook 'ac-html-csswatcher+)


;;; counsel-rg

(setq ta-counsel-rg-ivy-callers-alist
      '((counsel-quick-access . (lambda (selection)
                                  (file-name-directory (quick-access-get-filename selection))))
        (counsel-find-file . (lambda (selection)
                               (expand-file-name selection ivy--directory)))))

(defun ta-counsel-rg-ivy-command ()
  "Trigger `counsel-rg' with `ivy' selection as initial directory.

Inteded to be bind in `ivy-minibuffer-map'."
  (interactive)
  (ivy-set-action
   (lambda (selection)
     (let ((caller (ivy-state-caller ivy-last)))
       (if (not (alist-get caller ta-counsel-rg-ivy-callers-alist))
           (message "caller (%s) not listed in ta-counsel-rg-ivy-callers-alist" caller)
         (when-let (resolver (alist-get caller ta-counsel-rg-ivy-callers-alist))
           (counsel-rg nil (funcall resolver selection)))))))
  (ivy-done))

(define-key ivy-minibuffer-map (kbd "C-M-a") 'ta-counsel-rg-ivy-command)

;;; Footer

(provide 'setup-completion)

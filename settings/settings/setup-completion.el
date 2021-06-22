;;; Packages

(require 'counsel)
(require 'ivy)

;;; ivy and counsel
(counsel-mode 1)
(ivy-mode 1)

(setq ivy-height 8)
(setq ivy-wrap t)
(setq ivy-extra-directories '("./"))
(setq ivy-use-virtual-buffers nil)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

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

;;; Footer

(provide 'setup-completion)

;;; About
;; Here, I define some utilities I find useful to make The Inside Emacs videos.

;;; Global variables

(setq ie-directory "~/work/videos/inside-emacs/")

;;; Last Video

(defun ie-last-video-name (inside-emacs-dir)
  "Return the directory of the last Inside Emacs video being edited.

INSIDE-EMACS-DIR is the directory of the video Inside Emacs."
  (let* ((default-directory inside-emacs-dir)
         (ls-list (s-split "\n" (shell-command-to-string "ls"))))
    (-last-item (--filter (s-contains-p "inside-" it) ls-list))))

(defun ie-last-video-readme ()
  "Return the path of the README of the last Inside Emacs being edited."
  (let* ((last-video (ie-last-video-name ie-directory))
         (last-video-dir (f-join ie-directory last-video))
         (readme (f-join last-video-dir "README.org")))
    readme))

(defun ie-find-last-video-readme ()
  "Find last video readme in the current buffer."
  (interactive)
  (find-file (ie-last-video-readme)))

(global-set-key (kbd "C-c l") 'ie-find-last-video-readme)

;;;; COMMENTS
;; COMMENTS
;; (ie-last-video-name "~/work/videos/inside-emacs/")
;; (s-contains-p "inside" "insiemacs")
;; (--filter (s-contains-p "inside-" it) '("inside-emacs-1" "inside-emacs-2" "uie"))
;; (-last-item '(1 3 2))

;; COMMENTS
;; (ie-last-video-readme)

;;; Footer

(provide 'inside-emacs)

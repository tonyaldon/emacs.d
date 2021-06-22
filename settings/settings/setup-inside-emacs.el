;;; Packages

(require 'screencast)
(require 'ie-story-generate)
(require 'ie-last-video)

(declare-function linux-toggle-dpi "ext:linux")

(setq ie-last-video-main-dir "~/work/inside-emacs/videos/")

(setq screencast-hook-to-remove-alist nil)
(setq screencast-display-buffer-alist display-buffer-alist)

(global-set-key (kbd "C-c d") 'linux-toggle-dpi)
(global-set-key (kbd "C-c s") 'screencast-mode)

(global-set-key (kbd "C-c i") 'ie-last-video-find-readme)
(global-set-key (kbd "C-c g") 'ie-story-generate-all)


(provide 'setup-inside-emacs)

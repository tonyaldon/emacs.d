;;; Packages

(require 'screencast)
(require 'ie-story-generate)
(require 'ie-last-video)

(declare-function linux-toggle-dpi "ext:linux")
(declare-function linux-toggle-i3bar "ext:linux")

(setq screencast-hook-to-remove-alist
      '((window-configuration-change-hook . ta-frame-set-display-alist)))

(setq screencast-display-buffer-alist '())

(global-set-key (kbd "C-c d") 'linux-toggle-dpi)
(global-set-key (kbd "C-c b") 'linux-toggle-i3bar)
(global-set-key (kbd "C-c s") 'screencast-mode)

(global-set-key (kbd "C-c i") 'ie-last-video-find-readme)
(global-set-key (kbd "C-c g") 'ie-story-generate-all)


(provide 'setup-inside-emacs)

(require 'tern)

;; let emacs know where executables 'tern' and 'node' are installed
(setenv "PATH" (concat "/home/tony/.nvm/versions/node/v8.9.0/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/home/tony/.nvm/versions/node/v8.9.0/bin")

(defun ta-tern-js-mode-hook ()
  "Turn `tern-mode' on in `js-mode'"
  (tern-mode t))

(add-hook 'js-mode-hook 'ta-tern-js-mode-hook)


(provide 'setup-js-mode)

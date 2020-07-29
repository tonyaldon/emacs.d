(require 'company)
(require 'company-web)
(require 'company-jedi)
(require 'company-web-html)
(require 'company-emoji)
(require 'company-fuzzy)
(require 'lsp-mode)
(require 'company-lsp)
(require 'ac-html-csswatcher)

;; (company-web-csswatcher-setup)

(setq company-fuzzy-show-annotation nil)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)
(setq company-dabbrev-code-ignore-case nil)
(setq company-dabbrev-code-other-buffers 'all)
(setq company-dabbrev-code-everywhere nil)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)
(setq company-tooltip-limit 6)
(setq company-transformers '(company-sort-by-backend-importance))
(setq company-require-match nil)

(make-variable-buffer-local 'company-minimum-prefix-length)

(defun ta-company-next-show-doc ()
  "Select the next candidate and show the doc."
  (interactive)
  (company-select-next-or-abort)
	(company-show-doc-buffer))

(defun ta-company-previous-show-doc ()
  "Select the previous candidate and show the doc."
  (interactive)
  (company-select-previous-or-abort)
	(company-show-doc-buffer))

(defun ta-company-fuzzy-mode-toggle ()
  "Specific toggle `company-fuzzy-mode'. To be used in `company-active-map'."
  (interactive)
	(if company-fuzzy-mode (delete-char -1))
	(company-fuzzy-mode 'toggle))

(defun ta-company-completion-hook (string-or-symbol)
  "Always turn off `company-fuzzy-mode'."
	(company-fuzzy-mode -1))

(defun ta-company-sh-mode ()
  "Setup `company-mode' for `sh-mode-hook'"
  (set (make-local-variable 'company-backends)
       '((company-capf
          company-yasnippet
          company-dabbrev-code
          company-files)
         company-dabbrev)))

(defun ta-company-org-mode ()
  "Setup `company-mode' for `org-mode-hook'"
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)
  (setq company-minimum-prefix-length 4)
	(set (make-local-variable 'company-backends)
       '((company-yasnippet
					:with
          company-emoji
          company-capf
          company-dabbrev
          company-files)
         company-dabbrev-code)))

(defun ta-company-php-mode ()
  "Setup `company-mode' for `php-mode-hook'"
  (set (make-local-variable 'company-backends)
       '((php-extras-company
          company-yasnippet
          company-dabbrev-code
          company-files)
         company-dabbrev
         company-capf)))

(defun ta-company-emacs-lisp-mode ()
  "Setup `company-mode' for `emacs-lisp-mode-hook'"
  (set (make-local-variable 'company-backends)
       '((company-yasnippet
					:with
          company-elisp
          company-dabbrev-code
          company-files)
         company-dabbrev
         company-capf)))

(defun ta-company-js-mode ()
  "Setup `company-mode' for `js-mode-hook'"
  (set (make-local-variable 'company-backends)
       '((company-yasnippet
          :with
          company-tide
					company-dabbrev-code
					company-web-html
          company-files)
         company-dabbrev
         company-capf)))

(defun ta-company-python-mode ()
  "Setup `company-mode' for `python-mode-hook'"
  (set (make-local-variable 'company-backends)
       '((company-yasnippet
					:with
          company-anaconda
          company-dabbrev-code
          company-files)
         company-dabbrev
         company-capf)))

(defun ta-company-html-mode ()
  "Setup `company-mode' for `html-mode'"
  (set (make-local-variable 'company-backends)
       '((company-dabbrev-code
          company-web-html
          company-yasnippet
          company-files)
         company-dabbrev
         company-capf)))

(defun ta-company-css-mode ()
  "Setup `company-mode' for `css-mode'"
  (set (make-local-variable 'company-backends)
       '((company-yasnippet
          :with
					;; company-dabbrev-code
          company-css
					company-web-html
          ;; company-files
					)
         company-dabbrev
         company-capf)))

(add-hook 'after-init-hook 'global-company-mode)
(setq company-completion-finished-hook 'ta-company-completion-hook)
(add-hook 'emacs-lisp-mode-hook 'ta-company-emacs-lisp-mode)
(add-hook 'php-mode-hook 'ta-company-php-mode)
(add-hook 'python-mode-hook 'ta-company-python-mode)
(add-hook 'org-mode-hook 'ta-company-org-mode)
(add-hook 'sh-mode-hook 'ta-company-sh-mode)
(add-hook 'mhtml-mode-hook 'ta-company-html-mode)
(add-hook 'html-mode-hook 'ta-company-html-mode)
(add-hook 'css-mode-hook 'ta-company-css-mode)

(add-hook 'js-mode-hook 'ta-company-js-mode)
(add-hook 'js-mode-hook 'ac-html-csswatcher+)
(add-hook 'js-jsx-mode-hook 'ac-html-csswatcher+)

;; -- completion for LaTeX-mode
;; next few lines adapted from "lsp-clients.el" file
(defcustom lsp-clients-digestif-executable
  "~/Downloads/luarocks-3.2.1/lua_modules/bin/digestif"
  "Command to start the Digestif language server."
  :group 'lsp-tex)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-digestif-executable)
                  :major-modes '(plain-tex-mode LaTeX-mode latex-mode)
                  :priority -1
                  :server-id 'digestif))

(defun ta-company-latex-mode ()
  "Setup `company-mode' for `latex-mode-hook'"
  (set (make-local-variable 'company-backends)
       '((company-lsp
          company-yasnippet
          company-dabbrev-code
          company-files)
         company-dabbrev
         company-capf)))

;; lsp must be added last in LaTeX hook
(add-hook 'LaTeX-mode-hook 'ta-company-latex-mode)
(add-hook 'LaTeX-mode-hook #'lsp)


(provide 'setup-company)

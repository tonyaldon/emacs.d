(require 'company)
(require 'company-web)
(require 'company-tern)
(require 'company-jedi)
(require 'company-web-html)
(require 'company-emoji)

(eval-after-load 'company
  '(progn
     (setq company-idle-delay 0
           company-dabbrev-downcase nil
           company-dabbrev-ignore-case t
           company-dabbrev-code-ignore-case t
           company-minimum-prefix-length 1)))

(make-variable-buffer-local 'company-minimum-prefix-length)

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
  (set (make-local-variable 'company-backends)
       '((company-emoji
					company-capf
					company-yasnippet
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
       '((company-elisp
          company-yasnippet
          company-dabbrev-code
          company-files)
         company-dabbrev
         company-capf)))

(defun ta-company-js-mode ()
  "Setup `company-mode' for `js-mode-hook'"
  (set (make-local-variable 'company-backends)
       '((company-tern
          company-yasnippet
          company-dabbrev-code
          company-files)
         company-dabbrev
         company-capf)))

(defun ta-company-python-mode ()
  "Setup `company-mode' for `python-mode-hook'"
  (set (make-local-variable 'company-backends)
       '((company-anaconda
          company-yasnippet
          company-dabbrev-code
          company-files)
         company-dabbrev
         company-capf)))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'emacs-lisp-mode-hook 'ta-company-emacs-lisp-mode)
(add-hook 'js-mode-hook 'ta-company-js-mode)
(add-hook 'php-mode-hook 'ta-company-php-mode)
(add-hook 'python-mode-hook 'ta-company-python-mode)
(add-hook 'org-mode-hook 'ta-company-org-mode)
(add-hook 'sh-mode-hook 'ta-company-sh-mode)


;; --- html / css ...

(defun ta-company-html-mode ()
  "Setup `company-mode' for `html-mode'"
  (set (make-local-variable 'company-backends)
       '((company-web-html
          company-yasnippet
          company-dabbrev-code
          company-files)
         company-dabbrev
         company-capf)))




(add-hook 'mhtml-mode-hook 'ta-company-html-mode)
(add-hook 'html-mode-hook 'ta-company-html-mode)
;; (add-hook 'css-mode-hook 'ta-company-css-mode)


;; -- completion for LaTeX-mode
;; LSP / DIGESTIF (via 'lua')
;; https://github.com/astoff/digestif
;; https://github.com/emacs-lsp/lsp-mode
;; https://github.com/tigersoldier/company-lsp
;; https://luarocks.org/
(require 'lsp-mode)
(require 'company-lsp)

;; installation of 'digestif'
;; -- I had to remove 'lua' from the system
;; $ sudo apt remove lua
;; $ sudo apt install lua5.3
;; $ sudo apt install liblua5.3-dev
;; -- and I install it from the source available here: https://luarocks.org/
;; $ wget https://luarocks.org/releases/luarocks-3.2.1.tar.gz
;; $ tar zxpf luarocks-3.2.1.tar.gz
;; $ cd luarocks-3.2.1
;; $ ./configure && make && sudo make install
;; $ sudo luarocks install luasocket
;; -- then I have installed 'digest' localy (in 'luacrocks-3.2.1' directory)
;; $ sudo luarocks install –server digestif
;; -- And finally, 'digestif' has been installed localy here:
;; luacrocks-3.2.1/lua_modules/bin/digestif

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

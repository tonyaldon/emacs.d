(require 'kb)
(require 'kb-lines)
(require 'smartparens)
(require 'setup-smartparens-config)
(require 'avy)
;; (defvar sp--html-modes '(
;;                          sgml-mode
;;                          html-mode
;;                          rhtml-mode
;;                          nxhtml-mode
;;                          nxml-mode
;;                          web-mode
;;                          jinja2-mode
;;                          html-erb-mode
;;                          js-jsx-mode
;;                          js2-jsx-mode
;;                          rjsx-mode
;;                          )
;;   "List of HTML modes.")

;; (defcustom sp-navigate-consider-sgml-tags '(
;;                                             html-mode
;;                                             )
;;   "List of modes where sgml tags are considered to be sexps."
;;   :type '(repeat symbol)
;;   :group 'smartparens)

;; function: mhtml--syntax-propertize-submode
;; function: set-text-properties
;; function: get-text-property
;;   ex: (get-text-property (point) 'mhtml-submode)
;; function: get-char-property-and-overlay
;; (get-char-property-and-overlay (point) 'mmm-mode)


(add-hook 'css-mode-hook (lambda () (setq emmet-use-css-transform t)))



;; (defun emmet-after-hook ()
;;   "Initialize Emmet's buffer-local variables."
;;   (if (memq major-mode emmet-css-major-modes)
;;       (setq emmet-use-css-transform t))
;;   (if (eq major-mode 'sass-mode)
;;       (setq emmet-use-sass-syntax t)))


;; see macro: sp-get
;; see function: sp-get-sexp
;; see function: sp-get-expression
;; see function: sp-get-textmode-stringlike-expression
;; see function: sp-get-enclosing-sexp

;; practical function:
;;   - what-cursor-position
;;   - describe-char
;;   - describe-text-properties

(global-set-key (kbd "C-<f1>") 'what-cursor-position)
(global-set-key (kbd "C-<f2>") 'describe-text-properties)


(add-to-list 'sp-navigate-consider-sgml-tags 'mhtml-mode)
(add-to-list 'sp-navigate-consider-sgml-tags 'vue-mode)
(add-to-list 'sp-navigate-consider-sgml-tags 'vue-html-mode)
(add-to-list 'sp-navigate-consider-sgml-tags 'markdown-mode)



(defun ta-sp-toggle-consider-sgml-tags ()
  "Toggle navigation with sgml-tags in html-mode."
  (interactive)
  (if (-contains? sp-navigate-consider-sgml-tags 'mhtml-mode)
      (setq sp-navigate-consider-sgml-tags
            (-remove-item 'mhtml-mode sp-navigate-consider-sgml-tags))
		(add-to-list 'sp-navigate-consider-sgml-tags 'mhtml-mode)))

;; (-remove-item 'mhtml-mode sp-navigate-consider-sgml-tags)
;; (-remove-item 'oups '('iue 'uie))

;; (add-to-list 'sp--html-modes 'mhtml)

(--each sp--html-modes
  (eval-after-load it                      '(require 'smartparens-html)))
(eval-after-load 'mhtml-mode '(require 'smartparens-html))

(defun sp-get-sgml-tag (&optional back)
  (sp--maybe-init)
  (sp--with-case-sensitive
    (save-excursion
      (let ((search-fn (if (not back) 'sp--search-forward-regexp 'search-backward-regexp))
            tag tag-name needle
            open-start open-end
            close-start close-end)
        (when (and (funcall search-fn "</?.*?\\s-?.*?>" nil t)
                   (progn
                     (setq tag (substring-no-properties (match-string 0)))
                     (setq tag-name (sp--sgml-get-tag-name tag))
                     (not (sp--sgml-ignore-tag tag-name))))
          (setq needle (concat "</?" tag-name))
          (let* ((forward (sp--sgml-opening-p tag))
                 (search-fn (if forward 'sp--search-forward-regexp 'search-backward-regexp))
                 (depth 1))
            (save-excursion
              (if (not back)
                  (progn
                    (setq open-end (point))
                    (search-backward-regexp "<" nil t)
                    (setq open-start (point)))
                (setq open-start (point))
                (search-forward-regexp ">" nil t)
                (setq open-end (point))))
            (cond
             ((and (not back) (not forward))
              (goto-char (match-beginning 0)))
             ((and back forward)
              (goto-char (match-end 0))))
            (while (> depth 0)
              (if (funcall search-fn needle nil t)
                  (if (sp--sgml-opening-p (match-string 0))
                      (if forward (setq depth (1+ depth)) (setq depth (1- depth)))
                    (if forward (setq depth (1- depth)) (setq depth (1+ depth))))
                (setq depth -1)))
            (if (eq depth -1)
                (progn ;(sp-message :no-matching-tag) ; to not have a message
                                        ; in html-mode on tag like this <meta charset="utf-8"/>
                                        ; when runing sp-forward-sexp
                  nil
                  )
              (save-excursion
                (if forward
                    (progn
                      (setq close-start (match-beginning 0))
                      (search-forward-regexp ">" nil t)
                      (setq close-end (point)))
                  (setq close-start (point))
                  (search-forward-regexp ">" nil t)
                  (setq close-end (point))))
              (let ((op (buffer-substring-no-properties open-start open-end))
                    (cl (buffer-substring-no-properties close-start close-end)))
                (list :beg (if forward open-start close-start)
                      :end (if forward close-end open-end)
                      :op (if forward op cl)
                      :cl (if forward cl op)
                      :prefix ""
                      :suffix "")))))))))

;; ---------------------
(eval-after-load 'text-mode                '(require 'smartparens-text))
(eval-after-load 'markdown-mode            '(require 'smartparens-markdown))
(eval-after-load 'org                      '(require 'smartparens-org))
(eval-after-load 'latex                    '(require 'smartparens-latex))
(eval-after-load 'tex-mode                 '(require 'smartparens-latex))
(eval-after-load 'LaTeX '(require 'smartparens-latex))
(--each '(python-mode python)
  (eval-after-load it                      '(require 'smartparens-python)))
(--each '(js js2-mode)
  (eval-after-load it                      '(require 'smartparens-javascript)))

(smartparens-global-mode t)
(smartparens-global-strict-mode -1)
(show-smartparens-global-mode t)
(setq sp-navigate-interactive-always-progress-point t)
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)
(setq sp-show-pair-from-inside nil)

(defun ta-drag-sexp-to-left ()
  "Drag next sexp to the left of the previous sexp.

Work as I want with `sp-navigate-interactive-always-progress-point' set to non-nil value."
  (interactive)
  (sp-forward-sexp)
  (sp-backward-sexp)
  (sp-transpose-sexp)
  (sp-backward-sexp 2))

(defun ta-drag-sexp-to-right ()
  "Drag next sexp to the left of the previous sexp.

Work as I want with `sp-navigate-interactive-always-progress-point' set to non-nil value."
  (interactive)
  (sp-forward-sexp)
  (sp-transpose-sexp)
  (sp-backward-sexp))

(defun ta-avy-copy-sexp ()
  "Copy a selected sexp at the current point"
  (interactive)
  (let ((initial-window (selected-window)))
    (save-excursion
      (call-interactively 'avy-goto-char)
      (sp-copy-sexp))
    (select-window initial-window)
    (yank)))

(defun ta-avy-kill-sexp ()
  "Kill a selected sexp and save it in the kill ring"
  (interactive)
  (let ((initial-window (selected-window)))
    (save-excursion
      (call-interactively 'avy-goto-char)
      (sp-kill-sexp))
    (select-window initial-window)))

(defhydra hydra-sp
  (
   :pre (hydra-color-pre-sp)
   :post (hydra-color-post)
   :hint nil)
  ("<prior>" hydra-sp-reshape/body :color blue)
  ("<next>" hydra-lines/body :color blue)
  ("j" hydra-org/body :color blue)
  ("m" hydra-scrolling/body :color blue)
  ;; ---
  ("S" smartparens-global-strict-mode :color blue)
  ;; ---
  ("." set-mark-command)
  ("t" exchange-point-and-mark)
  ("r" join-line)
  ;; ---
  ("k" sp-kill-whole-line)
  ("," sp-kill-sexp)
  ("DEL" sp-backward-kill-sexp)
  ("c" sp-copy-sexp)
  ("@" sp-backward-copy-sexp)
	("C-y" sp-clone-sexp)
  (";" sp-comment)
  ("N" sp-narrow-to-sexp)
  ;; sp-motion
  (">" sp-beginning-of-next-sexp)
  ("<" sp-beginning-of-previous-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-next-sexp)
  ("p" sp-previous-sexp)
  ("u" sp-down-sexp)
  ("i" sp-up-sexp)
  ("y" sp-backward-up-sexp)
  ("x" sp-backward-down-sexp)
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ;; to insert text
  ("_" ta-add-space :color blue)
  ;; ---
  ("$" sp-show-enclosing-pair)
  ("{" sp-wrap-curly)
  ("(" sp-wrap-round)
  ("[" sp-wrap-square)
  ("/" sp-rewrap-sexp)
  ("s" sp-swap-enclosing-sexp)
  ;; ---
  ("g" cleanup-buffer)
  ("M--" undo)
  ("q" nil))

(defhydra hydra-sp-reshape
  (
   :pre (hydra-color-pre-sp-reshape)
   :post (hydra-color-post)
   :hint nil)
  ("<next>" hydra-lines/body :color blue)
  ("." hydra-sp/body :color blue)
  ("j" hydra-org/body :color blue)
  ("m" hydra-scrolling/body :color blue)
  ;; ---
  ("k" sp-kill-whole-line)
  ("," sp-kill-sexp)
  ("DEL" sp-backward-kill-sexp)
  ("c" ta-avy-copy-sexp :color blue)
  ("@" ta-avy-kill-sexp :color blue)
  (";" sp-comment)
  ("N" sp-narrow-to-sexp)
  ;; ---
  ("a" sp-absorb-sexp)
  ("i" sp-change-inner :color blue)
  ("/" sp-change-enclosing :color blue)
  ("e" sp-emit-sexp)
	;; ---
	("%" sp-convolute-sexp)
  ;; ---
  ("d" ta-drag-sexp-to-left)
  ("s" ta-drag-sexp-to-right)
  ;; ---
  ("f" sp-forward-slurp-sexp)
  ("b" sp-backward-slurp-sexp)
  (")" sp-forward-barf-sexp)
  ("(" sp-backward-barf-sexp)
  ("p" sp-add-to-previous-sexp)
  ("n" sp-add-to-next-sexp)
  ;; ---
  (":" sp-split-sexp)
  ("'" sp-join-sexp)
  ;; ---
  ("[" sp-splice-sexp-killing-backward)
  ("]" sp-splice-sexp-killing-forward)
  ("u" sp-splice-sexp :color blue)
  ("r" sp-raise-sexp)
  ;; ---
  ("g" cleanup-buffer)
  ("M--" undo)
  ("q" nil))

(global-set-key (kbd "M-i") 'hydra-sp/body)
(key-chord-define-global "-0" 'hydra-sp-reshape/body)


(provide 'kb-smartparens)

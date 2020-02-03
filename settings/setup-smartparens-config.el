(require 'smartparens)

(defun sp-lisp-invalid-hyperlink-p (_id action _context)
  "Test if there is an invalid hyperlink in a Lisp docstring.
ID, ACTION, CONTEXT."
  (when (eq action 'navigate)
    ;; Ignore errors due to us being at the start or end of the
    ;; buffer.
    (ignore-errors
      (or
       ;; foo'|bar
       (and (looking-at "\\sw\\|\\s_")
            ;; do not consider punctuation
            (not (looking-at "[?.,;!]"))
            (save-excursion
              (backward-char 2)
              (looking-at "\\sw\\|\\s_")))
       ;; foo|'bar
       (and (save-excursion
              (backward-char 1)
              (looking-at "\\sw\\|\\s_"))
            (save-excursion
              (forward-char 1)
              (looking-at "\\sw\\|\\s_")
              ;; do not consider punctuation
              (not (looking-at "[?.,;!]"))))))))

;; emacs is lisp hacking enviroment, so we set up some most common
;; lisp modes too
(sp-with-modes sp-lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil))

(sp-with-modes (-difference sp-lisp-modes sp-clojure-modes)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'"
                 :when '(sp-in-string-p
                         sp-in-comment-p)
                 :unless '(sp-lisp-invalid-hyperlink-p)
                 :skip-match (lambda (ms _mb _me)
                               (cond
                                ((equal ms "'")
                                 (or (sp-lisp-invalid-hyperlink-p "`" 'navigate '_)
                                     (not (sp-point-in-string-or-comment))))
                                (t (not (sp-point-in-string-or-comment)))))))

;; TODO: this should only be active in docstring, otherwise we want
;; the regexp completion \\{\\}.  To handle this feature, we must
;; allow multiple pairs on same opening (therefore, the unique ID must
;; become the opening and closing pair)
(sp-local-pair 'emacs-lisp-mode "\\\\{" "}" :when '(sp-in-docstring-p))

;; NOTE: Normally, `sp-local-pair' accepts list of modes (or a single
;; mode) as a first argument.  The macro `sp-with-modes' adds this
;; automatically.  If you want to call sp-local-pair outside this
;; macro, you MUST supply the major mode argument.


(provide 'setup-smartparens-config)

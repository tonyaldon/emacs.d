;;; About

;; When `region-active-spc-mode' is turned on, this modify some
;; typed character (self-insert-command) when the `region-active-p' is
;; true and instead of inserting the typed character it trigger any
;; command you set in `region-active-spc-map' variable.
;;
;; With the stantard setup, when a region is active and you type "b",
;; the command `backward-word' is call instead of inserting character
;; "b".
;;
;; "spc" in `region-active-spc-mode' means SPeed Command.
;;
;; `region-active-spc-mode' mode is incompatible with
;; `pending-delete-mode' alias of `delete-selection-mode'.

;;; Code

(defvar region-active-spc-self-insert-commands
  '(self-insert-command
    org-self-insert-command
    outline-spc-self-insert-command)
  "List of command like `self-insert-command' we want to
advice \":before-until\" with `region-active-spc-trigger'.")

(defvar region-active-spc-map
  '(("." . narrow-to-region)
    (":" . eval-region)
    ("," . kill-region)
    ("c" . kill-ring-save)
    ("q" . backward-delete-char-untabify)
    ("t" . exchange-point-and-mark)
    ("p" . previous-logical-line)
    ("n" . next-logical-line)
    ("b" . backward-word)
    ("f" . forward-word)
    ("u" . sp-backward-sexp)
    ("i" . sp-forward-sexp)
    ("a" . beginning-of-line)
    ("e" . end-of-line)
    ("y" . backward-paragraph)
    ("x" . forward-paragraph))
  "Alist of speed commands.
The car of each entry is a string with a single letter, which must
be assigned to `self-insert-command' in the global map.
The cdr is either a command to be called interactively.")

(defun region-active-spc-trigger (&rest r)
  "When region is active, trigger a command instead of inserting
the character if it is mapped to a command in `region-active-spc-map'.

Function intended to be used as advice \":before-until\" of
any command like `self-insert-command'."
  (if (region-active-p)
      (let* ((key-vector (this-command-keys-vector))
             (key-string (make-string 1 (aref key-vector 0)))
             (speed-command
              (cdr (assoc key-string region-active-spc-map))))
        (when speed-command
          (call-interactively speed-command) t))))

;;; region-active-spc-mode

(defvar region-active-spc-delete-selection-mode-user nil)

(define-minor-mode region-active-spc-mode
  "Toggle `region-active-spc-mode' mode on or off."
  :global nil
  (if region-active-spc-mode
      (progn
        (--each region-active-spc-self-insert-commands
          (advice-add it :before-until 'region-active-spc-trigger))
        (when (boundp delete-selection-mode)
          (setq region-active-spc-delete-selection-mode-user
                delete-selection-mode)
          (delete-selection-mode -1)))
    (--each region-active-spc-self-insert-commands
      (advice-remove it 'region-active-spc-trigger))
    (when region-active-spc-delete-selection-mode-user
      (delete-selection-mode t)
      (setq region-active-spc-delete-selection-mode-user nil))))

;;; Comments

(comment ; when, call-interactively, assoc, type-of
 (when nil "uie")
 (when t "uie" "uieee")

 (equal t (call-interactively 'next-line)) ; nil
 (assoc "a" '(("a" . jim) ("b" . tony))) ; ("a" . jim)
 (assoc "c" '(("a" . jim) ("b" . tony))) ; nil

 (call-interactively nil) ; error
 (call-interactively
  (cdr (assoc "n" '(("n" . next-line) ("p" . previous-line)))))
 (call-interactively
  (cdr (assoc "x" '(("n" . next-line) ("p" . previous-line))))) ; error

 (type-of (cdr (assoc "n" '(("n" . next-line) ("p" . previous-line))))) ; symbol
 )

(comment ; vectorp, this-command-keys-vector, make-string, aref, symbol-name
 (vectorp (this-command-keys-vector)) ; t
 (vectorp (make-string 2 (aref (this-command-keys-vector) 0))) ; error
 (vectorp (make-string 2 (symbol-name (aref (this-command-keys-vector) 0)))) ; error
 (type-of (aref (this-command-keys-vector) 0)) ; symbol
 (make-string 1 'a) ; error
 (symbol-name 'a) ; "a"
 )

(comment ; read-key-sequence-vector, num-input-keys, this-command-keys-vector
 ;; (info "(elisp) Key Sequence Input")
 (let ((dot (read-key-sequence-vector "type a dot:")))
   ;; (equal [46] dot)
   (equal [?.] dot))
 (read-key-sequence-vector nil)
 (read-key-sequence-vector "type key")
 (read-key-sequence "type key")
 num-input-keys
 (this-command-keys-vector)
 (advice-remove 'self-insert-command 'region-active-spc-trigger)
 (advice-remove 'org-self-insert-command 'region-active-spc-trigger)
 (advice-remove 'outline-spc-self-insert-command 'region-active-spc-trigger)
 )

;;; Footer

(provide 'region-active-spc)

(defun ta-start-learning-smartparens ()
  "Set frame and buffers to start learning smartparens keybinding."
  (interactive)
  (sp-cheat-sheet)
  (split-window-below)
  (describe-function 'handy-sexp-reshape/body)
  (ta-drag-window-left)
  (enlarge-window-horizontally 35)
  (split-window-right)
  (find-file "~/Documents/learning/key-bindings/mhtml-mode.el")
  (windmove-right)
  (find-file "~/Documents/learning/key-bindings/_mhtml-mode.el"))

(defun ta-yas-describe-table-by-namehash ()
  "Small changes on `yas-describe-table-by-namehash'"
  (interactive)
  (with-current-buffer (get-buffer-create "*YASnippet Tables by NAMEHASH*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "YASnippet tables by NAMEHASH: \n")
      (maphash
       (lambda (_mode table)
         (insert (format "\nSnippet table `%s':\n\n" (yas--table-name table)))
         (maphash
          (lambda (key _v)
            (insert (format "   %s --> %s\n" key
                            (let ((names))
                              (maphash #'(lambda (k _v)
                                           (push k names))
                                       (gethash key (yas--table-hash table)))
                              names))))
          (yas--table-hash table)))
       yas--tables))
    (view-mode +1)
    (goto-char 1)
    (display-buffer (current-buffer))))


(provide 'util-learning)

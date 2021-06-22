(declare-function ivy-read "ext:ivy")

(defvar quick-access-alist ()
  "Association list of quick-access names and its full filename.

The value is an alist with quick-access of the form

 (QUICK-ACCESS-NAME . FILENAME).

QUICK-ACCESS-NAME must be uniq.")

(defun quick-access-all-names ()
  "Return the list of all the quick-access-name define in `quick-access-alist'."
  (-map 'car quick-access-alist))

(defun quick-access-get-filename (quick-access-name)
  "Return the full filename of QUICK-ACCESS-NAME, or nil if none."
  (assoc-default quick-access-name quick-access-alist))

(defun counsel-quick-access ()
  "Open a quick-access file listed in `quick-access-alist'."
  (interactive)
  (ivy-read "Open quick-access: "
            (quick-access-all-names)
            :history 'quick-access-history
            :action (lambda (x)
                      (find-file (quick-access-get-filename x)))
            :caller 'counsel-quick-access))


(provide 'quick-access)

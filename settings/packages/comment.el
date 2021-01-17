;;; About

;; I find the "comment" macro in the clojure language really convenient.
;; See https://betweentwoparens.com/rich-comment-blocks#rich-comment.
;;
;; Now, I have it in emacs.
;;
;; You can use it to wrap code:
;; - you don't want emacs evaluates,
;; - but you want to keep in your code.
;;
;; The 4 sexps inside `comment' macro below are ignored:
;;
;; (comment
;;  (newline)
;;  1
;;  "uie"
;;  (kill-line -1))
;;
;; It is a way to comment sexps ready to be evaluated related to your code,
;; which is really handy to explore and understand the code itself.
;;
;; One of the advantage of the `comment' macro compared to ';' to comment
;; the code is that you have syntax highlighting.

;;; Code

(defmacro comment (&rest body)
	"Ignores body and yield nil.

It's a copy of the \"comment\" macro in the clojure language.
See https://betweentwoparens.com/rich-comment-blocks#rich-comment."
	nil)

;;; Comments

(comment
 (newline)
 1
 "uie"
 (kill-line -1))

;;; Footer

(provide 'comment)

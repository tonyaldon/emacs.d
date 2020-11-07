(deftheme discreet  "A discreet dark theme for Emacs")

(custom-theme-set-variables 'discreet)

(let (
      (d-black-1       "#151515")
      (d-black-2       "#161a1f")
      (d-black-3       "#222")
      (d-black-4       "#333")
      (d-gray-1        "#555")
      (d-gray-2        "#5e5e5e")
      (d-gray-3        "#8c8c8c")
      (d-gray-4        "#b3b3b3")
      (d-white-0       "#ffffff")
      (d-white-1       "#dedede")
      (d-red           "#ff6c60")
      (d-orange-1      "#fd721f")
      (d-orange-2      "#fd971f")
      (d-yellow-1      "#ffd500") ; not used here (for cursor)
      (d-yellow-2      "#eedc82")
      (d-yellow-3      "#f5ebb6")
      (d-green-1       "#60ff6c")
      (d-green-2       "#26f9ad")
      (d-aquamarine-1  "#7fffd4")
      (d-aquamarine-2  "#15bb84")
      (d-aquamarine-3  "#359b79")
      (d-aquamarine-4  "#458b74")
      (d-cyan-1        "#457f8b")
      (d-cyan-2        "#5297a5")
      (d-blue-1        "#87cefa")
      (d-blue-2        "#8795fa") ; not used here (for cursor)
      (d-pink-1        "#fa87ce") ; not used here (for cursor)
      (d-pink-2        "#f92672"))


  (custom-theme-set-faces
   'discreet

   `(bold ((t (:bold t))))
   `(default ((t (:background ,d-black-1 :foreground ,d-white-1))))
   `(hl-line ((t (:background ,d-black-4 ))))
   `(cursor ((t (:background ,d-green-2))))
   `(mc/cursor-bar-face ((t (:background ,d-white-1 :height 3))))

   `(highlight ((t (:foreground ,d-white-0 :bold t))))
   `(lazy-highlight ((t (:underline ,d-orange-2 :bold t))))
   `(region ((t (:background "#394851"))))

   `(isearch ((t (:inherit highlight :underline t))))
   `(isearch-fail ((t (:background ,d-red))))

   `(iedit-occurrence ((t (:underline ,d-orange-2 :bold t))))

   `(show-paren-match ((t (:background ,d-black-4 :foreground ,d-white-0 :underline t :weight ultra-bold))))
   `(show-paren-mismatch ((t (:background ,d-red :foreground ,d-white-1 :weight bold))))

   `(avy-lead-face ((t (:foreground ,d-red :weight bold))))
   `(avy-lead-face-0 ((t (:inherit avy-lead-face))))
   `(avy-lead-face-1 ((t (:inherit avy-lead-face))))
   `(aw-leading-char-face ((t (:inherit avy-lead-face))))

   `(minibuffer-prompt ((t (:foreground ,d-pink-2 :bold t))))

   `(header-line ((t (:background ,d-black-1 :foreground ,d-pink-2 :weight bold))))
   `(ta-dired-header-face ((t (:foreground ,d-black-1))))
   `(dired-directory ((t (:foreground ,d-aquamarine-2 :weight bold :underline t))))
   `(dired-flagged ((t (:inherit error))))
   `(dired-header ((t (:inherit ta-dired-header-face))))
   `(dired-ignored ((t (:inherit shadow))))
   `(dired-mark ((t (:inherit font-lock-variable-name-face))))
   `(dired-marked ((t (:inherit font-lock-variable-name-face))))
   `(dired-perm-write ((t (:inherit font-lock-comment-delimiter-face))))
   `(dired-symlink ((t (:foreground ,d-yellow-2))))
   `(dired-warning ((t (:inherit font-lock-warning-face))))

   `(mode-line ((t (:background ,d-aquamarine-4 :foreground ,d-black-1))))
   `(mode-line-inactive ((t (:background ,d-gray-1 :foreground ,d-gray-3))))
   `(mode-line-buffer-id ((t (:foreground ,d-gray-4 :weight bold))))
   `(vc-state-base ((t (:foreground ,d-orange-2))))

   `(error ((t (:foreground ,d-red))))
   `(warning ((t (:foreground ,d-orange-1))))
   `(success ((t (:foreground ,d-green-1))))
   `(match ((t (:foreground ,d-orange-2 :weight bold))))

   `(info-xref ((t (:foreground ,d-aquamarine-3 :underline t))))
   `(info-xref-visited ((t (:foreground ,d-yellow-3 :underline t))))
   `(info-header-xref ((t (:foreground ,d-white-1 :underline t))))
   `(info-menu-star ((t (:foreground ,d-white-1))))
   `(link ((t (:foreground ,d-aquamarine-3 :underline t))))

	 `(wgrep-done-face ((t (:foreground ,d-blue-1 :weight bold))))
   `(wgrep-face ((t (:underline (:color ,d-gray-4 :style wave)))))
   `(wgrep-file-face ((t (:background ,d-gray-2 :foreground ,d-white-1))))
   `(wgrep-reject-face ((t (:foreground ,d-pink-2 :weight bold))))

   `(compilation-error ((t (:foreground ,d-red))))
   `(compilation-info ((t (:foreground ,d-cyan-2 :underline t))))
   `(compilation-line-number ((t (:foreground ,d-yellow-2 :underline t))))
   `(compilation-warning ((t (:foreground ,d-orange-2))))
	 `(compilation-mode-line-exit ((t (:foreground ,d-green-1))))
	 `(compilation-mode-line-fail ((t (:foreground ,d-red))))
	 `(compilation-mode-line-run ((t (:foreground ,d-orange-2))))

   `(company-preview ((t (:foreground ,d-white-1 :inherit hl-line :bold t))))
   `(company-preview-common ((t (:foreground ,d-white-1 :inherit hl-line :bold t))))
   `(company-preview-search ((t (:foreground ,d-orange-2 :inherit hl-line :bold t))))
   `(company-scrollbar-bg ((t (:background ,d-black-4))))
   `(company-scrollbar-fg ((t (:inherit highlight))))
   `(company-tooltip ((t (:foreground ,d-gray-3 :background ,d-black-1))))
   `(company-tooltip-common ((t (:foreground "#017a7e" :weight bold))))
   `(company-tooltip-common-selection ((t (:foreground "#02eaf3" :weight bold))))
   `(company-tooltip-selection ((t (:foreground ,d-white-1 :bold t))))
   `(company-tooltip-annotation ((t (:foreground ,d-orange-2))))
   `(company-tooltip-annotation-selection ((t (:foreground ,d-orange-2))))
   `(company-tooltip-search ((t (:foreground ,d-orange-2 :bold t))))
   `(company-tooltip-search-selection ((t (:foreground ,d-orange-2 :bold t))))

   `(ivy-current-match ((t (:underline ,d-white-1 :bold t))))
   `(ivy-cursor ((t (:background ,d-white-1))))
   `(ivy-minibuffer-match-face-1 ((t (:underline ,d-orange-2 :bold t))))
   `(ivy-minibuffer-match-face-2 ((t (:underline "#feb259" :foreground ,d-white-0 :bold t))))
   `(ivy-minibuffer-match-face-3 ((t (:underline "#59feb2" :foreground ,d-white-0 :bold t))))
   `(ivy-minibuffer-match-face-4 ((t (:underline "#59a5fe" :foreground ,d-white-0 :bold t))))
   `(ivy-prompt-match ((t (:inherit ivy-current-match))))

   `(swiper-background-match-face-1 ((t (:underline ,d-orange-2 :bold t))))
   `(swiper-background-match-face-2 ((t (:underline "#feb259" :foreground ,d-white-0 :bold t))))
   `(swiper-background-match-face-3 ((t (:underline "#59feb2" :foreground ,d-white-0 :bold t))))
   `(swiper-background-match-face-4 ((t (:underline "#59a5fe" :foreground ,d-white-0 :bold t))))
   `(swiper-isearch-current-match ((t (:background "black" :foreground "white"))))
   `(swiper-line-face ((t (:underline ,d-white-1 :bold t))))
   `(swiper-match-face-1 ((nil)))
   `(swiper-match-face-2 ((nil)))
   `(swiper-match-face-3 ((nil)))
   `(swiper-match-face-4 ((nil)))

   `(counsel--mark-ring-highlight ((t (:inherit highlight))))
   `(counsel-application-name ((t (:inherit font-lock-builtin-face))))
   `(counsel-key-binding ((t (:inherit font-lock-keyword-face))))

   ;; font-lock
   `(font-lock-negation-char-face ((t (:foreground ,d-red))))
   `(font-lock-warning-face ((t (:foreground ,d-orange-2))))
   `(font-lock-variable-name-face ((t (:foreground ,d-orange-2))))
   `(font-lock-doc-face ((t (:foreground ,d-yellow-2))))
   `(font-lock-doc-string-face ((t (:foreground ,d-yellow-2))))
   `(font-lock-string-face ((t (:foreground ,d-yellow-2))))
   `(font-lock-function-name-face ((t (:foreground ,d-aquamarine-4))))
   `(font-lock-builtin-face ((t (:foreground ,d-cyan-1))))
   `(font-lock-type-face ((t (:foreground ,d-cyan-2))))
   `(font-lock-keyword-face ((t (:foreground ,d-pink-2))))
   `(font-lock-preprocessor-face ((t (:foreground ,d-pink-2))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,d-gray-3))))
   `(font-lock-comment-face ((t (:foreground ,d-gray-3))))
   `(font-lock-constant-face ((t (:foreground ,d-blue-1))))
   `(font-lock-reference-face ((t (:foreground ,d-yellow-3))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,d-blue-1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,d-blue-1))))
   `(font-lock-number-face ((t (:foreground ,d-yellow-2))))))


;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'discreet)

;; Local Variables:
;; no-byte-compile: t
;; End:

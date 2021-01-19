;;; About
;;
;; A boring part but really important when making Inside Emacs is
;; to transform the story (writed in an text file, exactly an org-mode
;; file) to beautiful footage.  So far it involves, to transform
;; story text to svg files that are put together to produce the footage
;; telling the story of the videos.
;;
;; In this file, we define some commands that 'parse' the text file
;; where the story is written, and generate the necessary svg files
;; we then use to produce the footage.
;;
;; Find in the ./story.org, how the story must be written to be well
;; parsed and we can generate the svg files.
;;
;; The story belongs to a subsection (** scenes 1: scene title) which belongs
;; to the section (* scenes).  Each paragraph of the story must start in
;; the line following (# description) line and end up at the first empty lines.
;;
;; So the stories of Inside Emacs videos are written in the following format:
;; * scenes
;; ** scene 0: intro
;; # description
;; here a story splited
;; in two lines
;;
;; ** scene 1: Blank Field
;; # description
;; A one line paragraph
;;
;; # description
;; we handle only
;; paragraph with 4 lines
;; to be readable
;; for the reader
;;
;; Usage:
;; To generate the description svg files telling the story of the video,
;; in the file where the story is written (following the format previously
;; described), run: M-x ie-story-generate-all-descriptions-svg

;;; Parse story file

(defun ie-story-goto-next-description (&optional bound)
  "Move point to the next description paragraph.

Return (point) if a description paragraph is found.
nil if not."
  (when (looking-at "# description") (forward-char))
  (when (search-forward "# description" bound t)
    (next-line)
    (beginning-of-line)
    (point)))

(defun ie-story-description-lines (description-beginning)
  "Return the description paragraph at START-POINT as a list of string

Each element represents a line."
  (save-excursion
    (when description-beginning
      (let ((description-end
             (progn (search-forward-regexp "^$" nil t) (backward-char) (point))))
        (s-lines
         (buffer-substring-no-properties description-beginning description-end))))))

(defun ie-story-next-scene ()
  "Return buffer position of the next scene heading."
  (save-excursion
    (when (looking-at "^\\** scene ") (forward-char))
    (when (search-forward-regexp "^\\** scene " nil t)
      (beginning-of-line)
      (point))))

(defun ie-story-goto-next-scene ()
  "Go to the next heading scene."
  (when-let ((next-scene (ie-story-next-scene)))
    (goto-char next-scene)))

(defun ie-story-scenes ()
  "Return the list of buffer position of the scenes in the current buffer."
  (save-excursion
    (beginning-of-buffer)
    (let ((scenes '()))
      (while (ie-story-goto-next-scene)
        (beginning-of-line)
        (add-to-list 'scenes (point))
        (next-line))
      (reverse scenes))))

(defun ie-story-scene-title (scene-buffer-position &optional kebab-case)
  "Return the title of the scene at SCENE-BUFFER-POSITION.

If KEBAB-CASE is t, return the title of the scene but in kebab-case."
  (save-excursion
    (goto-char scene-buffer-position)
    (search-forward-regexp ": *" nil t)
    (let* ((beg (point))
           (end (progn (end-of-line) (point)))
           (name (buffer-substring-no-properties beg end)))
      (if kebab-case
          (s-downcase (s-replace " " "-" name))
        name))))

;;; Generate svg files

;;;; svg description characteristics
;;
;; :style "font-style:normal;font-variant:normal;font-weight:bold;font-stretch:normal;font-size:96px;line-height:1.25;font-family:Ramabhadra;-inkscape-font-specification:'Ramabhadra Bold';letter-spacing:0px;word-spacing:0px;fill:#f0f0f0;fill-opacity:0.941176;stroke:none;stroke-width:0.264583"
;; :x 91    (all lines have same x ordinate)
;; :y 656   (line 4)
;; :y 776   (line 3)
;; :y 896   (line 2)
;; :y 1016  (line 1)
;;
;; scene title
;; :style "font-style:normal;font-variant:normal;font-weight:bold;font-stretch:normal;font-size:96px;line-height:1.25;font-family:Ramabhadra;-inkscape-font-specification:'Ramabhadra Bold';letter-spacing:0px;word-spacing:0px;fill:#f0f0f0;fill-opacity:0.941176;stroke:none;stroke-width:0.264583"
;; :x 90
;; :y 194

;;;; Packages

(require 'svg)

;;;; Global variables

(setq ie-r-images "r-images")

;;;; Generate svg files

(defun ie-story-generate-scene-title-svg (scene-buffer-position)
  "Generate svg scene title of Inside Emacs at SCENE-BUFFER-POSITION.

The svg file generated is save in the directory `ie-r-images' with
a unique name."
  (unless (f-exists? ie-r-images) (f-mkdir ie-r-images))
  (let* ((title (ie-story-scene-title scene-buffer-position))
         (title-kebab-case (ie-story-scene-title scene-buffer-position t))
         (file (f-join ie-r-images
                       (s-concat
                        (s-join "-" `("description" ,title-kebab-case "title"))
                        ".svg")))
         (svg (svg-create 1920 1080))
         (style "font-style:normal;font-variant:normal;font-weight:bold;font-stretch:normal;font-size:96px;line-height:1.25;font-family:Ramabhadra;-inkscape-font-specification:'Ramabhadra Bold';letter-spacing:0px;word-spacing:0px;fill:#f0f0f0;fill-opacity:0.941176;stroke:none;stroke-width:0.264583")
         (x-start-line 90)
         (y-start-line 194))
    (svg-text svg title
              :style style :x x-start-line :y y-start-line)
    (with-temp-buffer
      (svg-print svg)
      (write-region (point-min) (point-max) file))))

(defun ie-story-generate-description-svg (lines title index)
  "Generate svg description of Inside Emacs.

LINES is a list of strings representing the description.
TITLE is the title (kebab-case) of the scene the description belongs to.
INDEX is the appearance order of the description in the SCENE.

The svg file generated is save in the directory `ie-r-images' with
a unique name."
  (unless (f-exists? ie-r-images) (f-mkdir ie-r-images))
  (let ((file (f-join ie-r-images
                      (s-concat
                       (s-join "-" `("description" ,title ,(number-to-string index)))
                       ".svg")))
        (svg (svg-create 1920 1080))
        (style "font-style:normal;font-variant:normal;font-weight:bold;font-stretch:normal;font-size:96px;line-height:1.25;font-family:Ramabhadra;-inkscape-font-specification:'Ramabhadra Bold';letter-spacing:0px;word-spacing:0px;fill:#f0f0f0;fill-opacity:0.941176;stroke:none;stroke-width:0.264583")
        (lines-reversed (reverse lines))
        (x-start-line 90)
        (y-start-lines '(1016 896 776 656)))
    (while lines-reversed
      (svg-text svg (pop lines-reversed)
                :style style :x x-start-line :y (pop y-start-lines)))
    (with-temp-buffer
      (svg-print svg)
      (write-region (point-min) (point-max) file))))

(defun ie-story-generate-descriptions-in-scene-svg (scene-buffer-position)
  "Generate all svg descriptions of Inside Emacs in the scene at SCENE-BUFFER-POSITION."
  (save-excursion
    (goto-char scene-buffer-position)
    (let* ((bound (ie-story-next-scene))
           (scene-name (ie-story-scene-title scene-buffer-position t))
           (description-index 1)
           description-lines)
      (while (ie-story-goto-next-description bound)
        (setq description-lines (ie-story-description-lines (point)))
        (ie-story-generate-description-svg description-lines
                                           scene-name
                                           description-index)
        (setq description-index (1+ description-index))))))

(defun ie-story-generate-all-descriptions-svg ()
  "Generate all svg descriptions of Inside Emacs for the current buffer."
  (interactive)
  (--each (ie-story-scenes)
    (ie-story-generate-scene-title-svg it)
    (ie-story-generate-descriptions-in-scene-svg it)))

;;; COMMENTS:
;; COMMENTS:
;; (f-join "r-images"
;;        (s-concat
;;         (s-join "-" `("description" "scene" ,(number-to-string 1)))
;;         ".svg"))
;; (f-mkdir "dir-test")
;; (setq test-list '(a b c))
;; (reverse test-list)
;; (pop test-list)
;; test-list
;; (ie-story-generate-description-svg '("line 1") "my-scene" 1)
;; (ie-story-generate-description-svg '("line 1" "line 2") "my-scene" 2)
;; (ie-story-generate-description-svg '("line 1" "line 2" "line 3") "my-scene" 3)
;; (ie-story-generate-description-svg '("line 1" "line 2" "line 3" "line 4") "my-scene" 4)

;; COMMENTS:
;; (search-forward-regexp "^$")
;; (s-lines "uie\ntony")
;; (global-set-key (kbd "C-<f1>") 'ie-story-goto-next-description)
;; (ie-story-description-lines (ie-story-goto-next-description))
;; # description
;; line 1
;; line 2

;; COMMENTS:
;; (ie-story-generate-description-svg
;;  (ie-story-description-lines (ie-story-goto-next-description)) "scene" 1)
;; # description
;; line 1
;; line 2

;; COMMENTS:
;; in the file, description.org
;; M-x eval-expression RET (equal (ie-story-scenes) '(413 494 829)) ; t
;; M-x eval-expression RET (ie-story-scene-title 413)   ; intro
;; M-x eval-expression RET (ie-story-scene-title 413 t) ; intro
;; M-x eval-expression RET (ie-story-scene-title 494)   ; Blank Field
;; M-x eval-expression RET (ie-story-scene-title 494 t) ; blank-field
;; M-x eval-expression RET (ie-story-scene-title 829)   ; org-table-copy-down
;; M-x eval-expression RET (ie-story-scene-title 829 t) ; org-table-copy-down
;; (global-set-key (kbd "C-<f1>") 'ie-story-scenes)
;; (global-set-key (kbd "C-<f1>") 'ie-story-goto-next-scene)
;; (s-replace " " "-" "tony aldon")

;; COMMENTS:
;; in the file, description.org
;; M-x eval-expression RET (ie-story-generate-scene-title-svg 413)
;; M-x eval-expression RET (ie-story-generate-scene-title-svg 494)
;; M-x eval-expression RET (ie-story-generate-scene-title-svg 829)
;; M-x eval-expression RET (ie-story-generate-descriptions-in-scene-svg 413)
;; M-x eval-expression RET (ie-story-generate-descriptions-in-scene-svg 494)
;; M-x eval-expression RET (ie-story-generate-descriptions-in-scene-svg 829)
;; (global-set-key (kbd "C-<f1>") 'ie-story-generate-all-descriptions-svg)

;;; Footer

(provide 'story)

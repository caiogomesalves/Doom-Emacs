;;----------------------------------------------------------------------
;; Functions.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; Split window and open shell.

(defun open-shell-split-window ()
  "Open shell and split window."
  (interactive)
  (split-window)
  (shell)
  (previous-buffer) ;; 1
  (other-window 1)
  (next-buffer)     ;; 2
  (other-window 1))

;;----------------------------------------------------------------------
;; Duplicate lines (like in Geany).
;; http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs

(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))

;;----------------------------------------------------------------------
;; Cut and copy without selection.
;; http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html

(defun copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save
       (region-beginning)
       (region-end))
    (kill-ring-save
     (line-beginning-position)
     (line-beginning-position 2))))

(defun cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region
       (region-beginning)
       (region-end))
    (kill-region
     (line-beginning-position)
     (line-beginning-position 2))))

;;----------------------------------------------------------------------
;; (un)Comment without selection.

(defun comment-line-or-region ()
  "Comment or uncomment current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region
       (region-beginning)
       (region-end))
    (comment-or-uncomment-region
     (line-beginning-position)
     (line-beginning-position 2))))

;;----------------------------------------------------------------------
;; Mark the word where the point is. -- Walmes Zeviani.

(defun mark-whole-word ()
  "Mark the word where the point is."
  (interactive)
  (skip-chars-backward "[[:alnum:]]._")
  (set-mark (point))
  (skip-chars-forward "[[:alnum:]]._"))

;;----------------------------------------------------------------------
;; Return the font face at point.
;; http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs

;; (defun what-face (pos)
;;   (interactive "d")
;;   (let ((face (or (get-char-property (point) 'read-face-name)
;;                   (get-char-property (point) 'face))))
;;     (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;----------------------------------------------------------------------
;; Infill paragraph.
;; http://www.emacswiki.org/emacs/UnfillParagraph

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line
   of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; http://ergoemacs.org/emacs/emacs_unfill-paragraph.html
(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
   This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

;;----------------------------------------------------------------------
;; Commented rules to divide code.

;; t in line is empty (oly whitespaces), nil otherwise.
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-11/msg00212.html
(defun blank-line-p ()
  (string-match "^[[:space:]]*$"
                (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))

(defun wz-insert-rule-from-point-to-margin (&optional char)
  "Insert a commented rule with dashes (-) from the `point' to
   the `fill-column' if the line has only spaces. If the line has
   text, fill with dashes until the `fill-column'. Useful to
   divide your code in sections. If a non nil optional argument is
   passed, then it is used instead."
  (interactive)
  (if (blank-line-p)
      (progn (insert "-")
             (comment-line-or-region)
             (delete-char -2))
    nil)
  (if char
      (insert (make-string (- fill-column (current-column)) char))
    (insert (make-string (- fill-column (current-column)) ?-))))

(defun wz-insert-rule-and-comment-3 ()
  "Insert a commented rule with 43 dashes (-). Useful to divide
   your code in sections."
  (interactive)
  (if (blank-line-p)
      (progn (insert "-")
             (comment-line-or-region)
             (delete-char -2))
    nil)
  (let ((column-middle (floor (* 0.625 fill-column))))
    (if (< (current-column) column-middle)
        (insert (make-string (- column-middle (current-column)) ?-)))))

;;----------------------------------------------------------------------

(defun replace-buffer-divisions-by-Caio-style (beg end &optional char)
  "This functions replace divisions in R code by Caio's style
   code division: start with single # and have 71 dashes, total
   length is `fill-column'. All rules greater than 44 characters
   will be replaced until complete margin."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let ((comment-char
           (if char
               char
             (read-from-minibuffer "Comment char: "))))
      (while
          ;; To have a prompt to pass the comment char.
          (re-search-forward
           (concat "^" comment-char ".-\\{43,\\}")
           nil t)
        (replace-match
         (concat comment-char
                 (make-string
                  (- fill-column (string-width comment-char)) ?-))
         nil nil)))))

;; (defun wz-make-line-end-dashes-fill-column (beg end)
;;   "This function fix those dashes at end of lines used as
;;    decoration making them have a end at `fill-column'. At least
;;    must have five dashes after a space, because 3 dashes are yaml
;;    header and 4 are markdown horizontal rule."
;;   (interactive "r")
;;   (save-excursion
;;     (goto-char beg)
;;     (while (re-search-forward " -\\{5,\\}" end t)
;;       (let ((xmax fill-column)
;;             (xval (match-beginning 0) )
;;             (null (beginning-of-line))
;;             (xmin (point)))
;;         (replace-match
;;          (concat
;;           " "
;;           (make-string (- xmax (- xval xmin) 1) ?-)) nil nil)))))

;;----------------------------------------------------------------------
;; Header.

(defun wz-right-align-commented-text (text comment-char-size)
  "Write text aligned to the right margin at `fill-column' and
   comment it out."
  (let ((number-of-spaces (- fill-column (length text)))
        (string-length (length text)))
    (insert (concat "\n" text))
    (comment-line-or-region)
    (backward-char string-length)
    (insert (make-string (- number-of-spaces comment-char-size) ? ))
    (forward-char string-length)))

(defun wz-header ()
  "Insert a header."
  (interactive)
  (wz-insert-rule-from-point-to-margin)
  ;; Get the number of characters used as comment.
  (let ((comment-char-size
         (- (+ fill-column 1)
            (how-many "-" (line-beginning-position) (point) t))))
    (wz-right-align-commented-text
     "Caio Gomes Alves"
     comment-char-size)
    (wz-right-align-commented-text
     "github.com/caiogomesalves"
     comment-char-size)
    (wz-right-align-commented-text
     "caio.gomesalves@ufpr.br"
     comment-char-size)
    (wz-right-align-commented-text
     "Programa de Educação Tutorial (PET-Estatística)"
     comment-char-size)
    (wz-right-align-commented-text
     "Department of Statistics · Federal University of Paraná"
     comment-char-size)
    (wz-right-align-commented-text
     (concat (format-time-string "%Y-%b-%d") " · Curitiba/PR/Brazil")
     comment-char-size)
    )
  (insert "\n")
  (wz-insert-rule-from-point-to-margin))

;;----------------------------------------------------------------------
;; Code based on
;; http://www.emacswiki.org/emacs/CamelCase.

(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string
       "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun camel-case (s)
  (concat (car (split-name s))
          (mapconcat 'capitalize (cdr (split-name s)) "")))
(defun dot-case (s)
  (mapconcat 'downcase (split-name s) "."))
(defun snake-case (s)
  (mapconcat 'downcase (split-name s) "_"))

(defun camel-dot-snake ()
  "Cycle among camelCase, dot.case and snake_case in words. If
   the region is not active the current word at point is used."
  (interactive)
  (setq trs (and transient-mark-mode mark-active))
  (unless trs
    (skip-chars-backward "[[:alnum:]]._")
    (set-mark (point))
    (skip-chars-forward "[[:alnum:]]._"))
  (let* ((beg (region-beginning))
         (end (region-end))
         (str (buffer-substring-no-properties beg end)))
    (if (string-match "^\s*$" str)
        (message "Not a word at point")
      (delete-region beg end)
      (insert
       (cond ((string-match-p "\\." str) (snake-case str))
             ((string-match-p "_"   str) (camel-case str))
             (t                          (dot-case   str))))
      (if trs (setq deactivate-mark nil)))))

;; Another interesting implementation:
;; https://www.bunkus.org/blog/2009/12/switching-identifier-naming
;; -style-between-camel-case-and-c-style-in-emacs/

;;----------------------------------------------------------------------
;; Functions related do Rmd files.

;; Insert a new (empty) chunk to R markdown.
(defun wz-insert-chunk ()
  "Insert chunk environment Rmd sessions."
  (interactive)
  (if (derived-mode-p 'ess-mode)
      (insert "```\n\n```{r}\n")
    (insert "```{r}\n\n```")
    (forward-line -1)))

;; Goes to next chunk.
(defun wz-polymode-next-chunk ()
  "Go to next chunk. This function is not general because is
   assumes all chunks are of R language."
  (interactive)
  (search-forward-regexp "^```{.*}$" nil t)
  (forward-line 1))

;; Goes to previous chunk.
(defun wz-polymode-previous-chunk ()
  "Go to previous chunk. This function is not general because is
   assumes all chunks are of R language."
  (interactive)
  (search-backward-regexp "^```$" nil t)
  (search-backward-regexp "^```{.*}$" nil t)
  (forward-line 1))

;; Evals current R chunk.
(defun wz-polymode-eval-R-chunk ()
  "Evals all code in R chunks in a polymode document (Rmd files)."
  (interactive)
  (if (derived-mode-p 'ess-mode)
      (let ((ptn (point))
            (beg (progn
                   (search-backward-regexp "^```{r.*}$" nil t)
                   (forward-line 1)
                   (line-beginning-position)))
            (end (progn
                   (search-forward-regexp "^```$" nil t)
                   (forward-line -1)
                   (line-end-position))))
        (ess-eval-region beg end nil)
        (goto-char ptn))
    (message "ess-mode weren't detected.")))

;; Evals R chunk and goes to next chunk.
(defun wz-polymode-eval-R-chunk-and-next ()
  "Evals a R chunk and move point to next chunk."
  (interactive)
  (wz-polymode-eval-R-chunk)
  (wz-polymode-next-chunk))

(defun wz-polymode-eval-line-by-line (end)
  "This function evaluates only R code inside Rmd chunks from
   `point' to `end'. The function determine the boundaries of a
   chunk based on regex. If the point is a chunk, then each line
   is evaluated and the point go to the next line."
  (interactive)
  ;; Turn on/off the `inside-chunk' based on the major-mode at point.
  (if (derived-mode-p 'ess-mode)
      (setq inside-chunk t)
    (setq inside-chunk nil))
  ;; Go through each line till reach `end'.
  (while (< (point) end)
    ;; (beginning-of-line)
    ;; Test if point is at the chunk header.
    (if (looking-at "```{r")
        (progn
          (setq inside-chunk t)
          (forward-line 1)))
    ;; If inside-chunk is t then evaluate the line.
    (if inside-chunk
        (ess-eval-line t))
    (forward-line 1)
    ;; (beginning-of-line)
    ;; Test if point is at the chunk tail.
    (if (looking-at "```$")
        (setq inside-chunk nil))))

(defun wz-polymode-eval-chunks-on-region ()
  "This function calls `wz-polymode-eval-line-by-line'.
   It only determines if region is activated. Then it evaluates
   the code inside the chunks in the region."
  (interactive)
  (if (region-active-p)
      ;; If region is activated.
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char beg)
        ;; Calls the function.
        (wz-polymode-eval-line-by-line end))
    ;; If the region is not activated.
    (message "Region must be activated.")))

;;----------------------------------------------------------------------

;; ;; Based on:
;; ;; http://stackoverflow.com/questions/4697322/elisp-call-command-on-current-file
;; (defun wz-ess-rmarkdown-render ()
;;   "Run rmarkdown::render() in the buffer. Tested only in Rmd files."
;;   (interactive)
;;   (shell-command
;;    (format "Rscript -e 'library(rmarkdown); render(\"%s\")'"
;;            (shell-quote-argument (buffer-file-name))))
;;   (revert-buffer t t t))

;; ;; Mark a word at a point.
;; ;; http://www.emacswiki.org/emacs/ess-edit.el
;; (defun ess-edit-word-at-point ()
;;   (save-excursion
;;     (buffer-substring
;;      (+ (point) (skip-chars-backward "a-zA-Z0-9._"))
;;      (+ (point) (skip-chars-forward "a-zA-Z0-9._")))))

;; ;; Eval any word where the cursor is (objects, functions, etc).
;; (defun ess-eval-word ()
;;   (interactive)
;;   (let ((x (ess-edit-word-at-point)))
;;     (ess-eval-linewise (concat x))))

(defun wz-ess-forward-R-assigment-symbol ()
  "Move cursor to the next occurrence of 「<-」 「=」. Adapted from:
   URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'."
  (interactive)
  (search-forward-regexp "=+\\|<-" nil t))

(defun wz-ess-backward-R-assigment-symbol ()
  "Move cursor to the previous occurrence of 「<-」 「=」. Adapted from:
   `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'."
  (interactive)
  (search-backward-regexp "=+\\|<-" nil t))

(defun wz-ess-align-R-assigment-operators ()
  "Função que alinha a região com a primeira ocorrência de sinais
   ` <- ' e ` = '. Baseado em:
   http://stackoverflow.com/questions/13315064/
   marker-does-not-point-anywhere-from-align-regexp-emacs"
  (interactive)
  (save-excursion
    (align-regexp
     (region-beginning) (region-end)
     "\\(\\s-*\\) *\\(<-\\|=\\) *" 1 1 nil)))

(defun wz-ess-backward-break-line-here ()
  "Searches a point backward where a break there is allowed."
  (interactive)
  (re-search-backward "\\([-+*/%<>(,]\\|[<>=!]=\\) *[[:alnum:]({]")
  (forward-char 1))

(defun wz-ess-forward-break-line-here ()
  "Searches a point forward where a break there is allowed. I
   don't know why, but the forward some times skips correct
   points that backward get."
  (interactive)
  (re-search-forward "\\([-+*/%<>(,]\\|[<>=!]=\\) *[[:alnum:]({]")
  (forward-char -1))

(defun string-face-p ()
  "Return t if font-face at point is string-face, nil otherwise."
  (eq 'font-lock-string-face (get-char-property (point) 'face)))

(defun comment-face-p ()
  "Return t if font-face at point is comment-face, nil otherwise."
  (eq 'font-lock-comment-face (get-char-property (point) 'face)))

(defun wz-ess-break-or-join-lines-wizard ()
  "Break line wizard in R scripts. This function helps break and indent
   or join lines in R code. The keybings are:
   <right>    : go to next matching;
   <left>     : go to previous matching;
   <down>     : go to next line;
   <up>       : go to previous line;
   <return>   : break and indent newline;
   <delete>   : join line below;
   <backspace>: join line above;
   <C-z>      : undo;
   <any>      : unhighlight and exit."
  (interactive)
  (setq rgxp "\\([-+*/%<>(,]\\|[<>=!]=\\) *[[:alnum:]({]")
  (highlight-regexp rgxp)
  (let (done event)
    (while (not done)
      (let ((inhibit-quit t))
        (setq event (read-event
                     "Press: right|left|down|up|RET|DEL|BKS|any"))
        (if inhibit-quit (setq quit-flag nil))
        (cond ((eq event 'right)
               (progn (re-search-forward rgxp)
                      (while (or (string-face-p) (comment-face-p))
                        (re-search-forward rgxp))
                      (forward-char -1)))
              ((eq event 'left)
               (progn (re-search-backward rgxp)
                      (while (or (string-face-p) (comment-face-p))
                        (re-search-backward rgxp))
                      (forward-char 1)
                      ))
              ((eq event 'return)
               (progn (indent-new-comment-line)
                      (re-search-forward rgxp)
                      (forward-char -1)))
              ((eq event 'delete)
               (progn (forward-line)
                      (delete-indentation)
                      (re-search-backward rgxp)
                      (forward-char 1)))
              ((eq event 'backspace)
               (progn (delete-indentation)
                      (re-search-backward rgxp)
                      (forward-char 1)))
              ((eq event 'down)
               (forward-line))
              ((eq event 'up)
               (forward-line -1))
              ((eq event ?\C-z)
               (undo))
              ((eq event 'escape)
               (setq done t))
              (t
               (setq done t)))))
    (unhighlight-regexp rgxp)))

;;----------------------------------------------------------------------
;; Function based in the bm-bookmark-regexp-region.
;; This function bookmark all chunks in *.Rnw and *.Rmd buffers.

(defun wz-bm-bookmark-chunk-in-buffer ()
  "Set bookmark on chunk header lines in Rnw and Rmd files."
  (interactive)
  (let ((regexp "^<<.*>>=$\\|^```{.*}$")
        (annotation nil)
        (count 0))
    (save-excursion
      (if bm-annotate-on-create
          (setq annotation
                (read-from-minibuffer
                 "Annotation: " nil nil nil 'bm-annotation-history)))
      (goto-char (point-min))
      (while (and (< (point) (point-max))
                  (re-search-forward regexp (point-max) t))
        (bm-bookmark-add annotation)
        (setq count (1+ count))
        (forward-line 1)))
    (message "%d bookmark(s) created." count)))

;;----------------------------------------------------------------------
;; Font:
;; https://github.com/basille/.emacs.d/blob/master/functions/ess-indent-region-as-R-function.el

;; The function below is a modification of the original to use
;; formatR::tidy_source(). This is because formatR have functions with
;; arguments to control the output, as keep comments and set the width
;; cutoff.

(defun ess-indent-region-with-formatR-tidy-source (beg end)
  "Format region of code R using formatR::tidy_source()."
  (interactive "r")
  (let ((string
         (replace-regexp-in-string
          "\"" "\\\\\\&"
          (replace-regexp-in-string ;; how to avoid this double matching?
           "\\\\\"" "\\\\\\&"
           (buffer-substring-no-properties beg end))))
        (buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({
          formatR::tidy_source(text = \"\n%s\",
                               arrow = TRUE, width.cutoff = 60) })\n"
      string) buf)
    (with-current-buffer buf
      (goto-char (point-max))
      ;; (skip-chars-backward "\n")
      (let ((end (point)))
        (goto-char (point-min))
        (goto-char (1+ (point-at-eol)))
        (setq string (buffer-substring-no-properties (point) end))))
    (delete-region beg end)
    (insert string)))

(defun wz-ess-stringi-escape-unicode (beg end)
  "Replace non-ASCII by the corresponding unicode. Select the text
   without the quotes and apply the function. By Walmes Zeviani."
  (interactive "r")
  (let ((string
         (replace-regexp-in-string
          "\"" "\\\\\\&"
          (replace-regexp-in-string
           "\\\\\"" "\\\\\\&"
           (buffer-substring-no-properties beg end))))
        (buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({
          cat(stringi::stri_escape_unicode(\"%s\"),
             \"\\n\") })\n"
      string) buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (let ((end (point)))
        (goto-char (point-min))
        (skip-chars-forward " +")
        (setq string (buffer-substring-no-properties (point) end))))
    (delete-region beg end)
    (insert string)
    (delete-char -2)))

(defun wz-ess-find-and-insert-namespace (beg end)
  "Preceds a function with its namespace,
   so `mean(x) -> stats::mean(x)' and
   `xyplot(...) -> lattice::xyplot()'.
   Call this function in a R major mode buffer with the function name
   selected. By Walmes Zeviani."
  (interactive "r")
  (let ((string
         (replace-regexp-in-string
          "\"" "\\\\\\&"
          (replace-regexp-in-string
           "\\\\\"" "\\\\\\&"
           (buffer-substring-no-properties beg end))))
        (buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({
           x <- \"%s\"
           cat(paste0(sub('.*:', '', utils::find(x)), '::', x), \"\\n\")
       })\n"
      string) buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (let ((end (point)))
        (goto-char (point-min))
        (skip-chars-forward " +")
        (setq string (buffer-substring-no-properties (point) end))))
    (delete-region beg end)
    (insert string)
    (delete-char -1)))

(defun wz-ess-insert-function-args (beg end)
  "This function inserts all arguments of a function call.
   When you mark the word `plot' this function returns
   `plot(x, y, ....)'. By Walmes Zeviani."
  (interactive "r")
  (let ((string
         (replace-regexp-in-string
          "\"" "\\\\\\&"
          (replace-regexp-in-string
           "\\\\\"" "\\\\\\&"
           (buffer-substring-no-properties beg end)))
         )
        (buf (get-buffer-create "*ess-command-output*"))
        )
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({
           usage <- function(f = 'lm', shift = 0L) {
               f_width <- nchar(f)
               u <- capture.output(args(f)) |>
                   sub(pattern = 'function [(]',
                       replacement = paste0(f, '(')) |>
                   head(n = -1)
               u <- u |>
                   paste(collapse = '') |>
                   gsub(pattern = '[[:space:]]+', replacement = ' ') |>
                   strsplit(split = ', ')
               u <- u[[1]]
               u[-length(u)] <- paste0(u[-length(u)], ',\n')
               spaces <- c(paste(rep(' ', shift),
                                 collapse = ''),
                           paste(rep(' ', shift + f_width - 1),
                                 collapse = ''))
               u[1] <- paste(spaces[1], u[1])
               u[-1] <- paste(spaces[-1], u[-1])
               cat(u, '\n')
           }
           usage(\"%s\", %d)
       })\n"
      string (save-excursion (goto-char beg) (current-column))) buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (let ((end (point)))
        (goto-char (point-min))
        (skip-chars-forward " +")
        (setq string (buffer-substring-no-properties (point) end))))
    (delete-region beg end)
    (insert string)
    (delete-char -1)
    (goto-char end)))

(defun wz-ess-open-html-documentation (beg end)
  "TODO. By Walmes Zeviani."
  (interactive "r")
  (let ((string
         (replace-regexp-in-string
          "\"" "\\\\\\&"
          (replace-regexp-in-string
           "\\\\\"" "\\\\\\&"
           (buffer-substring-no-properties beg end))))
        (buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({
           help(%s, help_type = \"html\")
       })\n"
      string) buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (let ((end (point)))
        (goto-char (point-min))
        (skip-chars-forward " +")
        (setq string (buffer-substring-no-properties (point) end))))
    )
  )


;;----------------------------------------------------------------------
;; Improved version of occur. Quick navigation.
;; http://ignaciopp.wordpress.com/2009/06/10/customizing-emacs-occur/

(defun my-occur (&optional arg)
  "Make sure to always put occur in a vertical split, into a
   narrower buffer at the side. I didn't like the default
   horizontal split, nor the way it messes up the arrangement of
   windows in the frame or the way in which the standard way uses
   a neighbor window."
  (interactive "P")
  ;; store whatever frame configuration we are currently in
  (window-configuration-to-register ?y)
  (occur (read-from-minibuffer "Regexp: "))
  (if (occur-check-existence)
      (progn
        (delete-other-windows)
        ;; (split-window-horizontally)
        ;; (enlarge-window-horizontally -30)
        (split-window-vertically)
        (enlarge-window -10)
        ;; (set-cursor-color "green")
        )
    )
  (occur-procede-accordingly)
  (next-error-follow-minor-mode) ;;+
  )

(defun occur-procede-accordingly ()
  "Switch to occur buffer or prevent opening of the occur window
   if no matches occurred."
  (interactive "P")
  (if (not(get-buffer "*Occur*"))
      (message "There are no results.")
    (switch-to-buffer "*Occur*")))

(defun occur-check-existence()
  "Signal the existence of an occur buffer depending on the
   number of matches."
  (interactive)
  (if (not(get-buffer "*Occur*")) nil t))

;; http://www.emacswiki.org/emacs/OccurMode
;; To show more context lines, use
;; C-U 5 M-x occur regexp-to-search

(defun occur-mode-quit ()
  "Quit and close occur window. I want to press 'q' and leave
   things as they were before in regard of the split of windows
   in the frame. This is the equivalent of pressing C-x 0 and
   reset windows in the frame, in whatever way they were, plus
   jumping to the latest position of the cursor which might have
   been changed by using the links out of any of the matches
   found in occur."
  (interactive)
  (switch-to-buffer "*Occur*")
  ;; in order to know where we put the cursor they might have jumped from occur
  (other-window 1)                  ;; go to the main window
  (point-to-register ?1)            ;; store the latest cursor position
  (switch-to-buffer "*Occur*")      ;; go back to the occur window
  (kill-buffer "*Occur*")           ;; delete it
  (jump-to-register ?y)             ;; reset the original frame state
  ;; (set-cursor-color "rgb:ff/fb/53") ;; reset cursor color
  (register-to-point ?1))           ;; re-position cursor

;; Some key bindings defined below. Use "p" ans "n" as in dired mode
;; (without Cntrl key) for previous and next line; just show occurrence
;; without leaving the "occur" buffer; use RET to display the line of
;; the given occurrence, instead of jumping to i,t which you do clicking
;; instead; also quit mode with Ctrl-g.

;;----------------------------------------------------------------------
;; Based on https://github.com/emacs-lsp/lsp-treemacs/issues/35.
;; Requieres lsp-treemacs: https://github.com/emacs-lsp/lsp-treemacs

(defun lsp-treemacs-symbols-toggle ()
  "Toggle the lsp-treemacs-symbols buffer."
  (interactive)
  (if (get-buffer "*LSP Symbols List*")
      (kill-buffer "*LSP Symbols List*")
    (progn (lsp-treemacs-symbols)
           (other-window -1))))

(defun lsp-ui-imenu-toggle ()
  "Toggle the lsp-ui-imenu buffer."
  (interactive)
  (if (get-buffer "*lsp-ui-imenu*")
      (kill-buffer "*lsp-ui-imenu*")
    ;; (progn (lsp-ui-imenu)
    ;;        (other-window -1))
    (lsp-ui-imenu)
    )
  )


;;; px.el --- preview inline latex in any mode

;; Copyright (C) 2014 Aurélien Aptel <aurelien.aptel@gmail.com>
;; Copyright (C) 2013 Rüdiger Sonderfeld <ruediger@c-plusplus.de>

;; Author: Aurélien Aptel <aurelien.aptel@gmail.com>
;; URL: http://github.com/aaptel/preview-latex
;; Version: 1.1


;;; Commentary:

;; Provides functions to preview LaTeX codes like $x^2$ in any
;; buffer/mode.

;; Use `px-preview-region' to preview LaTeX codes delimited by $ pairs
;; in the region.
;; Use `px-preview' to process the whole buffer.
;; Use `px-remove' to remove all images and restore the text back.
;; Use `px-toggle' to toggle between images and text on the whole
;; buffer.

;; Most of this code comes from weechat-latex.el which in turn uses
;; org-mode previewer.


;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'org)

(defvar px-temp-file-prefix "px-"
  "Prefix for temporary files.")

(defvar px-temp-directory-prefix "px-"
  "Prefix for temporary directory.")

(defvar px-image-program org-latex-create-formula-image-program
  "Program to convert LaTeX fragments.
See `org-latex-create-formula-image-program'")

(defvar px-temp-dir nil
  "The temporary directory used for preview images.")

(defvar px--active nil)
(make-variable-buffer-local 'px--active)

(defun px--create-preview (&optional beg end)
  "Wrapper for `org-format-latex'.
The parameter AT should be nil or in (TYPE . POINT) format.  With TYPE being a
string showing the matched LaTeX statement (e.g., ``$'') and POINT being the
POINT to replace.  If AT is nil replace statements everywhere."
  (if (version< "9" org-version)
      (org-format-latex px-temp-file-prefix
                        beg end
                        temporary-file-directory
                        'overlays
                        "Creating images...%s"
                        'forbuffer
                        px-image-program)
    (condition-case e
        (org-format-latex px-temp-file-prefix
                          px-temp-dir
                          'overlays
                          "Creating images...%s"
                          (if beg (cons "$" beg) nil)
                          'forbuffer
                          px-image-program)

      ;; if wrong arity, try with one less argument (cf. issue #1)
      (wrong-number-of-arguments
       (org-format-latex px-temp-file-prefix
                         px-temp-dir
                         'overlays
                         "Creating images...%s"
                         'forbuffer
                         px-image-program)))))


(defun px--set-temp-dir ()
  "Set `px-temp-dir' unless it is already set."
  (unless px-temp-dir
    (setq px-temp-dir
          (make-temp-file px-temp-directory-prefix
                          'directory))))

;;;###autoload
(defun px-preview ()
  "Preview LaTeX fragments in the current buffer."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (px--set-temp-dir)
      (px-remove)
      (px--create-preview)
      (setq px--active t))))

;;;###autoload
(defun px-preview-region (beg end)
  "Preview LaTeX fragments in region."
  (interactive "r")
  (let* ((math-regex (assoc "$" org-latex-regexps))
         (regex (nth 1 math-regex))
         (n (nth 2 math-regex))
         matches)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regex end t)
        (setq matches (cons (cons (match-beginning n) (match-end n)) matches)))
      (let ((inhibit-read-only t))
        (px--set-temp-dir)
        (dolist (i matches)
          (px--create-preview i))))))

;;;###autoload
(defun px-remove ()
  "Remove LaTeX preview images in current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (if (version< "9" org-version)
        (delete-all-overlays)
      (org-remove-latex-fragment-image-overlays)))
  (setq px--active nil))

;;;###autoload
(defun px-toggle ()
  "Toggle display of LaTeX preview in the current buffer."
  (interactive)
  (if px--active
      (px-remove)
    (px-preview)))


(provide 'px)

;;; px.el ends here
;;;
;;----------------------------------------------------------------------

;;; Commentary:
;; Open new buffer with list of devices (sd..) with `mount' command.

;;

;;; Code:
(defcustom mount-devices-mask "^sd[c-z][0-9]"
  ; I have all sda and sdb I want already mounted.
  "Regexp to identify devices in the /dev/ directory to mount."
  :group 'mount
  :type 'regexp)

(defcustom mount-default-mount "/mnt"
  "Default mount point"
  :group 'mount
  :type 'directory)

(defvar mount-manual-mounts ()
  "List of manually mounted directories")

(defun mount-do-mount (button)
  (message "Mounting %s" (button-label button))
  (let ((dir (read-directory-name "Mount at: " mount-default-mount)))
    (start-process "mount" "*mount*"
		   "sudo" "mount" (button-label button) dir)
    (push (cons (button-label button) dir) mount-manual-mounts))
  (mount-insert-manual-mounts))

(defun mount-do-umount (button)
  (message "Mounting %s" (button-label button))
  (start-process "umount" "*mount*"
		 "sudo" "umount" (button-label button))
  (setq mount-manual-mounts
	(cl-remove (button-get button 'point) mount-manual-mounts
		   :key 'car :test 'equal))
  (mount-insert-manual-mounts))

(defun mount-clear-section (section)
  (goto-char 1)
  (search-forward section nil t)
  (narrow-to-page)
  (delete-region (point-min) (point-max))
  (insert section "\n"))

(defun mount-insert-devices ()
  (interactive)
  (mount-clear-section "Mountable devices:")
  (dolist (dev (directory-files "/dev" t mount-devices-mask))
    (insert ?\^I)
    (insert-button dev
		   'action 'mount-do-mount)
    (insert "\n"))
  (widen))

(defun mount-insert-manual-mounts ()
  (interactive)
  (mount-clear-section "Mounted:")
  (dolist (dev mount-manual-mounts)
    (insert ?\^I)
    (insert-button (car dev)
		   'action 'mount-do-umount
		   'point (car dev))

    (insert " on ")
    (insert-button (cdr dev)
		   'action 'mount-do-umount
		   'point (car dev))
    (insert "\n"))
  (widen))

(define-derived-mode mount-mode special-mode "MOUNTS"
  (setq buffer-read-only nil))

(bind-key "I" 'mount-insert-devices mount-mode-map)
(bind-key "M" 'mount-insert-manual-mounts mount-mode-map)
(bind-key "g" 'mount-clear mount-mode-map)
(bind-key "o" 'ffap mount-mode-map)

(defun mount-clear ()
  (interactive)
  (switch-to-buffer "*mount*")
  (delete-region (point-min) (point-max))
  (insert "Mounts: \n\n")
  (insert "Mounted: \n\n")
  (mount-insert-devices)
  (mount-insert-manual-mounts))

(defun mount ()
  (interactive)
  (mount-clear)
  (mount-mode))


(provide 'mount-mode)
;;; mount.el ends here

(define-key global-map "\M-Q" 'unfill-region)
(define-key global-map (kbd "C-S-o") 'my-occur)
(define-key occur-mode-map (kbd "q") 'occur-mode-quit)

(global-set-key (kbd "S-<delete>") 'cut-line-or-region)  ; cut.
(global-set-key (kbd "C-<insert>") 'copy-line-or-region) ; copy.
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-x w") 'mark-whole-word)
(global-set-key (kbd "C-x t") 'open-shell-split-window)
(global-set-key (kbd "M-;") 'comment-line-or-region)
(global-set-key (kbd "C-ç") 'camel-dot-snake)
(global-set-key (kbd "C-c -") 'wz-insert-rule-from-point-to-margin)
(global-set-key (kbd "C-M--") 'wz-insert-rule-and-comment-3)
(global-set-key (kbd "C-c =")
                (lambda ()
                  (interactive)
                  (wz-insert-rule-from-point-to-margin ?=)))

(add-hook
 'markdown-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c i i") 'wz-insert-chunk)
   (local-set-key (kbd "<f6>")    'wz-polymode-eval-R-chunk)
   (local-set-key (kbd "S-<f6>")  'wz-polymode-eval-R-chunk-and-next)
   (local-set-key (kbd "S-<f7>")  'wz-polymode-previous-chunk)
   (local-set-key (kbd "S-<f8>")  'wz-polymode-next-chunk)))

(add-hook
 'ess-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c i i") 'wz-insert-chunk)
   (local-set-key (kbd "<C-f1>")  'wz-ess-open-html-documentation)
   (local-set-key (kbd "<C-f1>")  'wz-ess-open-html-documentation)
   (local-set-key (kbd "<f6>")    'wz-polymode-eval-R-chunk)
   (local-set-key (kbd "<f7>")    'wz-ess-break-or-join-lines-wizard)
   (local-set-key (kbd "S-<f6>")  'wz-polymode-eval-R-chunk-and-next)
   (local-set-key (kbd "S-<f7>")  'wz-polymode-previous-chunk)
   (local-set-key (kbd "S-<f8>")  'wz-polymode-next-chunk)
   (local-set-key (kbd "C-c r")   'ess-eval-word)
   (local-set-key (kbd "C-c a")   'wz-ess-align-R-assigment-operators)
   (local-set-key (kbd "C-,")     'wz-ess-backward-break-line-here)
   (local-set-key (kbd "C-.")     'wz-ess-forward-break-line-here)
   (local-set-key (kbd "C-:")     'wz-ess-find-and-insert-namespace)
   (local-set-key (kbd "<S-f9>")  'wz-ess-backward-R-assigment-symbol)
   (local-set-key (kbd "C-#")     'replace-buffer-division-by-Caio-style)
   ;; (local-set-key (kbd "C-c C-h") 'ess-edit-indent-call-sophisticatedly)
   (local-set-key (kbd "C-M-|")
                  'ess-indent-region-with-formatR-tidy-source)
   (local-set-key (kbd "C-?")
                  'wz-ess-stringi-escape-unicode)))

;;----------------------------------------------------------------------

(provide 'funcs)

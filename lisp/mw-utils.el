;;;; mw-utils.el  --- Useful functions
;;; Commentary:
;;;    Contains some utility and helper functions useful at work and elsewhere.
;;; Code:
;;;

;;;###autoload
(defun mw-insert-time ()
  "Insert at point the current time in 'HH:MM:SS' format."
  (interactive)
  (insert (format-time-string "%H:%M:%S")))


;;;###autoload
(defun mw-insert-date ()
  "Insert at point the current date in 'DoW, Month Day, Year' format."
  (interactive)
  (insert (format-time-string "%A, %b %d, %Y")))


;;;###autoload
(defun mw-from-unixtime ()
  "Convert a Unix timestamp to locale-specific time."
  (interactive)
  (message (format-time-string "%c" (number-at-point))))


;;;###autoload
(defun mw-guid-clean (start end)
  "Replace colons (:) with hyphens (-), and set the region to upper case.
If a region is selected, operate on text between START and END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward ":" end t)
      (replace-match "-"))
    (upcase-region start end)))


;;;###autoload
(defun mw-create-sql-buffer ()
  "Create a scratch SQL buffer with some basic settings enabled."
  (interactive)
  (save-excursion
    (or (bufferp (get-buffer "*sql*"))
        (progn (set-buffer
                (get-buffer-create "*sql*"))
               (sql-mode)
               (display-line-numbers-mode t)
               (set-display-table-slot standard-display-table 'wrap ?\ )  ; Sets line-wrap character to space ( ) instead of backslash (\)
               (insert "--\n-- Use this buffer for SQL snippets\n--\n\n")))))


(defun mw-find-file (prefix)
    "Delegate the `find-file' request to the appropriate function.
Call `helm-find-files' unless PREFIX arg is present, in which
case we call `helm-projectile-find-file' instead."
    (interactive "p")
    (call-interactively (or
                            (and
                             (eql prefix 1)
                             (fboundp #'helm-find-files)
                             #'helm-find-files)
                            
                            (and
                             (eql prefix 4)
                             (fboundp #'helm-projectile-find-file)
                             #'helm-projectile-find-file)
                            
                            (and
                             (eql prefix 16)
                             #'find-file))))


;;;###autoload
(defun mw-reload-dir-locals ()
  "Reload dir-locals.el for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))


;;;###autoload
(defun mw-insert-curly-braces (start end)
  "Enclose the region defined by START and END within curly braces.

The braces will be the sole glyph placed on their respctive lines
above and below the region.  If the region is not active, simply
open three lines, place the braces each on their own line and place
the cursor on the line between.  In both cases, indent according to
current `major-mode'."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (progn
       (let ((single-point (point)))
         (list single-point single-point)))))
   (if (region-active-p)
       (curly--braces-region start end)
     (curly--braces-at-point)))
  

(defun curly--braces-at-point ()
  "Add curly braces at point by opening two braces with a blank line between."
  (let ((s (point)))
    (insert "{\n\n}")
    (indent-region-line-by-line s (point)) ; does not indent blank lines
    (forward-line -1)
    (funcall indent-line-function))) ; indent (current) blank line


(defun curly--braces-region (start end)
  "Logic to add curly braces around region defined by START and END."
  (let ((text (buffer-substring start end))
        (add-newlines-p (maybe--newlines start end)))
    (message "begin with newline? %s\nend with newline? %s" (car add-newlines-p) (cdr add-newlines-p))
    (goto-char start)
    (delete-region start end)
    (let ((opening-brace (if (car add-newlines-p) "{\n" "{"))
          (closing-brace (if (cdr add-newlines-p) "\n}" "}")))
      (insert opening-brace text closing-brace))
    (indent-region start (point))
    (end-of-line 0)))


(defun maybe--newlines (start end)
  "Look at end of START and END lines to determine if a newline must be added.

A newline must be added if there are graphic characters between
START and the end of the line.  Likewise on line containing END."
  (save-excursion
    (goto-char start)
    (let ((head-newline-p (looking-at-p ".*[[:graph:]]")))

      (goto-char end)
      (beginning-of-line)
      (let ((tail-newline-p (looking-at-p ".*[[:graph:]]")))
        (cons head-newline-p tail-newline-p)))))  ; create dotted pair


;;;###autoload
(defun mw-toggle-selective-display (level)
  "Wrap `set-selective-display' to allow us to toggle text 'folding'.
If the optional prefix is set, then set the folding level to LEVEL, otherwise
set LEVEL to 1."
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or level 1))))

;;;###autoload
(defun mw-find-next-todo (prefix)
  "Find the next TODO or TODO-like comment in current buffer.
If PREFIX is negative, search backward from point."
  (interactive "p")
  (push-mark)
  (search-forward-regexp "TODO\\|TBD\\|FIXME" nil 'no-error prefix))


;;;###autoload
(defun mw-large-file-precautions ()
  "If a file is over a certain size, take some precautions to make it easier to view."
  (when (> (buffer-size) (* 5 1024 1024))
    (buffer-disable-undo)
    (fundamental-mode)
    (projectile-mode -1)))
;(add-hook 'find-file-hook 'mw-large-file-precautions)


;;;###autoload
(defun mw-compilation-completed-notification (buffer status)
  "Display a system notification upon completion of compilation.
Accepts the compilation BUFFER and a STATUS string describing
how the process finished."
  (with-current-buffer buffer
    (require 'notifications)
    (when (string= major-mode "compilation-mode")
      (notifications-notify
       :title "Compilation"
       :body (concat (buffer-name) ": " status)))))


(provide 'mw-utils)
;;; mw-utils.el ends here

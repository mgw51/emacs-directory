;;;; file: my-work-utils.el
;;;; Desc: Contains some utility and helper functions useful at work.

(defun guid-clean (start end)
  "Replace colons (:) with hyphens (-), and set the region to upper case."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward ":" end t)
      (replace-match "-"))
    (upcase-region start end)))

(defun create-sql-buffer ()
  "Create a scratch SQL buffer with some basic settings enabled."
  (interactive)
  (save-excursion 
    (or (bufferp (get-buffer "*sql*"))
        (progn (set-buffer
                (get-buffer-create "*sql*"))
               (sql-mode)
               (linum-mode -1)
               (set-display-table-slot standard-display-table 'wrap ?\ )  ; Sets line-wrap character to space ( ) instead of backslash (\)
               (insert "--\n-- Use this buffer for SQL snippets\n--\n\n")))))

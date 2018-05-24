;;;; select-comment-by-lang.el
;;;; Written: 8/2015
;;;; By:      Matt
;;;;
;;;; This file selects the appropriate type of comment character to
;;;; use based on the language extension of the buffer.  For instance,
;;;; '#' will be used for bash-like languages, python, and perl.
;;;;
;;;; TODO: If there is no extension in the filename from which to choose
;;;;       a language, fall back on the major mode of the buffer.
;;;;
;;;; TODO: Tweak debug-comment to work better with region selection.
;;;;       Sometimes there is no newline char at the end of a line
;;;;       and a debug comment is not added.
(provide 'select-comment-by-lang)

(defun get--buffer-suffix ()
  "If there is a file extension, strip the '.' and return the extension."
  (car (last (split-string (buffer-name) "\\."))))

(defun insert-triplet ()
  "Insert comment-triplet appropriate to language in which we are writing."
  (interactive)
  (let ((char (gethash (get--buffer-suffix) *lang-suffixes*)))
      (when (char-or-string-p char) ; do we have a character to use?
        (save-excursion
          (set-mark (point))
          (activate-mark)
          (insert char ?\u000a char " " ?\u000a char)
          (indent-for-tab-command))
        (forward-line 1)        
        (end-of-line))))

(defun debug-comment (&optional begin end)
  "If a region is selected, insert a debug comment at the end of every line.  Only works if the entire
 line is part of the region.  If no region is active, insert a single debug comment at the end of the
 current line."
  (interactive "r")
  (let ((char (gethash (get--buffer-suffix) *lang-suffixes*)))
    (if (use-region-p)
      ;; Use the active region
      (let ((blob (buffer-substring begin end))
            (db-string (concat "\\1  " char " [DEBUG]\\2")))
        (setf blob (replace-regexp-in-string "\\(^.*[[:graph:]].*\\)\\(\n\\)" db-string blob))
        (save-excursion
          (delete-region begin end)
          (goto-char begin)
          (insert blob)))
      ;; Use the current line
      (progn
	(end-of-line)
	(insert "  " char " [DEBUG]")
	(forward-line 1)))))

;; Declare and fill the hash table.
(setf *lang-suffixes* (make-hash-table :test 'equal))
(puthash "py" "#" *lang-suffixes*)
(puthash "sh" "#" *lang-suffixes*)
(puthash "bash" "#" *lang-suffixes*)
(puthash "pl" "#" *lang-suffixes*)
(puthash "perl" "#" *lang-suffixes*)
(puthash "cpp" "//" *lang-suffixes*)
(puthash "hpp" "//" *lang-suffixes*)
(puthash "c" "//" *lang-suffixes*)
(puthash "h" "//" *lang-suffixes*)
(puthash "lisp" ";" *lang-suffixes*)
(puthash "el" ";" *lang-suffixes*)
(puthash "elisp" ";" *lang-suffixes*)
(puthash "emacs" ";" *lang-suffixes*)

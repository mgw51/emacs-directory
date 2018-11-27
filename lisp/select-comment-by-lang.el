;;; select-comment-by-lang.el --- Guess which comment character to use based on programming language.
;;;
;;; Commentary:
;;;
;;; This file selects the appropriate type of comment character to
;;; use based on the language extension of the buffer.  For instance,
;;; '#' will be used for bash-like languages, python, and perl.
;;;
;;; TODO: If there is no extension in the filename from which to choose
;;;       a language, fall back on the major mode of the buffer.
;;;
;;; TODO: Tweak debug-comment to work better with region selection.
;;;       Sometimes there is no newline char at the end of a line
;;;       and a debug comment is not added.
;;; Originally written in 8/2015
;;;
;;; Code:

(defun get--buffer-suffix ()
  "If there is a file extension, strip the '.' and return the extension."
  (car (last (split-string (buffer-name) "\\."))))

;;;###autoload
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


;;;###autoload
(defun debug-comment (begin end)
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


;;;###autoload
(defun remove-debug (&optional start end)
  "Remove debug comments from region START to END."
  (interactive
   (if (use-region-p) (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (push-mark)
  (save-excursion
    (save-restriction
      (when (not (use-region-p))
        (setf start (point-min)
              end (point-max)))
      (goto-char start)
      (narrow-to-region start end)
      (while (re-search-forward "^.*// \\[DEBUG\\].*\n" end t)
        (replace-match "")))))
      

;; Declare and fill the hash table.
(and (defvar *lang-suffixes* (make-hash-table :test 'equal)
       "This hash table maps common programming language file extensions with that languages' associated comment character(s).")
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
     (puthash "emacs" ";" *lang-suffixes*))

(provide 'select-comment-by-lang)
;;; select-comment-by-lang.el ends here

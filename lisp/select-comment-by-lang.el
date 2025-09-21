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

(defvar *lang-list* '((('sh-mode 'python-mode 'awk-mode 'perl-mode) . "#")
                      (('c++-mode 'c-mode 'go-mode 'v-mode) . "//")
                      (('sql-mode 'elm-mode) . "--")
                      (('emacs-lisp-mode 'lisp-mode) . ";"))
  "This is a list of assoc lists.  Each assoc list is a list of one or more
symbols associated with a string representing the comment character(s)
of the given language.")

;; Declare and fill the hash table.
(and (defvar *lang-comments* (make-hash-table :test 'equal)
       "This hash table maps common programming language file extensions with that language's associated comment character(s).")
     (puthash 'python-mode "#" *lang-comments*)
     (puthash 'sh-mode "#" *lang-comments*)
     (puthash 'perl-mode "#" *lang-comments*)
     (puthash 'c++-mode "//" *lang-comments*)
     (puthash 'c-mode "//" *lang-comments*)
     (puthash 'lisp-mode ";" *lang-comments*)
     (puthash 'emacs-lisp-mode ";" *lang-comments*)
     (puthash 'sql-mode "--" *lang-comments*)
     (puthash 'go-mode "//" *lang-comments*)
     (puthash 'awk-mode "#" *lang-comments*)
     (puthash 'v-mode "//" *lang-comments*)
     (puthash 'elm-mode "--" *lang-comments*))


(defun get--buffer-suffix ()
  "If there is a file extension, strip the '.' and return the extension."
  (car (last (split-string (buffer-name) "\\."))))


;;;###autoload
(defun mw-insert-triplet ()
  "Insert comment-triplet appropriate to language in which we are writing."
  (interactive)
  (let ((comment-str (gethash major-mode *lang-comments* "//")))
      (when (char-or-string-p comment-str) ; do we have a character to use?
        (save-excursion
          (set-mark (point))
          (activate-mark)
          (insert comment-str ?\u000a comment-str ?\u000a comment-str)
          (indent-for-tab-command))
        (forward-line 1)
        (end-of-line)
        (insert " "))))


;;;###autoload
(defun mw-debug-comment (&optional begin end)
  "Insert debug comment at end of line.
If a region is selected, insert a debug comment at the end of
every line within the region defined by BEGIN through END.  This
only works if the entire line is part of the region.  If no
region is active, insert a single debug comment at the end of the
current line."
  (interactive "r")
  (let ((char (gethash major-mode *lang-comments*)))
      (if (use-region-p)
          (save-mark-and-excursion
            ;; Use the active region
            (let ((blob (buffer-substring begin end))
                  (db-string (concat "\\1  " char " [DEBUG]\n")))
              (setf blob
                    (replace-regexp-in-string
                     (format "\\(^.*[[:graph:]]\\)\\( +%s \\[DEBUG]\\)?\\(\n\\)" char) db-string blob))
              (delete-region begin end)
              (goto-char begin)
              (insert blob)))
        ;; Use the current line
        (progn
          (save-mark-and-excursion
            (set-mark (point)) ; in case no mark has yet been set
            (end-of-line)
            (insert "  " char " [DEBUG]"))
          (forward-line 1)
          (back-to-indentation)))))


;;;###autoload
(defun mw-remove-debug (&optional start end)
  "Remove debug comments from region START to END.

If region is not active (or 'use-empty-active-region' is nil)
then operate on the whole file."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (push-mark)
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (while (re-search-forward "^.*// \\[DEBUG\\].*\n" end t)
        (replace-match "")))))


(provide 'select-comment-by-lang)
;;; select-comment-by-lang.el ends here

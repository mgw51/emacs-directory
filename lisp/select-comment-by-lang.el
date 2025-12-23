;;; -*- lexical-binding: t; -*-
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
(and (defvar *lang-comments* (make-hash-table :test 'eq) ; use 'eq because we are comparing symbols
       "This hash table maps common programming language file extensions with that language's associated comment character(s).")
     (puthash 'python-mode "#" *lang-comments*)
     (puthash 'sh-mode "#" *lang-comments*)
     (puthash 'bash-ts-mode "#" *lang-comments*)
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
  "Insert a debug comment at end of the current line of code.
If a region is selected, insert a vertically aligned debug comment at
the end of every line within the region defined by BEGIN through END.
This only works if the entire line is part of the region.  If no region
is active, insert a single debug comment at the end of the current line."
  (interactive "r")
  (let ((comment (gethash major-mode *lang-comments*)))
    (if (use-region-p)
        (save-mark-and-excursion
          (save-restriction
            ;; Use the active region
            (let ((db-string (format "\\&  %s [DEBUG]" comment)))
              (narrow-to-region begin end)
              (goto-char begin)
              (while (re-search-forward "^.*[[:graph:]]+.*$" nil t)
                (replace-match db-string))
              (align-regexp (point-min) (point-max)
                            (format "\\(\\s-*\\)%s \\[DEBUG\\]$" comment))))) ; see `align-regexp' for regexp requirements
      ;; Use the current line
      (progn
        (save-mark-and-excursion
          (set-mark (point)) ; in case no mark has yet been set
          (end-of-line)
          (insert (format "  %s [DEBUG]" comment)))
        (forward-line 1)
        (back-to-indentation)))))


;;;###autoload
(defun mw-remove-debug (&optional start end arg)
  "Remove debug comments from the selected region or the entire buffer.

This function searches for debug comments associated with the
current major mode within the specified region or the entire
buffer if no region is selected. It uses the comment style
defined in the `*lang-comments*` hash table for the current mode.

When called interactively:
- If a region is selected, it operates on that region.
- If no region is selected, it processes the entire buffer.

The optional prefix argument (ARG) modifies the behavior:
- If ARG is 4 or greater, it removes the debug portion but
  retains the preceding code lines.
- Otherwise, it removes both the code lines and the debug
  comments.

Usage:
- Use this function to clean up your code by eliminating debug 
  comments in a convenient manner."
  (interactive
   (let ((prefix (if current-prefix-arg
                     (prefix-numeric-value current-prefix-arg)
                   1)))
     (if (use-region-p)
         (list (region-beginning) (region-end) prefix)
       (list (point-min) (point-max) prefix))))
  (save-excursion
    (push-mark)
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (let* ((comment (gethash major-mode *lang-comments*))
             ;; non-greedy match up to the beginning of the comment portion
             (regexp (format "\\(^.*?\\) *%s \\[DEBUG\\] *\\(\n?\\)" comment))
             (fn (if (>= arg 4)
                     (lambda() (replace-match "\\1\\2"))
                   (lambda() (replace-match "")))))
        (while (re-search-forward regexp end t)
          (funcall fn))))))


(provide 'select-comment-by-lang)
;;; select-comment-by-lang.el ends here

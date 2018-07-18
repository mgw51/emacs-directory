;;;; my-work-utils.el  --- Useful functions
;;; Commentary:
;;;    Contains some utility and helper functions useful at work.
;;; Code:
;;;

(defun guid-clean (start end)
  "Replace colons (:) with hyphens (-), and set the region to upper case.
If a region is selected, operate on text between START and END."
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


(defun load-cpp-mode ()
    "Cause `c++-mode' to be enabled in the current buffer.
This is a work-around because helm does not appear to like enabling
c++-mode using the `helm-Mx' function.  The `c++-mode' option does
not appear in helm's completion list."
    (interactive)
    (execute-extended-command t "c++-mode"))


(require 'find-file)

(defun my-find-proper-mode()
  "Flycheck does not seem to be smart enough to detect when a header file
ending in '.h' is a c++ or c header file.   This function is a workaround
for this problem.  I found it on SO: `https://stackoverflow.com/a/1016389/1456187'."
  (interactive)
  ;; only run this on '.h' files
  (when (string= "h" (file-name-extension (buffer-file-name)))
    (save-window-excursion
      (save-excursion
        (let* ((alist (append auto-mode-alist nil))  ;; use whatever auto-mode-alist has
               (ff-ignore-include t)                 ;; operate on buffer name only
               (src (ff-other-file-name))            ;; find the src file corresponding to .h
               re mode)
          ;; Go through the a-list and find the mode associated with
          ;; the src file: that is the mode we want to use for the header.
          (while (and alist
                      (setf mode (cdar alist)
                            re (caar alist))
                      (not (string-match re src)))
            (setf alist (cdr alist)))
          (when mode (funcall mode)))))))


(defun my-find-file (prefix)
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


(defun reload-dir-locals ()
  "Reload dir-locals.el for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))


(defun insert-curly-braces (start end)
  "Enclose the region defined by START and END within curly braces.

The braces will be the sole glyph placed on their respctive lines
above and below the region.  If the region is not active, simply
open three lines, place the braces each on their own line and place
the cursor on the line between.  In both cases, indent according to
current `major-mode'."
  (interactive "*r")
  (push-mark)
  (if (region-active-p)
      (let ((text (buffer-substring start end)))
        (goto-char start)
        (delete-region start end)
        (insert "{\n" text "}\n")
        (indent-region start (point))
        (end-of-line 0))
    (let ((start (point))
          (end)
          (jump-location))
      (insert "{\n")
      (setq jump-location (point))
      (insert "\n}\n")
      (setq end (point))
      (goto-char jump-location)
      (indent-region start end)
      (funcall indent-line-function))))
  

(provide 'my-work-utils)
;;; my-work-utils.el ends here

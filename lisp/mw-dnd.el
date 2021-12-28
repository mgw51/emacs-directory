;;; mw-dnd.el --- Provide some Dungeons and Dragons helper functions.
;;;
;;; Commentary:
;;;    This file contains some helper functions to be used by the DM while running a
;;;    DnD session.

;;; Code:
(defun mw-dnd/make-battle-chart-from-region (start end)
  "Create a battle chart from the region defined by START to END.

A battle chart is composed from two space-separated columns and
as many rows as necessary which have been provided by the caller.
This function will insert two additional columns for each row,
both of which begin at zero.

Columns:
  1st - Character name (provided by caller)
  2nd - Starting HP    (provided by caller)
  3rd - Damage sustained during combat
  4th - Current HP

During each round of combat the DM should update total damage for
each player character and NPC.  The table formula will handle
calculating each character's current HP when updated."
  (interactive "*r")
  (message "\nCreate a table")  ; [DEBUG]
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (message text)  ; [DEBUG]
      (insert (with-temp-buffer
                (insert text)
                (goto-char (point-min))
                (insert "Char sHP Dam cHP\n")
                (message (buffer-string))  ; [DEBUG]
                (let ((curr-point (point)))
                  (if (not (char-equal (char-before (point-max)) ?\n))
                      (message "  Insert newline")  ; [DEBUG]
                      (let ((curr (point)))
                        (goto-char (point-max))
                        (insert "\n")
                        (goto-char curr)))
                  (mw-dnd/add--columns)
                  (message (buffer-string))  ; [DEBUG]
                  (org-table-convert-region (point-min) (point-max))
                  (message (buffer-string))  ; [DEBUG]
                  (goto-char curr-point))
                (org-table-insert-hline)
                (message (buffer-string))  ; [DEBUG]
                (goto-char (1- (point-max)))
                (org-table-insert-hline)
                (message (buffer-string))  ; [DEBUG]
                (goto-char (point-max))
                (insert "#+tblfm: $4=vsum($2 - $3)\n")
                (forward-line -1)
                (org-ctrl-c-ctrl-c)
                (buffer-string))))))


(defun mw-dnd/add--columns ()
  "Iteratively add two zero-filled columns to end of each row.

Search forward and add two columns of '0' to each successive line
 through the end of the selected region."
  (while (re-search-forward "\n" nil t)
    (replace-match " 0 0\n")))

(defun count-newlines ()
  "Alald."
  (let ((count 0))
    (while (not (eq (point) (point-max)))
      (forward-char)
      (if (char-equal (char-after) ?\n)
          (1+ count)))
    count))
  
  (provide 'mw-dnd)
;;; mw-dnd.el ends here

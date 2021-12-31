;;; mw-dnd.el --- Provide some Dungeons and Dragons helper functions.
;;;
;;; Commentary:
;;;    This file contains some helper functions to be used by the DM while running a
;;;    DnD session.

;;; Code:
(require 'org)

(defun mw-dnd/make-battle-chart-from-region (start end)
  "Create a battle chart from the region defined from START to END.

A battle chart is composed from two space-separated columns and
as many rows as necessary which have been provided by the caller.
This function will insert two additional columns for each row,
both of which begin at zero.

Columns:
  1st - Character name        (provided by caller)
  2nd - Creature's initiative (provided by caller)
  3rd - Starting HP           (provided by caller)
  4th - Damage sustained during combat
  5th - Current HP

During each round of combat the DM should update total damage for
each player character and NPC.  The table formula will handle
calculating each character's current HP when updated."
  (interactive "*r")
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (with-temp-buffer
                (insert text)
                (goto-char (point-min))
                (insert "Char Order sHP Dam cHP\n")
                (let ((curr-point (point)))
                  (if (not (char-equal (char-before (point-max)) ?\n))
                        (let ((curr (point)))
                          (goto-char (point-max))
                          (insert-char ?\n)
                          (goto-char curr)))
                  (mw-dnd/add--columns)
                  (org-table-convert-region (point-min) (point-max))
                  (goto-char curr-point))
                (org-table-insert-hline)
                (goto-char (1- (point-max)))
                (org-table-insert-hline)
                (org-table-goto-column 2)
                (org-table-sort-lines nil ?N) ; reverse numeric sort
                (goto-char (point-max))
                (insert "#+tblfm: $5=vsum($3 - $4)\n")
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
      (if (char-equal (char-after) ?\n)
          (setq count (1+ count)))
      (forward-char))
    count))
  
  (provide 'mw-dnd)
;;; mw-dnd.el ends here

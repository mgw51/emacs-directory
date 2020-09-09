;;; perl-related.el --- Elisp functions related to perl buffers.

;;; Commentary:
;;;    Provide some useful helpers.

;;; Code:

;;;###autoload
(defun mw-perl-setup(prefix)
  "Add a shebang and some `use' statements to the top of a perl script.
If the PREFIX key is used, use diagnostics instead of warnings at the top of the script."
  (interactive "p")
  (insert "#!/usr/bin/perl\n\n"
          "use strict;\n"
          (if (eq prefix 1)
              "use warnings;\n"
            "use diagnostics;\n")
          "use v5.24;\n\n")
  (save-buffer)
  (set-file-modes (buffer-file-name) #o744))


(provide 'mw-perl-related)
;;; mw-perl-related.el ends here

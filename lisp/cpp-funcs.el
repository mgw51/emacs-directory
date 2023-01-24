;;;; cpp-funcs.el --- Provide some functions useful when programming in C++ and C
;;;; Commentary:
;;;;   Some useful functions, mostly for C++, although the include-guard func
;;;;   is also great for C.
;;;; Code:


;;;###autoload
(defun mw-func-header ()
  "This function prints a standard header for a c++ function.

The header consists of three comment lines.  First line contains
a row of stars (no wider than 100th column), second line is for
the function name, third line is empty."
  (interactive)
  (indent-according-to-mode)
  (let ((stars (make-string (- 97 (current-column)) ?\*))
        (r-begin nil)
        (r-end nil)
        (exit-point nil))
    (save-excursion
      (setq r-begin (point))
      (insert "// " stars "\n//")
      (setq exit-point (point))
      (insert "\n//")
      (setq r-end (point))
      (setq mark-active t)
      (indent-region r-begin r-end)
      (setq mark-active nil))
    (goto-char exit-point)
    (end-of-line)
    (insert " ")))


;;;###autoload
(defun mw-include-guard ()
  "Generate include guards for a c or cpp header file."
  (interactive)
  (save-excursion
    (let ((header-name (split-string (buffer-name) "\\.")))
      (when (string-match "^\\(h\\|hpp\\)$" (cadr header-name))
        (let ((guard-name (concat "_" (upcase (car header-name))
			          "_" (upcase (car (last header-name)))
			          "_" (substring (secure-hash 'sha1 (number-to-string (float-time))) 0 8)
			          "_")))
          (goto-char (point-min))
          (insert "#ifndef " guard-name "\u000a#define " guard-name "\u000a\u000a")
          (goto-char (point-max))
          (if (looking-back "\u000a\u000a" (- 2 (point)))
              (insert "#endif  // " guard-name)
            (insert "\u000a\u000a#endif // " guard-name)))))))


;;;###autoload
(defun mw-create-basic-makefile (target lang)
  "Create a basic Makefile and use the most common flags.
TARGET is the binary output name, LANG is the programming language used,
since the source files can be either C or C++."
  (interactive "sExecutable name: \nsLanguage (c, c++, or cpp): ")
  (if (not (or
            (string= (downcase lang) "c")
            (string= (downcase lang) "c++")
            (string= (downcase lang) "cpp")))
      (message "Unrecognized language: %s" lang)
    (save-excursion
      (with-temp-file "Makefile"
        (let ((compiler)
              (compiler-flags)
              (file-suff))
          (if (string= (downcase lang) "c")
              (progn
                (setq compiler "$(CC)")
                (setq compiler-flags "$(CFLAGS)")
                (setq file-suff ".c")
                (insert "CC = gcc\n"
                        "CFLAGS = -Wall -c -g\n"))
            (progn
              (setq compiler "$(CXX)")
              (setq compiler-flags "$(CXXFLAGS)")
              (setq file-suff ".cpp")
              (insert "CXX = g++ -std=c++17\n"
                      "CXXFLAGS = -Wall -Wextra -pedantic -c -g\n")))
          (insert "LDFLAGS = \n\n"

                  "target = " target "\n\n"
                  
                  ".PHONY: all\n"
                  "all: $(target)\n\n"
                  
                  ".PHONY: clean\n"
                  "clean:\n"
                  "\t$(RM) *.o $(target)\n\n"
                  
                  "$(target): *.o\n"
                  "\t" compiler " -o$@ $^ $(LDFLAGS)\n\n"
                  
                  "%.o: %" file-suff "\n"
                  "\t" compiler " " compiler-flags " $^\n"))))))

(provide 'cpp-funcs)
;;; cpp-funcs.el ends here

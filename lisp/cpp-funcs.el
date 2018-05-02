;;;; cpp-funcs.el --- Provide some functions useful when programming in C++ and C
;;;; Commentary:
;;;;   Some useful functions, mostly for cpp, although the include-guard func
;;;;   is also great for c.
;;;; Code:

(defun func-header ()
  "This function prints a standard header for a c++ function.
The header consists of: three comment lines; first line contains a line of
stars, second line is function name, third line is empty."
  (interactive)
  (save-excursion
    (set-mark (point))
    (activate-mark)
    (indent-according-to-mode)
    (insert "// " (make-string 100 ?*) ?\u000a "// Name:" (make-string 12 ? ) ?\u000a "//")
    (indent-for-tab-command))
  (forward-line 1)
  (end-of-line))


(defun include-guard ()
  "Generate include guards for a c or cpp header file."
  (interactive)
  (defvar header-name)
  (setf header-name (split-string (buffer-name) "\\."))
  (when (string-match "^\\(h\\|hpp\\)$" (cadr header-name))
    (defvar guard-name)
    (setf guard-name (concat "_" (upcase (car header-name))
			     "_" (upcase (car (last header-name)))
			     "_" (substring (secure-hash 'sha1 (number-to-string (float-time))) 0 8)
			     "_"))
    (goto-char (point-min))
    (insert "#ifndef " guard-name "\u000a#define " guard-name "\u000a")
    (goto-char (point-max))
    (insert "#endif  // " guard-name)))


(defun get-class-name ()
  "Extract the class name from the filename.
Takes everything before the file extension and uses that as the class name."
  (interactive)
  (indent-according-to-mode)
  (insert (car (split-string (buffer-name) "\\."))))

(defun create-basic-makefile (target lang)
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

                  "SRC=$(wildcard *" file-suff ")\n"
                  "OBJ=$(SRC:" file-suff "=.o)\n"
                  "target = " target "\n\n"
                  
                  ".PHONY: all\n"
                  "all: $(target)\n\n"
                  
                  ".PHONY: clean\n"
                  "clean:\n"
                  "\t$(RM) $(OBJ) $(target)\n\n"
                  
                  "$(target): $(OBJ)\n"
                  "\t" compiler " -o$@ $< $(LDFLAGS)\n\n"
                  
                  "%.o: $(SRC)\n"
                  "\t" compiler " " compiler-flags " $<\n"))))))

(provide 'cpp-funcs)
;;; cpp-funcs.el ends here

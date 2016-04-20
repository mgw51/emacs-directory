;;;; cpp-funcs.el
;;;; Written: 8/2015
;;;; By:      Matt
;;;;
;;;; Some useful functions, mostly for cpp, although the include-guard func
;;;; is also great for c.
;;;;

(provide 'cpp-funcs)

(defun func-header ()
  "this function prints a standard header for a c++ function,
   which consists of: a triplet, first line contains a line of
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
    (setf guard-name (concat "_" (upcase (first header-name))
			     "_" (upcase (car (last header-name)))
			     "_" (substring (secure-hash 'sha1 (number-to-string (float-time))) 0 8)
			     "_"))
    (goto-char (point-min))
    (insert "#ifndef " guard-name "\u000a#define " guard-name "\u000a")
    (goto-char (point-max))
    (insert "#endif  // " guard-name)))


(defun get-class-name ()
  "Extract the class name from the filename.  Takes everything before the file extension and uses
 that as the class name."
  (interactive)
  (indent-according-to-mode)
  (insert (first (split-string (buffer-name) "\\."))))


(defun create-basic-makefile (target language)
  "Create a basic Makefile template which uses most common flags.  This is geared toward gcc 4.6"
  (interactive
   "sMakefile target: 
sLanguage (C or CPP): ")  ; 's' prefix causes the input to be bound to function args
  (defvar compiler)
  (defvar compiler-flags)
  (defvar file-suffix)
  (let ((buf (generate-new-buffer "Makefile")))
    (with-current-buffer buf
      (funcall #'makefile-mode)
      (if (string= (downcase language) "c")
          (progn
            (setq compiler "$(CC)")
            (setq compiler-flags "$(CFLAGS)")
            (setq file-suffix ".c")
            (insert "CC = gcc\n"
                    "CFLAGS = -Wall -c -g\n"))
        (progn
          (setq compiler "$(CXX)")
          (setq compiler-flags "$(CXXFLAGS)")
          (setq file-suffix ".cpp")
          (insert "CXX = g++ -std=c++0x\n"
                  "CXXFLAGS = -Wall -Wextra -pedantic -c -g\n")))
      (insert "LDFLAGS = \n"          
              "target = " target "\n\n"

              "all: $(target)\n\n"

              ".PHONY: clean\n"
              "clean:\n"
              "\t$(RM) *.o $(target)\n"

              "\n$(target): *.o\n"
              "\t" compiler " -o$@ $^ $(LDFLAGS)"

              "\n\n%.o: %" file-suffix "\n"
              "\t" compiler " " compiler-flags " $<"))
    (write-file "Makefile")))

;;;; file:     doxygen.el
;;;; author:   matt wood
;;;; desc:     Provide some basic commands for use with doxygen.
;;;;
;;;; TODO:
;;;;     1.  Detect language of current buffer at major mode load time? (C++, C, D, Java, etc)
;;;;     

(provide 'doxygen)

;;; Global variables  ! These don't appear to work:  backslashes are interpretted, not printed, and multiple backslashes don't work !
;;;
(defvar *function* (concat
                    "// ****************************************************************************************************\n"
                    "//  Name:    \n"
                    "//\n"
                    "/// \brief   \n"
                    "///\n"
                    "///          \n"
                    "///\n"
                    "/// \\\\\pre     \n"
                    "///\n"
                    "/// \\\\\post    \n"
                    "///\n"
                    "/// \\\\\return  \n"
                    "///\n")
  "Describes the function documentation template.")

(defvar *class* (concat
                 "// ****************************************************************************************************\n"
                 "/// \\class   \n"
                 "///\n"
                 "/// \\brief   \n"
                 "///\n"
                 "///          \n"
                 "///\n")
  "Describes the class documentation template.")

(defvar *struct* (concat
                  "// ****************************************************************************************************\n"
                  "/// \\struct  \n"
                  "///\n"
                  "/// \\brief   \n"
                  "///\n"
                  "///          \n"
                  "///\n")
  "Describes the struct documentation template.")
                    
;;; Functions
;;;
(defun doxygen-function (&optional number-args)
  "Insert doxygen function documentation template at point."
  (interactive "*P")  ; `*' for error on readonly buffer
  (if (null number-args)
      (setq number-args 0)  ; provice a default value if an argument was not passed
    (if (consp number-args)
        (setq number-args (car number-args))))
  (beginning-of-line)
  (save-excursion
    (let ((start (point))
          end)
      (insert (concat
               "// ****************************************************************************************************\n"
               "//  Name:    \n"
               "//\n"
               "/// \\brief   \n"
               "///\n"
               "///           \n"
               "///\n"
               (build--arg-string number-args)
               "/// \\pre     \n"
               "///\n"
               "/// \\post    \n"
               "///\n"
               "/// \\return  \n"
               "///\n"))
      (setq end (point))
      (indent-region start end)))
  (forward-line)
  (end-of-line))

(defun build--arg-string (num)
  (when (and (not (null num))
         (> num 0))
    (concat
     (build-string num "/// \\arg     \n")
     "///\n")))

(defun build-string (num arg)
  "Build a string that repeats the ARGument NUMber times."
  (if (eq num 0)
      nil
    (concat
     arg
     (build-string (1- num) arg))))

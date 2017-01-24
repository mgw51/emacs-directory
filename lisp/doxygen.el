;;;; File: doxygen.el
;;;; matt wood
;;;;
;;;; Implements some very basic helper functions used to generate
;;;; doxygen markup.

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
  (interactive "*")  ; `*' for error on readonly buffer
  (save-excursion
    (beginning-of-line)
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
               "/// \\arg     \n"
               "///\n"
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

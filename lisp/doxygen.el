;;;; file:     doxygen.el
;;;; author:   matt wood
;;;; desc:     Provide some basic commands for use with doxygen.
;;;;
;;;; TODO:
;;;;     1.  Detect language of current buffer at major mode load time? (C++, C, D, Java, etc)
;;;;     


;;; Code:
;;; "Public" Functions
;;;
(defun doxygen-function-template (&optional number-args)
  "Insert doxygen function documentation template at point.
If NUMBER-ARGS is specified, insert that number of param fields into the template."
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
               "/// @name    \n"
               "///\n"
               "/// @brief   \n"
               "///\n"
               "///          \n"
               "///\n"
               (build--arg-string number-args)
               "/// @pre     \n"
               "///\n"
               "/// @post    \n"
               "///\n"
               "/// @return  \n"
               "///\n"))
      (setq end (point))
      (indent-region start end)))
  (forward-line)
  (end-of-line))

(defun doxygen-class-template ()
  "Insert doxygen class documentatoin template at point."
  (interactive "*")
  (beginning-of-line)
  (save-excursion
    (let ((start (point))
          end)
      (insert (concat
               "// ****************************************************************************************************\n"
               "/// @class    \n"
               "///\n"
               "/// @brief    \n"
               "///\n"
               "///           \n"
               "///\n"))
      (setq end (point))
      (indent-region start end)))
  (forward-line)
  (end-of-line))

(defun doxygen-struct-template ()
  "Insert doxygen struct documentation template at point."
  (interactive "*")
  (beginning-of-line)
  (save-excursion
    (let ((start (point))
          end)
      (insert (concat
               "// ****************************************************************************************************\n"
               "/// @struct   \n"
               "///\n"
               "/// @brief    \n"
               "///\n"
               "///           \n"
               "///\n"))
      (setq end (point))
      (indent-region start end)))
  (forward-line)
  (end-of-line))

(defun doxygen-create-group (start end)
  ""
  (interactive "*r")
  (save-excursion
    (goto-char start)
    (back-to-indentation)
    (insert "///@{")
    (newline-and-indent)
    (goto-char end)
    (end-of-line)
    (insert "\n///@}")
    (c-indent-line-or-region)))

;;; Helper functions
;;;
(defun build-string (num arg)
  "Build a string that repeats the ARGument NUMber times."
  (if (eq num 0)
      nil
    (concat
     arg
     (build-string (1- num) arg))))

(defun build--arg-string (num)
  "Build a string containing NUM number of doxygen 'param' fields."
  (when (and (not (null num))
         (> num 0))
    (concat
     (build-string num "/// @param   \n")
     "///\n")))

(provide 'doxygen)
;;; doxygen.el ends here

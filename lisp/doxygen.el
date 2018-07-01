;;; doxygen.el --- Provide some basic commands for use with doxygen.
;;;
;;; Commentary:
;;;
;;; TODO:
;;;     1.  Detect language of current buffer at major mode load time? (C++, C, D, Java, etc)
;;;     2.  Write function to convert Sensaphone function docs to Doxygen docs.
;;;
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
    (end-of-line 2)
    (electric-newline-and-maybe-indent)
    (insert "///@}")
    (setf end (point))
    (c-indent-line-or-region)))


(defun doxygen-forward-block ()
  "Jump forward by one doxygen comment block."
  (interactive)
  (push-mark)
  )


(defun doxygen-backward-block ()
  "Jump backward by one doxygen comment block."
  (interactive)
  (push-mark)
  )


;;; Helper functions
;;;
(defun build-string (arg num)
  "Build a string that repeats ARG NUM times."
  (if (eq num 0)
      nil
    (concat
     arg
     (build-string arg (1- num)))))


(defun build--arg-string (num)
  "Build a string containing NUM number of doxygen 'param' fields."
  (when (and (not (null num))
             (> num 0))
    (concat
     (build-string "/// @param   \n" num)
     "///\n")))


;;;###autoload
(define-minor-mode doxygen-mode
  "Toggle doxygen minor mode.
Doxygen minor mode provides convenience functions for several common
documentation tasks such as creating function templates, class and struct
templates.

To view keybindings for `doxygen-mode', view help for the mode, `C-h m'.
Or type the prefix keys and summon help: `C-c C-d C-h'"
  :init-value nil
  :lighter " doxy"
  :keymap
  (let ((doxy-map (make-sparse-keymap)))
    (define-key doxy-map (kbd "C-c C-d") nil)  ; prefix
    (define-key doxy-map (kbd "C-c C-d f") #'doxygen-function-template)
    (define-key doxy-map (kbd "C-c C-d s") #'doxygen-struct-template)
    (define-key doxy-map (kbd "C-c C-d c") #'doxygen-class-template)
    (define-key doxy-map (kbd "C-c C-d g") #'doxygen-create-group)
    (define-key doxy-map (kbd "C-c C-d n") #'doxygen-forward-block)
    (define-key doxy-map (kbd "C-c C-d p") #'doxygen-backward-block)
    doxy-map))


;;;###autoload
(add-hook 'c-mode-common-hook 'doxygen-mode)


(provide 'doxygen-mode)
;;; doxygen.el ends here

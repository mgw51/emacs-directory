;;; doxygen.el --- Provide some basic commands for use with doxygen.
;;;
;;; Commentary:
;;;
;;; TODO:
;;;     1.  Write function to convert Sensaphone function docs to Doxygen docs.
;;;
;;; Code:
;;; "Public" Functions
;;;

;;;###autoload
(defun doxygen-function-template (&optional number-args)
  "Insert doxygen function documentation template at point.
If NUMBER-ARGS is specified, insert that number of param fields into the templatne."
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
  (end-of-line))


;;;###autoload
(defun doxygen-class-template ()
  "Insert doxygen class documentation template at point."
  (interactive "*")
  (beginning-of-line)
  (save-excursion
    (let ((start (point))
          end)
      (insert (concat
               "/// @brief   \n"
               "///\n"
               "///          \n"
               "///\n"))
      (setq end (point))
      (indent-region start end)))
  (end-of-line))


;;;###autoload
(defun doxygen-create-group (start end)
  "Create a doxygen group prefaced with 'name' and 'brief' tags.

When invoked on a region defined by START and END, wrap the
 region in the group tag.  If the region is not active, simply
 open a group at point."
  (interactive "*r")
  (let ((insertion-point)
        (new-start)
        (new-end))
    (cl-destructuring-bind (insertion-point new-start new-end)
        (save-excursion
          (if (use-region-p)
              (save-restriction
                (narrow-to-region start end)
                (doxygen--group-text-insert (delete-and-extract-region start end)))
            (doxygen--group-text-insert)))
      (goto-char insertion-point)
      (indent-region new-start new-end))))


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


;;;###autoload
(defun doxygen-run-doxygen ()
  "Run doxygen using projectile root if active, otherwise using Doxyfile as the dominant file to search for."
  (interactive)
  (if (boundp 'projectile-mode)
      (shell-command (concat "doxygen " (projectile-expand-root "Doxyfile")))
    (let ((file-path (locate-dominating-file "." "Doxyfile")))
      (when (not (null file-path))
        (shell-command (concat "doxygen " file-path "Doxyfile"))))))


;;; "Private" functions
;;;
(defun doxygen--group-text-insert (&optional text-string)
  "Insert group text at point.

If TEXT-STRING is non-nil, insert that between opening and
closing group characters."
  (let ((start (point)))
    (insert "/// @name   ")
    (let ((insertion-point (point)))
      (insert "\n///\n/// @brief  \n///\n///@{\n")
      (if text-string
          (insert text-string)
        (insert "\n"))
      (insert "///@}")
      (list insertion-point start (point)))))


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


;;; Font lock code found at:
;;;   https://emacs.stackexchange.com/questions/31392/syntax-highlight-doxygen-comments-in-c-c-c-doc-comment-style/36637#36637
;;;
(require 'cc-mode)


(defface doxygen-verbatim-face
  '((default :inherit default))
  "Face used to show Doxygen block regions"
  :group 'font-lock-faces)


(defface doxygen-match-face
  '((default :inherit default)
    (t :underline t))
  "Face used to show Doxygen region start end commands"
  :group 'font-lock-faces)


(defconst custom-font-lock-doc-comments
  `(
    ;; Highlight Doxygen special commands,
    ;;   \cmd or @cmd
    ;; and the non [a-z]+ commands
    ;;   \\ \@ \& \# \< \> \% \" \. \| \-- \--- \~[LanguageId]
    (,(concat
       "\\(?:"
       "[\\@][a-z]+"     ;; typical word Doxygen special @cmd or \cmd
       "\\|"
       ;; non-word commands, e.g. \\ or @\
       "[\\@]\\(?:\\\\\\|@\\|&\\|#\\|<\\|>\\|%\\|\"\\|\\.\\|::\\||\\|---?\\|~[a-z]*\\)"
       "\\)")
     0 ,c-doc-markup-face-name prepend nil)
    ;; Highlight autolinks. These are referring to functions, so we use a different font face
    ;; from the Doxygen special commands.
    (,(concat
       "\\(?:"
       ;; function() or function(int, std::string&, void*) or more complex where we only
       ;; match the first paren, function(x->(), 2*(y+z)).
       "[A-Za-z_0-9]+(\\([A-Za-z_0-9:&*, ]*)\\)?"
       ;; ClassName::memberFcn or the destructor ClassName::~ClassName. Can also do unqualified
       ;; references, e.g. ::member. The parens are optional, ::member(int, int), ::member(a, b).
       ;; We only require matching of first paren to make cases like ::member(x->(), 2*(y+z))
       ;; work. We don't want \::thing to be highlighed as a function, hence reason to look for
       ;; class::member or space before ::member.  Note '#' can be used instead of '::'
       "\\|"
       "\\(?:[A-Za-z_0-9]+\\|\\s-\\)\\(?:::\\|#\\)~?[A-Za-z_0-9]+(?\\(?:[A-Za-z_0-9:&*, \t]*)\\)?"
       ;; file.cpp, foo/file.cpp, etc. Don't want to pickup "e.g." or foo.txt because
       ;; these are not autolinked so look for common C++ extensions.
       "\\|"
       "[A-Za-z_0-9/]+\\.\\(?:cpp\\|cxx\\|cc\\|c\\|hpp\\|hxx\\|hh\\|h\\)"
       "\\)")
     0 font-lock-function-name-face prepend nil)
    ;; Highlight URLs, e.g. http://doxygen.nl/autolink.html note we do this
    ;; after autolinks highlighting (we don't want nl/autolink.h to be file color).
    ("https?://[^[:space:][:cntrl:]]+"
     0 font-lock-keyword-face prepend nil)
    ;; Highlight HTML tags - these are processed by Doxygen, e.g. <b> ... </b>
    (,(concat "</?\\sw"
                "\\("
                (concat "\\sw\\|\\s \\|[=\n\r*.:]\\|"
                        "\"[^\"]*\"\\|'[^']*'")
                "\\)*>")
     0 ,c-doc-markup-face-name prepend nil)
    ;; E-mails, e.g. first.last@domain.com. We don't want @domain to be picked up as a Doxygen
    ;; special command, thus explicitly look for e-mails and given them a different face than the
    ;; Doxygen special commands.
    ("[A-Za-z0-9.]+@[A-Za-z0-9_]+\\.[A-Za-z0-9_.]+"
     0 font-lock-keyword-face prepend nil)
    ;; Quotes: Doxygen special commands, etc. can't be in strings when on same line, e.g.
    ;; "foo @b bar line2 @todo foobar" will not bold or create todo's.
    ("\"[^\"[:cntrl:]]+\""
     0 ,c-doc-face-name prepend nil)

    ("[^\\@]\\([\\@]f.+?[\\@]f\\$\\)"  ;; single line formula but an escaped formula, e.g. \\f[
     1 'doxygen-verbatim-face prepend nil)

    ;; Doxygen verbatim/code/formula blocks should be shown using doxygen-verbatim-face, but
    ;; we can't do that easily, so for now flag the block start/ends
    (,(concat
       "[^\\@]"  ;; @@code shouldn't be matched
       "\\([\\@]\\(?:verbatim\\|endverbatim\\|code\\|endcode\\|f{\\|f\\[\\|f}\\|f]\\)\\)")
     1 'doxygen-match-face prepend nil)

    ;; Here's an attempt to get blocks shown using doxygen-verbatim-face. However, font-lock doesn't
    ;; support multi-line font-locking by default and I'm not sure the best way to make these work.
    ;;
    ;; Doxygen special commands, etc. can't be in verbatim/code blocks
    ;;   @verbatim
    ;;      @cmd  -> not a Doxygen special command
    ;;   @endverbatim
    ;; so set verbatim/code to a different font.  Verbatim/code blocks spans multiple lines and thus
    ;; a refresh of a buffer after editing a verbatim/code block may be required to have the font
    ;; updated.
    ;;("[^\\@][\\@]\\(verbatim\\|code\\)\\([[:ascii:][:nonascii:]]+?\\)[\\@]end\\1"
    ;; 2 'doxygen-verbatim-face prepend nil)
    ;; Doxygen formulas are link verbatim blocks, but contain LaTeX, e.g.
    ;;("[^\\@][\\@]f.+[\\@f]\\$"  ;; single line formula
    ;; 0 'doxygen-verbatim-face prepend nil)
    ;; multi-line formula,
    ;;   \f[ ... \f]     or    \f{ ... \}
    ;;("[^\\@][\\@]f\\(?:{\\|\\[\\)\\([[:ascii:][:nonascii:]]+?\\)[\\@]f\\(?:}\\|\\]\\)"
    ;; 1 'doxygen-verbatim-face prepend nil)

    ))


;; Matches across multiple lines:
;;   /** doxy comments */
;;   /*! doxy comments */
;;   /// doxy comments
;; Doesn't match:
;;   /*******/
(defconst custom-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "/\\(//\\|\\*[\\*!][^\\*!]\\)"
            limit custom-font-lock-doc-comments)))))


(setq-default c-doc-comment-style (quote (custom)))


;;; Found suggestion for this code at:
;;;   https://emacs.stackexchange.com/questions/31392/syntax-highlight-doxygen-comments-in-c-c-c-doc-comment-style/36637#36637
;;; Followed suggestion in the post and looked at `CWarn' minor mode.  I adapted this toggling code from theirs.
(defun doxygen-toggle-font-lock-keywords (addp)
  "Install/remove keywords into/from current buffer.
If ADDP is non-nil, install; else remove."
  (funcall (if addp 'font-lock-add-keywords 'font-lock-remove-keywords) nil custom-font-lock-keywords))


(define-minor-mode doxygen-mode
  "Toggle doxygen minor mode.
Doxygen minor mode provides convenience functions for several common
documentation tasks such as creating function templates, class and struct
templates.

For an easy list of available keybindings type the prefix keys and summon
help: `C-c w C-h'

Keybindings for `doxygen-mode':

\\{doxygen-mode-map}"
  :init-value nil
  :lighter " doxy"
  :keymap
  (let ((doxy-map (make-sparse-keymap)))
    (define-key doxy-map (kbd "C-c w") nil)
    (define-key doxy-map (kbd "C-c w f") #'doxygen-function-template)
    (define-key doxy-map (kbd "C-c w c") #'doxygen-class-template)
    (define-key doxy-map (kbd "C-c w g") #'doxygen-create-group)
    (define-key doxy-map (kbd "C-c w n") #'doxygen-forward-block)
    (define-key doxy-map (kbd "C-c w p") #'doxygen-backward-block)
    doxy-map)

  (doxygen-toggle-font-lock-keywords doxygen-mode)
  (font-lock-flush))


;;;###autoload
(add-hook 'c-mode-common-hook 'doxygen-mode)


(provide 'doxygen-mode)
;;; doxygen-mode.el ends here

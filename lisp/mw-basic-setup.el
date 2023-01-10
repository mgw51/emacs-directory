;;; mw-basic-setup.el --- Summary: Provide setup helper functions to be called from init.el.
;;;
;;; Commentary:
;;; Use this file to house some functions that would
;;; otherwise clutter up the init file which makes it harder to read.

;;; Code:

(defun basic/set-and-load-custom-config-file()
  "Use a separate config file for package-generated configuration.

Some packages will add configuration data to the main init file,
`init.el' or `.emacs'.  Instead, reserve the main init file for
manual config only, and relegate all other config changes to
`init-custom.el'"

  (setf custom-file (expand-file-name "lisp/init-custom.el"))
  (unless (file-exists-p custom-file)
    (with-temp-file custom-file
      (insert ";;; init-custom.el --- Dedicated file into which Emacs packages may freely write configuration data.\n"
              ";;;\n"
              ";;; Commentary:\n"
              ";;;   Provide this file to prevent packages from writing their config data into my `init.el' file.\n"
              ";;;   This file was automatically generated from my `init.el' file.\n"
              ";;;\n"
              ";;; Code:\n"
              ";;;\n\n\n"

              ";;; init-custom.el ends here.\n")))
  (load custom-file))


(defun basic/setup-package-el(archives)
  "Setup package.el and assign ARCHIVES."
  (require 'package)
  (setf package-archives archives))
    

(defun basic/use-package-setup(&optional settings)
  "Setup `use-package'.

That may mean downloading it from the package repository, if
necessary."
  (eval-when-compile
    (when (not (package-installed-p 'use-package))
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package))
  (dolist (pair settings)
    (set (car pair) (cdr pair))))

(defun basic/ui-setup()
  "This function provides a base-line UI configuration.

It does things like turn off the tool bar and scroll bars, enable line numbers, etc."
  ;;; off
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (tooltip-mode -1)

  ;;; on
  (global-display-line-numbers-mode 1)
  (winner-mode 1)
  (setq visible-bell t))


(defun basic/quality-of-life()
  "Set up some of my emacs-wide preferences."

  ;; Narrowing
  (put 'narrow-to-defun 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  ;; turn off tabs
  (setq-default indent-tabs-mode nil)

  ;; various things
  (setq make-backup-files nil
	backup-directory-alist nil
	inhibit-splash-screen t
	save-abbrevs 'silent))
;;       abbrev-file-name (expand-file-name "abbrev_defs")


(provide 'mw-basic-setup)
;;; mw-basic-setup.el ends here

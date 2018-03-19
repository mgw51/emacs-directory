
;;; init.el -- emacs init file
;;; Commentary:
;;;     This file has been automatically generated from `init.org'
;;;     Any changes made to this file will be eliminated the next
;;;     time the org file is 'tangled'.  To make permanent changes,
;;;     edit the org file directly.

;;; Code:

;;; A few global settings.
(setf make-backup-files nil
      backup-directory-alist nil
      inhibit-splash-screen t
      visible-bell t)

(when (not (display-graphic-p))
    (setf linum-format "%d "))

(setq-default indent-tabs-mode nil)

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; Initialize package archives; ensure all required packages are installed;
;;; finally, load them all.
(require 'package)     ; Pull in package.el
(package-initialize)   ; Initialize it
(setf package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(require 'ensure-packages-installed)  ; custom function that installs missing packages listed below
(let ((package-list '(iy-go-to-char
                      helm
                      python-mode
                      key-chord
                      yasnippet
                      flycheck
                      magit
                      abyss-theme
                      dockerfile-mode
                      docker-tramp
                      yaml-mode)))
  (ensure-packages-installed package-list)
  ;;; Now load each package, plus some custom packages.
  (append package-list
          helm-config             ; required for helm package
          select-comment-by-lang  ; see lisp directory
          cpp-funcs               ; ""
          doxygen                 ; ""
          my-work-utils)          ; ""
  (dolist (cool-thing package-list
                      t)
    (funcall 'require cool-thing)))

;;; Set default lisp impelentation, and load the `slime-fancy' package.
(if (not (null (locate-file "sbcl" "/usr/bin")))
    (setf inferior-lisp-program "/usr/bin/sbcl"))
(if (package-installed-p 'slime)
    (setf slime-contribs '(slime-fancy))

;;; Turn ON some UI elements
(dolist (mode '(global-linum-mode     ; display line numbers in margin
                column-number-mode))  ; display line and column number in status bar
  (funcall mode 1))

;;; Turn OFF some UI elements
(dolist (mode '(tool-bar-mode
                menu-bar-mode
                horizontal-scroll-bar-mode
                tooltip-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; Enable some commands which are disabled by default
(put 'narrow-to-defun  'disabled nil)  ;
(put 'narrow-to-page   'disabled nil)  ; Narrowing
(put 'narrow-to-region 'disabled nil)  ;

(create-sql-buffer)

(and
 (global-set-key (kbd "M-x") #'helm-M-x)
 (global-set-key (kbd "C-x C-f") #'helm-find-files)
 (helm-mode 1))  ; Start helm automatically

;;; `key-chord-define-local' function is used in mode hooks below.  These
;;; are global definitions here.
(and
 (key-chord-mode 1)
 (key-chord-define-global "fj" #'iy-go-up-to-char)
 (key-chord-define-global "fk" #'iy-go-to-char-backward))

(and
 (fset 'sort-buffer-by-name  ; Create function cell and assign it to key chord
       "\M-2\M-x Buffer-menu-sort")
 (global-set-key (kbd "C-c 2") #'sort-buffer-by-name)) ; sort buffer by name
(global-set-key (kbd "<f1>") #'shell-command)          ; shell command
(global-set-key (kbd "<select>") #'move-end-of-line)   ; <end> -> end of line
(global-set-key (kbd "C-c C-g") #'magit-status)        ; Invoke magit-status screen, from which all magit commands are available
(global-set-key (kbd "C-x C-b") #'ibuffer)  ; Use ibuffer instead of default buffer list

(add-hook 'c-mode-common-hook #'c-style-lang-hook-func)
(add-hook 'c++-mode-hook #'cpp-hook-func)
(add-hook 'python-mode-hook #'python-hook-func)
(add-hook 'emacs-lisp-mode-hook #'lisp-settings)
(add-hook 'lisp-mode-hook #'lisp-settings)
(add-hook 'sh-mode-hook #'bash-hook-func)
(add-hook 'projectile-mode-hook #'projectile-hook-func)
(add-hook 'text-mode-hook #'text-hook-func)
(add-hook 'org-mode-hook #'org-hook-func)
(add-hook 'after-init-hook #'global-flycheck-mode)

(defun c-style-lang-hook-func ()
  "Run these commands for all c-like languages."
  (setq-default c-basic-offset 2)
  (superword-mode -1)  ; treat underscore-separated words as a single word
  (subword-mode t)     ; treat camelCase words as separate words
  (key-chord-define-local "pq" "{\n\n}\C-p\t")
  (c-set-offset 'case-label '+) ; indent case statements in a switch block
  (show-paren-mode t)
  (which-function-mode)
  (yas-reload-all)
  (yas-minor-mode)
  (flycheck-mode)
  (local-set-key (kbd "C-c o") #'ff-find-other-file)
  (local-set-key (kbd "C-c c") #'insert-triplet)
  (local-set-key (kbd "C-c d") #'debug-comment)
  (local-set-key (kbd "C-c f") #'func-header)
  (local-set-key (kbd "C-c n") #'get-class-name)
  (local-set-key (kbd "C-c i") #'imenu))

(defun cpp-hook-func ()
  ;; Found this indentation info at: https://lists.gnu.org/archive/html/help-gnu-emacs/2013-03/msg00335.html
  ;; By issuing the following command, you can see what indentation vars are set to:
  ;;   M-x set-variable RET c-echo-syntactic-information-p RET t RET
  (c-set-offset 'inclass '++)
  (c-set-offset 'access-label '-)
  ;; Add some keywords to to C++ mode
  (font-lock-add-keywords 'c++-mode
                          '(("nullptr" . font-lock-keyword-face)
                            ("constexpr" . font-lock-keyword-face))))

(defun lisp-settings ()
  "Code to be evaluated when lisp major modes are enabled.  Currently, we
enable eldoc-mode."
  ;; This function probably does not need to be run for the slime hook, as
  ;; these functions and others are already included in that mode.
  (eldoc-mode)
  (yas-reload-all)
  (show-paren-mode t)
  (yas-minor-mode))

(defun python-hook-func ()
  "Some call me... Tim."
  (setq-default indent-tabs-mode nil)  ; use spaces, not tabs
  (setf tab-width 4)
  (yas-reload-all)
  (yas-minor-mode)
  (local-set-key (kbd "C-c c") #'insert-triplet)
  (local-set-key (kbd "C-c d") #'debug-comment)
  (local-set-key (kbd "C-c f") #'func-header))

(defun bash-hook-func ()
  "To be run when we open a bash shell script."
  (message "Welcome to shell script mode. Grrrrr!!")
  (yas-reload-all)
  (show-paren-mode t)
  (yas-minor-mode)
  (local-set-key (kbd "C-c c") #'insert-triplet)
  (local-set-key (kbd "C-c d") #'debug-comment))

(defun text-hook-func()
  "These settings will be applied to anything using text-mode.  Org-mode is
based on text-mode, so these settings affect that as well."
  (local-set-key (kbd "C-c c t") #'my-insert-time)
  (local-set-key (kbd "C-c c d") #'my-insert-date)
  (auto-fill-mode t)
  (setf fill-column 95)
  (yas-minor-mode))

(defun org-hook-func()
  "These are orgmode-specific settings."
  (setf org-log-done 'time))  ; timestamp when TODO item marked as DONE

(defun json-hook-func()
  (flycheck-mode))

;;; init.el ends here

;;; init.el -- My emacs init file.
;;; Commentary:
;;;     Some things.
;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Emacs Functionality

(setf make-backup-files nil       ; don't make backups
      backup-directory-alist nil) ; clear list of backup directories

(let ((default-directory "~/.emacs.d/lisp/"))    ; temporarily redefine the default directory for normal-top-level-add-to-load-path below.
  (normal-top-level-add-to-load-path '("."))     ; add current directory to load path
  (normal-top-level-add-subdirs-to-load-path))   ; recursively add subdirectories to load path

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customizations

;;; Packages
;;; Handle package archives and also ensure all required packages are installed.
(progn
  (require 'package)     ; Pull in package.el
  (setf package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			   ("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
  (package-initialize))  ; Initialize it

(eval-when-compile
  (when (not (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

;;; Personal libraries
;;;
(use-package select-comment-by-lang
  ; Load for various programming languages
  :config
  (add-hook 'c-mode-common-hook (lambda()
                                  (local-set-key (kbd "C-c c") #'mw-insert-triplet)
                                  (local-set-key (kbd "C-c d") #'mw-debug-comment))))

(use-package cpp-funcs
  :config
  (add-hook 'c-mode-common-hook (lambda()
                                  (local-set-key (kbd "C-c f") #'func-header)
                                  (local-set-key (kbd "C-c n") #'get-class-name))))

(use-package mw-utils
  ; Things like timestamps and other nice-to-haves
  :config
  (mw-create-sql-buffer)
  (global-set-key [f2] 'mw-toggle-selective-display)
  (define-key text-mode-map (kbd "C-c w t") #'mw-insert-time)
  (define-key text-mode-map (kbd "C-c w d") #'mw-insert-date))

;;; Installed packages
;;;
(use-package rainbow-delimiters
  :ensure t
  :pin melpa-stable
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package iy-go-to-char
  :ensure t
  :commands iy-go-up-to-char iy-go-to-char-backward)

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define-global "fj" #'iy-go-up-to-char)
  (key-chord-define-global "fk" #'iy-go-to-char-backward)
  (add-hook 'c-mode-common-hook (lambda()
                                  (key-chord-define-local "pq" #'mw-insert-curly-braces)))
  (add-hook 'sh-mode-hook (lambda()
                            (key-chord-define-local "pq" #'mw-insert-curly-braces)))
  (add-hook 'cperl-mode-hook (lambda()
                              (key-chord-define-local "pq" #'mw-insert-curly-braces))))
  
(use-package helm
  :ensure t
  :demand t
  :pin "melpa-stable"
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x c M-g M-g" . helm-grep-do-git-grep))
  :config
  (use-package helm-config)
  (helm-mode 1))

(use-package yasnippet
  :ensure t
  :pin "melpa-stable")

(use-package flycheck
  :ensure t
  :pin "melpa-stable")

(use-package flycheck-pycheckers
  :ensure t)

(use-package magit
  :ensure t
  :pin "melpa-stable"
  :config
  ;; Invoke magit-status screen
  (global-set-key (kbd "C-c C-g") #'magit-status))

(use-package python-mode
  :ensure t
  :pin "melpa-stable")

(use-package dockerfile-mode
  :ensure t
  :pin "melpa-stable")

(use-package docker-tramp
  :ensure t
  :pin "melpa-stable")

(use-package yaml-mode
  :ensure t
  :pin "melpa-stable")

(use-package company
  :ensure t
  :pin "melpa-stable")

(use-package shell-pop
  :ensure t
  :pin "melpa-stable"
  :bind (("C-c s" . shell-pop))
  :config
  (setf shell-pop-window-position "bottom"
        shell-pop-window-size 20))

;; (use-package sr-speedbar
;;   :config
;;   (setq speedbar-use-images nil))

(use-package buttercup
  :ensure t
  :pin "melpa-stable")

(use-package slime
  :ensure t
  :pin melpa-stable
  :config
  (setf inferior-lisp-program (executable-find "sbcl")
        slime-contribs '(slime-fancy)))

;; (use-package slime-company
;;   :ensure t
;;   :pin melpa-stable)

;;; Built-ins
;;;
;; (use-package smartparens
;;   :ensure t
;;   :config
;;   (let ((set-bindings (lambda()
;;                         (sp-base-key-bindings 'sp))))
;;     (add-hook emacs-lisp-mode-hook #'set-bindings)
;;     (add-hook lisp-mode-hook #'set-bindings)))

;;; Themes
;;;
(use-package zerodark-theme
  ; This theme is terminal-safe
  :ensure t
  :demand t
  :pin "melpa-stable")
;; (use-package solarized-theme)
;; (use-package abyss-theme)


(use-package doxygen-mode
  :demand t
  :commands
  doxygen-function-template doxygen-struct-template doxygen-class-template
  doxygen-create-group doxygen-backward-block doxygen-forward-block)

(use-package mw-perl-related)

;;; Toggle UI Elements
;;;
(dolist (mode-value '((global-linum-mode . 1)  ; display line numbers in margin
                      (column-number-mode . 1) ; display line and col num in mode line
                      (show-paren-mode . 1)    ; this should be on all the time
                      (tool-bar-mode . -1)
                      (menu-bar-mode . -1)
                      (horizontal-scroll-bar-mode . -1)
                      (scroll-bar-mode . -1)
                      (tooltip-mode . -1)))
  (when (fboundp (car mode-value))
    (funcall (car mode-value) (cdr mode-value))))

;;; Enable some commands
;;;
(put 'narrow-to-defun  'disabled nil)  ;
(put 'narrow-to-page   'disabled nil)  ; Narrowing
(put 'narrow-to-region 'disabled nil)  ;
(winner-mode 1)

;;; General Customizations
(when (not (display-graphic-p))
    (setf linum-format "%d "))      ; add space between line numbers and buffer text
(setq-default indent-tabs-mode nil) ; indent with spaces only
(setq-default c-basic-offset 2)     ; ensure that offset is two spaces and no more
(setf make-backup-files nil         ; do not make backup files (tilde files)
      backup-directory-alist nil    ; we don't need a backup directory
      inhibit-splash-screen t
      visible-bell t                ; Flash mode-bar instead of ringing system bell
      vc-handled-backends nil       ; Eliminates "Argument Error" issues with built-in vc package.
      abbrev-file-name "~/.emacs.d/abbrev_defs"
      save-abbrevs 'silent)         ; Abbrev-mode settings
;      enable-remote-dir-locals t)   ; Allow emacs to search remote directory trees for .dir-locals.el files.

;; Loading themes: Must be performed differently depending on whether this
;; is a daemonized server or a stand-alone instance.  For more info, see:
;;   `https://stackoverflow.com/questions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr'
;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions #'load-my-theme)
;;   (load-my-theme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Key Map and Bindings
;;; Anything that should happen across all modes (more or less)

;; General keybindings
(global-set-key (kbd "<f1>") #'shell-command)          ; shell command
(global-set-key (kbd "<select>") #'move-end-of-line)   ; <end> -> end of line

(global-set-key (kbd "C-x C-b") #'ibuffer)  ; Use ibuffer instead of default buffer list

;;; Macros and Hooks
;; Common C/C++ hooks. This hook will be run for many c-like languages,
;; but these keybindings may be overridden by defining local bindings in
;; a lower keymap for a given language. See 'https://www.masteringemacs.org/article/mastering-key-bindings-emacs'
;; for a discussion of this topic.
;;
;; TODO - Since my list of hook functions is always growing, I would like to move the hooks and hook functions
;;        into a list and then use something like mapcar to apply the add-hook function to everything in the list.
;;
(add-hook 'c-mode-common-hook #'c-style-lang-hook-func)
(add-hook 'c++-mode-hook #'cpp-hook-func)
(add-hook 'python-mode-hook #'python-hook-func)
(add-hook 'emacs-lisp-mode-hook #'lisp-settings)
(add-hook 'lisp-mode-hook #'lisp-settings)
(add-hook 'perl-mode-hook #'perl-settings)
(add-hook 'sh-mode-hook #'bash-hook-func)
(add-hook 'text-mode-hook #'text-hook-func)
(add-hook 'org-mode-hook #'org-hook-func)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'sql-mode-hook #'sql-hook-func)
(add-hook 'find-file-hook #'mw-find-proper-mode)  ; finds proper major mode for *.h files.
(add-hook 'cperl-mode-hook #'cperl-hook-func)

;;; Custom Hook functions
(defun c-style-lang-hook-func ()
  "Run these commands for all c-like languages."
  (superword-mode -1)  ; treat underscore-separated words as a single word?
  (subword-mode t)     ; treat camelCase words as separate words?
  (c-set-offset 'case-label '+) ; indent case statements in a switch block
  (which-function-mode)
;  (yas-reload-all)
  (yas-minor-mode)
  ;; (when (fboundp 'rtags-mode)
  ;;   (rtags-start-process-unless-running))
  (local-set-key (kbd "C-c o") #'ff-find-other-file)
  (local-set-key (kbd "C-c i") #'imenu)
  (local-set-key (kbd "C-c w C-t") #'mw-find-next-todo)
  (font-lock-add-keywords nil '(("\\<\\(TBD\\|TODO\\|FIXME\\)" 1 font-lock-warning-face prepend))))


(defun cpp-hook-func ()
  "Do some cpp things."
  ;; Found this info at: https://lists.gnu.org/archive/html/help-gnu-emacs/2013-03/msg00335.html
  ;; By issuing the following command, you can see what indentation vars are set to:
  ;;   M-x set-variable RET c-echo-syntactic-information-p RET t RET
  (c-set-offset 'inclass '++)
  (c-set-offset 'access-label '-)
  ;; Add some keywords to to C++ mode
  (font-lock-add-keywords 'c++-mode
                          '(("nullptr" . font-lock-keyword-face)
                            ("constexpr" . font-lock-keyword-face))))

(defun lisp-settings ()
  "Code to be evaluated when Lisp major modes are enabled."
  ;; This function probably does not need to be run for the slime hook, as
  ;; these functions and others are already included in that mode.
  (eldoc-mode)
;  (yas-reload-all)
  (show-paren-mode t)
  (yas-minor-mode)
  (local-set-key (kbd "C-m") #'newline-and-indent)
  (slime-mode)
  (company-mode))


(defun perl-settings()
  "Evaluate this stuff after perl mode has been started."
  (setf tab-width 4)
  (yas-minor-mode)
  (local-set-key (kbd "C-c c") #'mw-insert-triplet))

(defun python-hook-func ()
  "Some call me... Tim."
  (setq-default indent-tabs-mode nil)  ; use spaces, not tabs
  (setf tab-width 4)
;  (yas-reload-all)
  (yas-minor-mode)
  (local-set-key (kbd "C-c c") #'mw-insert-triplet)
  (local-set-key (kbd "C-c d") #'mw-debug-comment)
  (local-set-key (kbd "C-c f") #'func-header))

(defun bash-hook-func ()
  "To be run when we open a bash shell script."
  (message "Welcome to shell script mode. Grrrrr!!")
;  (yas-reload-all)
  (show-paren-mode t)
  (yas-minor-mode)
  (local-set-key (kbd "C-c c") #'mw-insert-triplet)
  (local-set-key (kbd "C-c d") #'mw-debug-comment))

(defun text-hook-func()
  "These settings will be applied to anything using text-mode.  Org-mode is
based on text-mode, so these settings affect that as well."
  (auto-fill-mode t)
  (setf fill-column 95)
  (yas-minor-mode))

(defun org-hook-func()
  "These are orgmode-specific settings."
  (setf org-log-done 'time    ; timestamp when TODO item marked as DONE
        org-latex-remove-logfiles t))

(defun json-hook-func()
  (flycheck-mode))

(defun sql-hook-func()
  (local-set-key (kbd "C-c c") #'mw-insert-triplet))

(defalias 'perl-mode 'cperl-mode)
(defun cperl-hook-func()
  (local-set-key (kbd "C-c w c") #'mw-insert-triplet)
  (local-set-key (kbd "C-c w s") #'mw-perl-setup))

;; Put all the configuration garbage generated by package into a separate file.
(setf custom-file "~/.emacs.d/lisp/init-custom.el")
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
(load custom-file)

;; Load rig-specific config files
(when (file-directory-p "~/.emacs.d/lisp/config")
  (dolist (file (directory-files "~/.emacs.d/lisp/config" 'full-path)
                (message "Finished loading custom config files"))
    (unless (file-directory-p file)
      (load file))))

;;; init.el ends here

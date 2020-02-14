;;; -*- lexical-binding: t; -*-

;;; init.el -- My emacs init file.
;;; Commentary:
;;;     Some things.
;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Emacs Functionality

(setf make-backup-files nil       ; don't make backups
      backup-directory-alist nil) ; clear list of backup directories

(defconst *base-dir*
  (if (eq emacs-major-version 27)
      "~/.config/emacs/"
    "~/.emacs.d/")
  "Base directory in which all config and LISP files reside.
This was changed in version 27 to conform with XDG standards.")

(let ((default-directory (concat *base-dir* "lisp/")))    ; temporarily redefine the default directory for normal-top-level-add-to-load-path below.
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
                           ("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")))
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
  :demand t
  ; Things like timestamps and other nice-to-haves
  :config
  (mw-create-sql-buffer)
  (global-set-key [f2] 'mw-toggle-selective-display)
  (define-key text-mode-map (kbd "C-c w t") #'mw-insert-time)
  (define-key text-mode-map (kbd "C-c w d") #'mw-insert-date)
  ;; see https://www.gnu.org/software/emacs/manual/html_mono/dbus.html#Bus-names
  (when (not (null (dbus-list-activatable-names :session)))
    (add-hook 'compilation-finish-functions #'mw-compilation-completed-notification)))


(use-package doxygen-mode
  :hook c-mode-common
  :commands
  doxygen-function-template doxygen-struct-template doxygen-class-template
  doxygen-create-group doxygen-backward-block doxygen-forward-block)


(use-package mw-perl-related)


;;; Installed packages
;;;
;; (use-package malinka   ; glues together various c/c++ packages e.g. rtags, projectile, etc
;;   ;;; Must define each project; probably best to do that in .dir-locals.el
;;   :ensure t
;;   :hook c-mode-common)


(use-package org
  :defines org-babel-load-languages org-export-backends
  :preface
  (require 'ox-confluence nil 'no-error)
  (require 'ox-md nil 'no-error)
  (setq org-export-backends '(ascii html icalendar latex confluence md))
  :init
  (push '(C . t) org-babel-load-languages)
  :config
  (use-package org-jira
    :defer)
  :hook org-jira)


(use-package restclient
  :ensure t
  :functions get-session
  :config
  (use-package restclient-helm
    :ensure t)
  (use-package restclient-test
    :ensure t)
  (use-package company-restclient
    :ensure t)
  (defun get-session ()
    "Get a session token returned from a REST login call."
    (interactive)
    (message "%s" (setq-local session-var
                              (with-current-buffer (get-buffer "*HTTP Response*")
                                (goto-char (point-min))
                                (when (search-forward "\"session\": \"" nil t)
                                  (buffer-substring-no-properties (point) (1- (search-forward "\""))))))))
  :bind ("C-c r s" . #'get-session))



(use-package systemd
  :ensure t
  :config
  (use-package helm-systemd
    :ensure t))


(use-package projectile
  :ensure t
  :pin melpa
  :commands projectile-register-project-type
  :bind-keymap ("C-c p" . projectile-command-map)
  :hook (prog-mode . projectile-mode)
  :custom
  (projectile-mode-line '(:eval (projectile-project-name)))
  (projectile-completion-system 'helm)
  (projectile-cache-file (concat (expand-file-name user-emacs-directory) "projectile/projectile.cache"))
  (projectile-enable-caching t)
  :init
  (projectile-register-project-type 'elisp '(".elisp-project")
                                                 :test-suffix "-test"
                                                 :test-dir "test/")
  (projectile-register-project-type 'c++-make '(".c++-project")
                                    :configure "%s/bootstrap && %s/configure"
                                    :compile "make"
                                    :test-suffix "_test"
                                    :src-dir "%s/src/"
                                    :test-dir "%s/test/unit_tests/")
  :delight '(:eval (concat " ¶[" (projectile-project-name) "]")))


(use-package cmake-mode
  :ensure t
  :pin melpa
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :init
  (use-package cmake-font-lock
    :ensure t
    :pin melpa
    :hook (cmake-mode . cmake-font-lock-activate))
  :config
  (company-mode))


;; (use-package cmake-ide
;;   :ensure t
;;   :pin melpa
;;   :after projectile
;;   :init (cmake-ide-setup)
;;   :defines (cmake-ide-rc-executable cmake-ide-rdm-executable)
;;   :config
;;   (let ((exec-path (mapcar #'directory-file-name (directory-files-recursively (expand-file-name "~/.emacs.d") "\\bbin\\b" t))))
;;     (setf cmake-ide-rc-executable (executable-find "rc")
;;           cmake-ide-rdm-executable (executable-find "rdm"))))


(use-package delight
  :ensure t
  :pin gnu)


(use-package rainbow-delimiters
  :ensure t
  :pin melpa-stable
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package iy-go-to-char
  :ensure t
  :commands iy-go-up-to-char iy-go-to-char-backward)


(use-package key-chord
  :ensure t
  :pin melpa
  :hook
  ((rust-mode c-mode-common sh-mode cperl-mode) . (lambda() (key-chord-define-local "pq" #'mw-insert-curly-braces)))
  :init
  (key-chord-mode 1)
  (key-chord-define-global "fj" #'iy-go-up-to-char)
  (key-chord-define-global "fk" #'iy-go-to-char-backward))
  

(use-package helm
  :ensure t
  :demand t
  :pin melpa
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x c M-g M-g" . helm-grep-do-git-grep))
  :config
  (use-package helm-config)
  (use-package helm-projectile
    :ensure t
    :pin melpa
    :after projectile)
  (helm-mode 1)
  :delight " Ƕ")


(use-package yasnippet
  :ensure t
  :pin melpa
  :delight " ¥")


(use-package yasnippet-snippets
  :ensure t
  :pin melpa
  :after yasnippet)


(use-package flycheck
  :ensure t
  :pin melpa
  :config
  (use-package flycheck-pycheckers
    :ensure t
    :pin melpa))
;  (use-package flycheck-clang-tidy
 ;   :hook (flycheck-mode . #'flycheck-clang-tidy-setup)))


(use-package magit
  :ensure t
  :pin melpa
  :config
  ;; Invoke magit-status screen
  (global-set-key (kbd "C-c C-g") #'magit-status))

(use-package rtags
  :ensure t
  :pin melpa
  :custom
  (rtags-verify-protocol-version nil)
  (rtags-autostart-diagnostics t)
  (rtags-use-helm t)
  (rtags-process-flags "-v --inactivity-timeout 300 --log-flush -j2 --rp-nice-value 19")
  :config
  (rtags-enable-standard-keybindings)
  (rtags-start-process-unless-running)
  (use-package flycheck-rtags
    :ensure t
    :pin melpa
    :after rtags)
  (use-package helm-rtags
    :ensure t
    :pin melpa
    :after rtags)
  (use-package company-rtags
    :ensure t
    :config
    (push 'company-rtags company-backends)))


(use-package irony
  :ensure t
  :pin melpa
  :after company
  :hook (((c++-mode c-mode) . irony-mode) ; start irony mode when c/c++ hooks are run
         (irony-mode . irony-cdb-autosetup-compile-options)) ; run autosetup when we enter irony mode
  :delight " Fe"
  :config
  (use-package company-irony
    :ensure t
    :pin melpa
    :after company
    :config
    (add-to-list 'company-backends 'company-irony)
    :bind ("C-c w i" . #'company-irony))
  (use-package company-irony-c-headers
    :ensure t
    :pin melpa
    :after company
    :config
    (add-to-list 'company-backends '(company-irony-c-headers comany-irony)))
  (use-package flycheck-irony
    :ensure t
    :pin melpa
    :hook (flycheck-mode . flycheck-irony-setup)))


(use-package elpy
  :ensure t
  :defer t
  :pin melpa
  :delight "🥧"
  :custom
  (exec-path (cons (expand-file-name "~/.local/bin") exec-path))  ; python pip installs live here
  :init
  (elpy-enable))


(use-package dockerfile-mode
  :ensure t
  :pin melpa)


(use-package docker-tramp
  :ensure t
  :pin melpa)


(use-package yaml-mode
  :ensure t
  :pin melpa)


(use-package company
  :ensure t
  :pin melpa
  :delight " Ç"
  :hook ((fundamental-mode text-mode prog-mode) . company-mode)
  :custom
  (company-idle-delay 0.25)
  :config
  (global-company-mode)
  (use-package company-shell
    ;; placed here for lack of a better place...
    :ensure t))


(use-package shell-pop
  :ensure t
  :pin melpa
  :bind (("C-c s" . shell-pop))
  :config
  (setf shell-pop-window-position "bottom"
        shell-pop-window-size 20))


;; (use-package sr-speedbar
;;   :config
;;   (setq speedbar-use-images nil))


(use-package buttercup
  :ensure t
  :pin melpa)


(use-package slime
  :if (executable-find "sbcl")
  :ensure t
  :pin melpa-stable
  :config
  (use-package slime-company
    :after 'slime
    :ensure t
    :pin melpa-stable)
  (setf inferior-lisp-program (executable-find "sbcl")
        slime-contribs '(slime-fancy)))


;;; Language Server Protocol setup.  Hook lsp-mode from
;;; the appropriate language mode so we don't call it
;;; globally.  Not all languages use LSP in this
;;; config.
(use-package lsp-mode
  :ensure t
  :pin melpa
  :commands lsp
  :custom
  (lsp-prefer-flymake nil)
  (lsp-auto-guess-root t)  ; will use projectile
  (lsp-auto-configure t)   ; auto configure dependencies etc.
  :config
  (require 'lsp-clients)
  (use-package lsp-ui
    :ensure t
    :pin melpa
    :commands lsp-ui-mode)
  (use-package company-lsp
    :ensure t
    :pin melpa
    :commands company-lsp
    :config
    (push 'company-lsp company-backends))
  (use-package helm-lsp
    :ensure t
    :pin melpa
    :commands helm-lsp-workspace-symbol)
  ;;; For debugging
  (use-package dap-mode
    :ensure t
    :pin melpa
    :config
    (dap-mode t)
    (dap-ui-mode t))
  ;;; C-family language server
  (when (let ((exec-path
               (cons exec-path (cons (expand-file-name "~") (cons (expand-file-name "~/Scripts") nil)))))
          (setf ccls-executable (executable-find "ccls")))
    (use-package ccls
      :defines ccls-executable
      :preface
      (defun find-ccls ()
        "Add home directory to the `exec-path' variable and then look for ccls executable."
        (let ((exec-path
               (cons exec-path (cons (expand-file-name "~") (cons (expand-file-name "~/Scripts") nil)))))
          (setf ccls-executable (executable-find "ccls"))))
      :if (find-ccls)  ; only load if we can find ccls binary
      :after lsp-mode projectile
      :ensure t
      :pin melpa
      :hook (c++-mode c-mode)
      :custom
      (ccls-args nil)
      (ccls-executable "/usr/local/bin/ccls");(find-ccls))
      (projectile-project-root-files-top-down-recurring
       (append '("compile_commands.json" ".ccls")
               projectile-project-root-files-top-down-recurring))
      :config
      (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))))


(use-package toml-mode
  :ensure t
  :pin melpa)


(use-package rust-mode
  :defer t
  :ensure t
  :pin melpa
  :hook (rust-mode . lsp)
  :custom
  (exec-path (cons (expand-file-name "~/.cargo/bin") exec-path))  ; add cargo bin directory to exec-path
  :config
  (use-package cargo
    :ensure t
    :pin melpa
    :hook (rust-mode . cargo-minor-mode))
  (use-package flycheck-rust
    :ensure t
    :pin melpa
    :hook (flycheck-mode . flycheck-rust-setup)))


(use-package php-mode
  :ensure t
  :config
  (use-package company-php))


;;; Built-ins
;;;
(use-package smartparens
  :ensure t
  :hook
  ((emacs-lisp-mode lisp-mode) . smartparens-mode)
  :init
  (use-package smartparens-config))


;;; Themes
;;;
(use-package zerodark-theme
  ; This theme is terminal-safe
  :ensure t
  :demand t
  :pin melpa
  :init
  ;; emacs27 changed default behavior, requiring a theme no longer automatically loads that theme.
  (load-theme 'zerodark 'NO-CONFIRM))
;; (use-package solarized-theme)
;; (use-package abyss-theme)


;;; Toggle UI Elements
;;;
(dolist (mode-value '((global-linum-mode . 1)  ; display line numbers in margin
                      (column-number-mode . 1) ; display line and col num in mode line
                      (show-paren-mode . 1)    ; this should be on all the time
                      (tool-bar-mode . -1)
                      (menu-bar-mode . -1)
                      (horizontal-scroll-bar-mode . -1)
                      (scroll-bar-mode . -1)
                      (tooltip-mode . -1)
                      (winner-mode . 1)))
  (when (fboundp (car mode-value))
    (funcall (car mode-value) (cdr mode-value))))

;;; Enable some commands
;;;
(put 'narrow-to-defun  'disabled nil)  ;
(put 'narrow-to-page   'disabled nil)  ; Narrowing
(put 'narrow-to-region 'disabled nil)  ;

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
      abbrev-file-name (concat *base-dir* "abbrev_defs")
      save-abbrevs 'silent          ; Abbrev-mode settings
      compilation-scroll-output 'first-error)

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
  "These settings will be applied to anything using 'text-mode'.
Org-mode is based on 'text-mode', so these settings affect that as well."
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
(setf custom-file (concat *base-dir* "lisp/init-custom.el"))
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
(when (file-directory-p (concat *base-dir* "lisp/config"))
  (dolist (file (directory-files (concat *base-dir* "lisp/config") 'full-path)
                (message "Finished loading custom config files"))
    (unless (file-directory-p file)
      (load file))))

;;; init.el ends here

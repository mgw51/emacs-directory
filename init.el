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
                        flycheck-rtags
                        magit
			abyss-theme
                        dockerfile-mode
                        docker-tramp
                        yaml-mode)))
    (ensure-packages-installed package-list)))

;;; Load libraries and packages.
(dolist (cool-thing '(python-mode
                      select-comment-by-lang ; one of my functions
                      cpp-funcs              ; my c/c++ helper functions
                      iy-go-to-char
                      helm                   ; helm provides excellent incremental completion
                      helm-config
                      key-chord              ; map chord combinations to regular key-pairs pressed simultaneously
                      yasnippet              ; snippet functionality
                      flycheck               ; flycheck package for syntax checking on the fly
                      flycheck-rtags
                      magit
                      doxygen                ; my own simple doxygen template insert library
                      my-work-utils)         ; utilities file
                    t)
  (funcall 'require cool-thing))
(if (locate-library "rtags")
    (progn
      (require 'rtags)
      (setf rtags-path "/usr/local/bin")
      (rtags-enable-standard-keybindings)))

;;; User Interface

;; Turn ON some UI elements
(dolist (mode '(global-linum-mode     ; display line numbers in margin
                column-number-mode))  ; display line and column number in status bar
  (funcall mode 1))

;; Turn OFF some UI elements
(dolist (mode '(tool-bar-mode
                menu-bar-mode
                horizontal-scroll-bar-mode
                tooltip-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; Enable some commands
(put 'narrow-to-defun  'disabled nil)  ;
(put 'narrow-to-page   'disabled nil)  ; Narrowing
(put 'narrow-to-region 'disabled nil)  ;

;;; General Customizations
;; Create an SQL scratch buffer
(create-sql-buffer)
(when (not (display-graphic-p))
    (setf linum-format "%d "))      ; add space between line numbers and buffer text
(setq-default indent-tabs-mode nil) ; indent with spaces only
(setq-default c-basic-offset 2)     ; ensure that offset is two spaces and no more
(setf make-backup-files nil         ; do not make backup files (tilde files)
      backup-directory-alist nil    ; we don't need a backup directory
      inhibit-splash-screen t
      inferior-lisp-program "/usr/bin/sbcl"  ; Slime: Default lisp
      slime-contribs '(slime-fancy) ; Slime: slime-fancy loads pretty much everything
      visible-bell t                ; Flash mode-bar instead of ringing system bell
      vc-handled-backends nil)      ; Eliminates "Argument Error" issues with built-in vc package.
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

;; helm
(and
 (global-set-key (kbd "M-x") #'helm-M-x)
 (global-set-key (kbd "C-x C-f") #'helm-find-files)
 (helm-mode 1))  ; Start helm automatically
;; key chord
;; 'key-chord-define-local function is used in mode hooks below.  These are global definitions here.
(and
 (key-chord-mode 1)
 (key-chord-define-global "fj" #'iy-go-up-to-char)
 (key-chord-define-global "fk" #'iy-go-to-char-backward))
;; General keybindings
(and
 (fset 'sort-buffer-by-name  ; Create function cell and assign it to key chord
       "\M-2\M-x Buffer-menu-sort")
 (global-set-key (kbd "C-c 2") #'sort-buffer-by-name)) ; sort buffer by name
(global-set-key (kbd "<f1>") #'shell-command)          ; shell command
(global-set-key (kbd "<select>") #'move-end-of-line)   ; <end> -> end of line
(global-set-key (kbd "C-c C-g") #'magit-status)        ; Invoke magit-status screen, from which all magit commands are available
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
(add-hook 'sh-mode-hook #'bash-hook-func)
(add-hook 'projectile-mode-hook #'projectile-hook-func)
(add-hook 'text-mode-hook #'text-hook-func)
(add-hook 'org-mode-hook #'org-hook-func)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'find-file-hook 'my-find-proper-mode)  ; finds proper major mode for *.h files.

(defun my-insert-time ()
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun my-insert-date ()
  (interactive)
  (insert (format-time-string "%A, %b %d, %Y")))

;;; Custom Hook functions
(defun c-style-lang-hook-func ()
  "Run these commands for all c-like languages."
  (superword-mode -1)  ; treat underscore-separated words as a single word?
  (subword-mode t)     ; treat camelCase words as separate words?
  (key-chord-define-local "pq" "{\n\n}\C-p\t")
  (c-set-offset 'case-label '+) ; indent case statements in a switch block
  (show-paren-mode t)
  (which-function-mode)
  (yas-reload-all)
  (yas-minor-mode)
  (flycheck-mode)
  (rtags-start-process-unless-running)
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
  (yas-minor-mode)
  (local-set-key (kbd "C-m") #'newline-and-indent)
  (local-set-key (kbd "C-c c") #'insert-triplet))

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
  (setf org-log-done 'time    ; timestamp when TODO item marked as DONE
        org-latex-remove-logfiles nil))

(defun json-hook-func()
  (flycheck-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-safe-themes
   (quote
    ("dd2346baba899fa7eee2bba4936cfcdf30ca55cdc2df0a1a4c9808320c4d4b22" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "b56ea05564419bcdd994e4c97ad9167d0d6abe535bd01ef5be986836c43389b7" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "14f0fbf6f7851bfa60bf1f30347003e2348bf7a1005570fd758133c87dafe08f" default)))
 '(fci-rule-color "#5E5E5E")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (docker-tramp dockerfile-mode yaml-mode abyss-theme magit flycheck yasnippet key-chord python-mode helm iy-go-to-char)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(safe-local-variable-values
   (quote
    ((eval let*
           ((project-root
             (file-truename
              (locate-dominating-file default-directory ".dir-locals.el")))
            (warnings
             (quote
              ("all" "extra" "shadow" "non-virtual-dtor")))
            (include-list
             (list
              (concat project-root "include")
              (concat project-root "catch"))))
           (set
            (make-local-variable
             (quote flycheck-clang-include-path))
            include-list)
           (set
            (make-local-variable
             (quote flycheck-clang-warnings))
            warnings)
           (set
            (make-local-variable
             (quote flycheck-gcc-include-path))
            include-list)
           (set
            (make-local-variable
             (quote flycheck-gcc-warnings))
            warnings)
           (set
            (make-local-variable
             (quote flycheck-cppcheck-include-path))
            include-list))
     (eval let*
           ((project-root
             (file-truename
              (locate-dominating-file default-directory ".dir-locals.el")))
            (warnings
             (quote
              ("all" "extra" "shadow" "non-virtual-dtor")))
            (include-list
             (list
              (concat project-root "include"))))
           (set
            (make-local-variable
             (quote flycheck-clang-include-path))
            include-list)
           (set
            (make-local-variable
             (quote flycheck-clang-warnings))
            warnings)
           (set
            (make-local-variable
             (quote flycheck-gcc-include-path))
            include-list)
           (set
            (make-local-variable
             (quote flycheck-gcc-warnings))
            warnings)
           (set
            (make-local-variable
             (quote flycheck-cppcheck-include-path))
            include-list))
     (eval let*
           ((project-root
             (file-truename
              (locate-dominating-file default-directory ".dir-locals.el")))
            (warnings
             (quote
              ("all" "extra" "shadow" "non-virtual-dtor")))
            (include-list
             (list
              (concat project-root "include")
              (concat project-root "test"))))
           (set
            (make-local-variable
             (quote flycheck-clang-include-path))
            include-list)
           (set
            (make-local-variable
             (quote flycheck-clang-warnings))
            warnings)
           (set
            (make-local-variable
             (quote flycheck-gcc-include-path))
            include-list)
           (set
            (make-local-variable
             (quote flycheck-gcc-warnings))
            warnings)
           (set
            (make-local-variable
             (quote flycheck-cppcheck-include-path))
            include-list))
     (eval let*
           ((project-root
             (file-truename
              (locate-dominating-file default-directory ".dir-locals.el")))
            (warnings
             (quote
              ("all" "extra" "shadow" "non-virtual-dtor")))
            (include-list
             (list
              (concat project-root "include")
              (concat project-root "build/googletest-src/googlemock/include")
              (concat project-root "build/googletest-src/googletest/include")
              (concat project-root "test"))))
           (set
            (make-local-variable
             (quote flycheck-clang-include-path))
            include-list)
           (set
            (make-local-variable
             (quote flycheck-clang-warnings))
            warnings)
           (set
            (make-local-variable
             (quote flycheck-gcc-include-path))
            include-list)
           (set
            (make-local-variable
             (quote flycheck-gcc-warnings))
            warnings)
           (set
            (make-local-variable
             (quote flycheck-cppcheck-include-path))
            include-list)))))
 '(vc-annotate-background "#202020")
 '(vc-annotate-color-map
   (quote
    ((20 . "#C99090")
     (40 . "#Dn9A0A0")
     (60 . "#ECBC9C")
     (80 . "#DDCC9C")
     (100 . "#EDDCAC")
     (120 . "#FDECBC")
     (140 . "#6C8C6C")
     (160 . "#8CAC8C")
     (180 . "#9CBF9C")
     (200 . "#ACD2AC")
     (220 . "#BCE5BC")
     (240 . "#CCF8CC")
     (260 . "#A0EDF0")
     (280 . "#79ADB0")
     (300 . "#89C5C8")
     (320 . "#99DDE0")
     (340 . "#9CC7FB")
     (360 . "#E090C7"))))
 '(vc-annotate-very-old-color "#E090C7"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here


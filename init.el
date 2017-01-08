;;;; Matt's ~/.emacs

(when (not (display-graphic-p))
    (setq linum-format "%d "))      ; add space between line numbers and buffer text
(setq-default indent-tabs-mode nil) ; indent with spaces only
(global-linum-mode 1)               ; display line numbers in margin
(column-number-mode 1)              ; display line and column number in status bar
(setq-default c-basic-offset 2)     ; ensure that offset is two spaces and no more
(setq make-backup-files nil)        ; do not make backup files (tilde files)
(setq backup-directory-alist nil)
(setq inhibit-splash-screen t)
(progn                              ; Force tooltips to display in echo area
  (tooltip-mode t)
  (setq tooltip-use-echo-area t))
(setq visible-bell t)               ; Flash mode-bar instead of ringing system bell

;;; Turn off the following modes...
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; ERC: Send IRC notices to my minibuffer
(setq erc-echo-notices-in-minibuffer-flag t)

;;; Set load paths
;; set top-level directory, and automatically add subdirectories
(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; Create settings for slime
(progn
  ;; Default lisp (not elisp) for use with slime
  (setq inferior-lisp-program "/usr/bin/sbcl")
  ;; slime-fancy loads pretty much everything
  (setq slime-contribs '(slime-fancy))
  (setq slime-lisp-implementations  ; We created and saved a core file for faster load times.
        '((sbcl ("sbcl" "--core" "lisp/sbcl/sbcl.core-for-slime")))))

;;; Packages
;;; Handle package archives and also ensure all required packages are installed.
(progn
  (require 'package)     ; Pull in package.el
  (package-initialize)   ; Initialize it
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")))
  (require 'ensure-packages-installed)  ; custom function that installs missing packages listed below
  (let ((package-list '(iy-go-to-char
                        helm
                        key-chord
                        yasnippet
                        flycheck
                        magit)))
    (ensure-packages-installed package-list)))

;;; Place all other Requires here
(progn
  (require 'php-mode)                ; this resides in my lisp folder
  (progn
    (setq py-install-directory "/emacs.d/lisp/python-mode.el-6.2.0")
    (require 'python-mode))
  (require 'select-comment-by-lang)  ; one of my functions
  (require 'cpp-funcs)               ; my c/c++ helper functions
  (require 'iy-go-to-char)
  (and                               ; helm provides excellent incremental completion
   (require 'helm)
   (require 'helm-config))
  (require 'key-chord)               ; map chord combinations to regular key-pairs pressed simultaneously
  (require 'yasnippet)               ; snippet functionality
  (require 'flycheck)                ; flycheck package for syntax checking on the fly
  (require 'magit))


;;; Global Key Map and Bindings
;;; Anything that should happen across all modes (more or less)
;; helm
(and
 (global-set-key (kbd "M-x") #'helm-M-x)
 (global-set-key (kbd "C-x C-f") #'helm-find-files)
 (helm-mode 1))  ; Start helm automatically
;; key chord
(and
 (key-chord-mode 1)
 (key-chord-define c++-mode-map "{}" "{\n\n}\C-p\t")
 (key-chord-define-global "fj" #'iy-go-up-to-char)
 (key-chord-define-global "fk" #'iy-go-to-char-backward))
;; General keybindings
(and
 (fset 'sort-buffer-by-name  ; Create function cell and assign it to key chord
       "\M-2\M-x Buffer-menu-sort")
 (global-set-key (kbd "C-c 2") #'sort-buffer-by-name)) ; sort buffer by name
;(global-set-key (kbd "M-s") #'query-replace-regexp)    ; regex query replace
(global-set-key (kbd "<f1>") #'shell-command)          ; shell command
(global-set-key (kbd "<select>") #'move-end-of-line)   ; <end> -> end of line
(global-set-key (kbd "C-c C-g") #'magit-status)        ; Invoke magit-status screen, from which all magit commands are available
;; ibuffer (better than the default buffer screen)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;;; Enable some commands
(put 'narrow-to-defun  'disabled nil)  ;
(put 'narrow-to-page   'disabled nil)  ; Narrowing
(put 'narrow-to-region 'disabled nil)  ;


;; ;;; Evaluate these after load
;; ;; Turn on 'which-func-mode' minor mode for these languages
;; (eval-after-load "which-func"
;;                  '(setq which-func-modes '(c++-mode c-mode python-mode php-mode lisp-mode emacs-lisp-mode)))


;;; Macros and Hooks
;; Common C/C++ hooks. This hook will be run for many c-like languages,
;; but these keybindings may be overridden by defining local bindings in
;; a lower keymap for a given language. See 'https://www.masteringemacs.org/article/mastering-key-bindings-emacs'
;; for a discussion of this topic.
(add-hook 'c-mode-common-hook #'c-style-lang-hook-func)
(add-hook 'c++-mode-hook #'cpp-hook-func)
(add-hook 'python-mode-hook #'python-hook-func)
(add-hook 'emacs-lisp-mode-hook #'lisp-settings)
(add-hook 'lisp-mode-hook #'lisp-settings)
(add-hook 'sh-mode-hook #'bash-hook-func)

;;; Custom Hook functions
(defun c-style-lang-hook-func ()
  (electric-pair-mode)
  (superword-mode t)  ; treat underscore-separated words as a single word : true
  (subword-mode -1)   ; treat camelCase words as separate words: false     
  (c-set-offset 'case-label '+) ; indent case statements in a switch block
  (show-paren-mode t)
  (which-function-mode)
  (yas-reload-all)
  (local-set-key (kbd "C-c o") #'ff-find-other-file)
  (local-set-key (kbd "C-c c") #'insert-triplet)
  (local-set-key (kbd "C-c d") #'debug-comment)
  (local-set-key (kbd "C-c f") #'func-header)
  (local-set-key (kbd "C-c n") #'get-class-name)
  (global-set-key (kbd "C-c i") #'imenu))

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
  ; enable C++11 support
  ;  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11"))))


(defun lisp-settings ()
  "Code to be evaluated when lisp major modes are enabled.  Currently, we
enable eldoc-mode."
  ;; This function probably does not need to be run for the slime hook, as
  ;; these functions and others are already included in that mode.
  (eldoc-mode)
  (yas-reload-all)
  (local-set-key (kbd "C-m") #'newline-and-indent)
  (local-set-key (kbd "C-c c") #'insert-triplet))

(defun python-hook-func ()
  "Some call me... Tim."
  (setq-default indent-tabs-mode nil)  ; use spaces, not tabs
  (setq tab-width 4)
  (yas-reload-all)
  (local-set-key (kbd "C-c c") #'insert-triplet)
  (local-set-key (kbd "C-c d") #'debug-comment)
  (local-set-key (kbd "C-c f") #'func-header))

(defun bash-hook-func ()
  "To be run when we open a bash shell script."
  (message "Welcome to shell script mode. Grrrrr!!")
  (yas-reload-all)
  (local-set-key (kbd "C-c c") #'insert-triplet)
  (local-set-key (kbd "C-c d") #'debug-comment))


(defun flycheck-hook-func()
  ; recommends use of gcc over clang on this page:  http://wiki.opencog.org/w/Flycheck_help#Configuration
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(c/c++-clang))))

;; Color theme selection
;;  This could be moved in to the auto-generated `custom-set-variables' section below,
;;  however, since we are not using any of the new custom theme stuff in 24+, I moved
;;  hoisted this out and put it here.
;; (and
;;  (color-theme-initialize)
;;  (color-theme-clarity))

;;; Auto added by emacs24
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#313131" "#D9A0A0" "#8CAC8C" "#FDECBC" "#99DDE0" "#E090C7" "#A0EDF0" "#DCDCCC"])
 '(background-color "#ffffff")
 '(background-mode light)
 '(cursor-color "#ffff00")
 '(custom-safe-themes
   (quote
    ("14f0fbf6f7851bfa60bf1f30347003e2348bf7a1005570fd758133c87dafe08f" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "108b3724e0d684027c713703f663358779cc6544075bc8fd16ae71470497304f" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" default)))
 '(fci-rule-color "#2D2D2D")
 '(foreground-color "#ffff00")
 '(vc-annotate-background "#202020")
 '(vc-annotate-color-map
   (quote
    ((20 . "#C99090")
     (40 . "#D9A0A0")
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

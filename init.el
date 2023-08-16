;;; -*- lexical-binding: t; -*-

;;; init.el -- My emacs init file.
;;; Commentary:
;;;
;;;
;;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.
(setq gc-cons-threshold (* 100 1024 1024))

;; Shadow `default-directory' with a temporary value so we can:
;;  1. selectively add to our load path based on the current value of
;;     `default-directory', and
;;  2. recursively add subdirectories
;;
(let ((default-directory (expand-file-name "lisp/" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(progn
  ;;; Set and load custom config file as early as possible because we
  ;;; want all package-generated config to go into a custom config
  ;;; file set here -- not into our init.el file.
  (require 'mw-basic-setup)
  (basic/set-and-load-custom-config-file)
  (basic/setup-package-el '(("gnu" . "http://elpa.gnu.org/packages/")
                            ("melpa" . "https://melpa.org/packages/")
                            ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (basic/use-package-setup '((use-package-verbose . t)
                             (use-package-compute-statistics . t)
                             (use-package-always-defer . t)
                             (use-package-always-ensure . t)))
  (basic/ui-setup)
  (basic/quality-of-life))

;; Bootstrap straight package manager
(require 'straight-bootstrap)
(straight-bootstrapper)

;;; Configure my lisp files
(use-package mw-utils
  :ensure nil
  :demand t
  :commands (mw-insert-curly-braces mw-create-sql-buffer mw-compilation-completed-notification)
  :config
  (mw-create-sql-buffer)
  (require 'dbus)
  (when (not (null (dbus-list-activatable-names :session)))
    (add-hook 'compilation-finish-functions #'mw-compilation-completed-notification)))

(use-package select-comment-by-lang
  :ensure nil
  :commands (mw-insert-triplet mw-debug-comment mw-remove-debug mw-find-next-todo))

(use-package cpp-funcs
  :ensure nil
  :commands (mw-func-header mw-include-guard mw-create-basic-makefile))

;;; Configure everything else
(use-package exec-path-from-shell
  :demand t
  :config
  ;; Copy important environment variables into emacs session
  ;; Mainly needed because OSX and systemd do not pass user env to emacs
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package key-chord)

(use-package use-package-chords
  :demand t
  :config (key-chord-mode 1))

(use-package ace-jump-mode
  :chords (("jj" . ace-jump-char-mode)
           ("jk" . ace-jump-word-mode)
           ("jl" . ace-jump-line-mode)))

(use-package zerodark-theme
  :demand t
  :config
  ;; load-theme is required to actually activate the theme
  (load-theme 'zerodark 'NO-CONFIRM))

(use-package powerline
  :demand t
  :config
  (powerline-default-theme))

(use-package blackout
  ;; Use this to change or remove package lighter in modeline.  May
  ;; use in `use-package' stanzas.
  :demand t
  :config
  ;; Miscellaneous lighter changes that don't have a use-package
  ;; stanza
  (blackout 'auto-revert-mode " ↻")
  (blackout 'eldoc-mode " ℓ")
  (blackout 'auto-fill-mode))

(use-package which-key
  :demand t
  :blackout
  :config
  (which-key-mode))

(use-package company
  :demand t
  :blackout " Ç"
  :config
  (global-company-mode))

(use-package yasnippet
  :blackout (yas-minor-mode . " ꪗ")
  :commands yas-reload-all
  :init (yas-global-mode 1)
  :custom
  (yas-indent-line 'auto "Indent each line of the snippet with 'indent-according-to-mode'")
  (yas-also-auto-indent-first-line t "Indent first line according to mode.")
  :config
  (yas-reload-all))

(use-package magit
  :ensure t
  :commands magit-status
  :bind ("C-c C-g" . #'magit-status))

(use-package helm
  :demand t
  :blackout " Ƕ"
  ;; We must explicitly bind these helm commands (globally)
  :bind (("M-x" . #'helm-M-x)
         ("C-x C-f" . #'helm-find-files)
         ("C-x C-b" . #'helm-buffers-list)
         ("M-s o" . #'helm-occur)
         ("C-x r b" . #'helm-filtered-bookmarks))
  :custom
  (completion-styles '(flex))
  :config
  ;; Other emacs commands can be swapped out for helm commands by
  ;; enabling helm-mode
  (helm-mode 1))

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :preface
  (defun mw-vlang-same-directory-src-and-test-files(dir)
    "vlang test files reside in the same directory as the corresponding source files."
    dir)
  :custom
  (projectile-mode-line-prefix " ¶")
  (projectile-completion-system 'helm)
  (projectile-cache-file (concat (expand-file-name user-emacs-directory) "projectile/projectile.cache"))
  (projectile-enable-caching t)
  (projectile-enable-cmake-presets t)
  :init (use-package ag
          :after projectile)
        (use-package helm-projectile
          :after projectile)
  :config
  (projectile-register-project-type 'vlang '("v.mod" "src" ".git" ".gitignore" ".gitattributes" ".editorconfig")
                                    :project-file "v.mod"
                                    :compile "v ."
                                    :test "v test ."
                                    :run "v run ."
                                    :src-dir #'mw-vlang-same-directory-src-and-test-files
                                    :test-dir #'mw-vlang-same-directory-src-and-test-files
                                    :test-suffix "_test")
  (projectile-register-project-type 'c++at '(".c++at") ; C++ autotools (work-specific)
                                    :project-file ".c++at"
                                    :compile "make -kj15"
                                    :test "test/unit_tests/unit_tests"
                                    :test-dir "test/unit_tests"
                                    :test-suffix "_test"))

(use-package lsp-mode
  :bind-keymap ("C-c l" . lsp-command-map)
  :bind (:map lsp-command-map
              ("g i" . #'lsp-ui-imenu)
              ("n" . #'lsp-ui-find-next-reference)
              ("p" . #'lsp-ui-find-prev-reference))
  :commands lsp
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (when (string-equal-ignore-case (system-name) "sensa-ripper")
    ;; sometimes the executable location must be set manually or you get errors because
    ;; it defaults to the system version of clangd, which is OLD.
    (lsp-clients-clangd-executable "/home/mwood/.emacs.d/.cache/lsp/clangd/clangd_15.0.6/bin/clangd")
    (lsp-clients-clangd-args '("--header-insertion-decorators")))
  (lsp-prefer-flymake nil "Use flycheck instead")
  (lsp-auto-guess-root t "Uses projectile, when available")
  (lsp-auto-configure t)
  (lsp-enable-on-type-formatting nil "Disable LSP's attempts to format code")
  (read-process-output-max (* 1024 1024 2) "Increase the process output max because code servers may return large amounts of data")
  (flycheck-checker-error-threshold 600 "Increase error threshold from 400 to 600")
  :init (use-package lsp-ui
          :commands lsp-ui-mode
          :custom
          (lsp-ui-sideline-show-code-actions t)
          (lsp-ui-peek-enable t)
          :after lsp-mode)
        (use-package helm-lsp
          :commands helm-lsp-workspace-symbol
          :after (helm lsp-mode))
        (use-package lsp-treemacs
          :commands lsp-treemacs-error-list
          :after lsp-mode))

(use-package flycheck
  :commands flycheck-mode
  :hook flycheck-color-mode-line-mode
  :init (use-package flycheck-color-mode-line
          :ensure t
          :after flycheck))

;;; For all programming modes that I use
(use-package prog-mode
  :ensure nil
  :bind (:map prog-mode-map
              ("C-c c" . #'mw-insert-triplet)
              ("C-c d d" . #'mw-debug-comment)
              ("C-c d r" . #'mw-remove-debug)))

(use-package cc-mode
  :ensure nil
  :commands (c++-mode c-mode awk-mode java-mode)
  :chords (:map c++-mode-map
                ("pq" . mw-insert-curly-braces))
  :bind (:map c-mode-base-map
              ("C-c f" . #'mw-func-header))
  :init
  (add-hook 'c-mode-common-hook #'common-settings)
  (add-hook 'c++-mode-hook #'c++-settings)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  :preface
  (defun common-settings()
    (superword-mode t) ; underscores
    (subword-mode t) ; camel-case
    (auto-revert-mode t)
    (c-set-offset 'case-label '+) ; indent case statements
    (setq compilation-scroll-output 'first-error
          c-doc-comment-style '((java-mode . javadoc)
                                (pike-mode . autodoc)
                                (c-mode . doxygen)
                                (c++-mode . doxygen)))
    (font-lock-add-keywords nil '(("\\<\\(TBD\\|TODO\\|FIXME\\|DEBUG\\)"
                                   1
                                   font-lock-warning-face)))
    ;;                                      append ---.  .--- buffer-local
    ;;                                                |  |
    (add-hook 'before-save-hook #'whitespace-cleanup nil t))
  (defun c++-settings()
    (c-set-offset 'inclass '++)
    (c-set-offset 'access-label '-)))
    ;    (setq c-basic-offset 2)

(use-package go-mode
  :hook (('go-mode . #'lsp-deferred)
         ('go-mode . #'mw-add-go-company-backend)
         ('go-mode . #'lsp-go-install-save-hooks-local)
         ('go-mode . #'flycheck-golangci-lint-setup))
  :chords (:map go-mode-map
                ("pq" . mw-insert-curly-braces))
  :preface
  (defun lsp-go-install-save-hooks-local ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (defun mw-add-go-company-backend()
    "Add company-go to company-backends only for Go buffers."
    (set (make-local-variable 'company-backends) '(company-go)))
  :init
  (use-package company-go :after company-mode)
  (use-package flycheck-golangci-lint :after (flycheck go-mode) )
  :config
  ;; Configure some projectile settings:
  ;; want to set this to a buffer-local variable instead of polluting the global variable further.
  (setq-local projectile-project-root-files-top-down-recurring
              (append '("go.mod" ".git") projectile-project-root-files-top-down-recurring))
  (setq-local projectile-project-root-files
              (append '("main.go") projectile-project-root-files))
  (setq-local projectile-project-test-suffix "_test.go"))
  
(use-package v-mode
  :custom (lsp-v-vls-executable "vls_linux_x64"))

(use-package yaml-mode)
(use-package toml-mode)
(use-package cql-mode)
(use-package systemd
  :init (use-package helm-systemd
          :after (helm systemd)))

(use-package cmake-mode
  :hook cmake-font-lock-activate
  :init (use-package cmake-font-lock
          :commands cmake-font-lock-activate
          :after cmake-mode)
        (use-package eldoc-cmake
          :hook (cmake-mode . eldoc-cmake-enable)))

;; ;; (use-package smartparens
;; ;;   :ensure t
;; ;;   :hook
;; ;;   ((emacs-lisp-mode lisp-mode) . smartparens-mode)
;; ;;   :init
;; ;;   (use-package smartparens-config))

(use-package org
  :ensure nil
  :bind (;; Global bindings
         ("C-c o l" . #' org-store-link)
         ("C-c o a" . #' org-agenda)
         ("C-c o c" . #' org-capture)
         :map org-mode-map
         ("C-c b" . #'mw-dnd/make-battle-chart-from-region)
         ("C-c r" . #'mw-org-table-recalc))
  :custom ((fill-column 86 "Set `auto-fill-mode' fill column to something reasonable.")
           (org-hide-emphasis-markers t "Hiding markup characters makes for a cleaner looking buffer.")
           (org-hide-leading-stars t "Omit leading stars in subheadings.")
           (org-pretty-entities t "Show entities as UTF8 characters.")
           (org-fontify-done-headline t "Fontify the whole headline when it is done.")
           (org-startup-indented t "Enforce proper indentation of headlines.")
           (org-log-done "time" "Insert timestamp when a task is marked as 'Done'")
           (org-todo-keywords
            '((sequence "TODO(t)" "CURR(c)" "|" "DONE(d)"))))
  :preface
  (defun mw-org-table-recalc()
    "Recalculate an org table.

Position the cursor within an org table and call this function to
recalculate any formulas that exist within it."
    (interactive)
    (org-table-recalculate 'all))
  :init
  (use-package ob-go :after org)
  (use-package ob-rust :after org)
  (use-package ob-restclient :after org)
  :config
  (require 'ox-confluence nil 'no-error)
  (require 'ox-md nil 'no-error)
  (require 'ox-man nil 'no-error)
  (require 'mw-dnd nil 'no-error)
  (require 'c2-rowing)
  (setq org-export-backends '(ascii html icalendar latex confluence md man))
  ;; Add minimal support for generally unsupported modes.
  (add-to-list 'org-src-lang-modes '("CQL" . "cql-mode"))
  ;; Enable some languages in org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t) ; C, C++, and D are all handled by same ob-C.el file.
     (go . t)
     (emacs-lisp . t)
     (lisp . t)
     (plantuml . t)
     (python . t)
     (perl . t)
     (lua . t)
     (shell . t)
     (latex . t)
     (rust . t)
     (sql . t)))
  (add-hook 'org-mode-hook #'turn-on-auto-fill)
  (add-hook 'org-mode-hook #'turn-on-auto-revert-mode))

(use-package restclient
  :commands restclient-mode
  :mode (("\\.restclient\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode))
  :preface
  (defun mw-restclient-get-session ()
    "Get a session token returned from a REST login call."
    (interactive)
    (message "%s"
             (setq-local session-var
                         (with-current-buffer "*HTTP Response*"
                           (goto-char (point-min))
                           (when (search-forward-regexp "\"session\": \"\\(.*\\)\"" nil t)
                             (match-string 1))))))
  :bind (:map restclient-mode-map
              ("C-c r s" . #'mw-restclient-get-session))
  :init
  (use-package restclient-helm :after restclient)
  (use-package restclient-test :after restclient)
  (use-package company-restclient :after restclient))

(use-package scad-mode)

(use-package scad-dbus
  :commands scad-dbus-connected
  :after scad-mode
  :straight (:host github :repo "Lenbok/scad-dbus" :branch "master")
  :bind (:map scad-mode-map
              ("C-c s" . 'hydra-scad-dbus/body)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Set GC to something reasonable
;;
(setq gc-cons-threshold (* 2 1024 1024))

;;; init.el ends here

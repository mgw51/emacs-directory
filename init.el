;;; -*- lexical-binding: t; -*-

;;; init.el -- My emacs init file.
;;; Commentary:
;;;
;;;
;;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.
(setq gc-cons-threshold (* 100 1024 1024))

;; ;; Bootstrap straight package manager
;; (require 'straight-bootstrap)
;; (straight-bootstrapper)

;;; General Emacs config
(use-package emacs
  :ensure nil
  :demand t
  :bind (("C-x C-b" . #'ibuffer))
  :config
  (progn
    ;; Shadow `default-directory' with a temporary value so we can:
    ;;  1. selectively add to our load path based on the current value of
    ;;     `default-directory', and
    ;;  2. recursively add subdirectories
    ;;
    (let ((default-directory (expand-file-name "lisp/" user-emacs-directory)))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))
    ;;; Set and load custom config file as early as possible because we
    ;;; want all package-generated config to go into a custom config
    ;;; file set here -- not into our init.el file.
    (require 'mw-basic-setup)
    (basic/set-and-load-custom-config-file)
    (basic/setup-package-el '(("gnu" . "http://elpa.gnu.org/packages/")
                              ("melpa" . "https://melpa.org/packages/")
                              ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                              ("melpa-stable" . "https://stable.melpa.org/packages/")))
    (basic/use-package-setup '((use-package-verbose . t)
                               (use-package-compute-statistics . t)
                               (use-package-always-defer . t)
                               (use-package-always-ensure . t)))
    (basic/ui-setup)
    (basic/quality-of-life)))

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
  :after cc-mode
  :commands (mw-func-header mw-include-guard mw-create-basic-makefile mw-create-cmakelists))

;;; Configure everything else
(use-package exec-path-from-shell
  :demand t
  :config
  ;; Copy important environment variables into emacs session
  ;; Mainly needed because OSX and systemd do not pass user env to emacs
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package vterm
  :if (locate-file "libvterm" '("/usr/lib/x86_64-linux-gnu") '("a" "so")))

(use-package plantuml)

(use-package key-chord)

(use-package gptel
  :commands (gptel-send gptel-add gptel-add-file gptel-menu gptel)
  :bind ("C-c g" . gptel-prefix-map)
  :init
  (define-prefix-command 'gptel-prefix-map)
  :bind (:map gptel-prefix-map
              ("RET" . #'gptel-send) ; or prefix arg to enter menu
              ("s" . #'gptel-send) ; or prefix arg to enter menu
              ("a" . #'gptel-add) ; add context
              ("f" . #'gptel-add-file) ; add file(s) as context
              ("m" . #'gptel-menu) ; open transient menu
              ("r" . #'gptel-rewrite) ; re-write or refactor region
              ("c" . #'gptel))) ; open a dedicated chat buffer

(use-package hippie-expand
  :ensure nil
  :bind ([remap dabbrev-expand] . 'hippie-expand))

(use-package abbrev-mode
  :ensure nil
  :custom
  (save-abbrevs 'silently)
  (abbrev-file-name (expand-file-name "abbrev_defs")))

(use-package use-package-chords
  :demand t
  :config (key-chord-mode 1))

(use-package avy
  :bind-keymap ("C-c a" . avy-command-map)
  :bind (:prefix-map avy-command-map ; define the variable 'avy-command-map' as a prefix
           :prefix "C-c a"
           ("c 2" . #'avy-goto-char-2)
           ("c 1" . #'avy-goto-char)
           ("c t" . #'avy-goto-char-timer)
           ("w" . #'avy-goto-word-1)
           ("l" . #'avy-goto-line)))
 
(use-package tiny
  ; Insert ranges of all types (text, numbers, code, etc)
  :demand t
  :bind ("C-c ;" . #'tiny-expand))

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
  (blackout 'auto-revert-mode " A↻")
  (blackout 'magit-auto-revert-mode " M↻")
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
  :custom
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 1)
  :config
  (global-company-mode))

(use-package yasnippet
  :blackout (yas-minor-mode . " Ȳ")
  :commands yas-reload-all
  :init (yas-global-mode 1)
  :custom
  (yas-indent-line 'auto "Indent each line of the snippet with 'indent-according-to-mode'.")
  (yas-also-indent-empty-lines t "Also indent empty lines according to mode.")
  (yas-also-auto-indent-first-line t "Indent first line according to mode.")
  :config
  (yas-reload-all))

(use-package magit
  :ensure t
  :commands magit-status
  :bind ("C-c C-g" . #'magit-status))

(use-package vertico
  :init
  (vertico-mode)
  :hook (rfn-eshadow-update-overlay . #'vertico-directory-tidy)
  :bind
  (:map vertico-map
        ("RET" . #'vertico-directory-enter)
        ("DEL" . #'vertico-directory-delete-char)
        ("C-l" . #'vertico-directory-delete-word))
  :custom
  (vertico-resize t))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("C-x M-c" . consult-company)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)              
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(use-package consult-lsp)
(use-package consult-projectile)
(use-package consult-flycheck)

(use-package marginalia
  ;; add annotations to the minibuffer entries
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  ;; Adding the marginalia-mode call to the init section forces this package to be
  ;; loaded immediately, not deferred.
  :init
  (marginalia-mode))

(use-package embark
  :bind
  (("C-c M-e" . embark-act)         ;; pick some comfortable binding
   ("C-c e" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  ;; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  ;; fuzzy search in completion buffers
  :custom
  (completion-styles '(orderless basic))
  (completion-catagory-overrides '((file (styles basic partial-completion)))))

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              ("C-c p h" . #'consult-projectile))
  :preface
  (defun mw-vlang-same-directory-src-and-test-files(dir)
    "vlang test files reside in the same directory as the corresponding source files."
    dir)
  (defun mw-sensacloudapi-test-dir(dir)
    "Point to the location of test files for this project.\n\nA function takes precedence over a simple string."
    (concat (projectile-project-root) "test/unit_tests/"))
  :custom
  (projectile-mode-line-prefix " ¶")
  (projectile-completion-system 'auto) ; use `completing-read'
  (projectile-cache-file (concat (expand-file-name user-emacs-directory) "projectile/projectile.cache"))
  (projectile-enable-caching t)
  (projectile-enable-cmake-presets t)
  (projectile-create-missing-test-files t)
  :init (use-package ag
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
                                    :compile "make -kj20"
                                    :test "test/unit_tests/unit_tests"
                                    :test-dir #'mw-sensacloudapi-test-dir
                                    :test-suffix "_test")
  (projectile-register-project-type 'reports '(".c++rep") ; C++ autotools (work-specific)
                                    :project-file ".sensa-reports"
                                    :compile "make -kj20"
                                    :test "test/unit_tests"
                                    :test-dir "test"
                                    :test-suffix "_test")
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :project-file "CMakeLists.txt"
                                    :compilation-dir "build"
                                    :test-suffix "_test"
                                    :test-dir "test"
                                    :src-dir "src"))

(use-package lsp-mode
  ;;; See: https://emacs-lsp.github.io/lsp-mode/
  :bind-keymap ("C-c l" . lsp-command-map)
  :bind (:map lsp-command-map
              ("g i" . #'lsp-ui-imenu)
              ("g t" . #'lsp-treemacs-type-hierarchy)
              ("n" . #'lsp-ui-find-next-reference)
              ("p" . #'lsp-ui-find-prev-reference))
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (sh-mode . lsp-deferred)
         (python-mode . lsp-deferred))
  :preface (setenv "LSP_USE_PLISTS" "true") ; Use of plists is recommended: https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
  :custom
  (lsp-prefer-flymake nil "Use flycheck instead")
  (lsp-auto-guess-root t "Uses projectile, when available")
  (lsp-auto-configure t)
  (lsp-enable-on-type-formatting nil "Disable LSP's attempts to format code")
  (read-process-output-max (* 1024 1024 2) "Increase the process output max because code servers may return large amounts of data")
  (flycheck-checker-error-threshold 600 "Increase error threshold from 400 to 600")
  :config
    (when (string-equal-ignore-case (system-name) "sensa-ripper")
      ;; sometimes the executable location must be set manually or you get errors because
      ;; it defaults to the system version of clangd, which is OLD.
      (setq lsp-clients-clangd-executable "/home/mwood/.emacs.d/.cache/lsp/clangd/clangd_15.0.6/bin/clangd")
      (setq lsp-clients-clangd-args '("--enable-config" "--header-insertion-decorators=0")))
  :init (use-package lsp-ui
          :commands lsp-ui-mode
          :custom
          (lsp-ui-sideline-show-code-actions t)
          (lsp-ui-peek-enable t)
          :after lsp-mode)
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

(use-package ansi-color
  ;;; Should interpret ansi color codes in compilation buffer.  Found at:
  ;;; http://disq.us/p/2gdjkr9 on the Endless Parentheses blog and adapter for use
  ;;; here.
  :preface
  (defun colorize-compilation-buffer ()
    (when (derived-mode-p 'compilation-mode)
      (ansi-color-process-output nil)
      (setq-local comint-last-output-start (point-marker))))
  :hook ('compilation-filter . #'colorize-compilation-buffer))

(use-package cc-mode
  :ensure nil
  :commands (c++-mode c-mode awk-mode java-mode)
  :chords (:map c++-mode-map
                ("pq" . mw-insert-curly-braces))
  :bind (:map c-mode-base-map
              ("C-c f" . #'mw-func-header))
  :init
  (add-hook 'c-mode-common-hook #'common-settings)
  (add-hook 'c-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook #'c++-settings)
  (add-hook 'c++-mode-hook #'lsp-deferred)
  :preface
  (defun common-settings()
    (superword-mode t) ; underscores
    (subword-mode t) ; camel-case
    (auto-revert-mode t)
    (c-set-offset 'case-label '+) ; indent case statements
    (setq compilation-scroll-output 'just-keep-going!!!
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
  :hook lsp-deferred
  :custom (lsp-v-vls-executable "vls_linux_x64"))

(use-package elixir-mode
  :hook #'lsp-deferred)

(use-package slime
  :if (executable-find "sbcl")
  :init
  (progn
    (require 'slime-autoloads)
    (add-hook 'slime-mode-hook
              (lambda ()
                (unless (slime-connected-p)
                  (save-excursion (slime))))))
  :config
  (progn
    (use-package slime-company)
    (setf inferior-lisp-program "sbcl")
    (slime-setup '(slime-fancy slime-company))
    (setq slime-net-coding-system 'utf-8-unix)))

(use-package yaml-mode)
(use-package toml-mode)
(use-package cql-mode)
(use-package systemd)

(use-package cmake-mode
  :hook cmake-font-lock-activate
  :init (use-package cmake-font-lock
          :commands cmake-font-lock-activate
          :after cmake-mode)
        (use-package eldoc-cmake
          :hook (cmake-mode . eldoc-cmake-enable)))

(use-package smartparens
  :hook ((emacs-lisp-mode lisp-mode) . smartparens-mode)
  :custom
  (sp-base-key-bindings sp-smartparens-bindings)
  :config
  ;; The following comes from the smartparens docs: https://readthedocs.org/projects/smartparens/downloads/pdf/latest/
  (sp-with-modes 'emacs-lisp-mode
    ;; only use the psuedo-quote inside strings where it serves as a hyperlink
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
    ;; disable "'", it's the quote character!
    (sp-local-pair "'" nil :actions nil)))

(use-package paren
  ;; This is the built-in mode
  :ensure nil
  :custom
  (show-paren-style 'mixed "Highlight matching paren if it is visible in the window, the expression otherwise")
  (show-paren-when-point-inside-paren t "Cause paren highlighting when point is inside-adjacent to one of them")
  (show-paren-when-point-in-periphery t "Highlight parens when in periphery and otherwise adjacent to one of them")
  (show-paren-context-when-offscreen t "Show context in echo area if matching paren is offscreen"))

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
            '((sequence "TODO(t)" "CURR(c)" "|" "DONE(d)")))
           (org-todo-keyword-faces
            '(("CURR" . org-curr))))
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
  (defface org-curr		    ;Copied from `org-faces.el'
    ;; Define an org-face for the Current todo keyword
    '((((class color) (min-colors 16) (background light)) (:foreground "gold" :bold t))
      (((class color) (min-colors 16) (background dark)) (:foreground "gold" :bold t))
      (((class color) (min-colors 8)) (:foreground "gold"))
      (t (:bold t)))
    "Face used for todo keywords that indicate CURR items."
    :group 'org-faces)
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
  (use-package restclient-test :after restclient)
  (use-package company-restclient :after restclient))


(use-package auth-source
  :demand t
  :config
  (add-to-list 'auth-sources "~/.config/emacs/.authinfo"))

(use-package scad-mode)

;; (use-package scad-dbus
;;   :commands scad-dbus-connected
;;   :after scad-mode
;;   :straight (:host github :repo "Lenbok/scad-dbus" :branch "master")
;;   :bind (:map scad-mode-map
;;               ("C-c s" . 'hydra-scad-dbus/body)))

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs ready in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))

;; Set GC to something reasonable
;;
;(setq gc-cons-threshold (* 2 1024 1024))

;;; init.el ends here

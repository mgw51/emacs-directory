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
(let ((default-directory (expand-file-name "lisp/")))
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

(use-package exec-path-from-shell
  :demand t
  :config
  ;; Copy important environment variables into emacs session
  ;; Mainly needed because OSX and systemd do not pass user env to emacs
  (when (daemonp)
    (exec-path-from-shell-initialize)))

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
  (blackout 'eldoc-mode " ℓ"))

(use-package which-key
  :demand t
  :blackout
  :config
  (which-key-mode))

(use-package company
  :demand t
  :blackout " Ç"
  :hook (after-init-hook . #'global-company-mode))

(use-package yasnippet
  :blackout (yas-minor-mode . " ꪗ")
  :commands yas-reload-all
  :init (yas-global-mode 1))

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
  :custom
  (projectile-mode-line-prefix " ¶")
  (projectile-completion-system 'helm)
  (projectile-cache-file (concat (expand-file-name user-emacs-directory) "projectile/projectile.cache"))
  (projectile-enable-caching t)
  (projectile-enable-cmake-presets t)  
  :config
  (use-package helm-projectile
    :after projectile))

(use-package ag
  :after projectile)

(use-package lsp-mode
  :bind-keymap ("C-c l" . lsp-command-map)
  :bind (:map lsp-command-map
	      ("g i" . #'lsp-ui-imenu))
  :commands lsp
  :custom
  (lsp-prefer-flymake nil "Use flycheck instead")
  (lsp-auto-guess-root t "Uses projectile, when available")
  (lsp-auto-configure t)
  (lsp-enable-on-type-formatting nil "Disable LSP's attempts to format code")
  (read-process-output-max (* 1024 1024 2) "Increase the process output max because code servers may return large amounts of data")
  :config
  (use-package lsp-ui
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
  :config
  (use-package flycheck-color-mode-line
    :ensure t
    :after flycheck))

(use-package cc-mode
  :ensure nil
  :commands (c++-mode c-mode awk-mode java-mode)
  :init
  (add-hook 'c-mode-common-hook #'common-settings)
  (add-hook 'c++-mode-hook #'c++-settings)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  :preface
  (defun common-settings()
    (yas-minor-mode)
    (superword-mode t) ; underscores
    (subword-mode t) ; camel-case
    (auto-revert-mode t)
    (setq compilation-scroll-output 'first-error
          c-doc-comment-style 'doxygen)
    ;;                                      append ---.  .--- buffer-local
    ;;                                                |  |
    (add-hook 'before-save-hook #'whitespace-cleanup nil t))
  (defun c++-settings()
    (c-set-offset 'inclass '++)
    (c-set-offset 'access-label '-)
    ;    (setq c-basic-offset 2)
    ))

;; ;; (use-package smartparens
;; ;;   :ensure t
;; ;;   :hook
;; ;;   ((emacs-lisp-mode lisp-mode) . smartparens-mode)
;; ;;   :init
;; ;;   (use-package smartparens-config))

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

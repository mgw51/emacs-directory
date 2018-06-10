;;; init-custom.el --- Customization file for use by Emacs packages.
;;;
;;; Commentary:
;;;    Provides a file separate from `init.el' into which Emacs packages may
;;;    write automatically-generated customization information.  Emacs
;;;    packages should not be writing to the main init file, and this
;;;    prevents that from happening.

;;; Code:
;;;
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
;;; init-custom.el ends here

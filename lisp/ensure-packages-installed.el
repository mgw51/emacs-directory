(provide 'ensure-packages-installed)

;;;; ensure-packages-installed
;;;; Ensure that all packages specified are installed on the local system.

(defun ensure-packages-installed (package-list)
  "Ensure that all packages in PACKAGE-LIST are installed on the local system."
  (message "Checking for required packages on local system...")
  (let ((missing-package-p nil)
        (packages-updated-p nil))

    (dolist (p package-list)
      (when (not (package-installed-p p))
        (message "Install missing package \"%s\"" p)
        (setq missing-package-p t)
        (unless packages-updated-p
          (message "Update package list...")
          (package-refresh-contents)
          (setq packages-updated-p t))  ;  Perform this explicitly in case last operation in `package-refresh-contents' returns nil
        (package-install p)))
    (when (not missing-package-p)
      (message "All packages present"))))
      
      

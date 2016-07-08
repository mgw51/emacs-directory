;;;; ensure-packages-installed
;;;; Ensure that all packages specified are installed on the local system.

(provide 'ensure-packages-installed)

(defun ensure-packages-installed (package-list)
  "Ensure that all packages in PACKAGE-LIST are installed on the local system."
  (message "Checking for required packages on local system...")
  (let ((missing-package-p nil))
    (dolist (p package-list)
      (when (not (package-installed-p p))
        (message "Install missing package \"%s\"" p)
        (setq missing-package-p t)
        (package-install p)))
    (when (not missing-package-p)
      (message "All packages present"))))
      
      

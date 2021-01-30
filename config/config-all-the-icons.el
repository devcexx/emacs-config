;;; config-all-the-icons.el --- Automatically install all-the-icons package
;;; Commentary:
;;; Code:

(defun all-the-icons-fonts-install-if-not-present ()
  "Check whether the all the icons fonts are installed on the filesystem, and install it otherwise."

  (let* ((font-dest (cl-case window-system
		      (x  (concat (or (getenv "XDG_DATA_HOME")
				      (concat (getenv "HOME") "/.local/share"))
				  "/fonts/"))
		      (mac (concat (getenv "HOME") "/Library/Fonts/" ))
		      (ns (concat (getenv "HOME") "/Library/Fonts/" ))))

	 (fonts-path (mapcar (lambda (font-name) (concat font-dest font-name)) all-the-icons-font-names)))

    (if (seq-every-p #'file-exists-p fonts-path)
	(message "all-the-icons fonts seems to be installed")
      (progn (message "all-the-icons fonts needs to be installed")
	     (all-the-icons-install-fonts t)))
    ))

(when window-system
  (use-package all-the-icons
    :ensure t
    :config (all-the-icons-fonts-install-if-not-present)))

(provide 'all-the-icons-fonts-install-if-not-present)
(provide 'config-all-the-icons)
;;; config-all-the-icons.el ends here

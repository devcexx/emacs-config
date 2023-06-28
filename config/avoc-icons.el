;;; avoc-icons.el --- All the icons setup.

;;; Commentary:
;;; Configures all-the-icons pacakge, by automatically installing
;;; all-the-icons fonts if required.

;;; Code:
(require 'cl-lib)

(defun avoc-icons--font-installed-p (fonts)
  "Check whether the all given FONTS are installed."

  (let* ((font-dest (cl-case system-type
		      (gnu/linux  (concat (or (getenv "XDG_DATA_HOME")
				      (concat (getenv "HOME") "/.local/share"))
				  "/fonts/"))
		      (darwin (concat (getenv "HOME") "/Library/Fonts/" ))))

	 (fonts-path (mapcar (lambda (font-name) (concat font-dest font-name)) fonts)))

    (seq-every-p #'file-exists-p fonts-path)))

(defun avoc-icons--install-nerd-icons-if-not-present ()
  "Check whether the nard icons fonts are installed on the filesystem, and install it otherwise."
  (unless (avoc-icons--font-installed-p nerd-icons-font-names)
    (message "nerd-icons fonts needs to be installed")
    (nerd-icons-install-fonts t)))

(defun avoc-icons--install-all-the-icons-if-not-present ()
  "Check whether the all the icons fonts are installed on the filesystem, and install it otherwise."
  (unless (avoc-icons--font-installed-p all-the-icons-font-names)
    (message "all-the-icons fonts needs to be installed")
    (all-the-icons-install-fonts t)))

(use-package nerd-icons
    :ensure t
    :config (avoc-icons--install-nerd-icons-if-not-present))

(when window-system
  (use-package all-the-icons
    :ensure t
    :config (avoc-icons--install-all-the-icons-if-not-present)))

(provide 'avoc-icons)
;;; avoc-all-the-icons.el ends here.

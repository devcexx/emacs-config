;;; avoc-run-mode.el --- Run mode utils.
;;;
;;; Commentary:
;;; Contains utilities and definitions of the available run modes for
;;; the Emacs config.  Each run modes define a set of features that can
;;; be either enabled or disabled.  The current run mode can be changed
;;; before the Emacs initialization, by setting the EMACS_RUN_MODE
;;; environment variable.
;;;
;;; Code:

(defconst avoc-run-mode-current 'default
  "Define the way Emacs was configured for the current instance.
The run mode affects to the way some packages or features might be loaded
or not in the init file.
There are a few run modes that might fit different use cases:
  - :default: Runs Emacs fully featured, for using it as a
    single-instance main editor.
  - :spot: Runs Emacs with most of the features, but accomodates it
    for using as a secondary editor instance, for using it for editing
    simple and very specific files.
  - :light: Runs Emacs disabling most of its features, keeping
    only basic features like some programming modes, suitable for
    running Emacs on a remote server.")

(defconst avoc-run-mode-features
  '(
    (default .
      (package-refresh
       position-beacon
       desktop-save-mode
       open-in-emacs
       linum
       theme
       nyancat
       modeline
       treemacs
       treemacs-autoshow
       fill-column-indicator
       flycheck
       company
       helm
       git
       projectile
       elcord
       lsp
       lsp-ui
       undo-tree))

    (spot .
      (package-refresh
       position-beacon
       linum
       theme
       nyancat
       modeline
       treemacs
       projectile
       lsp
       lsp-ui
       undo-tree))

    (light . (linum undo-tree company flycheck helm))))

(defmacro avoc-run-mode-features-enabled ()
  "Return the list of features currently enabled by the run mode."
  `(alist-get avoc-run-mode-current avoc-run-mode-features))

(defun avoc-run-mode-feature-enabled-p (feature)
  "Return whether the given FEATURE is currently enabled on the current run mode."
  (member feature (avoc-run-mode-features-enabled)))

(defun avoc-run-mode-features-enabled-p (features)
  "Return whether the given set of FEATURES is currently enabled on the current run mode."
   (let ((feature-list (avoc-run-mode-features-enabled)))
     (seq-every-p (lambda (feature) (member feature feature-list)) features)))

(let* ((env-name "EMACS_RUN_MODE")
       (env-value (getenv env-name))
       (mode (pcase env-value
	       (`nil 'default)
	       ("default" 'default)
	       ("spot" 'spot)
	       ("light" 'light))))

  (unless mode
    (error "Invalid run mode specified in %s: '%s'" env-name env-value))

  (setq avoc-run-mode-current mode))

(message "Run mode selected: %S" avoc-run-mode-current)

(provide 'avoc-run-mode)
;;; avoc-run-mode.el ends here

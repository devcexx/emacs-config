;;; config-theme.el --- Theme configuration
;;; Commentary:
;;; Code:

;; Default theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-Iosvkem t)
  (set-face-attribute 'tooltip nil :background "#132236"))

(provide 'config-theme)
;;; config-theme.el ends here

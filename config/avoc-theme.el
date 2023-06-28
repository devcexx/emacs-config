;;; avoc-theme.el --- Theme configuration
;;; Commentary:
;;; Code:

;; Default theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-Iosvkem t)
  (set-face-attribute 'tooltip nil :background "#132236"))

(provide 'avoc-theme)
;;; avoc-theme.el ends here

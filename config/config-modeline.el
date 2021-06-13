;;; config-modeline.el --- Modeline configuration
;;; Commentary:
;;; Code:

(when (avoc-run-mode-feature-enabled-p 'modeline)
  (use-package doom-modeline
    :ensure t
    :after all-the-icons
    :config (doom-modeline-mode 1))

  (when (avoc-run-mode-feature-enabled-p 'nyancat)
    (use-package nyan-mode ;; Essential
      :ensure t
      :after doom-modeline
      :config
      (nyan-mode)
      (nyan-start-animation)))

  (use-package fancy-battery
    :ensure t
    :after doom-modeline))

(provide 'config-modeline)
;;; config-modeline.el ends here

;;; config-modeline.el --- Modeline configuration
;;; Commentary:
;;; Code:

(use-package doom-modeline
  :ensure t
  :defer t
  :after all-the-icons
  :init (doom-modeline-mode 1))

(use-package nyan-mode ;; Essential
  :ensure t
  :after doom-modeline
  :config
  (nyan-mode)
  (nyan-start-animation))

(use-package fancy-battery
  :ensure t
  :after doom-modeline)

(provide 'config-modeline)
;;; config-modeline.el ends here

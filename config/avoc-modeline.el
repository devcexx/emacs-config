;;; avoc-modeline.el --- Modeline configuration.

;;; Commentary:
;;; This file configures the Emacs modeline.

;;; Code:

(when (avoc-run-mode-feature-enabled-p 'modeline)
  (use-package doom-modeline
    :ensure t
    :after all-the-icons
    :config

    ;; https://github.com/seagle0128/doom-modeline/issues/505
    (setq doom-modeline-fn-alist
      (--map
       (cons (remove-pos-from-symbol (car it)) (cdr it))
       doom-modeline-fn-alist))
    
    (doom-modeline-mode 1))

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

(provide 'avoc-modeline)
;;; avoc-modeline.el ends here

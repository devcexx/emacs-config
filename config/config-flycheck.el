(use-package flycheck
  :ensure t
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix
    flycheck-command-map)

  ;; Configure how the Flycheck buffers will be shown.
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.20)))

  ;; Workaround for preventing continuation line dots to appear on the
  ;; left margin. See
  ;; https://github.com/flycheck/flycheck/issues/1886.
  (advice-add
   'flycheck--setup-highlighting
   :around
   (lambda (oldfun err overlay)
     (apply oldfun (list err overlay))
     (when (or (eq flycheck-indication-mode 'left-margin)
	       (eq flycheck-indication-mode 'right-margin))

       ;; The three dots are set on the line-prefix overlay
       ;; property. Just remove it.
       (overlay-put overlay 'line-prefix nil))))

  (global-flycheck-mode))

(provide 'config-flycheck)

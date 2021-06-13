(defun avoc--setup-winsys ()
  "Enables specific Emacs configuration when running on a terminal."

  (setq-default left-margin-width 2)

  ;; Enable terminal mouse mode. Haters gonna hate.
  (xterm-mouse-mode t)

  ;; Workaround to make scrolling work on some terminals.
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down-line 2)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up-line 2))))

(provide 'winsys-none)

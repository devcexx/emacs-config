;;; avoc-winsys-none.el --- Specific configuration for TTY displays.

;;; Commentary:
;;; This file specifies global configuration for correcatly displaying
;;; Emacs when running in a TTY display.

;;; Code:

;; Enable terminal mouse mode. Haters gonna hate.
(xterm-mouse-mode t)

;; Workaround to make scrolling work on some terminals.
(global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down-line 2)))
(global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up-line 2)))

(provide 'avoc-winsys-none)

;;; avoc-winsys-graphic.el --- Specific configuration for graphical displays.

;;; Commentary:
;;; This file specifies global configuration for correctly displaying
;;; Emacs when running in a graphical display.

;;; Code:

;; Set default fonts
(add-to-list 'default-frame-alist
	     '(font . "Hack-11"))

(provide 'avoc-winsys-graphic)

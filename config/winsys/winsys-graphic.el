(defun emacs-setup-winsys-graphic ()
  "Enables specific Emacs configuration when running on a graphical interface (any of them)."

  ;; Set default fonts
  (add-to-list 'default-frame-alist
	       '(font . "Hack-11")))

(provide 'winsys-graphic)

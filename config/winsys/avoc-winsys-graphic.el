(defun avoc--setup-winsys ()
  "Enables specific Emacs configuration when running on a graphical interface (any of them)."

  ;; Set default fonts
  (add-to-list 'default-frame-alist
	       '(font . "Hack-11")))

(provide 'avoc-winsys-graphic)

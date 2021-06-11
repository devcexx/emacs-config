(defun emacs-setup-winsys-graphic ()
  "Enables specific Emacs configuration when running on a graphical interface (any of them)."

  ;; Setup fringes
  (set-fringe-mode '(8 . 0))

  ;; Set default fonts
  (add-to-list 'default-frame-alist
	       '(font . "Hack-11")))

(provide 'winsys-graphic)

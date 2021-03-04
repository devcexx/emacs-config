(defun linum-relative-transient ()
  (interactive)
  (when linum-mode
    (linum-mode -1)
    (linum-relative-mode 1)
    (set-transient-map nil nil (lambda () (linum-relative-mode -1) (linum-mode 1)))))

(provide 'linum-relative-transient)
(provide 'config-linum-relative)

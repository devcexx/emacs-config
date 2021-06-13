;;; avoc-linum-relative.el --- Linum relative config.

;;; Commentary:
;; This file configures linum relative and a transient mode for it.

;;; Code:

(defun avoc-linum-relative-show-transient ()
  (interactive)
  (when linum-mode
    (linum-mode -1)
    (linum-relative-mode 1)
    (set-transient-map nil nil (lambda () (linum-relative-mode -1) (linum-mode 1)))))

(use-package linum-relative
  :ensure t
  :commands linum-relative-mode)

(global-set-key (kbd "<f6>") 'avoc-linum-relative-show-transient)

(provide 'avoc-linum-relative)
;;; avoc-linum-relative.el ends here

;;; avoc-projects.el --- Projects-related packages and configuration
;;; Commentary:
;;; Code:

(require 'avoc-util)
(require 'avoc-run-mode)

;; Projectile: project management for Emacs
(when (avoc-run-mode-feature-enabled-p 'projectile)
  (use-package projectile
    :ensure t
    :config
    (projectile-mode)
    (setq projectile-enable-caching t))

  (use-package ag
    :ensure t)

  (use-package helm-ag
    :ensure t)

  (when (avoc-run-mode-feature-enabled-p 'helm)
    (use-package helm-projectile
      :ensure t
      :after ((projectile))
      :bind (("C-x p p" . helm-projectile)
	     ("C-x p s" . helm-projectile-ag)
             ("C-x p P" . helm-projectile-switch-project)))))

(when (avoc-run-mode-feature-enabled-p 'treemacs)
  (use-package treemacs
    :ensure t
    :demand t
    :config
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-follow-mode -1)

    ;; Only show Treemacs automatically when activated and there's any
    ;; project to show. Otherwise, Treemacs will interactively ask
    ;; user to add a new one, which is horrible to have it during
    ;; Emacs initialization.
    (when (and (avoc-run-mode-feature-enabled-p 'treemacs-autoshow) (not (treemacs-workspace->is-empty?)))
      (add-hook 'window-setup-hook
		(lambda ()
		  (let ((last-window (selected-window)))
		    (treemacs)
		    (select-window last-window))
		  )))

    :bind
    ([f8] . treemacs)
    ("C-c t l" . treemacs-find-file))

  (when (avoc-run-mode-feature-enabled-p 'projectile)
    (use-package treemacs-projectile
      :ensure t
      :after treemacs projectile
      :bind ("C-x p a" . treemacs-add-project))))

(defun projectile-kill-non-project-buffers ()
  "Kill all the buffers that doesn't belong to the current project."

  (interactive)
  (let ((root (projectile-project-root)) (bufs (buffer-list (selected-frame))))
    (when (null root) (user-error "Not in a Projectile buffer"))
    (when (yes-or-no-p (format "Do you want to kill all the buffers that doesn't belong to \"%s\"? " root))
      (dolist (buf bufs)
	(let ((buf-name (buffer-name buf)))
	  ; " ?" -> Treemacs buffers has an space at the beginning, because potato.
	  (unless (or (projectile-project-buffer-p buf root)
		      (string-match "^ ?\\*\\(\\scratch\\|Messages\\|Treemacs\\|tab-line-hscroll\\)" buf-name))

	    (message "Killing buffer '%s'" buf-name)
            (kill-buffer buf)))))))

(provide 'projectile-kill-non-project-buffers)
(provide 'avoc-projects)
;;; avoc-projects.el ends here

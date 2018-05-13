;;; package --- Summary
;;; Commentary:
;;; Code:

;; Disable right option modifier key on macOS
(cond ((string-equal system-type "darwin")
       (setq mac-right-option-modifier nil)))

;; Enable desktop save mode
(desktop-save-mode 1)


;; Move temporal files to Emacs folder
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/temp")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/temp" t)))

;; Init repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Theme loading
(use-package darkokai-theme
  :ensure t
  :config (load-theme 'darkokai t))

;; WindMove: move between buffers using shift+arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Disable bars and unnecesary menus
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

;; General packages

;; Flycheck: syntax check on the fly
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; exec-path-from-shell: Set the Emacs path value
;; to the value of the user shell PATH variable value.
(use-package exec-path-from-shell
  :ensure t
  :config
  (add-hook 'after-init-hook 'exec-path-from-shell-initialize))

;; Projectile: project management for Emacs
(use-package projectile
  :ensure t)

;; Helm: enhaced completion window.
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-buffers-list)
	 ("C-x C-f" . helm-find-files))
  :init (setq helm-split-window-inside-p t)
  :config (helm-autoresize-mode 1))

;; Multi-term: terminal for emacs.
(use-package multi-term
  :ensure t
  :config (setq multi-term-program "/bin/zsh")
  :bind ("C-x t" . multi-term-dedicated-open))

;; company: autocompletion.
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  
  :bind
  ("C-c SPC" . company-complete)
  ("C-c C-SPC" . company-complete))
(add-hook 'after-init-hook 'global-company-mode)

;; Magit: Git client
(use-package magit
  :ensure t
  :bind (("C-x v" . magit-status)
         ("C-x M-v" . magit-dispatch-popup)))

;; Language-specific packages

;; Markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; autex: LaTeX integration
(use-package auctex
  :defer t
  :ensure t)

;; js2-mode: Javascript integration
(use-package js2-mode
  :ensure t
  :bind (("C-x n" . js2-next-error))
  )
(add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode))

(defun kill-buffers()
  (let (buffer buffers)
    (setq buffers (buffer-list))
    (dotimes (i (length buffers))
      (setq buffer (pop buffers))
      (if (not (string-equal (buffer-name buffer) "*scratch*")) (kill-buffer buffer) nil))))

(defun clean-buffers()
  (interactive)
  (if (yes-or-no-p "Do you really want to clean all buffers? ")
      (kill-buffers) nil))

(global-set-key (kbd "C-x C-k") 'clean-buffers)

(provide 'clean-buffers)

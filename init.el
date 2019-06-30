;;; init.el --- Emacs initialization file
;;; Commentary:
;;; Code:

;; Disable right option modifier key on macOS
(cond ((string-equal system-type "darwin")
       (setq mac-right-option-modifier nil)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

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

;; Configure status bar
(custom-set-variables
'(display-time-24hr-format t)
'(display-time-default-load-average nil)
'(display-time-mode t))

(use-package smart-mode-line
  :ensure t
  :config
  )

(use-package smart-mode-line-powerline-theme
  :ensure t
  :after smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/name-width 15)
  (setq powerline-height 20)
  
  (set-face-attribute 'powerline-active1 nil :foreground "#FFFFFF" :background "DarkOrange")
  (set-face-attribute 'powerline-active2 nil :foreground "#FFFFFF" :background "#000000")

  (custom-set-faces
   '(sml/filename ((t (:inherit sml/global :background "DarkOrange" :foreground "Black"))))
   '(sml/modes ((t (:inherit sml/global :background "#000000" :foreground "White"))))
   '(sml/position-percentage ((t (:inherit sml/prefix :background "#000000" :foreground "White" :weight normal))))
   '(sml/prefix ((t (:inherit sml/global :background "DarkOrange" :foreground "Black"))))
   '(sml/vc ((t (:inherit sml/git :background "#000000" :foreground "#aa0000"))))
   '(sml/vc-edited ((t (:inherit sml/prefix :background "#000000" :foreground "White")))))

  (sml/setup)
  (sml/apply-theme 'powerline))

(setq neo-window-width 35)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-smart-open t)

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :after (all-the-icons)
  :ensure t
  :bind ([f8] . neotree-toggle))

;; WindMove: move between buffers using shift+arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(load-file "~/.emacs.d/buffer-move.el")

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

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
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-enable-caching t))


(use-package helm-projectile
  :ensure t
  :after ((projectile))
  :bind (("C-c p" . helm-projectile)
         ("C-c P" . helm-projectile-switch-project)))

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

(use-package company-terraform
  :ensure t
  :init (company-terraform-init))

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
  :bind (("C-x n" . js2-next-error)))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode))

;; rust-mode: Rust integration
(use-package rust-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq flycheck-rust-cargo-executable "~/.cargo/bin/cargo")

;; racer: Rust completion through Racer
;; (requires installing Rust source code and racer):
;; $ rustup component add rust-src
;; $ cargo install racer
;; May be required to set the RUST_SRC_PATH variable environment to
;; the path where it's located the Rust source code on the system
(use-package racer
  :ensure t
  :bind (("C-?" . racer-describe)))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(unless (package-installed-p 'elcord)
  (package-install 'elcord))

(require 'elcord)
(elcord-mode)

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

(provide 'clean-buffers)

(defun delete-current-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(provide 'delete-current-line)

(defun center-rectangle (beg end)
  (interactive "*r")
  (kill-rectangle beg end)
  (with-temp-buffer
    (yank-rectangle)
    (setq fill-column (current-column))
    (center-region (point-min) (point-max))
    (goto-char (point-max))
    (move-to-column fill-column t)
    (kill-rectangle (point-min) (point)))
  (goto-char beg)
  (yank-rectangle))

(provide 'center-rectangle)

;; Custom global key bindings

;; Center text in selected rectangle ("hello    " => "  hello  ").
;; Mainly added for helping me writing centered text while playing with artist mode.
(global-set-key (kbd "C-c C-r") 'center-rectangle)

;; Toggle text overwrite mode, for the same reason as above lol.
(global-set-key [C-help] 'overwrite-mode)

;; Fast close all opened buffers, for that situations that you have a thousand
;; opened buffers and you need to reboot your mind.
(global-set-key (kbd "C-x C-k") 'clean-buffers)

;; Remove the current line without copying it to the copy buffer.
(global-set-key (kbd "M-k") 'delete-current-line)

;; Shortcut to artist mode.
(global-set-key (kbd "C-c C-a") 'artist-mode)

;;; init.el --- Emacs initialization file
;;; Commentary:
;;; Code:

(defun conf-rel-path (path)
  (concat user-emacs-directory path))

;; Disable right option modifier key on macOS
(cond ((string-equal system-type "darwin")
       (setq mac-right-option-modifier nil)))

(setq custom-file (conf-rel-path "custom.el"))
(load custom-file 'noerror)

;; Enable desktop save mode
(desktop-save-mode 1)

;; Move temporal files to Emacs folder
(setq backup-directory-alist
      `((".*" . , (conf-rel-path "temp"))))
(setq auto-save-file-name-transforms
      `((".*" , (conf-rel-path "temp") t)))

;; Init repositories
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; First install use-package
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Theme
(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-Iosvkem t))

;; Modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package nyan-mode
  :ensure t
  :after doom-modeline
  :config
  (nyan-mode)
  (nyan-start-animation))

(use-package fancy-battery
  :ensure t
  :after doom-modeline)

(custom-set-variables
'(display-time-24hr-format nil)
'(display-time-default-load-average nil)
'(display-time-mode t))

;; Kawaii rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Scrolling tweaking
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
(setq scroll-step 2)

;; Fill column indicator
(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-width 2)
  (setq fci-rule-color "darkred")
  (setq fci-rule-use-dashes nil)
  (setq fci-rule-column 120)) ;; Keep disabled fci by default (gives problems with Company)

; Fuck off tabs. Still having issues with doom-modeline
; Let's just wait until Emacs 27.1 is released.

;; Cursor highlight
;; Only enabled when Emacs is running on a graphical interface
(use-package beacon
  :ensure t
  :config
  (setq beacon-color "#fc20bb")
  )

;; Disable beacon if we're on a tty
;; Dunno why but it must to be done on window-setup-hook, otherwise it does
;; not have any effect
(add-hook 'window-setup-hook (lambda () (if window-system (beacon-mode 1) (beacon-mode -1))))

(use-package browse-kill-ring
  :ensure t)

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

(load-file (conf-rel-path "buffer-move/buffer-move.el"))

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; Disable bars and unnecesary menus
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Prettify symbols mode
;; Global symbols
(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq prettify-symbols-alist
		  '(
		    ("=>"  . ?⇒)
		    ("->"  . ?→)
		    ("!=" . ?≠)
		    ("<=" . ?≤)
		    (">=" . ?≥)
		    ))
	    (prettify-symbols-mode)))

;; Mode-specific symbols
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq prettify-symbols-alist
		  (append
		   prettify-symbols-alist
		   '(
		     ("lambda" . ?λ)
		     )))
	    (prettify-symbols-mode)
	    ))

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq prettify-symbols-alist
		  (append
		   prettify-symbols-alist
		   '(
		     ("lambda" . ?λ)
		     )))
	    (prettify-symbols-mode)
	    ))

;; Elcord: support for Discord. The elcord folder contains a git
;; submodule that points to a custom elcord mode without reconnect
;; messages repeating each 15 seconds.
(add-to-list 'load-path (conf-rel-path "elcord/"))
(require 'elcord)
(setq elcord-silent-mode 1)
(elcord-mode)

;; Undo tree
(unless (package-installed-p 'undo-tree)
  (package-install 'undo-tree))

(require 'undo-tree)
(global-undo-tree-mode)

;; General packages

;; Flycheck: syntax check on the fly
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; LSP Mode
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config (require 'lsp-clients)
  (setq lsp-lens-auto-enable t)
  (setq lsp-headerline-breadcrumb-enable t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-update-mode "point")
  (setq lsp-ui-sideline-delay 0.2)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

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
  (setq company-minimum-prefix-length 1)
  
  :bind
  ("C-c SPC" . company-complete)
  ("C-c C-SPC" . company-complete))
(add-hook 'after-init-hook 'global-company-mode)

(use-package company-terraform
  :ensure t
  :init (company-terraform-init))

(use-package company-jedi
  :ensure t
  :init
  (add-to-list 'company-backends 'company-jedi)
  (setq jedi:tooltip-method '('popup)))

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
  :config
  (define-key js2-mode-map (kbd "M-.") nil)
  :bind (("C-x n" . js2-next-error)))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Rust integration
(use-package toml-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp))
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

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


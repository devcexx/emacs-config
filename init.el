;;; init.el --- Emacs initialization file
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic configurations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun conf-rel-path (path)
  (concat user-emacs-directory path))

(defmacro emacs-27 ()
  `(eq (symbol-value 'emacs-major-version) 27))

;; High initial GC threshold for speeding up Emacs load.
(setq gc-cons-threshold 1000000000)
(global-auto-revert-mode t)

;; Set default fonts
(when window-system
(add-to-list 'default-frame-alist
             '(font . "Hack-11")))

;; Required by lsp-mode for increasing performance.
(setq read-process-output-max (* 3 (* 1024 1024)))

;; Disable right option modifier key on macOS
(cond ((string-equal system-type "darwin")
       (setq mac-right-option-modifier nil)))

(setq custom-file (conf-rel-path "custom.el"))
(load custom-file 'noerror)

;; Enable desktop save mode
(desktop-save-mode 1)

;; Attempt to fix a bug that produces sometimes the desktop-save-mode
;; to fail reading the desktop file.
(setq desktop-restore-forces-onscreen nil)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes, decorators and visuals ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add configuration scripts from the config/ folder
(add-to-list 'load-path (conf-rel-path "config/"))

;; Ensures that all-the-icons is installed.
(require 'config-all-the-icons)
(require 'config-modeline)
(require 'config-theme)
(require 'config-prettify-symbols)
(require 'active-minibuffer-lock-mode)
(require 'util)

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

;; Tabs
(when (emacs-27)
  (global-tab-line-mode t)
  (set-face-attribute 'tab-line-tab-current nil :foreground "#ffffff" :background nil :inherit 'highlight)
  (set-face-attribute 'tab-line-highlight nil :foreground "#ffffff" :background "#bd158b")
  (set-face-attribute 'tab-line-tab nil :background "gray35")
  (global-set-key (kbd "C-,") 'tab-line-switch-to-prev-tab)
  (global-set-key (kbd "C-.") 'tab-line-switch-to-next-tab))

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

(use-package treemacs
  :ensure t
  :config
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-follow-mode -1)
  :bind
  ([f8] . treemacs)
  ("C-c t l" . treemacs-find-file))

(use-package treemacs-projectile
  :ensure t
  :after treemacs projectile)

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

(require 'config-prettify-symbols)

;; Enable line numbers
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'linum-mode 1))

;; I have an issue on my Emacs, where the margin gets completely
;; fucked up when zooming in and out, and seems to be related to the
;; linum-mode. Disable and enable it each time the scale factor
;; changes as a workaround.
(add-hook 'text-scale-mode-hook
	  (lambda () (when linum-mode (linum-mode -1) (linum-mode 1))))

;; Enable global hl mode
(global-hl-line-mode 1)

;; Enable active minibuffer lock mode
(active-minibuffer-lock-mode 1)

;; Highlight the minibuffer on enable
(add-hook 'minibuffer-setup-hook #'minibuffer-emph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General puropose packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elcord: support for Discord. The elcord folder contains a git
;; submodule that points to a custom elcord mode without reconnect
;; messages repeating each 15 seconds.
(add-to-list 'load-path (conf-rel-path "elcord/"))
(require 'elcord)
(setq elcord-silent-mode 1)
(elcord-mode)


(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))

;; Flycheck: syntax check on the fly
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix
    flycheck-command-map)
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.20))))

;; LSP Mode
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  (lsp-mode . lsp-signature-activate)
  (lsp-mode . lsp-ui-mode)
  :config
  (require 'lsp-lens)
  :init
  (setq lsp-keymap-prefix "C-c")
  (setq lsp-lens-auto-enable t)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation nil)

  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-analyzer-inlay-hints-mode t))


(use-package lsp-treemacs
  :ensure t
  :init
  (lsp-treemacs-sync-mode 1)
  :bind
  ("C-c t s" . lsp-treemacs-symbols)
  ("C-c t c" . lsp-treemacs-call-hierarchy)
  ("C-c t t" . lsp-treemacs-type-hierarchy)
  ("C-c t e" . lsp-treemacs-errors-list))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-update-mode "point")
  (setq lsp-ui-sideline-delay 0.2)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-delay 2.5)
  (setq lsp-ui-doc-position 'at-point)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

  :bind
  ("C-c e e" . lsp-ui-flycheck-list)
  ("C-c d" . lsp-ui-doc-show))

;; Used by lsp-mode for applying code suggestions
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

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

(use-package diff-hl
  :ensure t
  :config

  ;; Match the background color of the fringe with the background
  ;; color of the diff-hl faces.
  (let ((default-background (face-attribute 'fringe :background)))
    (set-face-attribute 'diff-hl-insert nil :foreground "#00ff00" :background default-background)
    (set-face-attribute 'diff-hl-delete nil :foreground "#ff0000" :background default-background)
    (set-face-attribute 'diff-hl-change nil :foreground "#da8548" :background default-background))
  (global-diff-hl-mode)

  ;; Keep flydiff (real-time diff-hl mode) enabled.
  :hook (diff-hl-mode . diff-hl-flydiff-mode))

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-keyword-faces
	'(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF")))

  :bind
  ("C-c C-t p" . hl-todo-previous)
  ("C-c C-t n" . hl-todo-next)
  ("C-c C-t o" . hl-todo-occur)
  ("C-c C-t i" . hl-todo-insert)

  :hook
  (prog-mode . hl-todo-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language-specific packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; toc-org for generating Table Of Contents automatically in org.
(use-package toc-org
  :ensure t
  :hook
  (org-mode . toc-org-mode)

  :bind
  ("C-c C-o" . toc-org-markdown-follow-thing-at-point))

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

(use-package typescript-mode
  :ensure t
  :hook
  (typescript-mode . lsp))
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

(use-package web-mode
  :ensure t
  :hook (web-mode . lsp))
(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))

;; Rust integration
(use-package toml-mode
  :ensure t)

(use-package rustic
  :ensure t
  :mode ("\\.rs$" . rustic-mode))

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
		      (string-match "^ ?\\*\\(\\scratch\\|Messages\\|Treemacs\\)" buf-name))

	    (message "Killing buffer '%s'" buf-name)
            (kill-buffer buf)))))))

(provide 'projectile-kill-non-project-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special keybindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-mode is not an excuse to make WindMove and BufferMove bindings for stop working
;; I'll find some replacements when I need it.
(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map (kbd "<S-left>") nil)
	    (define-key org-mode-map (kbd "<S-right>") nil)
	    (define-key org-mode-map (kbd "<S-up>") nil)
	    (define-key org-mode-map (kbd "<S-down>") nil)

	    (define-key org-mode-map (kbd "<C-S-left>") nil)
	    (define-key org-mode-map (kbd "<C-S-right>") nil)
	    (define-key org-mode-map (kbd "<C-S-up>") nil)
	    (define-key org-mode-map (kbd "<C-S-down>") nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom global key bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Center text in selected rectangle ("hello    " => "  hello  ").
;; Mainly added for helping me writing centered text while playing with artist mode.
(global-set-key (kbd "C-c C-r") 'center-rectangle)

;; Toggle text overwrite mode, for the same reason as above lol.
(global-set-key [C-help] 'overwrite-mode)

;; Fast close all opened buffers, for that situations that you have a thousand
;; opened buffers and you need to reboot your mind.
(global-set-key (kbd "C-x C-k") 'clean-buffers)

;; Pretty much for the same, but projectile-aware.
(global-set-key (kbd "C-x p M-k") 'projectile-kill-non-project-buffers)

;; Remove the current line without copying it to the copy buffer.
(global-set-key (kbd "M-k") 'delete-current-line)

;; Shortcut to artist mode.
(global-set-key (kbd "C-c C-a") 'artist-mode)

;; Bind <Home> and <end> keys to beginning-of-buffer and end-of-buffer
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; Debug
(global-set-key (kbd "C-<") (lambda () (interactive) (profiler-start 'cpu+mem)))
(global-set-key (kbd "C->") 'profiler-stop)

;;;;;;;;;;;;;;;;;
;; Other hooks ;;
;;;;;;;;;;;;;;;;;

;; This is practical to have it on prog-mode,
;; plus fixes issues on lsp-ui trying to render on
;; new lines and fucking up the GUI while coding.
(add-hook 'prog-mode-hook
	  (lambda () (setq truncate-lines t)))

;; GC for cleaning up memory of Emacs initialization
(garbage-collect)

;; Setting up final 50MB GC threshold for supporting lsp-mode loads.
(setq gc-cons-threshold 100000000)

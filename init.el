;;; init.el --- Emacs initialization file
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of Emacs run modes and features ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst emacs-run-mode 'default
  "Define the way Emacs was configured for the current instance.
The run mode affects to the way some packages or features might be loaded
or not in the init file.
There are a few run modes that might fit different use cases:
  - :default: Runs Emacs fully featured, for using it as a
    single-instance main editor.
  - :spot: Runs Emacs with most of the features, but accomodates it
    for using as a secondary editor instance, for using it for editing
    simple and very specific files.
  - :light: Runs Emacs disabling most of its features, keeping
    only basic features like some programming modes, suitable for
    running Emacs on a remote server.")

(defconst run-modes-features
  '(
    (default .
      (package-refresh
       position-beacon
       desktop-save-mode
       open-in-emacs
       linum
       theme
       nyancat
       modeline
       treemacs
       treemacs-autoshow
       fill-column-indicator
       flycheck
       company
       helm
       git
       projectile
       elcord
       lsp
       lsp-ui
       undo-tree))

    (spot .
      (package-refresh
       position-beacon
       linum
       theme
       nyancat
       modeline
       treemacs
       projectile
       lsp
       lsp-ui
       undo-tree))

    (light . (linum undo-tree company flycheck helm))))

(setq create-lockfiles nil)
(defmacro features-enabled-list ()
  "Return the list of features currently enabled by the run mode."
  `(alist-get emacs-run-mode run-modes-features))

(defun feature-enabled-p (feature)
  "Return whether the given FEATURE is currently enabled on the current run mode."
  (member feature (features-enabled-list)))

(defun features-enabled-p (features)
  "Return whether the given set of FEATURES is currently enabled on the current run mode."
   (let ((feature-list (features-enabled-list)))
     (seq-every-p (lambda (feature) (member feature feature-list)) features)))

(let* ((env-name "EMACS_RUN_MODE")
       (env-value (getenv env-name))
       (mode (pcase env-value
	       (`nil 'default)
	       ("default" 'default)
	       ("spot" 'spot)
	       ("light" 'light))))

  (unless mode
    (error "Invalid run mode specified in %s: '%s'" env-name env-value))

  (setq emacs-run-mode mode))

(message "Run mode selected: %S" emacs-run-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic configurations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun conf-rel-path (path)
  (concat user-emacs-directory path))

(defmacro emacs-27 ()
  `(eq (symbol-value 'emacs-major-version) 27))

(defmacro emacs-28 ()
  `(eq (symbol-value 'emacs-major-version) 28))

;; High initial GC threshold for speeding up Emacs load.
(setq gc-cons-threshold 1000000000)

;; Setup fringes
(when window-system
  (set-fringe-mode '(8 . 0)))

;; Enable delete selection mode by default. I hate the default
;; behaviour.
(delete-selection-mode 1)

;; Enable auto revert from disk when file changes.
(global-auto-revert-mode t)

;; Allow repeating C-u C-SPC.
(setq set-mark-command-repeat-pop t)

;; Prevent startup welcome Emacs buffer from being shown.
(setq inhibit-startup-message t)

;; Recursive minibuffers.
(setq enable-recursive-minibuffers t)

;; Prevent showing warning about cl package deprecated.
(setq byte-compile-warnings '(cl-functions))

;; Set default fonts
(when window-system
(add-to-list 'default-frame-alist
             '(font . "Hack-11")))

;; Specific terminal configs
(unless window-system
  (xterm-mouse-mode t)
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down-line 2)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up-line 2))))

;; Required by lsp-mode for increasing performance.
(setq read-process-output-max (* 10 (* 1024 1024)))

;; Disable right option modifier key on macOS
(cond ((string-equal system-type "darwin")
       (setq mac-right-option-modifier nil)))

(setq custom-file (conf-rel-path "custom.el"))
(load custom-file 'noerror)

;; Enable desktop save mode
(when (feature-enabled-p 'desktop-save-mode)
  (desktop-save-mode 1))

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
(package-initialize)

(when (feature-enabled-p 'package-refresh)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			   ("melpa" . "https://melpa.org/packages/")))

  ;; First install use-package
  (unless package-archive-contents
    (package-refresh-contents)))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; If variable set, then will print emacs startup time and write a
;; message when each file is loaded, for helping package loading
;; debugging.
(when (getenv "EMACS_DEBUG_LOADING")
  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed
                      (float-time
                       (time-subtract after-init-time before-init-time))))
                 (message "Loading %s...done (%.2fs) [after-init]"
                          ,load-file-name elapsed))) t)

  (setq use-package-verbose t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes, decorators and visuals ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add configuration scripts from the config/ folder
(add-to-list 'load-path (conf-rel-path "config/"))

;; ;; Ensures that all-the-icons is installed.
(require 'config-all-the-icons)
(require 'config-modeline)
(require 'config-linum-relative)
(require 'text-utils)

(when (feature-enabled-p 'theme)
  (require 'config-theme))

(require 'config-prettify-symbols)

(when (and window-system (feature-enabled-p 'git))
  (use-package git-gutter-fringe
    :ensure t
    :config
    (global-set-key (kbd "C-x v n") 'git-gutter:next-hunk)
    (global-set-key (kbd "C-x v p") 'git-gutter:previous-hunk)
    (global-set-key (kbd "C-x v /") 'git-gutter:revert-hunk)
    (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

    (set-face-foreground 'git-gutter-fr:modified "#da8548")
    (set-face-foreground 'git-gutter-fr:added    "#00ff00")
    (set-face-foreground 'git-gutter-fr:deleted  "#ff0000")

    (let ((fringe
	   (eval-when-compile
	     (fringe-helper-convert
	      "..XXXX.."
	      "..XXXX.."
	      "..XXXX.."
	      "..XXXX.."
	      "..XXXX.."
	      "..XXXX.."
	      "..XXXX.."
	      "..XXXX.."))))

      (define-fringe-bitmap 'git-gutter-fr:added fringe nil nil '(top repeat))
      (define-fringe-bitmap 'git-gutter-fr:deleted fringe nil nil '(top repeat))
      (define-fringe-bitmap 'git-gutter-fr:modified fringe nil nil '(top repeat)))
    (global-git-gutter-mode +1)))

(require 'active-minibuffer-lock-mode)
(require 'open-in-emacs-mode)

(require 'util)

;; Kawaii rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Scrolling tweaking
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
(setq scroll-step 2)

(use-package linum-relative
  :ensure t
  :commands linum-relative-mode)

;; Fill column indicator
(when (feature-enabled-p 'fill-column-indicator)
  (use-package fill-column-indicator
    :ensure t
    :config
    (setq fci-rule-width 2)
    (setq fci-rule-color "darkred")
    (setq fci-rule-use-dashes nil)
    (setq fci-rule-column 120))) ;; Keep disabled fci by default (gives problems with Company)

;; Cursor highlight
;; Only enabled when Emacs is running on a graphical interface
(when (feature-enabled-p 'position-beacon)
  (use-package beacon
    :ensure t
    :config
    (setq beacon-color "#fc20bb"))

  ;; Disable beacon if we're on a tty
  ;; Dunno why but it must to be done on window-setup-hook, otherwise it does
  ;; not have any effect
  (add-hook 'window-setup-hook (lambda () (if window-system (beacon-mode 1) (beacon-mode -1)))))

(use-package browse-kill-ring
  :commands browse-kill-ring
  :ensure t)

(when (feature-enabled-p 'treemacs)
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
    (when (and (feature-enabled-p 'treemacs-autoshow) (not (treemacs-workspace->is-empty?)))
      (add-hook 'window-setup-hook
		(lambda ()
		  (let ((last-window (selected-window)))
		    (treemacs)
		    (select-window last-window))
		  )))

    :bind
    ([f8] . treemacs)
    ("C-c t l" . treemacs-find-file))

  (when (feature-enabled-p 'projectile)
    (use-package treemacs-projectile
      :ensure t
      :after treemacs projectile
      :bind ("C-x p a" . treemacs-add-project))))

;; WindMove: move between buffers using shift+arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-to-list 'load-path (conf-rel-path "buffer-move/"))
(use-package buffer-move
  :ensure nil
  :bind
  ("<C-S-up>" . buf-move-up)
  ("<C-S-down>" . buf-move-down)
  ("<C-S-left>" . buf-move-left)
  ("<C-S-right>" . buf-move-right))

(require 'config-prettify-symbols)

;; Enable line numbers
(when (feature-enabled-p 'linum)
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'linum-mode 1))

  ;; I have an issue on my Emacs, where the margin gets completely
  ;; fucked up when zooming in and out, and seems to be related to the
  ;; linum-mode. Disable and enable it each time the scale factor
  ;; changes as a workaround.
  (add-hook 'text-scale-mode-hook
	    (lambda () (when linum-mode (linum-mode -1) (linum-mode 1)))))

(column-number-mode t)

;; Enable global hl mode
(global-hl-line-mode 1)

;; Enable active minibuffer lock mode
(active-minibuffer-lock-mode 1)

;; Enable Open In Emacs mode
(when (and (open-in-emacs-available) (feature-enabled-p 'open-in-emacs))
  (open-in-emacs-mode 1))

;; Highlight the minibuffer on enable
(add-hook 'minibuffer-setup-hook #'minibuffer-emph)

(when (emacs-28)
  ;; TODO Change tab-line-tabs-function to filter special buffers
  ;; (like helm, messages...) ?
  (add-hook 'change-major-mode-hook (lambda () (global-tab-line-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General puropose packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elcord: support for Discord. The elcord folder contains a git
;; submodule that points to a custom elcord mode without reconnect
;; messages repeating each 15 seconds.
(when (feature-enabled-p 'elcord)
  (add-to-list 'load-path (conf-rel-path "elcord/"))
  (require 'elcord)
  (setq elcord-silent-mode 1)
  (elcord-mode))

(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :config
  (set-face-attribute 'sp-show-pair-match-content-face nil :background "#5a5d5e")
  :bind
 ; ("M-[" . sp-beginning-of-sexp)
  ("M-]" . sp-end-of-sexp)
  ("M-{" . sp-unwrap-sexp)
  ("M-}" . sp-backward-unwrap-sexp)
  :hook
  (prog-mode . show-smartparens-mode)
  (prog-mode . smartparens-mode))

(when (feature-enabled-p 'undo-tree)
  (use-package undo-tree
    :ensure t
    :init (global-undo-tree-mode)))

(use-package which-key
  :ensure t
  :init (which-key-mode 1))

;; Flycheck: syntax check on the fly
(use-package flycheck
  :ensure t
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
		 (window-height   . 0.20)))
  (global-flycheck-mode))

;; LSP Mode
(when (feature-enabled-p 'lsp)
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
    (setq lsp-rust-analyzer-inlay-hints-mode t)
    (setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
    (setq lsp-rust-analyzer-proc-macro-enable t)
    (setq lsp-rust-all-features t)
    (setq lsp-completion-use-last-result t))


  (when (feature-enabled-p 'treemacs)
    (use-package lsp-treemacs
      :ensure t
      :config
      (lsp-treemacs-sync-mode 1)
      :bind
      ("C-c t s" . lsp-treemacs-symbols)
      ("C-c t c" . lsp-treemacs-call-hierarchy)
      ("C-c t t" . lsp-treemacs-type-hierarchy)
      ("C-c t e" . lsp-treemacs-errors-list)))

  (when (feature-enabled-p 'lsp-ui)
    (use-package lsp-ui
      :ensure t
      :commands lsp-ui-mode
      :config
      (setq lsp-ui-sideline-show-diagnostics t)
      (setq lsp-ui-sideline-diagnostic-max-lines 10)
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
      ("C-c d" . lsp-ui-doc-show)))

  ;; Used by lsp-mode for applying code suggestions
  ;; Only used when lsp feature is active, since it's only used on it.
  (use-package yasnippet
    :ensure t
    :after lsp-mode
    :config (yas-global-mode 1)))

;; exec-path-from-shell: Set the Emacs path value
;; to the value of the user shell PATH variable value.
(use-package exec-path-from-shell
  :ensure t
  :hook (after-init . exec-path-from-shell-initialize))

;; Projectile: project management for Emacs
(when (feature-enabled-p 'projectile)
  (use-package projectile
    :ensure t
    :config
    (projectile-mode)
    (setq projectile-enable-caching t))

  (when (feature-enabled-p 'helm)
    (use-package helm-projectile
      :ensure t
      :after ((projectile))
      :bind (("C-x p p" . helm-projectile)
             ("C-x p P" . helm-projectile-switch-project)))))

;; Helm: enhaced completion window.
(when (feature-enabled-p 'helm)
  (use-package helm
    :ensure t
    :bind (("M-x" . helm-M-x)
	   ("C-x b" . helm-mini)
	   ("C-x C-b" . helm-buffers-list)
	   ("C-x C-f" . helm-find-files))
    :config
    (helm-autoresize-mode 1)
    (setq helm-split-window-inside-p t
	  helm-candidate-number-limit 500
	  helm-buffers-fuzzy-matching t
	  helm-recentf-fuzzy-match t
	  helm-M-x-fuzzy-match t)))

;; Multi-term: terminal for emacs.
(use-package multi-term
  :ensure t
  :config (setq multi-term-program "/bin/zsh")
  :bind ("C-x t" . multi-term-dedicated-open))

;; company: autocompletion.
(when (feature-enabled-p 'company)
  (use-package company
    :ensure t
    :defer t
    :init
    (global-company-mode)
    :custom
    (company-echo-delay 0)
    (company-idle-delay 0.15)
    (company-minimum-prefix-length 2)
    (company-tooltip-flip-when-above t)
    (company-require-match nil)
    
    :bind
    ("C-c SPC" . company-complete)
    ("C-c C-SPC" . company-complete)))

;; Magit: Git client
(when (feature-enabled-p 'git)
  (use-package magit
    :ensure t
    :bind (("C-x v v" . magit-status)
           ("C-x M-v" . magit-dispatch-popup))))

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
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :hook (latex-mode . lsp))

;; js2-mode: Javascript integration
(use-package js2-mode
  :ensure t
  :mode
  ("\\.js\\'" . js2-mode)
  ("\\.jsx\\'" . js2-jsx-mode)
  :config
  (define-key js2-mode-map (kbd "M-.") nil)
  :bind (("C-x n" . js2-next-error)))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :hook
  (typescript-mode . lsp))

(use-package web-mode
  :ensure t
  :mode ("\\.tsx\\'" . web-mode)
  :hook (web-mode . lsp))

;; Rust integration
(use-package toml-mode
  :mode ("\\.toml\\'" . toml-mode)
  :ensure t)

(use-package rustic
  :ensure t
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . lsp)
  :bind (("C-c C-c f" . rustic-format-file)))

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

;; I don't like commands like kill-word, kill-line to fill the kill
;; ring buffer by default.  Here, I'm pointing its default keybindings
;; to versions of these functions that doesn't save the killed region
;; into the buffer, but just deleting instead. The original kill-word
;; and kill-line functions will be available through alternate
;; keybindings.
(global-set-key (kbd "C-k") 'delete-current-line)
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)
(global-set-key (kbd "C-<delete>") 'delete-word)

(global-set-key (kbd "M-k") 'kill-line)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-<delete>") 'kill-word)

(global-set-key (kbd "C-c n") 'duplicate-line)

;; Shortcut to artist mode.
(global-set-key (kbd "C-c C-a") 'artist-mode)

;; Bind <Home> and <end> keys to beginning-of-buffer and end-of-buffer
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

(global-set-key (kbd "C-S-L") (kbd "C-SPC C-SPC"))

;; Used for hiding buffers without closing them, especially for tabs
;; mode.
(global-set-key (kbd "C-x k") 'bury-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer)

;; Navigation keybindings
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)

(global-set-key (kbd "<f6>") 'linum-relative-transient)

;; Debug keybindings
(global-set-key (kbd "C-x & ,") (lambda () (interactive) (profiler-start 'cpu+mem)))
(global-set-key (kbd "C-x & .") (lambda () (interactive) (profiler-stop) (profiler-report)))

(defun last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer nil)))
(provide 'prev-buffer)

;; Previous buffer
;; TODO Improve with some sort of list of last recent
;; visited buffers, would be cool.
(global-set-key (kbd "C-<") 'last-buffer)


;;;;;;;;;;;;;;;;;
;; Other hooks ;;
;;;;;;;;;;;;;;;;;

;; This is practical to have it on prog-mode,
;; plus fixes issues on lsp-ui trying to render on
;; new lines and fucking up the GUI while coding.
(add-hook 'prog-mode-hook
	  (lambda () (setq truncate-lines t)))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Work configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((amzn-config-folder (conf-rel-path "amzn"))
       (amzn-entry-point (concat amzn-config-folder "/init.el")))

  (when (file-exists-p amzn-entry-point)
    (message "Will load work configuration...")
    (load-file amzn-entry-point)))

;; GC for cleaning up memory of Emacs initialization
(garbage-collect)

;; Setting up final 50MB GC threshold for supporting lsp-mode loads.
(setq gc-cons-threshold 100000000)

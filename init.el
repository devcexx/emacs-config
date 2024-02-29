

;;; init.el --- Emacs initialization file
;;; Commentary:
;;; Code:

;; High initial GC threshold for speeding up Emacs load.
(setq gc-cons-threshold 1000000000)

(defun avoc-init-conf-rel-path (path)
  (concat user-emacs-directory path))

(defmacro avoc-init-add-to-config-load-path (folder)
  `(add-to-list 'load-path (avoc-init-conf-rel-path ,folder)))

;; Setup load paths.
(avoc-init-add-to-config-load-path "config/")
(avoc-init-add-to-config-load-path "config/winsys/")

(require 'avoc-util)
(require 'avoc-run-mode)
(require 'avoc-basics)
(require 'avoc-packaging)
(require 'avoc-terminal-clip)

(if window-system
    (require 'avoc-winsys-graphic)
  (require 'avoc-winsys-none))

(require 'avoc-icons)
(require 'avoc-modeline)
(require 'avoc-text-utils)

(when (avoc-run-mode-feature-enabled-p 'theme)
  (require 'avoc-theme))

(require 'avoc-prettify-symbols)
(require 'avoc-git-gutter)
(require 'open-in-emacs-mode)

(require 'avoc-margins)

(use-package browse-kill-ring
  :commands browse-kill-ring
  :ensure t)

(require 'avoc-prettify-symbols)

;; Enable Open In Emacs mode
(when (and (open-in-emacs-available) (avoc-run-mode-feature-enabled-p 'open-in-emacs))
  (open-in-emacs-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General puropose packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elcord: support for Discord. The elcord folder contains a git
;; submodule that points to a custom elcord mode without reconnect
;; messages repeating each 15 seconds.
(when (avoc-run-mode-feature-enabled-p 'elcord)
  (add-to-list 'load-path (avoc-init-conf-rel-path "elcord/"))
  (require 'elcord)
  (setq elcord-silent-mode 1)
  (elcord-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode 1))

(require 'avoc-tree-sitter)
(require 'avoc-prog)
(require 'avoc-projects)

(when (avoc-run-mode-feature-enabled-p 'linum)
  (require 'avoc-line-numbers))

;; exec-path-from-shell: Set the Emacs path value
;; to the value of the user shell PATH variable value.
(use-package exec-path-from-shell
  :ensure t
  :hook (after-init . exec-path-from-shell-initialize))

;; Helm: enhaced completion window.
(when (avoc-run-mode-feature-enabled-p 'helm)
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
	  helm-M-x-fuzzy-match t)

    (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)))

;; Multi-term: terminal for emacs.
(use-package multi-term
  :ensure t
  :config (setq multi-term-program "/bin/zsh")
  :bind ("C-x t" . multi-term-dedicated-open))

;; company: autocompletion.
(when (avoc-run-mode-feature-enabled-p 'company)
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
(when (avoc-run-mode-feature-enabled-p 'git)
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

(global-set-key (kbd "M-k") 'kill-line)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-<delete>") 'kill-word)

;; Shortcut to artist mode.
(global-set-key (kbd "C-c C-a") 'artist-mode)

;; Bind <Home> and <end> keys to beginning-of-buffer and end-of-buffer
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

(global-set-key (kbd "C-S-L") (kbd "C-SPC C-SPC"))

;; Navigation keybindings
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)

;; Debug keybindings
(global-set-key (kbd "C-x & ,") (lambda () (interactive) (profiler-start 'cpu+mem)))
(global-set-key (kbd "C-x & .") (lambda () (interactive) (profiler-stop) (profiler-report)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Work configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((amzn-config-folder (avoc-init-conf-rel-path "amzn"))
       (amzn-entry-point (concat amzn-config-folder "/init.el")))

  (when (file-exists-p amzn-entry-point)
    (message "Will load work configuration...")
    (load-file amzn-entry-point)))

;; GC for cleaning up memory of Emacs initialization
(garbage-collect)

;; Setting up final 100MB GC threshold.
(setq gc-cons-threshold 100000000)

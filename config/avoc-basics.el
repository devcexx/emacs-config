;;; avoc-basics.el --- Emacs basic configurations.
;;;
;;; Commentary:
;;; Contains basic initial configurations to make Emacs
;;; more useful.
;;;
;;; Code:

;; Disable lock file creation.
(require 'avoc-util)
(require 'avoc-run-mode)
(require 'cl-lib)

(defvar kill-buffers-exclude-alist
  '("*scratch*"
    "*Messages*"
    (lambda (buffer-name) (string-prefix-p " *Treemacs-Scoped-Buffer-" buffer-name))))

(defun kill-buffers()
  (let (buffer buffers)
    (setq buffers (buffer-list))
    (dotimes (i (length buffers))
      (let ((buffer (pop buffers))
	    (bufname (buffer-name buffer)))
	(when
	    (not (cl-some
	     (lambda (excluded-buffer-elem)
	       (cond ((stringp excluded-buffer-elem) (string-equal bufname excluded-buffer-elem))
		     ((functionp excluded-buffer-elem) (apply excluded-buffer-elem (cons bufname ())))
		     (t nil))) kill-buffers-exclude-alist))
	   (kill-buffer bufname))))))

(defun clean-buffers()
  (interactive)
  (if (yes-or-no-p "Do you really want to clean all buffers? ")
      (kill-buffers) nil))

(provide 'clean-buffers)

(setq create-lockfiles nil)

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

;; Required by lsp-mode for increasing performance.
(setq read-process-output-max (* 10 (* 1024 1024)))

;; Disable right option modifier key on macOS
(cond ((string-equal system-type "darwin")
       (setq mac-right-option-modifier nil)))

(setq custom-file (avoc-init-conf-rel-path "custom.el"))
(load custom-file 'noerror)

;; Enable desktop save mode
(when (avoc-run-mode-feature-enabled-p 'desktop-save-mode)
  (desktop-save-mode 1))

;; Attempt to fix a bug that produces sometimes the desktop-save-mode
;; to fail reading the desktop file.
(setq desktop-restore-forces-onscreen nil)

;; Move temporal files to Emacs folder
(setq backup-directory-alist
      `((".*" . , (avoc-init-conf-rel-path "temp"))))
(setq auto-save-file-name-transforms
      `((".*" , (avoc-init-conf-rel-path "temp") t)))


;; WindMove: move between buffers using shift+arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-to-list 'load-path (avoc-init-conf-rel-path "buffer-move/"))
(use-package buffer-move
  :ensure nil
  :bind
  ("<C-S-up>" . buf-move-up)
  ("<C-S-down>" . buf-move-down)
  ("<C-S-left>" . buf-move-left)
  ("<C-S-right>" . buf-move-right))

(when (avoc-run-mode-feature-enabled-p 'undo-tree)
  (use-package undo-tree
    :ensure t
    :config
    ;; Prevent undo tree files from polluting your git repo
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
    :init (global-undo-tree-mode)))

(column-number-mode t)

;; Enable global hl mode
(global-hl-line-mode 1)

;; Enable active minibuffer lock mode
(require 'active-minibuffer-lock-mode)
(active-minibuffer-lock-mode 1)

;; Highlight the minibuffer on enable
(add-hook 'minibuffer-setup-hook #'avoc-util-minibuffer-emph)

(provide 'avoc-basics)
;;; avoc-basics.el ends here

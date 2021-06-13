;;; avoc-basics.el --- Emacs basic configurations.
;;;
;;; Commentary:
;;; Contains basic initial configurations to make Emacs
;;; more useful.
;;;
;;; Code:

;; Disable lock file creation.
(require 'avoc-run-mode)
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

(setq custom-file (conf-rel-path "custom.el"))
(load custom-file 'noerror)

;; Enable desktop save mode
(when (avoc-run-mode-feature-enabled-p 'desktop-save-mode)
  (desktop-save-mode 1))

;; Attempt to fix a bug that produces sometimes the desktop-save-mode
;; to fail reading the desktop file.
(setq desktop-restore-forces-onscreen nil)

;; Move temporal files to Emacs folder
(setq backup-directory-alist
      `((".*" . , (conf-rel-path "temp"))))
(setq auto-save-file-name-transforms
      `((".*" , (conf-rel-path "temp") t)))

(provide 'avoc-basics)
;;; avoc-basics.el ends here

;;; avoc-terminal-clip.el --- terminal-clip.el integration.
;;; Commentary:

;; This file supports the automatic installation and load for
;; terminal-clip.el.

;;; Code:

(defcustom avoc-terminal-clip-build-attempted nil
  "Indicate if an attempt has been made to build terminal-clip.el.
Set this value to a nil value for forcing a
re-attempt."
  :type 'boolean
  :group 'emacs-avocado)

(defconst avoc-terminal-clip--module-path
  (avoc-init-conf-rel-path "terminal-clip.el/")
  "The folder where the terminal-clip.el module is located.")

(defun avoc-terminal-clip--module-compiled ()
  "Check if the terminal-clip native library exists or not."
  (file-exists-p (concat avoc-terminal-clip--module-path "terminal-clip-native" module-file-suffix)))


(defun avoc-terminal-clip--attempt-enable ()
  "Attempt to enable terminal-clip.el and silently fail in case of error."
  (condition-case-unless-debug nil
      (terminal-clip-mode t)
    (message "Couldn't enable terminal-clip.el correctly. Please verify your installation.")))


(defun avoc-terminal-clip--auto-build-completed-handler (p e)
  "Function executed when terminal-clip building finishes.
If success, enable terminal-clip mode.  Otherwise, show an error to the user."
  (if (= 0 (process-exit-status p))
      ;; Build successful
      (progn
	(message "terminal-clip.el building successful.")
	(avoc-terminal-clip--attempt-enable))

    ;; Otherwise
    (progn
      (beep)
      (message "terminal-clip.el building failure. See the output buffer for more info."))))


(defun avoc-terminal-clip-reset-build-attempt ()
  "Reset the status of the terminal-clip.el build attempts.
Doing that will force Emacs to ask again for recompiling the module during the startup."
  (interactive)
  (customize-save-variable 'avoc-terminal-clip-build-attempted nil))


(defun avoc-terminal-clip-auto-build ()
  "Automatically attempt to build termina-clip.el native modules."
  (interactive)
  (let ((build-buffer-name "*avoc-terminal-clip-build*"))
    (with-current-buffer (get-buffer-create build-buffer-name)
      (compilation-mode)
      (display-buffer build-buffer-name)
      (set-process-sentinel
       (start-process
	"avoc-terminal-clip-build"
	build-buffer-name
	"bash"
	"-c"
	(concat "cd \"" (shell-quote-argument (expand-file-name avoc-terminal-clip--module-path)) "\" && cmake . && cmake --build ."))
       #'avoc-terminal-clip--auto-build-completed-handler)
      )))


(add-to-list 'load-path avoc-terminal-clip--module-path)
(require 'terminal-clip)

(when terminal-clip-available ;; Don't even try when is not available (e.g using window system).
  (if (avoc-terminal-clip--module-compiled)
      ;; Module already compiled
      (avoc-terminal-clip--attempt-enable)

    ;; Otherwise
    (unless avoc-terminal-clip-build-attempted
      (customize-save-variable 'avoc-terminal-clip-build-attempted t)
      (when (yes-or-no-p "I've detected that the terminal-clip.el native modules might not be built yet. Do you want me to attempt to build them automatically? ")
	(avoc-terminal-clip-auto-build)))))

(provide 'avoc-terminal-clip)

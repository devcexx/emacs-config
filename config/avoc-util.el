;;; avoc-util.el --- Margins and fringes management.
;;; Commentary:

;; This file contains common utility functions used across the whole
;; configuration

;;; Code:


(defmacro avoc-util-check-emacs-27 ()
  "Check whether the current Emacs running version is the major version 27."
  `(eq (symbol-value 'emacs-major-version) 27))

(defmacro avoc-util-check-emacs-28 ()
  "Check whether the current Emacs running version is the major version 28."
  `(eq (symbol-value 'emacs-major-version) 28))

(defmacro avoc-util-check-emacs-29 ()
  "Check whether the current Emacs running version is the major version 29."
  `(eq (symbol-value 'emacs-major-version) 29))

(defun avoc-util-beep-emph ()
  "Play an emphasizing beep."
  (when (not visible-bell)
    (beep)
    (run-with-timer 0.2 nil #'beep)
    (run-with-timer 0.4 nil #'beep)
    ))

(defun avoc-util-minibuffer-emph ()
  "Blink the visible minibuffer it for drawing the attention of the user over it."
  (invert-face 'minibuffer-prompt)
  (run-with-timer 0.2 nil #'invert-face 'minibuffer-prompt)
  (run-with-timer 0.4 nil #'invert-face 'minibuffer-prompt)
  (run-with-timer 0.6 nil #'invert-face 'minibuffer-prompt))

(defmacro avoc-util-hook-for-mode (mode)
  "The hook name for the given mode, expressed as a symbol in the MODE parameter."
  `(intern (concat (symbol-name ,mode) "-hook")))

(provide 'avoc-util)
;;; avoc-util.el ends here

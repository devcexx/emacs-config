;;; avoc-line-numbers.el --- Line numbers display.
;;;
;;; Commentary:
;;; Line number display set up.
;;;
;;; Code:

(require 'avoc-util)
(require 'display-line-numbers)

(defconst avoc-line-numbers--allowed-modes-alist
  '(prog-mode text-mode fundamental-mode)

  "The list of enabled modes with line numbers.")

(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (display-line-numbers-mode
	     (if (apply 'derived-mode-p avoc-line-numbers--allowed-modes-alist) 1 -1))))

(defvar-local avoc-line-numbers--last-known-cursor-pos 0
  "Holds the last known cursor position in the buffer.")

;; This is a workaround for preventing situations where the
;; display-line-numbers-mode dynamically changes the width of the
;; margin, but this change happens when Helm opens or the minibuffer
;; gets temporarily higher (like while showing eldoc). For preventing
;; glitches because of this possible constant change of width, which
;; is driving me nuts, we're just giving the chance for the
;; display-line-numbers-mode to update itself when the user moves the
;; cursor, and not in any other situation.
(defun avoc-line-numbers--on-cursor-change ()
  "Ran on `post-command-hook' check cursor movement and allow line number display to change its width."
  (when (and
	 (eq display-line-numbers-mode t)
	 (not (= (point) avoc-line-numbers--last-known-cursor-pos)))
    (setq display-line-numbers-width nil)
    (display-line-numbers-update-width)
    (setq avoc-line-numbers--last-known-cursor-pos (point))))

(add-hook 'post-command-hook 'avoc-line-numbers--on-cursor-change)


(provide 'avoc-line-numbers)

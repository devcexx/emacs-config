;;; avoc-text-utils.el -- Text util functions.

;;; Commentary:
;;; This file contains interactive functions to manipulate text.

;;; Code:

(defun avoc-text-utils-delete-current-line (arg)
  "Delete (not kill) the current line.
The ARG parameter indicates how many lines should be deleted."
  (interactive "p")
  (save-excursion
    (dotimes (i arg)
      (delete-region
       (progn (forward-visible-line 0) (point))
       (progn (forward-visible-line 1) (point))))))

;; Evily copied from kill-word and backward-kill-word, but changing it
;; to 'delete', for preventing filling the kill ring buffer with the
;; killed stuff.

(defun avoc-text-utils-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun avoc-text-utils-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (avoc-text-utils-delete-word (- arg)))

(defun avoc-text-utils-center-rectangle (beg end)
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

(defun avoc-text-utils--insert-many (count contents)
  "Perform COUNT insertions of CONTENTS in the current buffer."
  (dotimes (i count) (insert contents)))

(defun avoc-text-utils--dup-line-setup-fast-redup (count line-contents)
  "Setup a transient key map to repeat the duplication of LINE-CONTENTS, COUNT times, using the `last-input-event` key."
  (let ((repeat-key last-input-event))  
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (vector repeat-key)
	 ;; Due to Lisp dynamic binding, use a macro to define the
	 ;; action, but directly expanding variables into it.
	 `(lambda ()
	    (interactive)
	    (avoc-text-utils--insert-many ,count ,line-contents)
	    (avoc-text-utils--dup-line-setup-fast-redup ,count ,line-contents)))
       map))))

(defun avoc-text-utils-duplicate-line (arg)
  "Duplicate the current line ARG times."
  (interactive "p")

  (let ((line-contents (buffer-substring
			(progn (forward-visible-line 0) (point))
			(progn (forward-visible-line 1) (point)))))

    (avoc-text-utils--insert-many arg line-contents)
    (avoc-text-utils--dup-line-setup-fast-redup arg line-contents)))

;; Setup default keybindings for this utils.
(global-set-key (kbd "C-k") 'avoc-text-utils-delete-current-line)
(global-set-key (kbd "C-<backspace>") 'avoc-text-utils-backward-delete-word)
(global-set-key (kbd "C-<delete>") 'avoc-text-utils-delete-word)
(global-set-key (kbd "C-c n") 'avoc-text-utils-duplicate-line)

(provide 'avoc-text-utils)
;;; text-utils.el ends here

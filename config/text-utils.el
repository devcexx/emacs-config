(defun delete-current-line (arg)
  "Delete (not kill) the current line."
  (interactive "p")
  (save-excursion
    (dotimes (i arg)
      (delete-region
       (progn (forward-visible-line 0) (point))
       (progn (forward-visible-line 1) (point))))))

(provide 'delete-current-line)

;; Evily copied from kill-word and backward-kill-word, but changing it
;; to 'delete', for preventing filling the kill ring buffer with the
;; killed stuff.

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(provide 'delete-word)

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(provide 'backward-delete-word)

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

(defun insert-many (count contents)
  "Perform COUNT insertions of CONTENTS in the current buffer."
  (dotimes (i count) (insert contents)))

(defun dup-line-setup-fast-redup (count line-contents)
  "Setup a transient key map to repeat the duplication of LINE-CONTENTS, COUNT times, using the `last-input-event` key."
  (let ((repeat-key last-input-event))  
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (vector repeat-key)
	 ;; Due to Lisp dynamic binding, use a macro to define the
	 ;; action, but directly expanding variables into it.
	 `(lambda ()
	    (interactive)
	    (insert-many ,count ,line-contents)
	    (dup-line-setup-fast-redup ,count ,line-contents)))
       map))))

(defun duplicate-line (arg)
  "Duplicate the current line ARG times."
  (interactive "p")

  (let ((line-contents (buffer-substring
			(progn (forward-visible-line 0) (point))
			(progn (forward-visible-line 1) (point)))))

    (insert-many arg line-contents)
    (dup-line-setup-fast-redup arg line-contents)))

(provide 'duplicate-line)

(provide 'text-utils)
;;; text-utils.el ends here

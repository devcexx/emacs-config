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

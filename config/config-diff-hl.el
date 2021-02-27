(defun calculate--fringe-bmp (width height)
  "Calculate the diff-hl fringe based on the WIDTH of the fringe and the HEIGHT of the character."
  (let ((bar-width 4))
    (make-vector height (lsh (1- (expt 2 bar-width)) (/ (- width bar-width) 2)))))

(defun effective--left-fringe-width ()
  "Calculate the width of the current buffer fringe."
  (if left-fringe-width
      left-fringe-width
    (car (window-fringes))
  ))

(defun effective--char-height ()
  "Calculate the current buffer char height taking into account the text scale factor."
  (if (boundp 'text-scale-mode-step)
      (ceiling (* (frame-char-height) (expt text-scale-mode-step text-scale-mode-amount)))
    (frame-char-height)))

(defun update--diff-hl-bmp ()
  "Update the diff-hl fringe bitmap."
  (let ((fringe-width (min (effective--left-fringe-width) 16)))
    (when (> fringe-width 0)
      (let* ((char-height (effective--char-height))
	     (bitmap (calculate--fringe-bmp fringe-width char-height)))
	
	(define-fringe-bitmap 'diff-hl-def-bitmap bitmap char-height fringe-width)))))

(use-package diff-hl
  :ensure t
  :bind
  ("C-x v ." . diff-hl-next-hunk)
  ("C-x v ," . diff-hl-previous-hunk)
  ("C-x v n" . diff-hl-revert-hunk)
  :hook
  (after-init
   . (lambda ()
       (global-diff-hl-mode 1)
       (diff-hl-flydiff-mode 1)

       ;; Set the bitmap function before the first calculation, to prevent
       ;; delays applying the bitmap on the fringe.
       (setq diff-hl-fringe-bmp-function (lambda (type pos) 'diff-hl-def-bitmap))

       ;; Match the background color of the fringe with the background
       ;; color of the diff-hl faces.
       (let ((default-background (face-attribute 'fringe :background)))
	 (set-face-attribute 'diff-hl-insert nil :foreground "#00ff00" :background default-background)
	 (set-face-attribute 'diff-hl-delete nil :foreground "#ff0000" :background default-background)
	 (set-face-attribute 'diff-hl-change nil :foreground "#da8548" :background default-background))))
  (text-scale-mode . update--diff-hl-bmp)
  (after-setting-font . update--diff-hl-bmp)
  (window-setup . update--diff-hl-bmp)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))


(provide 'config-diff-hl)

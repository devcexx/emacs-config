(defun minibuffer--beep ()
  "Highlight the minibuffer when attention is required."
  (invert-face 'minibuffer-prompt)
  (run-with-timer 0.2 nil #'invert-face 'minibuffer-prompt)
  (run-with-timer 0.4 nil #'invert-face 'minibuffer-prompt)
  (run-with-timer 0.6 nil #'invert-face 'minibuffer-prompt)

  (unless visible-bell
    (beep)
    (run-with-timer 0.2 nil #'beep)
    (run-with-timer 0.4 nil #'beep)))

(defun ensure--minibuffer-selected-if-active (&rest _)
  "Ensure that, if there's an active minibuffer, then it is selected, and select it and visually notify the user otherwise."
  (let ((minibuf (active-minibuffer-window)) (cwindow (selected-window)))
    (when (and minibuf (not (eq minibuf cwindow)))
      (select-window minibuf)
      (minibuffer--beep)
      )))

(define-minor-mode active-minibuffer-lock-mode
  "Minor mode that prevents minibuffer loosing focus when active"
  :init-value nil
  :global t
  :group 'mode

  (if active-minibuffer-lock-mode
      (add-hook 'window-selection-change-functions #'ensure--minibuffer-selected-if-active)
    (remove-hook 'window-selection-change-functions #'ensure--minibuffer-selected-if-active)))

(provide 'active-minibuffer-lock-mode)

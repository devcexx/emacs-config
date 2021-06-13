(require 'avoc-util)

(defun ensure--minibuffer-selected-if-active (&rest _)
  "Ensure that, if there's an active minibuffer, then it is selected, and select it and visually notify the user otherwise."
  (let ((minibuf (active-minibuffer-window)) (cwindow (selected-window)))
    (when (and minibuf (not (eq minibuf cwindow)))
      (select-window minibuf)
      (avoc-util-minibuffer-emph)
      (avoc-util-beep-emph)
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

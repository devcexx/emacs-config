(defun beep-emph ()
  "Play a beep multiple times repeatedly for better emphasizing a situation that requires immediate attention."
  (when (not visible-bell)
    (beep)
    (run-with-timer 0.2 nil #'beep)
    (run-with-timer 0.4 nil #'beep)
    ))

(provide 'beep-emph)

(defun minibuffer-emph ()
  "If minibuffer is active, blinks it for drawing the attention of the user over it."
  (invert-face 'minibuffer-prompt)
  (run-with-timer 0.2 nil #'invert-face 'minibuffer-prompt)
  (run-with-timer 0.4 nil #'invert-face 'minibuffer-prompt)
  (run-with-timer 0.6 nil #'invert-face 'minibuffer-prompt))

(provide 'minibuffer-emph)
(provide 'util)

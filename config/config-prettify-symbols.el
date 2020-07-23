;;; config-prettify-symbols.el --- Theme configuration
;;; Commentary:
;;; Code:

;; Prettify symbols mode
;; Global symbols
(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq prettify-symbols-alist
		  '(
		    ("=>"  . ?⇒)
		    ("->"  . ?→)
		    ("!=" . ?≠)
		    ("<=" . ?≤)
		    (">=" . ?≥)
		    ))
	    (prettify-symbols-mode)))

;; Mode-specific symbols
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq prettify-symbols-alist
		  (append
		   prettify-symbols-alist
		   '(
		     ("lambda" . ?λ)
		     )))
	    (prettify-symbols-mode)
	    ))

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq prettify-symbols-alist
		  (append
		   prettify-symbols-alist
		   '(
		     ("lambda" . ?λ)
		     )))
	    (prettify-symbols-mode)
	    ))

(provide 'config-prettify-symbols)
;;; config-prettify-symbols.el ends here

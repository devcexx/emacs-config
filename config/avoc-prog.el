;;; avoc-prog.el --- Programming modes configuration
;;; Commentary:
;;; Code:

;; (define-key eglot-mode-map (kbd "C-c <tab>") #'company-complete) ; initiate the completion manually
;; (define-key eglot-mode-map (kbd "C-c e f n") #'flymake-goto-next-error)
;; (define-key eglot-mode-map (kbd "C-c e f p") #'flymake-goto-prev-error)
;; (define-key eglot-mode-map (kbd "C-c e r") #'eglot-rename)

;;;;;;;;;;;;;;;;;;;
;; Flymake setup ;;
;;;;;;;;;;;;;;;;;;;

(require 'flymake)
(define-key flymake-mode-map (kbd "C-c f n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-c f p") #'flymake-goto-previous-error)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eglot basic configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (use-package eglot
;;   :bind (:map eglot-mode-map
;; 	      ("C-c f n" . flymake-goto-next-error)
;; 	      ("C-c f p" . flymake-goto-previous-error))
;;   )


;;;;;;;;;;;;;;;;;
;; Other hooks ;;
;;;;;;;;;;;;;;;;;

;; This is practical to have it on prog-mode,
;; plus fixes issues on lsp-ui trying to render on
;; new lines and fucking up the GUI while coding.
(add-hook 'prog-mode-hook
	  (lambda () (setq truncate-lines t)))

;; Always initiate flymake with prog-mode
;;(add-hook 'prog-mode-hook #'flymake-mode)

(add-hook 'before-save-hook
          (lambda () (when (derived-mode-p 'prog-mode)
               (delete-trailing-whitespace))))

(provide 'avoc-prog)
;;; avoc-prog.el ends here

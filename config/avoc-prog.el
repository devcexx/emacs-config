;;; avoc-prog.el --- Programming modes configuration
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;
;; Basic setup ;;
;;;;;;;;;;;;;;;;;

(require 'eglot)
(require 'flymake)
(require 'avoc-tree-sitter)
(require 'avoc-util)

;; Automatically install tree-sitter languages
(avoc-tree-sitter-auto-install-langs)

(define-key flymake-mode-map (kbd "C-c f n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-c f p") #'flymake-goto-previous-error)

(define-key flymake-mode-map (kbd "M-;") #'comment-line)
(define-key flymake-mode-map (kbd "C-x ;") #'comment-dwim)
(define-key flymake-mode-map (kbd "C-x M-;") #'comment-set-column)

(define-key eglot-mode-map (kbd "C-c e a") #'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-c e r") #'eglot-rename)
(define-key eglot-mode-map (kbd "C-c e d") #'eldoc-doc-buffer)

(global-set-key (kbd "C-x x e") (lambda () (interactive) (eglot-ensure)))


;; Required for snippet-based completion through eglot. Not having
;; this enabled may produce that some LSP servers, such as the
;; vscode-html-language-server, completely disables completion
;; features. It took me like a few hours to figure out this. Just keep
;; it enabled.
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

 ;;;;;;;;;;;;;;;;;;;;;
 ;; Languages setup ;;
 ;;;;;;;;;;;;;;;;;;;;;

(defconst avoc-prog-eglot-enabled-mode-alist
  '(
    ;; For now I think I just prefer to enable eglot on my own so that
    ;; I don't need to deal with LSP servers for editing independent
    ;; files that are not owned by a specific project.

    ;; c-ts-mode
    ;; c++-ts-mode
    ;; cmake-ts-mode
    ;; toml-ts-mode
    ;; css-ts-mode
    ;; js-ts-mode
    ;; json-ts-mode
    ;; LaTeX-mode
    ;; python-ts-mode
    ;; rust-ts-mode
    ;; bash-ts-mode
    ;; typescript-ts-mode
    ;; web-mode
    )

  "The list of eglot-enabled major modes that will be automatically
  enabled when the major mode is set. For those major modes with an
  alias set (like the ones set for tree-sitter), prefer putting here
  the real mode name instead of the alias one. E. g use bash-ts-mode
  instead of sh-mode.")

(defconst avoc-prog-default-modes-with-tree-sitter-support
  '((sql-mode . sql)
    (makefile-mode . make))

  "The list of the Emacs default modes to which tree-sitter support
  will be enabled")

;; Eagerly load of some modes so auto-mode-alist can be set properly.
(require 'c-ts-mode)
(require 'cmake-ts-mode)
(require 'cmake-ts-mode)
(require 'css-mode)
(require 'js)
(require 'json-ts-mode)
(require 'python)
(require 'toml-ts-mode)
(require 'typescript-ts-mode)
(require 'yaml-ts-mode)

;; Remapping default Emacs modes to Tree-sitter custom modes
(setq major-mode-remap-alist
      '((c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (cmake-mode      . cmake-ts-mode)
        (conf-toml-mode  . toml-ts-mode)
        (css-mode        . css-ts-mode)
        (js-mode         . js-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-json-mode    . json-ts-mode)
        (python-mode     . python-ts-mode)
        (ruby-mode       . ruby-ts-mode)
        (sh-mode         . bash-ts-mode)))

;; For those Emacs modes that doesn't have enhanced tree-sitter modes,
;; just use the default mode, using tree-sitter syntax highlighting.
(dolist (entry avoc-prog-default-modes-with-tree-sitter-support)
  (let ((mode (car entry)) (language (cdr entry)))
    (add-hook (avoc-util-hook-for-mode mode) `(lambda () (treesit-parser-create ',language)))))

;; Configure eglot auto-initialization for known major modes
(dolist (mode avoc-prog-eglot-enabled-mode-alist)
  (add-hook (avoc-util-hook-for-mode mode) 'eglot-ensure))

;; Extra configuration for eglot server programs
(add-to-list 'eglot-server-programs
	     `((rust-ts-mode rust-mode) . ("rust-analyzer" :initializationOptions
					   ( :procMacros (:enable t)
					     :cargo ( :buildScripts (:enable t)
						      :features "all")))))


;; Install specific modes for modes not supported by tree-sitter by
;; default nor are available by default in Emacs.
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (add-hook 'LaTeX-mode-hook (lambda () (treesit-parser-create 'latex))))


(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" . web-mode)
  :config
  (add-hook 'web-mode-hook (lambda () (treesit-parser-create 'html)))
  (add-to-list 'eglot-server-programs
	       `(web-mode . ,(eglot-alternatives
			      '(("vscode-html-language-server" "--stdio") ("html-languageserver" "--stdio"))))))

(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook (lambda () (treesit-parser-create 'markdown)))
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Kawaii rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :config
  (set-face-attribute 'sp-show-pair-match-content-face nil :background "#5a5d5e")
  :bind
  ("C-c [" . sp-beginning-of-sexp)
  ("C-c ]" . sp-end-of-sexp)
  ("C-c {" . sp-beginning-of-next-sexp)
  ("C-c }" . sp-beginning-of-previous-sexp)
  ("C-c M-{" . sp-unwrap-sexp)
  ("C-c M-}" . sp-backward-unwrap-sexp)
  :hook
  (prog-mode . show-smartparens-mode)
  (prog-mode . smartparens-mode))

;;;;;;;;;;;;;;;;;
;; Other hooks ;;
;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook
	  (lambda () (progn
		  (setq truncate-lines t)
		  (flymake-mode t))))

;; Always initiate flymake with prog-mode
;;(add-hook 'prog-mode-hook #'flymake-mode)

(add-hook 'before-save-hook
          (lambda () (when (derived-mode-p 'prog-mode)
               (delete-trailing-whitespace))))

(provide 'avoc-prog)
;;; avoc-prog.el ends here

(require 'treesit)
(require 'cl-lib)

(defcustom avoc-tree-sitter-modules-missing-stop-asking-me nil
  "Indicates whether the user wants to be asked for automatically
building missing treesitter modules."
  :type 'boolean
  :group 'emacs-avocado)

;; List took from https://github.com/manateelazycat/lazycat-emacs/blob/16e3fd4ae21308c6723f9ca689dbf24d0989d42a/site-lisp/config/init-treesit.el#L88
(setq treesit-language-source-alist
'((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))

	;; TODO Fixing TS version to v0.20.3 because of a weird bug in emacs 29.2 (ef01b634d219bcceda17dcd61024c7a12173b88c)
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))

(defun avoc-tree-sitter--check-installed-p (lang)
  (file-exists-p (avoc-init-conf-rel-path (concat "tree-sitter/libtree-sitter-" (symbol-name lang) module-file-suffix))))

(defun avoc-tree-sitter-not-installed-langs ()
  (cl-remove-if
   (lambda (lang)
     (avoc-tree-sitter--check-installed-p lang)) (cl-mapcar 'car treesit-language-source-alist)))

(defun avoc-tree-sitter--auto-install-ask ()
  (if avoc-tree-sitter-modules-missing-stop-asking-me
      nil
    (let ((not-installed-langs (avoc-tree-sitter-not-installed-langs)))
	  (if (not (null not-installed-langs))
	      (if (yes-or-no-p (format "The following tree-sitter modules are not available: %s. Do you want to automatically build them? " not-installed-langs))
		  t
		(progn
		  (when (yes-or-no-p "There are missing tree-sitter modules in your Emacs installation. Do you want me to keep bothering you about this everytime you open Emacs? ")
		    (customize-save-variable 'avoc-tree-sitter-modules-missing-stop-asking-me t))
		  nil)
	    nil)))))


(defun avoc-tree-sitter-auto-install-langs (&optional ask)
  (interactive)
  (when (or (not ask) (avoc-tree-sitter--auto-install-ask))
    (dolist (lang-def treesit-language-source-alist)
      (let ((lang (car lang-def)))
	(unless (avoc-tree-sitter--check-installed-p lang)
	  (progn
	    (message "Installing language %s..." (symbol-name lang))
	    (treesit-install-language-grammar lang)))))))

(provide 'avoc-tree-sitter)

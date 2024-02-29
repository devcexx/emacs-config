(require 'treesit)


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
	(latex . ("https://github.com/latex-lsp/tree-sitter-latex"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))

(defun avoc-tree-sitter--check-installed-p (lang)
  (file-exists-p (avoc-init-conf-rel-path (concat "tree-sitter/libtree-sitter-" (symbol-name lang) module-file-suffix))))

(defun avoc-tree-sitter-auto-install-langs ()
  (interactive)
  (dolist (lang-def treesit-language-source-alist)
    (let ((lang (car lang-def)))
      (unless (avoc-tree-sitter--check-installed-p lang)
	(progn
	  (message "Installing language %s..." (symbol-name lang))
	  (treesit-install-language-grammar lang))))))

(provide 'avoc-tree-sitter)
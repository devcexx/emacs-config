;;; avoc-packaging.el --- Package initialization.

;;; Commentary:
;;; This file initializes Emacs packages, adds respositories and
;;; automatically checks and install use-package if required.

;;; Code:

;; Init repositories
(require 'package)
(package-initialize)

(when (avoc-run-mode-feature-enabled-p 'package-refresh)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			   ("melpa" . "https://melpa.org/packages/")))

  ;; First install use-package
  (unless package-archive-contents
    (package-refresh-contents)))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; If variable set, then will print emacs startup time and write a
;; message when each file is loaded, for helping package loading
;; debugging.
(when (getenv "AVOC_DEBUG_LOADING")
  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed
                      (float-time
                       (time-subtract after-init-time before-init-time))))
                 (message "Loading %s...done (%.2fs) [after-init]"
                          ,load-file-name elapsed))) t)

  (setq use-package-verbose t))


(provide 'avoc-packaging)
;;; avoc-packaging.el ends here

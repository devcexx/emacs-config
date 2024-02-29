(setq native-comp-speed 2)

(setq native-comp-jit-compilation-deny-list
      '("\\(?:[/\\\\]\\.dir-locals\\.el$\\)"
        ;; Don't native-compile *-authloads.el and *-pkg.el files as they
        ;; seem to produce errors during native-compile.
        "\\(?:[^z-a]*-autoloads\\.el$\\)"
        "\\(?:[^z-a]*-pkg\\.el$\\)"))

;; Disable bars and unnecesary menus
(tooltip-mode -1)

;; Some times during init the tool-bar-mode is not available, not sure
;; why.
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

(when (functionp 'scroll-bar-mode)
  (scroll-bar-mode -1))

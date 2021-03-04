(setq comp-speed 2)

(setq comp-deferred-compilation-deny-list
      '("\\(?:[/\\\\]\\.dir-locals\\.el$\\)"
        ;; Don't native-compile *-authloads.el and *-pkg.el files as they
        ;; seem to produce errors during native-compile.
        "\\(?:[^z-a]*-autoloads\\.el$\\)"
        "\\(?:[^z-a]*-pkg\\.el$\\)"))

;; Disable bars and unnecesary menus
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(defconst git-gutter-color-modified "#da8548")
(defconst git-gutter-color-added "#00ff00")
(defconst git-gutter-color-deleted "#ff0000")

;; TODO Consider add this mode to desktop-minor-mode-table.

(use-package git-gutter
  :ensure t
  :config
  (global-set-key (kbd "C-x v n") 'git-gutter:next-hunk)
  (global-set-key (kbd "C-x v p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x v /") 'git-gutter:revert-hunk)
  (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

  (custom-set-variables
   '(git-gutter:update-interval 1)
   '(git-gutter:hide-gutter nil)
   '(git-gutter:modified-sign "┃")
   '(git-gutter:added-sign "┃")
   '(git-gutter:deleted-sign "┃")
   '(git-gutter:unchanged-sign nil)
   )
  
  (set-face-foreground 'git-gutter:modified git-gutter-color-modified)
  (set-face-foreground 'git-gutter:added git-gutter-color-added)
  (set-face-foreground 'git-gutter:deleted git-gutter-color-deleted)
  (set-face-background 'git-gutter:unchanged nil)
  (global-git-gutter-mode +1))

(provide 'config-git-gutter)

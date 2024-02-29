;;; avoc-margins.el --- Margins and fringes management.
;;; Commentary:

;; This file contains logic for sorting and keeping the margins and
;; the fringes in their position according to the current enabled
;; modes on each buffer.

;;; Code:

(require 'cl-lib)
(require 'avoc-util)

(defconst avoc-margins--minor-modes-table
  '(
    (flycheck-mode . (0 1 0 0))
    (git-gutter-mode . (0 1 0 0)))
  "Relationship between each major mode and the required margins for each one.
The margins are defined as a list of the form (left-fringe
left-margin right-fringe right-margin).  The final applied margins
are the base margins for the current major mode plus the sum of all
the margins associated to each active minor mode, ignoring the
non-active ones.  This margins are recalculated each time one of the
present modes in the table are activated or deactivated.")

(defconst avoc-margins--major-modes-table
  '(
    (treemacs-mode . (8 0 0 0))
    (prog-mode . (0 0 0 0))
    (text-mode . (0 0 0 0))
    (minibuffer-inactive-mode . (0 0 0 0))
    (magit-status-mode  . (8 0 0 0))
    (magit-diff-mode  . (8 0 0 0)))
  "Relationship between each major mode and its base margins.
When a major mode is active, its base margin is added to the specific
margins for each minor mode activated, defined in
`avoc-margins---minor-modes-table'.  The margins that are used for a
specific major mode are looked up from the top to the bottom of the
table, by finding the first major mode that satisfies the
`derived-mode-p' predicate.  So, if this table contains multiple major
modes that derive one from another, place the most concrete ones
nearer to the beginning of the table.")

(defun avoc-margins--update-all (left-fringe left-margin right-fringe right-margin)
  "Update the margin and fringes for the current visible buffer.
This done by using the values passed on the the parameters LEFT-FRINGE
LEFT-MARGIN RIGHT-FRINGE and RIGHT-MARGIN accordingly."
  (setq left-margin-width left-margin)
  (setq left-fringe-width left-fringe)
  (setq right-fringe-width right-fringe)
  (setq right-margin-width right-margin)
  (let ((curwin (get-buffer-window)))
    (set-window-margins curwin left-margin right-margin)
    (set-window-fringes curwin left-fringe right-fringe)))

(defvar avoc-margins--linum-update-in-progress nil
  "Equal to t if the function linum-update-window is currently being executed.")

(defvar-local avoc-margins--last-margins nil
  "The last margins calculated for the current buffer and stored for preventing extra calculations.")

(defun avoc-margins--last-margins ()
  "Return the last calculated margins for the current buffer, or calculates them if are not present."
  (or
   avoc-margins--last-margins
   (setq-local avoc-margins--last-margins (avoc-margins--calculate-margins))))

(defun avoc-margins--base-margins-for-current-major-mode ()
  "Return the base margins for the current enabled major mode.
This function uses the data from `avoc-margins--major-modes-table' to
compute the most suitable margin for the current mode.  See the doc
from this table for more info."
  (or (cdr (seq-find (lambda (entry) (derived-mode-p (car entry)))
		     avoc-margins--major-modes-table))
      '(0 0 0 0)))


(defun avoc-margins--calculate-margins ()
  "Calculate the effective margins that should be applied on the current buffer.
This function returns the effective margins that a buffer must have
based on the minor modes it has activated and its current major
mode."
  (let* ((filtered-table
	 (seq-filter (lambda (entry) (and (boundp (car entry)) (symbol-value (car entry)))) avoc-margins--minor-modes-table))
	 (config-values-only (mapcar #'cdr filtered-table)))
    (seq-reduce (lambda (l r) (cl-mapcar #'+ l r)) config-values-only (avoc-margins--base-margins-for-current-major-mode))))

(defun avoc-margins--after-relevant-mode-changed ()
  "Callback function that is called when a relevant mode is enabled or disabled."
  (let ((margins (avoc-margins--calculate-margins)))
    (setq-local avoc-margins--last-margins margins)
    (apply 'avoc-margins--update-all margins)))

;; Default values for margins and fringes.
(setq-default left-margin-width 0)
(setq-default right-margin-width 0)
(setq-default left-fringe-width 0)
(setq-default right-fringe-width 0)

;; Fringe mode is only available on Emacs distributions built with
;; graphical support.
(when (boundp 'fringe-mode)
  (fringe-mode '(0 . 0)))

;; Add hooks for relevant minor modes.
(dolist (entry avoc-margins--minor-modes-table)
  (add-hook (avoc-util-hook-for-mode (car entry)) #'avoc-margins--after-relevant-mode-changed))

;; Add hooks for relevant major modes.
(dolist (entry avoc-margins--major-modes-table)
 (add-hook (avoc-util-hook-for-mode (car entry)) #'avoc-margins--after-relevant-mode-changed))

(provide 'avoc-margins)

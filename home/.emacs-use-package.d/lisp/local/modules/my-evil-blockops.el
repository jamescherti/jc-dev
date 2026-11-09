;;; my-evil-blockops.el --- my-evil-blockops -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/jc-dev
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'evil)
(eval-and-compile
  (require 'lightemacs-use-package))
(require 'my-defun) ; my-tree-sitter-p

;;; Combobulate

(lightemacs-use-package combobulate
  :commands (combobulate
             combobulate-mode
             combobulate-drag-up
             combobulate-drag-down
             combobulate-mark-node-dwim
             combobulate-navigate-next
             combobulate-navigate-previous)
  :init
  ;; (with-eval-after-load 'evil
  ;;   (evil-define-key 'normal 'global (kbd "<leader>co") #'combobulate))
  (eldoc-add-command-completions "combobulate-")
  :config
  (require 'combobulate-manipulation))

;;; Combobulate and tree-sitter integration

(defvar combobulate-mode)

(defun my-combobulate-active-p ()
  "Return non-nil if Combobulate is active or available in the current buffer."
  (or (bound-and-true-p combobulate-mode)
      (my-tree-sitter-p)
      (ignore-errors (combobulate-mode 1))
      (bound-and-true-p combobulate-mode)))

;;; Universal outline promotion, demotion, selection, and insertion

(defvar outline-indent-shift-width)
(defvar outline-indent-default-offset)
(defvar python-indent-offset)

(defun my-get-outline-indent-shift-width ()
  "Return a valid integer shift width for `outline-indent' commands."
  (or (bound-and-true-p outline-indent-shift-width)
      (and (derived-mode-p 'python-base-mode)
           (bound-and-true-p python-indent-offset))
      (bound-and-true-p evil-shift-width)
      (bound-and-true-p outline-indent-default-offset)
      standard-indent
      4))

(defun my-promote-subtree (&optional _arg)
  "Promote or shift the current subtree left according to the active mode.
_ARG specifies the number of levels to promote when passed as a prefix
argument."
  (interactive "p")
  (cond
   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-promote-subtree))
    (call-interactively #'org-promote-subtree))

   ;; Support tree-sitter modes and outline-indent modes.
   ((and (or (bound-and-true-p outline-indent-minor-mode)
             (derived-mode-p 'python-ts-mode)
             (string-suffix-p "-ts-mode" (symbol-name major-mode))
             (and (fboundp 'treesit-parser-list)
                  (treesit-parser-list)))
         (fboundp 'outline-indent-shift-left))
    ;; Ensure `outline-indent-shift-width' is non-nil when called in modes like
    ;; `python-ts-mode' where `outline-indent-minor-mode' is not enabled.
    (let ((outline-indent-shift-width (my-get-outline-indent-shift-width)))
      (call-interactively #'outline-indent-shift-left)))

   ((and (bound-and-true-p outline-minor-mode)
         (fboundp 'outline-promote))
    (call-interactively #'outline-promote))

   (t
    (user-error "No supported outline promotion command is available"))))

(defun my-demote-subtree (&optional _arg)
  "Demote or shift the current subtree right according to the active mode.
_ARG specifies the number of levels to demote when passed as a prefix argument."
  (interactive "p")
  (cond
   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-demote-subtree))
    (call-interactively #'org-demote-subtree))

   ;; Support tree-sitter modes and outline-indent modes.
   ((and (or (bound-and-true-p outline-indent-minor-mode)
             (derived-mode-p 'python-ts-mode)
             (string-suffix-p "-ts-mode" (symbol-name major-mode))
             (and (fboundp 'treesit-parser-list)
                  (treesit-parser-list)))
         (fboundp 'outline-indent-shift-right))
    ;; Ensure `outline-indent-shift-width' is non-nil when called in modes like
    ;; `python-ts-mode' where `outline-indent-minor-mode' is not enabled.
    (let ((outline-indent-shift-width (my-get-outline-indent-shift-width)))
      (call-interactively #'outline-indent-shift-right)))

   ((and (bound-and-true-p outline-minor-mode)
         (fboundp 'outline-demote))
    (call-interactively #'outline-demote))

   (t
    (user-error "No supported outline demotion command is available"))))

(defun my-evil-blockops-select-block ()
  "Select the current subtree or block according to the active mode."
  (interactive)
  (cond
   ;; Support Combobulate node selection for tree-sitter modes.
   ;; ((and (my-combobulate-active-p)
   ;;       (fboundp 'combobulate-mark-node-dwim)
   ;;       (fboundp 'evil-visual-select))
   ;;  (let ((bounds
   ;;         (save-mark-and-excursion
   ;;           (unless mark-active
   ;;             (set-marker (mark-marker) nil))
   ;;           (call-interactively #'combobulate-mark-node-dwim)
   ;;           (let ((anchor (mark t))
   ;;                 (cursor (point)))
   ;;             (unless (and anchor (/= anchor cursor))
   ;;               (user-error "Combobulate did not mark a nonempty node"))
   ;;             (list (min anchor cursor) (max anchor cursor)
   ;;                   'inclusive (if (< cursor anchor) -1 1))))))
   ;;    (apply #'evil-visual-select bounds)
   ;;    (setq deactivate-mark nil)))

   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-mark-subtree))
    (call-interactively #'org-mark-subtree))

   ((and (or (bound-and-true-p outline-indent-minor-mode)
             (derived-mode-p 'python-ts-mode)
             (string-suffix-p "-ts-mode" (symbol-name major-mode))
             (and (fboundp 'treesit-parser-list)
                  (treesit-parser-list)))
         (fboundp 'outline-indent-select))
    (call-interactively #'outline-indent-select))

   ((and (bound-and-true-p outline-minor-mode)
         (fboundp 'outline-mark-subtree))
    (call-interactively #'outline-mark-subtree))

   (t
    (user-error "No supported outline selection command is available"))))

(defun my-outline-forward-same-level (&optional _arg)
  "Move forward to next sibling heading.
_ARG specifies how many sibling headings to advance forward."
  (interactive "p")
  (cond
   ;; Support Combobulate sibling navigation for tree-sitter modes.
   ((my-combobulate-active-p)
    (let ((point (point)))
      ;; In YAML, starting mid-line can move only the current line down instead
      ;; of the entire block.
      (when (derived-mode-p 'yaml-ts-mode)
        (beginning-of-line))

      (condition-case err
          (call-interactively #'combobulate-navigate-next)
        (error
         (goto-char point)
         (user-error "%s" (error-message-string err))))))

   ((derived-mode-p 'prog-mode)
    ;; Check local normal state map and motion state map in addition to
    ;; `evil-normal-state-map' because Evil binds `]]' in `evil-motion-state-map'
    ;; by default.
    (let ((cmd (or (and (bound-and-true-p evil-normal-state-local-map)
                        (lookup-key evil-normal-state-local-map (kbd "]]")))
                   (lookup-key evil-normal-state-map (kbd "]]"))
                   (lookup-key evil-motion-state-map (kbd "]]")))))
      (if (commandp cmd)
          (call-interactively cmd)
        (user-error "No default command bound to ]] in prog-mode"))))

   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-forward-heading-same-level))
    (call-interactively #'org-forward-heading-same-level))

   ((and (bound-and-true-p outline-indent-minor-mode)
         (fboundp 'outline-indent-forward-same-level))
    (call-interactively #'outline-indent-forward-same-level))

   ((and (bound-and-true-p outline-minor-mode)
         (fboundp 'outline-forward-same-level))
    (call-interactively #'outline-forward-same-level))

   (t
    (user-error "No supported outline forward navigation command"))))

(defun my-outline-backward-same-level (&optional _arg)
  "Move backward to previous sibling heading.
_ARG specifies how many sibling headings to retreat backward."
  (interactive "p")
  (cond
   ;; Support Combobulate sibling navigation for tree-sitter modes.
   ((my-combobulate-active-p)
    (let ((point (point)))
      ;; In YAML, starting mid-line can move only the current line down instead
      ;; of the entire block.
      (when (derived-mode-p 'yaml-ts-mode)
        (beginning-of-line))

      (condition-case err
          (call-interactively #'combobulate-navigate-previous)
        (error
         (goto-char point)
         (user-error "%s" (error-message-string err))))))

   ((derived-mode-p 'prog-mode)
    ;; Fix invalid key sequence `"[[\\"' to `"[["' and fall back to
    ;; `evil-normal-state-local-map' and `evil-motion-state-map' where Evil
    ;; defines `[[' by default.
    (let ((cmd (or (and (bound-and-true-p evil-normal-state-local-map)
                        (lookup-key evil-normal-state-local-map (kbd "[[")))
                   (lookup-key evil-normal-state-map (kbd "[["))
                   (lookup-key evil-motion-state-map (kbd "[[")))))
      (if (commandp cmd)
          (call-interactively cmd)
        (user-error "No default command bound to [[ in prog-mode"))))

   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-backward-heading-same-level))
    (call-interactively #'org-backward-heading-same-level))

   ((and (bound-and-true-p outline-indent-minor-mode)
         (fboundp 'outline-indent-backward-same-level))
    (call-interactively #'outline-indent-backward-same-level))

   ((and (bound-and-true-p outline-minor-mode)
         (fboundp 'outline-backward-same-level))
    (call-interactively #'outline-backward-same-level))

   (t
    (user-error "No supported outline backward navigation command"))))

;; Keybindings in intercept map
(evil-define-key '(normal) my-intercept-mode-map
  (kbd "M-h") 'my-promote-subtree
  (kbd "M-l") 'my-demote-subtree
  (kbd "gV")  'my-evil-blockops-select-block
  (kbd "gj")  'my-outline-forward-same-level
  (kbd "gk")  'my-outline-backward-same-level)

;;; Insert heading

;; The function inserts a new heading at the current cursor position, and
;; prepends it with "TODO " if activated while on a "TODO" task, thus creating a
;; new to-do item. In addition to that, for those utilizing evil-mode the
;; function transitions the user into insert mode right after the "TODO "
;; insertion.
(defun my-org-insert-heading-respect-content-and-prepend-todo ()
  "Insert a new org heading respecting content and prepend it with TODO.
Additionally, ensure entry into insert state when `evil-mode' is active."
  (interactive)
  (when (and (fboundp 'org-entry-is-todo-p)
             (fboundp 'org-entry-is-done-p)
             (fboundp 'org-insert-heading-respect-content))
    (let ((entry-is-todo (org-entry-is-todo-p))
          (entry-is-done (org-entry-is-done-p)))
      (when (and (bound-and-true-p evil-local-mode)
                 (fboundp 'evil-insert-state))
        (evil-insert-state))
      (org-insert-heading-respect-content)
      (when (or entry-is-todo entry-is-done)
        (just-one-space)
        (insert "TODO")
        (just-one-space)))))

(defun my-evil-blockops-insert-heading ()
  "Insert a new line or heading at current level and switch to insert state."
  (interactive)
  (let ((prev-point (point)))
    (unwind-protect
        (cond
         ((derived-mode-p 'org-mode)
          (my-org-insert-heading-respect-content-and-prepend-todo))

         ((and (bound-and-true-p outline-indent-minor-mode)
               (fboundp 'outline-indent-insert-heading))
          (call-interactively #'outline-indent-insert-heading))

         ((and (bound-and-true-p outline-minor-mode)
               (fboundp 'outline-insert-heading))
          (call-interactively #'outline-insert-heading))

         (t
          (user-error "No outline heading insertion command is available")))
      ;; Switch to insert mode
      (when (/= prev-point (point))
        (when (and (bound-and-true-p evil-local-mode)
                   (fboundp 'evil-insert-state))
          (evil-insert-state))))))

(evil-define-key '(normal insert) my-intercept-mode-map
  (kbd "C-<return>") 'my-evil-blockops-insert-heading)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-<return>")
              'my-org-insert-heading-respect-content-and-prepend-todo))

;; (with-eval-after-load 'outline-indent
;;   (evil-define-key 'normal outline-mode-map (kbd "]]") nil)
;;   (evil-define-key 'normal outline-mode-map (kbd "[[") nil))

;; (defun my-evil-define-key-outline-indent-minor-mode ()
;;   "Set `M-h' and `M-l' to decrease/increase the indentation level of blocks."
;;   (evil-define-key 'normal 'local
;;     (kbd "M-h") 'outline-indent-shift-left
;;     (kbd "M-l") 'outline-indent-shift-right
;;     (kbd "gV") 'outline-indent-select)
;;
;;   (unless (derived-mode-p 'prog-mode)
;;     ;; In prog-mode, [[, ]], gj, and gk provide navigation to the previous
;;     ;; and next function, so there is no need to override them.
;;     (evil-define-key 'normal 'local
;;       (kbd "]]") 'outline-indent-forward-same-level
;;       (kbd "[[") 'outline-indent-backward-same-level
;;       (kbd "gj") 'outline-indent-forward-same-level
;;       (kbd "gk") 'outline-indent-backward-same-level))
;;
;;   ;; Set C-<return> to insert a new line with the same indentation
;;   ;; level/depth as the current line just before the next heading
;;   (evil-define-key '(normal insert) 'local
;;     (kbd "C-<return>")
;;     (defun my-evil-outline-indent-insert-heading ()
;;       (interactive)
;;       (when (fboundp 'outline-indent-insert-heading)
;;         (outline-indent-insert-heading)
;;         (evil-insert-state)))))
;;
;; (add-hook 'outline-indent-minor-mode-hook
;;           #'my-evil-define-key-outline-indent-minor-mode)

;;; outline move subtree up and down (M-j and M-k)

(defun my-patched-outline-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p")
  (when (and (fboundp 'outline-back-to-heading)
             (fboundp 'outline-end-of-subtree)
             (fboundp 'outline-end-of-heading)
             (fboundp 'outline-invisible-p)
             (fboundp 'outline-hide-subtree))
    ;; Default `arg' to 1 when omitted in non-interactive calls so `(> arg 0)'
    ;; and `(abs arg)' do not fail with a wrong-type-argument error.
    (setq arg (or arg 1))
    ;; TODO: PATCH3: Restore column
    (let ((column (current-column)))
      (unwind-protect
          (progn
            (outline-back-to-heading)
            (let* ((movfunc (if (> arg 0) 'outline-get-next-sibling
                              'outline-get-last-sibling))
                   ;; Find the end of the subtree to be moved as well as the
                   ;; point to move it to, adding a newline if necessary, to
                   ;; ensure these points are at bol on the line below the
                   ;; subtree.
                   (end-point-func (lambda ()
                                     (with-no-warnings
                                       (let ((outline-blank-line nil))
                                         (outline-end-of-subtree)))
                                     ;; PATCH 4: Include all empty spaces
                                     (skip-chars-forward " \t\n\r")
                                     (unless (eobp)
                                       (beginning-of-line))

                                     (when (and (eobp) (not (bolp)))
                                       (insert "\n"))
                                     (point)))
                   (beg (point))
                   (folded (save-match-data
                             (outline-end-of-heading)
                             (outline-invisible-p)))
                   (end (save-match-data
                          (funcall end-point-func)))
                   (ins-point (make-marker))
                   (cnt (abs arg)))
              ;; Find insertion point, with error handling.
              (goto-char beg)
              (while (> cnt 0)
                (or (funcall movfunc)
                    (progn (goto-char beg)
                           (user-error "Cannot move past superior level")))
                (setq cnt (1- cnt)))
              (if (> arg 0)
                  ;; Moving forward - still need to move over subtree.
                  (funcall end-point-func))
              (move-marker ins-point (point))
              (insert (delete-and-extract-region beg end))
              (goto-char ins-point)
              (if folded (outline-hide-subtree))
              (move-marker ins-point nil)))
        ;; TODO: PATCH3: Restore column
        (unless (= (current-column) column)
          (move-to-column column))))))

(defun my-patched-outline-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG sibling headlines.
Preserve the cursor column and adjust any trailing blank space after the moved
heading."
  (interactive "p")
  (my-patched-outline-move-subtree-down (- (prefix-numeric-value arg))))

;; Bind these to your preferred keys in org-mode-map
(with-eval-after-load 'org
  (when (boundp 'org-mode-map)
    (define-key org-mode-map (kbd "M-<up>") 'org-move-subtree-up)
    (define-key org-mode-map (kbd "M-<down>") 'org-move-subtree-down)))

(defun my-move-subtree-up ()
  "Move the current subtree up according to the active mode."
  (interactive)
  (cond
   ;; Support Combobulate node dragging for tree-sitter modes.
   ((my-combobulate-active-p)
    (let ((prev-point (point)))
      ;; In YAML, starting mid-line can move only the current line down instead
      ;; of the entire block.
      (when (derived-mode-p 'yaml-ts-mode)
        (beginning-of-line))

      (condition-case err
          (call-interactively #'combobulate-drag-up)
        (error
         (goto-char prev-point)
         (user-error "%s" (error-message-string err))))))

   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-move-subtree-up))
    (call-interactively #'org-move-subtree-up))

   ((and (or (bound-and-true-p outline-indent-minor-mode)
             (derived-mode-p 'python-ts-mode)
             (string-suffix-p "-ts-mode" (symbol-name major-mode))
             (and (fboundp 'treesit-parser-list)
                  (treesit-parser-list)))
         (fboundp 'outline-indent-move-subtree-up))
    (call-interactively #'outline-indent-move-subtree-up))

   ((and (bound-and-true-p outline-minor-mode)
         (fboundp 'my-patched-outline-move-subtree-up))
    (call-interactively #'my-patched-outline-move-subtree-up))

   (t
    (user-error "No supported outline movement command is available"))))

(defun my-move-subtree-down ()
  "Move the current subtree down according to the active mode."
  (interactive)
  (cond
   ;; Support Combobulate node dragging for tree-sitter modes.
   ((my-combobulate-active-p)
    (let ((prev-point (point)))
      ;; In YAML, starting mid-line can move only the current line down instead of
      ;; the entire block.
      (when (derived-mode-p 'yaml-ts-mode)
        (beginning-of-line))

      (condition-case err
          (call-interactively #'combobulate-drag-down)
        (error
         (goto-char prev-point)
         (user-error "%s" (error-message-string err))))))

   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-move-subtree-down))
    (call-interactively #'org-move-subtree-down))

   ((and (or (bound-and-true-p outline-indent-minor-mode)
             ;; Move blocks up/down on treesit-mode
             (derived-mode-p 'python-ts-mode)
             (string-suffix-p "-ts-mode" (symbol-name major-mode))
             (and (fboundp 'treesit-parser-list)
                  (treesit-parser-list)))
         (fboundp 'outline-indent-move-subtree-down))
    (call-interactively #'outline-indent-move-subtree-down))

   ((and (bound-and-true-p outline-minor-mode)
         (fboundp 'my-patched-outline-move-subtree-down))
    (call-interactively #'my-patched-outline-move-subtree-down))

   (t
    (user-error "No supported outline movement command is available"))))

(evil-define-key '(normal) my-intercept-mode-map
  (kbd "M-k") 'my-move-subtree-up
  (kbd "M-j") 'my-move-subtree-down)

;;; Navigation

;; M-[ and M-]: Previous and next section
(defun my-evil-blockops--nav-treesit-move-defun (count)
  "Move COUNT defuns using tree-sitter, falling back to buffer boundaries.
Positive COUNT moves backward, negative COUNT moves forward."
  (when (fboundp 'treesit-beginning-of-defun)
    (let ((start-pt (point)))
      (evil-signal-at-bob-or-eob (- count))
      (unless (and (treesit-beginning-of-defun count)
                   (/= (point) start-pt))
        (if (< count 0)
            (goto-char (point-max))
          (goto-char (point-min)))))))

(defun my-evil-blockops--nav-post-jump ()
  "Open folds, recenter window, and pulse the line after an interactive jump."
  (unless (evil-operator-state-p)
    ;; Guard against void-function errors if the package providing this is absent.
    (lightemacs-recenter-maybe)

    (when (fboundp 'pulse-momentary-highlight-one-line)
      (pulse-momentary-highlight-one-line (point)))))

(defun my-evil-blockops--nav-section-core (count is-backward)
  "Core logic for moving sections forward or backward.
COUNT is the absolute number of sections to move.
IS-BACKWARD is non-nil for backward movement, nil for forward."
  (let ((count (abs count))
        (signed-count (if is-backward
                          (- count)
                        count)))
    (evil-signal-at-bob-or-eob signed-count)

    (cond
     ;; Tree-sitter defun navigator
     ((and (fboundp 'my-tree-sitter-p)
           (my-tree-sitter-p)
           (derived-mode-p 'prog-mode)
           (fboundp 'my-evil-blockops--nav-treesit-move-defun))
      (my-evil-blockops--nav-treesit-move-defun signed-count))

     ;; Outline indent minor mode
     ((and (bound-and-true-p outline-indent-minor-mode)
           (fboundp 'outline-indent-backward-same-level)
           (fboundp 'outline-indent-forward-same-level))
      (if is-backward
          (outline-indent-backward-same-level count)
        (outline-indent-forward-same-level count)))

     ;; Standard fallback
     (t
      (if is-backward
          (evil-backward-beginning 'evil-defun count)
        (evil-forward-beginning 'evil-defun count))))

    (unless (evil-operator-state-p)
      (when (fboundp 'my-evil-blockops--nav-post-jump)
        (my-evil-blockops--nav-post-jump)))))

(evil-define-motion my-evil-blockops-nav-forward-section (count)
  "Move the cursor to the beginning of the COUNT-th next section.
COUNT specifies the number of sections to advance forward."
  :jump t
  :type exclusive
  (my-evil-blockops--nav-section-core (or count 1) nil))

(evil-define-motion my-evil-blockops-nav-backward-section (count)
  "Move the cursor to the beginning of the COUNT-th previous section.
COUNT specifies the number of sections to move backward."
  :jump t
  :type exclusive
  (my-evil-blockops--nav-section-core (or count 1) t))

(when (or (display-graphic-p)
          (daemonp))
  ;; Only on display-graphic-p because modern terminals use escape sequences
  ;; beginning with ESC [ (the byte-level equivalent of M-[) to communicate
  ;; complex input like mouse movements and bracketed pastes. Binding M-[
  ;; directly highjacks the prefix of these incoming messages, breaking terminal
  ;; features and dumping coordinate data into the buffer.
  (evil-define-key '(insert motion) 'global (kbd "M-[")
    'my-evil-blockops-nav-backward-section))

(evil-define-key '(insert motion) 'global (kbd "M-]")
  'my-evil-blockops-nav-forward-section)

(evil-define-key '(normal) my-intercept-mode-map
  (kbd "[[") 'my-evil-blockops-nav-backward-section
  (kbd "]]") 'my-evil-blockops-nav-forward-section)

(evil-define-key '(motion) my-intercept-mode-map
  (kbd "{") 'evil-backward-paragraph
  (kbd "}") 'evil-forward-paragraph)

;;; Provide

(provide 'my-evil-blockops)

;; Local variables:
;; env-allow-syntax-checker-package-lint: nil
;; byte-compile-warnings: (not free-vars)
;; End:

;;; my-evil-blockops.el ends here

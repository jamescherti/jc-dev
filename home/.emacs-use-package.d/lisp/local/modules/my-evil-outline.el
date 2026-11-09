;;; my-evil-outline.el --- my-evil-outline -*- lexical-binding: t -*-

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
(require 'outline)
(require 'buffer-guardian)
(require 'my-evil-intercept)

;;; Universal outline promotion, demotion, selection, and insertion

(defun my-promote-subtree (&optional _arg)
  "Promote or shift the current subtree left according to the active mode.
ARG specifies the number of levels to promote when passed as a prefix argument."
  (interactive "p")
  (cond
   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-promote-subtree))
    (call-interactively #'org-promote-subtree))

   ((and (or (bound-and-true-p outline-indent-minor-mode)
             (derived-mode-p 'python-ts-mode))
         (fboundp 'outline-indent-shift-left))
    (call-interactively #'outline-indent-shift-left))

   ((and (bound-and-true-p outline-minor-mode)
         (fboundp 'outline-promote))
    (call-interactively #'outline-promote))

   (t
    (user-error "No supported outline promotion command is available"))))

(defun my-demote-subtree (&optional _arg)
  "Demote or shift the current subtree right according to the active mode.
ARG specifies the number of levels to demote when passed as a prefix argument."
  (interactive "p")
  (cond
   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-demote-subtree))
    (call-interactively #'org-demote-subtree))

   ((and (or (bound-and-true-p outline-indent-minor-mode)
             (derived-mode-p 'python-ts-mode))
         (fboundp 'outline-indent-shift-right))
    (call-interactively #'outline-indent-shift-right))

   ((and (bound-and-true-p outline-minor-mode)
         (fboundp 'outline-demote))
    (call-interactively #'outline-demote))

   (t
    (user-error "No supported outline demotion command is available"))))

(defun my-select-subtree ()
  "Select the current subtree or block according to the active mode."
  (interactive)
  (cond
   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-mark-subtree))
    (call-interactively #'org-mark-subtree))

   ((and (or (bound-and-true-p outline-indent-minor-mode)
             (derived-mode-p 'python-ts-mode))
         (fboundp 'outline-indent-select))
    (call-interactively #'outline-indent-select))

   ((and (bound-and-true-p outline-minor-mode)
         (fboundp 'outline-mark-subtree))
    (call-interactively #'outline-mark-subtree))

   (t
    (user-error "No supported outline selection command is available"))))

(defun my-insert-heading ()
  "Insert a new line or heading at current level and switch to insert state."
  (interactive)
  (cond
   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-insert-heading-respect-content))
    (call-interactively #'org-insert-heading-respect-content)
    (when (fboundp 'evil-insert-state)
      (evil-insert-state)))

   ((and (or (bound-and-true-p outline-indent-minor-mode)
             (derived-mode-p 'python-ts-mode))
         (fboundp 'outline-indent-insert-heading))
    (outline-indent-insert-heading)
    (when (fboundp 'evil-insert-state)
      (evil-insert-state)))

   ((and (bound-and-true-p outline-minor-mode)
         (fboundp 'outline-insert-heading))
    (call-interactively #'outline-insert-heading)
    (when (fboundp 'evil-insert-state)
      (evil-insert-state)))

   (t
    (user-error "No supported outline heading insertion command is available"))))

(defun my-outline-forward-same-level (&optional _arg)
  "Move forward to next sibling heading, preserving standard prog-mode navigation.
ARG specifies how many sibling headings to advance forward."
  (interactive "p")
  (cond
   ((derived-mode-p 'prog-mode)
    (let ((cmd (lookup-key evil-normal-state-map (kbd "]]"))))
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
  "Move backward to previous sibling heading, preserving standard prog-mode navigation.
ARG specifies how many sibling headings to retreat backward."
  (interactive "p")
  (cond
   ((derived-mode-p 'prog-mode)
    (let ((cmd (lookup-key evil-normal-state-map (kbd "[[\\"))))
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
  (kbd "gV")  'my-select-subtree
  (kbd "gj")  'my-outline-forward-same-level
  (kbd "gk")  'my-outline-backward-same-level)

(evil-define-key '(normal insert) my-intercept-mode-map
  (kbd "C-<return>") 'my-insert-heading)

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
             (fboundp 'outline-hide-subtree))
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
                                     (let ((outline-blank-line nil))
                                       (outline-end-of-subtree))
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
   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-move-subtree-up))
    (call-interactively #'org-move-subtree-up))

   ((and (or (bound-and-true-p outline-indent-minor-mode)
             (derived-mode-p 'python-ts-mode))
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
   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-move-subtree-down))
    (call-interactively #'org-move-subtree-down))

   ((and (or (bound-and-true-p outline-indent-minor-mode)
             ;; Move blocks up/down on treesit-mode
             (derived-mode-p 'python-ts-mode))
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

;;; outline: Restore column: move subtree up/down

(defun my-advice-outline-move-subtree-up-down (orig-fun &rest args)
  "Move the current subtree up/down past ARGS headlines of the same level.
This function ensures the last blank line is included, even when
`outline-blank-line' is set to t. It also restores the cursor position,
addressing the issue where the cursor might be reset after the operation.
ORIG-FUN is the function ARGS are the arguments."
  (interactive "p")
  (if (bound-and-true-p outline-minor-mode)
      (apply orig-fun (or args 1))
    (let ((column (current-column)))
      (unwind-protect
          (apply orig-fun (or args 1))
        (move-to-column column)))))

(with-eval-after-load 'outline
  (advice-add 'outline-move-subtree-up
              :around #'my-advice-outline-move-subtree-up-down)
  (advice-add 'outline-move-subtree-down
              :around #'my-advice-outline-move-subtree-up-down))

;;; Restore column: next/previous heading

;; (evil-define-key 'normal 'global (kbd "[[") #'evil-backward-section-begin)
;; (evil-define-key 'normal 'global (kbd "]]") #'evil-backward-section-end)

;; Replace with

;; TODO fix this
;; (defun my-advice-outline-restore-column (orig-fun &rest args)
;;   "Advice to restore the column.
;; ORIG-FUN is the function ARGS are the arguments."
;;   (interactive "p")
;;   (if (bound-and-true-p outline-minor-mode)
;;       (apply orig-fun args)
;;     (let ((column (current-column)))
;;       (unwind-protect
;;           (apply orig-fun (or args 1))
;;         (move-to-column column)))))
;;
;; (with-eval-after-load 'outline
;;   (advice-add 'outline-next-visible-heading
;;               :around #'my-advice-outline-restore-column)
;;   (advice-add 'outline-previous-visible-heading
;;               :around #'my-advice-outline-restore-column))

;;; Provide

(provide 'my-evil-outline)

;; Local variables:
;; env-allow-syntax-checker-package-lint: nil
;; End:

;;; my-evil-outline.el ends here

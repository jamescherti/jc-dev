;;; mod-evil-smart-next-line.el --- Evil Smart Next Line -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/jc-dev
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

;;; Require

(require 'evil)

;;; Main code

;;; Smart previous/next line

(defsubst evilcursor--get-category-at-point ()
  "Get the category at point as an interned symbol.
Defined via `defsubst' so the byte-compiler inlines the execution,
eliminating function-call overhead (stack frames) during rapid scrolling."
  ;; Using `pos-bol' directly avoids the heavy overhead of wrapping the check
  ;; in a `save-excursion' block and executing a `goto-char' command.
  (let ((prop (get-text-property (pos-bol) 'category)))
    (cond
     ((stringp prop) (intern prop))

     ((symbolp prop)
      ;; Fast path: 'intern-soft' checks if the symbol is already interned. If
      ;; it is, it returns the symbol directly in O(1) time. This results in
      ;; zero memory allocation, completely avoiding the garbage collection
      ;; overhead that would occur if we created temporary strings.
      (or (intern-soft prop)
          ;; Slow path: If the symbol is uninterned (which can happen with
          ;; generated text properties), 'intern-soft' returns nil. We then
          ;; fallback to allocating a string and explicitly interning it. This
          ;; guarantees strict pointer equality ('eq') will work downstream.
          (intern (symbol-name prop))))

     (t
      nil))))

(defsubst evilcursor--outline-invisible-p (pos)
  "Return non-nil when POS is invisible.
POS is the buffer position to check."
  (when (>= pos 1)
    (cond
     ((or (eq major-mode 'org-mode)
          (derived-mode-p 'org-mode))
      (if (fboundp 'org-fold-folded-p)
          (org-fold-folded-p pos)
        (when (fboundp 'org-invisible-p)
          (org-invisible-p pos))))

     ((and (or (bound-and-true-p outline-minor-mode)  ; folded?
               (eq major-mode 'outline-mode)
               (derived-mode-p 'outline-mode))
           (fboundp 'outline-invisible-p))
      (outline-invisible-p pos))

     (t
      (invisible-p pos)))))

;; TODO should this be part of Emacs ? PATCH
;; Doesn't work when an org mode line contain: *line content*
(defun evilcursor--after-vertical-movement ()
  "Run this after a vertical movement."
  (unless (input-pending-p)
    (let ((p (point)))
      (when (and
             ;; Landed on an invisible line
             (evilcursor--outline-invisible-p
              (if (and (eolp) (not (bobp)))
                  ;; Without this, invisible-p is nil when eolp
                  (1- p)
                p)))
        (vertical-motion 0)
        (goto-char (pos-eol))))))

(defun evilcursor-next-visual-line (count)
  "Move the cursor COUNT screen lines down.
COUNT is the number of lines to move."
  (evil-next-visual-line (or count 1))
  (evilcursor--after-vertical-movement))

(defvar my-cpu-architecture nil
  "The native CPU architecture determined by GCC.")

(defun evilcursor-previous-visual-line (count)
  "Move the cursor COUNT screen lines up.
COUNT is the number of lines to move."
  (let ((line-move-visual t))
    (when (and (numberp temporary-goal-column)
               (< temporary-goal-column 0))
      (setq temporary-goal-column 0))
    (evil-line-move (- (or count 1))))
  (evilcursor--after-vertical-movement))

(defun evilcursor-forward-line (n)
  "Move N lines forward (backward if N is negative).
N is the number of lines to move.
More accurate than `evil-next-line' and `evil-previous-line' when lines are not
truncated."
  (interactive)
  (setq n (or n 1))

  ;; Prevent the command loop from moving the cursor after we place it
  ;; Skips the redundant C-level property scan since we handle it manually.
  (setq disable-point-adjustment t)

  (cond
   ;; ((minibufferp)
   ;;  ;; ignore-errors fixes issues with icomplete
   ;;  (ignore-errors
   ;;    (if (> n 0)
   ;;        (next-line-or-history-element)
   ;;      (previous-line-or-history-element))))

   ;; Not Minibuffer
   (t
    (let* ((gc-cons-threshold most-positive-fixnum)
           (forwardp (> n 0))
           (count (if forwardp n (- n)))
           (line-number-type (bound-and-true-p display-line-numbers-type))

           ;; By default, Emacs sets line-move-visual to t, which forces the
           ;; movement commands to query the C-level display engine. The display
           ;; engine calculates screen pixels, font sizes, and text wrapping to
           ;; determine where the next visual line starts.
           ;;
           ;; Binding (line-move-visual nil) bypasses the display engine
           ;; entirely. Emacs will move the point strictly by counting newline
           ;; characters in the buffer (\n), which is an O(1) buffer-position
           ;; math operation and significantly faster.
           ;;
           ;; Since the visual movement functions (evilcursor-next-visual-line
           ;; and evilcursor-previous-visual-line) explicitly handle visual
           ;; movement anyway, forcing nil at this scope guarantees that the
           ;; fast paths (like func-change-line) do not accidentally trigger
           ;; expensive visual calculations.
           (line-move-visual nil)

           ;; track-eol and evil-track-eol: I set both to nil. When these are
           ;; enabled, if the cursor is at the end of a line, moving vertically
           ;; forces Emacs to calculate the exact end position of the target
           ;; line to keep the cursor at the boundary. This requires checking
           ;; the current state (eolp) and then querying the buffer for the
           ;; target line's length. Setting these to nil bypasses this
           ;; EOL-tracking logic, relying solely on temporary-goal-column, which
           ;; involves less computation.
           (evil-track-eol nil)
           (track-eol nil)

           (evil-respect-visual-line-mode nil)

           (func-change-line (if forwardp
                                 #'evil-next-line
                               #'evil-previous-line))
           (func-change-line-visual (if forwardp
                                        #'evilcursor-next-visual-line
                                      #'evilcursor-previous-visual-line)))
      (cond
       ;; TODO patch embark
       ((eq major-mode 'embark-collect-mode)
        (let ((previous-cat (evilcursor--get-category-at-point))
              (start-point (point)))
          (funcall func-change-line count)
          (let ((current-cat (evilcursor--get-category-at-point)))
            (when (and (not (= start-point (point)))
                       previous-cat
                       current-cat
                       (eq current-cat 'embark-collect-group-button)
                       (not (eq previous-cat 'embark-collect-group-button)))
              (let ((next-cat (save-excursion
                                (funcall func-change-line count)
                                (evilcursor--get-category-at-point))))
                (when (and next-cat
                           (not (eq next-cat 'embark-collect-group-button)))
                  (funcall func-change-line count)))))))

       ((eq line-number-type 'visual)
        (funcall func-change-line-visual count))

       ((eq line-number-type 'relative)
        (if (and truncate-lines
                 (= count 1))
            ;; This speeds-up scrolling because it does not take into
            ;; consideration visual things
            (progn
              (funcall func-change-line count))
          (funcall func-change-line-visual count)))

       ((eq line-number-type t)
        ;; TODO doesn't work when count > 1
        (funcall func-change-line count))

       (t
        (message
         "Unsupported evilcursor-smart-next-line/evilcursor-smart-previous-line.")))))))

(evil-define-motion evilcursor-smart-next-line (count)
  "Move smart next line down by COUNT."
  :type line
  (unless count
    (setq count 1))
  (evilcursor-forward-line count))

(evil-define-motion evilcursor-smart-previous-line (count)
  "Move smart previous line up by COUNT."
  :type line
  (unless count
    (setq count 1))
  (evilcursor-forward-line (* count -1)))

(defun my-setup-local-evilcursor-smart-next-prev-line ()
  "Setup smart next/previous line."
  (evil-define-key 'normal 'local
    (kbd "k") #'evilcursor-smart-previous-line
    (kbd "j") #'evilcursor-smart-next-line)
  (evil-define-key 'insert 'local
    (kbd "M-k") #'evilcursor-smart-previous-line
    (kbd "M-j") #'evilcursor-smart-next-line))

(add-hook 'embark-collect-mode-hook #'my-setup-local-evilcursor-smart-next-prev-line)

;; Alternative: SMART

(evil-define-key '(insert visual) 'global
  (kbd "M-k") #'evilcursor-smart-previous-line
  (kbd "M-j") #'evilcursor-smart-next-line)
(evil-define-key 'normal 'global
  (kbd "k") #'evilcursor-smart-previous-line
  (kbd "j") #'evilcursor-smart-next-line)
(evil-define-key 'motion 'global
  (kbd "k") nil
  (kbd "j") nil)

(evil-define-key 'insert evil-eval-map
  (kbd "M-k") #'previous-history-element
  (kbd "M-j") #'next-history-element)
(evil-define-key 'insert evil-ex-completion-map
  (kbd "M-k") #'previous-history-element
  (kbd "M-j") #'next-history-element)
(evil-define-key 'insert evil-ex-search-keymap
  (kbd "M-k") #'previous-history-element
  (kbd "M-j") #'next-history-element)

;;; Provide

(provide 'mod-evil-smart-next-line)

;;; mod-evil-smart-next-line.el ends here

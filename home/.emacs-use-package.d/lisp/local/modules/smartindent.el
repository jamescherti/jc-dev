;;; smartindent.el --- Enhance code indentation -*- lexical-binding: t -*-

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

;;; Indent style

;; Bind the RET key to `comment-indent-new-line'. This ensures that pressing
;; Enter within a comment context will both insert a new line and correctly
;; indent it to match the comment's indentation level, facilitating more
;; consistent formatting of multi-line comments.
(unless noninteractive
  ;; RET: Rebound to 'comment-indent-new-line'. This ensures that pressing Enter
  ;; inside or after a comment block correctly continues the comment structure
  ;; with proper indentation, a standard practice for maintaining clean source
  ;; code documentation.
  (global-set-key (kbd "RET") #'comment-indent-new-line)

  ;; C-<return>: Explicitly mapped to 'newline-and-indent'. This gives the user
  ;; a dedicated shortcut to create a new line with proper indentation on
  ;; demand, rather than relying on automatic triggers.
  (global-set-key (kbd "C-<return>") #'newline-and-indent))

;; Disable 'electric-indent-mode' globally. This mode is the root cause of
;; unexpected cursor jumps and re-indentation triggered by character insertion.
;; By setting it to -1 and ensuring the default value is nil, we guarantee that
;; typing or pressing Enter never triggers automatic reformatting, providing a
;; consistent experience across all buffers.
(with-eval-after-load 'electric
  (when (bound-and-true-p electric-indent-mode)
    (electric-indent-mode -1))
  (setq-default electric-indent-mode nil)
  (advice-add 'electric-indent-mode :override #'ignore)
  (advice-add 'electric-indent-local-mode :override #'ignore))

;; In certain modes (like `text-mode'), pressing Return multiple times causes
;; Emacs to lose your current indentation. The cursor drops to column 0 on
;; consecutive blank lines instead of maintaining the indent level.
;;
;; The Cause:
;;
;; When you create a new indented line, Emacs runs `indent-according-to-mode'.
;; If the mode's specific indentation function (such as `indent-relative') is
;; listed in `indent-line-ignored-functions', Emacs intercepts the call. Instead
;; of running the proper function, Emacs applies a basic fallback that only
;; checks the immediately preceding line. If that preceding line is blank, the
;; fallback fails and sets the indentation to zero.
;;
;; The Solution:
;;
;; By setting `indent-line-ignored-functions' to nil globally, we disable this
;; interception mechanism completely. This forces Emacs to always execute the
;; assigned `indent-line-function'. These underlying functions already have the
;; proper logic to scan backward over multiple blank lines until they find actual
;; text to copy the indentation from. Clearing the ignore list ensures these
;; functions are permitted to run and do their job.
;;
;; TODO send an issue to Emacs?
(setq-default indent-line-ignored-functions nil)

;;; smartindent-relative: Default indentation (relative)

(defun smartindent-relative ()
  "Indent based on the previous non-blank line with lookahead for block structures.

This function wraps `indent-relative' with the following enhancements:

1. Strict First-Column Alignment: It hardcodes the `:first-only' flag, meaning
   it will only ever align to the first text column of the previous line,
   ignoring subsequent words (unlike the default `indent-relative' behavior).

2. Strict Tab Fallback: It suppresses the `tab-to-tab-stop' fallback unless
   the command was triggered by an explicit Tab key press. This prevents
   unwanted tab stops when pressing Return.

3. Lookahead Alignment: If inserting a blank line above an indented block
   (e.g., in YAML), it checks the indentation of the next line. If the next
   line is indented deeper than the current line, it aligns the current line
   to match the next line."

  (let ((orig-point (point)))
    (unwind-protect
        (indent-relative :first-only :unindented-ok)

      (when (= orig-point (point))
        (cond
         ;; Indent when the user presses tab (handles both terminal and GUI Emacs)
         ((memq last-command-event '(?\t tab))
          (tab-to-tab-stop))))

      ;; This is useful for Yaml
      ;; - name: Create file /etc/profile.d/mozilla-custom-wayland.sh
      ;;   ansible.builtin.copy: |     <-------------- Press enter here
      ;;     |     <------- Same indentation as next line
      ;;     dest: /etc/profile.d/mozilla-custom-wayland.sh
      (let ((previous-indentation (save-excursion
                                    (when (= 0 (forward-line -1))
                                      (current-indentation))))
            (cur-indentation (current-indentation))
            (next-indentation (save-excursion
                                (when (= 0 (forward-line 1))
                                  (current-indentation)))))
        (when (and previous-indentation
                   next-indentation
                   (> next-indentation cur-indentation))
          (indent-line-to next-indentation))))))

;; (defun smartindent-relative ()
;;   "Indent based on the previous non-blank line with lookahead for blocks.
;; This function wraps `indent-relative' with two enhancements:
;; 1. Strict Tab Fallback: It suppresses the default `tab-to-tab-stop' fallback
;;    behavior of `indent-relative' unless the command was triggered by an explicit
;;    TAB key press. This prevents unwanted tab stops from being inserted when
;;    simply pressing Return.
;; 2. Lookahead Alignment: After calculating the relative indentation from the
;;    previous line, it checks the indentation of the immediately following line.
;;    If the next line has a deeper indentation than the current line, it aligns
;;    the current line to match the next line. This is particularly useful in
;;    formats like YAML when inserting a new line above an indented block."
;;   ;; (indent-relative :first-only :unindented-ok)
;;   ;; (when indent-line-ignored-functions
;;   ;;   (setq-local indent-line-ignored-functions '()))
;;
;;   (let ((orig-point (point)))
;;     (unwind-protect
;;         ;; first-only: Indent the current line like the previous nonblank line.
;;         ;; Indent to the first indentation position in the previous nonblank
;;         ;; line if that position is greater than the current column.
;;         ;;
;;         ;; unindented-ok: controls whether indent-relative is allowed to do
;;         ;; nothing when it cannot find a sensible indentation target.
;;         (indent-relative :first-only :unindented-ok)
;;
;;       (when (= orig-point (point))
;;         (cond
;;          ;; Indent when the user presses tab
;;          ((eq last-command-event ?\t)
;;           (tab-to-tab-stop))))
;;
;;       ;; This is useful for Yaml
;;       ;; - name: Create file /etc/profile.d/mozilla-custom-wayland.sh
;;       ;;   ansible.builtin.copy: |     <-------------- Press enter here
;;       ;;     |     <------- Same indentation as next line
;;       ;;     dest: /etc/profile.d/mozilla-custom-wayland.sh
;;       (let ((previous-indentation (save-excursion
;;                                     (when (= 0 (forward-line -1))
;;                                       (current-indentation))))
;;             (cur-indentation (current-indentation))
;;             (next-indentation (save-excursion
;;                                 (when (= 0 (forward-line 1))
;;                                   (current-indentation)))))
;;         (when (and previous-indentation
;;                    next-indentation
;;                    (> next-indentation cur-indentation))
;;           (indent-line-to next-indentation))))))

(setq-default indent-line-function #'smartindent-relative)

;;; smartindent-indent-relative-to-visible

;; (defun my-set-indent-line-relative ()
;;   "Indent-line relative."
;;   (setq-local indent-line-functions #'smartindent-relative))
;;
;; (add-hook 'bash-ts-mode-hook #'my-set-indent-line-relative)
;; (add-hook 'sh-mode-hook #'my-set-indent-line-relative)
;; (add-hook 'python-mode-hook #'my-set-indent-line-relative)
;; (add-hook 'python-ts-mode-hook #'my-set-indent-line-relative)

(defun smartindent-indent-relative-to-visible ()
  "Indent to the next indent point in the previous visible, non-empty line.

An indent point is a non-whitespace character following whitespace.

If FIRST-ONLY is non-nil, indent to the first such point.

If no suitable indent point is found and UNINDENTED-OK is nil, fall back to
`tab-to-tab-stop`."
  (interactive "P")
  (let ((first-only t)
        (unindented-ok t))
    (when (and abbrev-mode
               (eq (char-syntax (preceding-char)) ?w))
      (expand-abbrev))
    (let ((start-column (current-column))
          indent)
      (save-excursion
        (goto-char (line-beginning-position))
        (while (and (not (bobp))
                    (progn
                      (forward-line -1)
                      (or (invisible-p (point))
                          (save-excursion
                            (goto-char (line-beginning-position))
                            (looking-at-p "^[ \t]*$"))))))
        (cond
         ((and (or (derived-mode-p 'yaml-mode)
                   (derived-mode-p 'yaml-ts-mode))
               (looking-at "^[ \t]*-"))
          (save-excursion
            (goto-char (line-beginning-position))
            (search-forward "-" nil t)
            (setq indent (+ 1 (current-column)))))
         (t
          (let ((end (save-excursion
                       (forward-line 1)
                       (point))))
            (move-to-column start-column)
            (when (> (current-column) start-column)
              (backward-char 1))
            (if (> (current-column) start-column)
                (backward-char 1))
            (or (looking-at "[ \t]")
                first-only
                (skip-chars-forward "^ \t" end))
            (skip-chars-forward " \t" end)
            (or (= (point) end)
                (setq indent (current-column)))))))
      (cond
       (indent
        (let ((opoint (point-marker)))
          (indent-to indent 0)
          (if (> opoint (point))
              (goto-char opoint))
          (move-marker opoint nil)))
       (unindented-ok nil)
       (t
        (tab-to-tab-stop))))))

;;; Provide

(provide 'smartindent)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; env-allow-syntax-checker-package-lint: nil
;; End:

;;; smartindent.el ends here

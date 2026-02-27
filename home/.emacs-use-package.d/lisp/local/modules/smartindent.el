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

(setq indent-tabs-mode t)
(defun my-disable-indent-tabs-mode ()
  "Disable `indent-tabs-mode'."
  (setq-local indent-tabs-mode nil))
(add-hook 'emacs-lisp-mode-hook #'my-disable-indent-tabs-mode)

;; Bind the `RET` (Return) key to `comment-indent-new-line`.
;; This ensures that pressing Enter within a comment context will both insert a
;; new line and correctly indent it to match the comment's indentation level,
;; facilitating more consistent formatting of multi-line comments.
(global-set-key (kbd "C-<return>") #'newline-and-indent)
(global-set-key (kbd "RET") #'comment-indent-new-line)

;; TODO: Contribute to Emacs?
;; Values that are ignored by indent-according-to-mode.
;;
;; Indent line in proper way for current major mode. Normally, this is done by
;; calling the function specified by the variable indent-line-function. However,
;; if the value of that variable is present in the indent-line-ignored-functions
;; variable, handle it specially (since those functions are used for tabbing);
;; in that case, indent by aligning to the previous non-blank line.
;;
;; In modes such as `text-mode', calling `newline-and-indent' multiple times
;; removes the indentation. This issue is caused by
;; `indent-line-ignored-functions'. The following code fixes the problem and
;; ensures that text is properly indented using `indent-relative' or
;; `indent-relative-first-indent-point'.
(setq indent-line-ignored-functions '())

;; This loses indentation
;; (push #'smartindent-relative indent-line-ignored-functions)
;; (push #'smartindent-indent-relative-to-visible indent-line-ignored-functions)

;;; Default indentation (relative)

(defun smartindent-relative ()
  "Indent based on the indentation of the previous non-blank line.
If the first indentation position of the previous non-blank line is greater than
the current column, indent the current line to match that position. If no
previous non-blank line exists or the current line is already indented properly,
align the line to the nearest tab stop."
  ;; (indent-relative :first-only :unindented-ok)
  (let ((point (point)))
    (unwind-protect
        ;; first-only: Indent the current line like the previous nonblank line.
        ;; Indent to the first indentation position in the previous nonblank
        ;; line if that position is greater than the current column.
        ;;
        ;; unindented-ok: controls whether indent-relative is allowed to do
        ;; nothing when it cannot find a sensible indentation target.
        (indent-relative :first-only :unindented-ok)
      ;; Point hasn't changed
      (when (eq point (point))
        (cond
         ;; TODO Indent when the next non blank line is not indented

         ;; Insert a tab when the user presses tab
         ((eq last-command-event ?\t)
          (tab-to-tab-stop)))))))

(setq-default indent-line-function #'smartindent-relative)

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
        (beginning-of-line)
        (while (and (not (bobp))
                    (progn
                      (forward-line -1)
                      (or (invisible-p (point))
                          (save-excursion
                            (beginning-of-line)
                            (looking-at-p "^[ \t]*$"))))))
        (cond
         ((and (derived-mode-p 'yaml-mode)
               (derived-mode-p 'yaml-ts-mode)
               (looking-at "^[ \t]*-"))
          (save-excursion
            (beginning-of-line)
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
;; End:

;;; smartindent.el ends here

;;; restore-column.el --- Restore cursor column after save hooks -*- lexical-binding: t; -*-

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

;; This package provides a minor mode that saves the cursor column position
;; before a buffer is saved and restores it afterward. It uses a virtual overlay
;; to display the cursor position if restoring the column would require
;; inserting trailing spaces.

;;; Code:

(defgroup restore-column nil
  "Restore the cursor column after save hooks."
  :group 'convenience
  :prefix "restore-column-")

(defcustom restore-column-before-save-hook-depth -99
  "Depth for `restore-column--before-save-hook' in `before-save-hook'.
A negative depth ensures it runs early to save the column before other
modifications occur."
  :type 'integer
  :group 'restore-column)

(defcustom restore-column-after-save-hook-depth 99
  "Depth for `restore-column--after-save-hook' in `after-save-hook'.
A positive depth ensures it runs late to restore the column after all other
post-save processing is complete."
  :type 'integer
  :group 'restore-column)

(defvar-local restore-column--saved-column nil
  "Internal variable to store the column position before saving.")

(defvar-local restore-column--virtual-overlay t
  "Overlay used to display virtual whitespace.")

(defsubst restore-column--pos-bol ()
  "Return the position of the first character on the current line."
  (if (fboundp 'pos-bol)
      (pos-bol)
    (let ((inhibit-field-text-motion t))
      (line-beginning-position))))

(defsubst restore-column--pos-eol ()
  "Return the position of the last character on the current line."
  (if (fboundp 'pos-eol)
      (pos-eol)
    (let ((inhibit-field-text-motion t))
      (line-end-position))))

(defun restore-column--clear-virtual-state ()
  "Clean up virtual overlay and hooks."
  (when restore-column--virtual-overlay
    (delete-overlay restore-column--virtual-overlay)
    (setq restore-column--virtual-overlay nil))
  (remove-hook 'pre-command-hook #'restore-column--pre-command t)
  (remove-hook 'post-command-hook #'restore-column--post-command t))

(defun restore-column--pre-command ()
  "Materialize virtual spaces before modifying commands."
  (let ((ov restore-column--virtual-overlay))
    (when ov
      (let ((pos (overlay-start ov))
            (spaces (overlay-get ov 'restore-column-spaces))
            (cursor-offset (overlay-get ov 'restore-column-cursor-offset))
            (current-pos (point)))
        (when (and pos spaces
                   (not (memq this-command
                              '(next-line previous-line forward-char backward-char
                                          right-char left-char end-of-line beginning-of-line
                                          ignore keyboard-quit mwheel-scroll pixel-scroll-precision
                                          isearch-forward isearch-backward
                                          evil-next-line evil-previous-line
                                          evil-forward-char evil-backward-char))))
          (let ((inhibit-redisplay t))
            (restore-column--clear-virtual-state)
            (goto-char pos)
            (insert spaces)
            (if (= current-pos pos)
                (when cursor-offset
                  (goto-char (+ pos cursor-offset)))
              (goto-char current-pos))))))))

(defun restore-column--post-command ()
  "Discard virtual spaces if point moves away from the line."
  (let ((ov restore-column--virtual-overlay))
    (when ov
      (let ((ov-pos (overlay-start ov)))
        (when (or (null ov-pos)
                  (< ov-pos (restore-column--pos-bol))
                  (> ov-pos (restore-column--pos-eol)))
          (restore-column--clear-virtual-state))))))

(defun restore-column--before-save-hook ()
  "Save the current cursor column position.
This function is intended to be added to `before-save-hook'."
  (when restore-column-local-mode
    (setq restore-column--saved-column (current-column))))

(defun restore-column--after-save-hook ()
  "Restore the cursor to the previously saved column.
This function is intended to be added to `after-save-hook'."
  (when (and restore-column-local-mode
             restore-column--saved-column
             (not buffer-read-only))
    (unwind-protect
        (let ((inhibit-redisplay t))
          (when (/= (current-column) restore-column--saved-column)
            (restore-column--clear-virtual-state)
            (move-to-column restore-column--saved-column)
            (let ((missing (- restore-column--saved-column (current-column))))
              (if (> missing 0)
                  (let* ((spaces (make-string missing ?\s))
                         (ov (make-overlay (point) (point) nil nil nil)))
                    (put-text-property (1- missing) missing 'cursor t spaces)
                    (overlay-put ov 'after-string spaces)
                    (overlay-put ov 'restore-column-spaces spaces)
                    (overlay-put ov 'restore-column-cursor-offset missing)
                    (setq restore-column--virtual-overlay ov)
                    (add-hook 'pre-command-hook #'restore-column--pre-command nil t)
                    (add-hook 'post-command-hook #'restore-column--post-command nil t))
                (set-buffer-modified-p nil)))))
      (setq restore-column--saved-column nil))))

(defun restore-column-enable ()
  "Turn on `restore-column-local-mode' for the current buffer."
  (restore-column-local-mode 1))

;;;###autoload
(define-minor-mode restore-column-local-mode
  "Minor mode to restore the cursor column after saving."
  :global nil
  :lighter " RCol"
  :group 'restore-column
  (if restore-column-local-mode
      (progn
        (add-hook 'before-save-hook #'restore-column--before-save-hook
                  (if (>= emacs-major-version 27)
                      restore-column-before-save-hook-depth
                    t)
                  t)
        (add-hook 'after-save-hook #'restore-column--after-save-hook
                  (if (>= emacs-major-version 27)
                      restore-column-after-save-hook-depth
                    t)
                  t))
    (restore-column--clear-virtual-state)
    (remove-hook 'before-save-hook #'restore-column--before-save-hook t)
    (remove-hook 'after-save-hook #'restore-column--after-save-hook t)))

;;;###autoload
(define-globalized-minor-mode restore-column-global-mode
  restore-column-local-mode
  restore-column-enable)

(provide 'restore-column)
;;; restore-column.el ends here

;;; restore-column.el --- Restore cursor column after save hooks -*- lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/jc-dev
;; Package-Requires: ((emacs "24.3"))
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

(defvar-local restore-column--virtual-overlay nil
  "Overlay used to display virtual whitespace.")

(defun restore-column--clear-virtual-state ()
  "Clean up virtual overlay and hooks without modifying the buffer."
  ;; Verify that `restore-column--virtual-overlay' holds an overlay object.
  (when (overlayp restore-column--virtual-overlay)
    ;; Detach the overlay from its buffer and destroy its visual display.
    (delete-overlay restore-column--virtual-overlay)
    (setq restore-column--virtual-overlay nil))
  (remove-hook 'pre-command-hook #'restore-column--clear-virtual-state t)
  ;; Ensure any lingering hook from previous versions is also removed
  (remove-hook 'pre-command-hook 'restore-column--pre-command-handler t))

(defun restore-column--before-save-hook ()
  "Save the current visual column position before saving.
This function is intended to be added to `before-save-hook'."
  (when (bound-and-true-p restore-column-local-mode)
    (let ((col (current-column)))
      ;; If the virtual overlay is active, the user's visual cursor is further
      ;; ahead than `point'. We must calculate the visual column. Verify the
      ;; variable is an overlay and `overlay-buffer' confirms it is still
      ;; attached to a live buffer (not deleted).
      (when (and (overlayp restore-column--virtual-overlay)
                 ;; Return the buffer OVERLAY belongs to.
                 ;; Return nil if OVERLAY has been deleted.
                 (overlay-buffer restore-column--virtual-overlay))
        ;; Retrieve the virtual whitespace string stored in the overlay's
        ;; `after-string' property.
        (let ((disp (overlay-get restore-column--virtual-overlay 'after-string)))
          (when (stringp disp)
            ;; Subtract 1 because the display string includes the propertized
            ;; cursor character at the end.
            (setq col (+ col (1- (length disp))))))
        (restore-column--clear-virtual-state))

      ;; Save the column
      (setq restore-column--saved-column col))))

(defun restore-column--after-save-hook ()
  "Restore the cursor to the previously saved column using an overlay.
This function is intended to be added to `after-save-hook'."
  (when (and (bound-and-true-p restore-column-local-mode)
             restore-column--saved-column)
    (unwind-protect
        (when (/= (current-column) restore-column--saved-column)
          (restore-column--clear-virtual-state)
          ;; Pass nil for FORCE so move-to-column never inserts spaces
          (move-to-column restore-column--saved-column nil)
          (let ((missing (- restore-column--saved-column (current-column))))
            (when (> missing 0)
              (let ((display-spaces (make-string (1+ missing) ?\s)))
                (put-text-property missing (1+ missing) 'cursor t display-spaces)
                ;; Create a zero-length overlay at `point' in the current
                ;; buffer where insertions at `point' stay before the overlay.
                (let ((ov (make-overlay (point) (point) (current-buffer) t nil)))
                  (overlay-put ov 'after-string display-spaces)
                  (setq restore-column--virtual-overlay ov)
                  (add-hook 'pre-command-hook
                            #'restore-column--clear-virtual-state nil t))))))
      (setq restore-column--saved-column nil))))

(defun restore-column--enable ()
  "Turn on `restore-column-local-mode' for the current buffer."
  (unless (minibufferp)
    (restore-column-local-mode 1)))

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
                    nil)
                  t)
        (add-hook 'after-save-hook #'restore-column--after-save-hook
                  (if (>= emacs-major-version 27)
                      restore-column-after-save-hook-depth
                    t)
                  t))
    (restore-column--clear-virtual-state)
    (setq restore-column--saved-column nil)
    (remove-hook 'before-save-hook #'restore-column--before-save-hook t)
    (remove-hook 'after-save-hook #'restore-column--after-save-hook t)))

;;;###autoload
(define-globalized-minor-mode restore-column-global-mode
  restore-column-local-mode
  restore-column--enable
  :group 'restore-column)

(provide 'restore-column)
;;; restore-column.el ends here

;;; restore-column.el --- Restore cursor column after save hooks -*- lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/jc-dev
;; Package-Requires: ((emacs "24.3"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

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

(defun restore-column--clear-virtual-state (&optional ov &rest _)
  "Clean up virtual overlay and hooks without modifying the buffer.
If OV is provided (called from an overlay modification hook), ensure cleanup
runs in that overlay's buffer."
  (let ((buf (if (overlayp ov) (overlay-buffer ov) (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (overlayp restore-column--virtual-overlay)
          (delete-overlay restore-column--virtual-overlay)
          (setq restore-column--virtual-overlay nil))
        (remove-hook 'post-command-hook #'restore-column--post-command-handler t)))))

(defun restore-column--post-command-handler ()
  "Clear the virtual overlay if point moved or user cancelled/moved."
  (when (or (not (overlayp restore-column--virtual-overlay))
            (not (eq (point) (overlay-start restore-column--virtual-overlay)))
            (memq this-command '(keyboard-quit
                                 backward-char left-char
                                 forward-char right-char
                                 move-beginning-of-line
                                 move-end-of-line)))
    (restore-column--clear-virtual-state)))

(defun restore-column--active-buffer ()
  "Return the selected window's buffer if it shares text with the buffer being saved."
  (let* ((win-buf (window-buffer (selected-window)))
         (win-base (or (buffer-base-buffer win-buf) win-buf))
         (cur-base (or (buffer-base-buffer (current-buffer)) (current-buffer))))
    (when (eq cur-base win-base)
      win-buf)))

(defun restore-column--before-save-hook ()
  "Save the current visual column position before saving."
  (let ((target-buf (restore-column--active-buffer)))
    (when target-buf
      (with-current-buffer target-buf
        (when restore-column-local-mode
          (let ((col (current-column)))
            (when (and (overlayp restore-column--virtual-overlay)
                       (overlay-buffer restore-column--virtual-overlay))
              (let ((disp (overlay-get restore-column--virtual-overlay 'after-string)))
                (when (stringp disp)
                  (setq col (+ col (1- (length disp))))))
              (restore-column--clear-virtual-state))
            (setq restore-column--saved-column col)))))))

(defun restore-column--after-save-hook ()
  "Restore the cursor to the previously saved column using an overlay."
  (let ((target-buf (restore-column--active-buffer)))
    (when target-buf
      (with-current-buffer target-buf
        (when (and restore-column-local-mode
                   restore-column--saved-column)
          (unwind-protect
              (when (/= (current-column) restore-column--saved-column)
                (move-to-column restore-column--saved-column nil)
                (let ((missing-spaces (- restore-column--saved-column
                                         (current-column))))
                  (when (> missing-spaces 0)
                    (let ((display-spaces (make-string (1+ missing-spaces) ?\s))
                          (ov (make-overlay (point) (point) (current-buffer) t nil)))
                      (put-text-property missing-spaces (1+ missing-spaces)
                                         'cursor t display-spaces)
                      (overlay-put ov 'window (selected-window))
                      (overlay-put ov 'after-string display-spaces)
                      (overlay-put ov 'modification-hooks
                                   (list #'restore-column--clear-virtual-state))
                      (overlay-put ov 'insert-in-front-hooks
                                   (list #'restore-column--clear-virtual-state))
                      (setq restore-column--virtual-overlay ov)
                      (add-hook 'post-command-hook
                                #'restore-column--post-command-handler nil t)))))
            (setq restore-column--saved-column nil)))))))

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

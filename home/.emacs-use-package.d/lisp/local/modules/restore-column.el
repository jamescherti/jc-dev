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

(defun restore-column--clear-virtual-state ()
  "Clean up virtual overlay and hooks without modifying the buffer."
  (when (overlayp restore-column--virtual-overlay)
    (delete-overlay restore-column--virtual-overlay)
    (setq restore-column--virtual-overlay nil))
  (remove-hook 'post-command-hook #'restore-column--post-command-handler t))

(defun restore-column--post-command-handler ()
  "Clear the virtual overlay if point moved or user cancelled/moved."
  (when (or (not (overlayp restore-column--virtual-overlay))
            (not (eq (overlay-buffer restore-column--virtual-overlay) (current-buffer)))
            (not (eq (point) (overlay-start restore-column--virtual-overlay)))
            (memq this-command '(keyboard-quit
                                 backward-char left-char
                                 forward-char right-char
                                 move-beginning-of-line
                                 move-end-of-line)))
    (restore-column--clear-virtual-state)))

(defun restore-column--shared-buffer-p ()
  "Return non-nil if the window's buffer shares text with `current-buffer'."
  (let* ((win-buf (window-buffer (selected-window)))
         (win-base (or (buffer-base-buffer win-buf) win-buf))
         (cur-base (or (buffer-base-buffer (current-buffer)) (current-buffer))))
    (eq cur-base win-base)))

(defun restore-column--before-save-hook ()
  "Save the current visual column position before saving."
  (when (restore-column--shared-buffer-p)
    ;; Operate directly in the selected window so `point' and `current-column'
    ;; reflect the indirect buffer currently shown in the window.
    (with-selected-window (selected-window)
      (when (bound-and-true-p restore-column-local-mode)
        (let ((col (current-column)))
          (when (and (overlayp restore-column--virtual-overlay)
                     (overlay-buffer restore-column--virtual-overlay))
            (let ((disp (overlay-get restore-column--virtual-overlay 'after-string)))
              (when (stringp disp)
                (setq col (+ col (1- (length disp))))))
            (restore-column--clear-virtual-state))
          (setq restore-column--saved-column col))))))

(defun restore-column--after-save-hook ()
  "Restore the cursor to the previously saved column using a virtual overlay."
  (when (restore-column--shared-buffer-p)
    (with-selected-window (selected-window)
      (when (and (bound-and-true-p restore-column-local-mode)
                 restore-column--saved-column)
        (unwind-protect
            (when (/= (current-column) restore-column--saved-column)
              ;; nil for FORCE guarantees real spaces are NEVER inserted
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
                    (setq restore-column--virtual-overlay ov)
                    (add-hook 'post-command-hook
                              #'restore-column--post-command-handler nil t)))))
          (setq restore-column--saved-column nil))))))

(defun restore-column--setup-hooks (enable)
  "Add or remove save hooks.
If ENABLE is non-nil, add the save hooks to the current buffer and its base
buffer (if indirect); otherwise, remove them."
  (let ((before-depth (if (>= emacs-major-version 27)
                          restore-column-before-save-hook-depth
                        nil))
        (after-depth (if (>= emacs-major-version 27)
                         restore-column-after-save-hook-depth
                       t))
        (base-buf (buffer-base-buffer)))
    (if enable
        (progn
          (add-hook 'before-save-hook #'restore-column--before-save-hook before-depth t)
          (add-hook 'after-save-hook #'restore-column--after-save-hook after-depth t)
          (when (buffer-live-p base-buf)
            (with-current-buffer base-buf
              (add-hook 'before-save-hook #'restore-column--before-save-hook before-depth t)
              (add-hook 'after-save-hook #'restore-column--after-save-hook after-depth t))))
      (remove-hook 'before-save-hook #'restore-column--before-save-hook t)
      (remove-hook 'after-save-hook #'restore-column--after-save-hook t))))

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
      (restore-column--setup-hooks t)
    (restore-column--clear-virtual-state)
    (setq restore-column--saved-column nil)
    (restore-column--setup-hooks nil)))

;;;###autoload
(define-globalized-minor-mode restore-column-global-mode
  restore-column-local-mode
  restore-column--enable
  :group 'restore-column)

(provide 'restore-column)
;;; restore-column.el ends here

;;; restore-column.el --- Restore cursor column after save hooks -*- lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/jc-dev
;; Package-Requires: ((emacs "24.3"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The restore-column package provides a mode that records the exact cursor
;; column position immediately before a buffer is saved and restores it
;; afterward.
;;
;; The problem: Save hooks (such as `delete-trailing-whitespace' or
;; `eglot-format-buffer') often truncate trailing spaces, which can force the
;; cursor back to the last non-whitespace character and disrupt the editing
;; workflow. This package ensures the cursor remains exactly where it was before
;; the save operation.
;;
;; This package doesn't modify your buffer. When restoring the cursor requires
;; placing it beyond the current physical end of a line, inserting real
;; whitespace would pollute the file with unwanted trailing spaces. To prevent
;; this, the restore-column dynamically applies a virtual overlay, which
;; visually renders the cursor at the correct column without modifying the
;; actual buffer text.
;;
;; The virtual overlay is ephemeral. Once the user moves the cursor, types, or
;; triggers another command, the overlay is automatically cleared via
;; `post-command-hook'.
;;
;; Features:
;; - Maintains the exact cursor column without modifying the underlying buffer
;;   text.
;; - Uses virtual overlay strings to render the cursor position.
;; - Overlays automatically clear on cursor movement or buffer modification.
;; - Operates correctly across base buffers and indirect buffers.
;;
;; Usage:
;; Enable locally via `restore-column-local-mode', or globally via
;; `restore-column-global-mode'.

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
  ;; Check if the variable holds a valid overlay descriptor.
  (when (overlayp restore-column--virtual-overlay)
    ;; Remove the overlay from the buffer display list to stop rendering it.
    (delete-overlay restore-column--virtual-overlay)
    (setq restore-column--virtual-overlay nil))
  (setq restore-column--saved-column nil)
  (remove-hook 'post-command-hook #'restore-column--post-command-handler t))

(defun restore-column--post-command-handler ()
  "Clear the virtual overlay if point moved or user cancelled/moved."
  (unless (and (overlayp restore-column--virtual-overlay)
               (eq (point) (overlay-start restore-column--virtual-overlay)))
    ;; Clean up overlay and unregister the hook.
    (restore-column--clear-virtual-state)))

(defun restore-column--before-save-hook ()
  "Save the current visual column position before saving."
  (when (eq (current-buffer) (window-buffer (selected-window)))
    ;; Operate directly in the selected window so `point' and `current-column'
    ;; reflect the indirect buffer currently shown in the window.
    ;; Check whether an active virtual overlay exists in the current buffer.
    ;; Extract the virtual text assigned to the overlay 'after-string
    ;; property.
    ;; Add the width of the virtual whitespace to the base column. (1-
    ;; (length disp)) discounts the extra character used for cursor
    ;; placement.
    ;; Delete the existing overlay before the buffer save operation runs.
    (let ((col (or restore-column--saved-column (current-column))))
      (restore-column--clear-virtual-state)
      (setq restore-column--saved-column col))))

(defun restore-column--after-save-hook ()
  "Restore the cursor to the previously saved column using a virtual overlay."
  (when (and (eq (current-buffer) (window-buffer (selected-window)))
             restore-column--saved-column)
    ;; nil for FORCE guarantees real spaces are NEVER inserted
    (move-to-column restore-column--saved-column)
    (let ((missing-spaces (- restore-column--saved-column (current-column))))
      (if (> missing-spaces 0)
          ;; Build a virtual string containing padding spaces plus one space
          ;; for cursor positioning.
          (let ((display-spaces (make-string (1+ missing-spaces) ?\s))
                ;; Instantiate a zero-width overlay at point in the current
                ;; buffer. front-advance is t (insertions at start move it
                ;; forward). rear-advance is nil (insertions at end do not
                ;; extend it).
                (ov (make-overlay (point) (point) nil t)))
            ;; Attach the 'cursor text property to the final character of
            ;; the virtual string. This tells the display engine to render
            ;; the cursor glyph at that visual column.
            (put-text-property missing-spaces (1+ missing-spaces)
                               'cursor t display-spaces)
            ;; Restrict the overlay display to the currently active window.
            (overlay-put ov 'window (selected-window))
            ;; Set the overlay 'after-string property to display the virtual
            ;; spaces visually without modifying the underlying buffer text.
            (overlay-put ov 'after-string display-spaces)
            ;; Store the overlay reference in the buffer-local variable for
            ;; tracking.
            (setq restore-column--virtual-overlay ov)
            ;; Register buffer-local post-command-hook to tear down the
            ;; overlay on user action.
            (add-hook 'post-command-hook
                      #'restore-column--post-command-handler nil t))
        (setq restore-column--saved-column nil)))))

(defun restore-column--enable ()
  "Turn on `restore-column-local-mode' for the current buffer."
  (unless (minibufferp)
    (restore-column-local-mode 1)))

;;;###autoload
(define-minor-mode restore-column-local-mode
  "Minor mode to restore the cursor column after saving."
  :lighter " RCol"
  (if restore-column-local-mode
      (progn
        (add-hook 'before-save-hook #'restore-column--before-save-hook
                  restore-column-before-save-hook-depth t)
        (add-hook 'after-save-hook #'restore-column--after-save-hook
                  restore-column-after-save-hook-depth t))
    ;; Delete overlay and detach post-command listener when the mode is
    ;; disabled.
    (restore-column--clear-virtual-state)
    (remove-hook 'before-save-hook #'restore-column--before-save-hook t)
    (remove-hook 'after-save-hook #'restore-column--after-save-hook t)))

;;;###autoload
(define-globalized-minor-mode restore-column-global-mode
  restore-column-local-mode
  restore-column--enable
  :group 'restore-column)

(provide 'restore-column)
;;; restore-column.el ends here

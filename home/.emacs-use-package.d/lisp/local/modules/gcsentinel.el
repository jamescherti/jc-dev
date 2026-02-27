;;; gcsentinel.el --- Gcsentinel -*- lexical-binding: t -*-

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
;;
;; Automatic garbage collection management.
;;
;; This package manages `gc-cons-threshold' to reduce interference during
;; active usage (like minibuffer operations) while ensuring memory is
;; reclaimed when Emacs is idle or loses focus.

;;; Code:

;;; Variables

(defgroup gcsentinel nil
  "Automatic garbage collection management in Emacs."
  :group 'gcsentinel
  :prefix "gcsentinel-")

(defcustom gcsentinel-low-cons-threshold gc-cons-threshold
  "Baseline value of `gc-cons-threshold' restored after GC tuning.

This variable records the normal threshold for garbage collection in Emacs Lisp.

`gcsentinel' temporarily raises `gc-cons-threshold' (for example, while the
minibuffer is active) to defer garbage collection during interactive operations.
Afterward, it resets `gc-cons-threshold' to the value stored here. Customizing
this variable allows you to control what threshold is used after startup or
after temporary adjustments are undone."
  :type 'integer
  :group 'gcsentinel)

(defcustom gcsentinel-high-cons-threshold most-positive-fixnum
  "Temporary value of `gc-cons-threshold' used to suppress garbage collection.

`gcsentinel' sets `gc-cons-threshold' to this value during periods where garbage
collection would be disruptive, such as while the minibuffer is active. By
raising the threshold, garbage collection is effectively disabled until the
operation completes, at which point the threshold is restored to
`gcsentinel-low-cons-threshold'.

Customize this variable to choose how aggressively garbage collection should be
deferred during such operations."
  :type 'integer
  :group 'gcsentinel)

(defcustom gcsentinel-handle-after-focus-change t
  "Non-nil to trigger garbage collection when Emacs loses focus."
  :type 'boolean
  :group 'gcsentinel)

(defcustom gcsentinel-handle-minibuffer t
  "Non-nil to trigger garbage collection after exiting the minibuffer.
Handles both normal exits and aborts (`C-g')."
  :type 'boolean
  :group 'gcsentinel)

(defcustom gcsentinel-handle-idle t
  "Non-nil to trigger garbage collection when Emacs is idle."
  :type 'boolean
  :group 'gcsentinel)

(defcustom gcsentinel-idle-seconds 30
  "Number of idle seconds before triggering garbage collection."
  :type 'integer
  :group 'gcsentinel)

(defcustom gcsentinel-debug nil
  "Enable displaying debug messages."
  :type 'boolean
  :group 'gcsentinel)

;; Compatibility with `gcmh'
(defvar gcsentinel--gcmh-high-cons-threshold nil)
(defvar gcsentinel--gcmh-low-cons-threshold nil)
(defvar gcmh-high-cons-threshold)
(defvar gcmh-low-cons-threshold)

;;; Internal variables

(defvar gcsentinel--idle-timer nil
  "Idle timer used for periodic garbage collection.")

;;; Internal functions

(defmacro gcsentinel-debug-message (&rest args)
  "Display a debug message with the same ARGS as `message'."
  (declare (indent 0) (debug t))
  `(when gcsentinel-debug
     (message (concat "[gcsentinel] " ,(car args)) ,@(cdr args))))

(defun gcsentinel--garbage-collect (&rest _)
  "Perform garbage collection if appropriate."
  ;; Prevent GC if we are inside a minibuffer context
  (when (or (not gcsentinel-handle-minibuffer)
            (not (minibufferp)))
    (gcsentinel-debug-message "Garbage collect")
    (when (fboundp 'gcmh-idle-garbage-collect)
      (gcmh-idle-garbage-collect))
    (garbage-collect)))

(defun gcsentinel--garbage-collect-when-idle (&rest _)
  "Garbage collect after Emacs has been idle."
  (gcsentinel-debug-message "Trigger: Idle")
  (gcsentinel--garbage-collect))

(defun gcsentinel--focus-change-func ()
  "Garbage collect when Emacs loses focus."
  (unless (frame-focus-state)
    (gcsentinel-debug-message "Trigger: Emacs lost focus")
    (gcsentinel--garbage-collect)

    ;; Update the mode-line
    (force-mode-line-update t)))

(defun gcsentinel--minibuffer-exit-hook ()
  "Trigger garbage collection after exiting the minibuffer normally."
  (unwind-protect
      (progn
        (gcsentinel-debug-message "Trigger: Minibuffer exit hook")
        (setq gc-cons-threshold gcsentinel-low-cons-threshold)

        ;; Restore gcmh values if we touched them
        (when gcsentinel--gcmh-low-cons-threshold
          (setq gcmh-low-cons-threshold gcsentinel--gcmh-low-cons-threshold)
          (setq gcsentinel--gcmh-low-cons-threshold nil))

        (when gcsentinel--gcmh-high-cons-threshold
          (setq gcmh-high-cons-threshold gcsentinel--gcmh-high-cons-threshold)
          (setq gcsentinel--gcmh-high-cons-threshold nil))

        (gcsentinel-debug-message
          "Restore gc-cons-threshold: %s" gc-cons-threshold)
        (gcsentinel--garbage-collect))
    ;; Ensure the abort trap is removed regardless of success/failure
    (remove-hook 'post-command-hook #'gcsentinel--minibuffer-abort-trap)))

(defun gcsentinel--minibuffer-abort-trap ()
  "Handle `C-g' aborts in the minibuffer.
Ensures garbage collection runs even when the minibuffer is aborted."
  ;; Check if we have effectively exited the minibuffer
  (unless (minibufferp)
    (gcsentinel-debug-message "Trigger: Abort trap")
    (gcsentinel--minibuffer-exit-hook)))

(defun gcsentinel--minibuffer-setup-hook ()
  "Disable garbage collection while the minibuffer is active."
  (gcsentinel-debug-message "Trigger: Setup minibuffer")
  (add-hook 'post-command-hook #'gcsentinel--minibuffer-abort-trap)
  (setq gc-cons-threshold gcsentinel-high-cons-threshold)

  ;; Temporarily override gcmh settings to prevent it from lowering
  ;; the threshold if it triggers during minibuffer usage.
  (when (bound-and-true-p gcmh-low-cons-threshold)
    (setq gcsentinel--gcmh-low-cons-threshold gcmh-low-cons-threshold)
    (setq gcmh-low-cons-threshold gcsentinel-high-cons-threshold))

  (when (bound-and-true-p gcmh-high-cons-threshold)
    (setq gcsentinel--gcmh-high-cons-threshold gcmh-high-cons-threshold)
    (setq gcmh-high-cons-threshold gcsentinel-high-cons-threshold))

  (gcsentinel-debug-message "Increase gc-cons-threshold: %s" gc-cons-threshold))

;;; Modes

;;;###autoload
(define-minor-mode gcsentinel-mode
  "Global minor mode for automatic garbage collection in Emacs.

When enabled, triggers garbage collection based on idle time, minibuffer
usage, and focus changes, depending on user customization."
  :global t
  :group 'gcsentinel
  (if gcsentinel-mode
      (progn
        ;; Idle timer setup
        (when (and gcsentinel-handle-idle (not gcsentinel--idle-timer))
          (setq gcsentinel--idle-timer
                (run-with-idle-timer gcsentinel-idle-seconds t
                                     #'gcsentinel--garbage-collect-when-idle)))

        ;; Minibuffer hooks
        (when gcsentinel-handle-minibuffer
          (add-hook 'minibuffer-setup-hook #'gcsentinel--minibuffer-setup-hook -90)
          (add-hook 'minibuffer-exit-hook #'gcsentinel--minibuffer-exit-hook 90))

        ;; Focus change handling
        (when gcsentinel-handle-after-focus-change
          (if (boundp 'after-focus-change-function)
              (add-function :after after-focus-change-function
                            #'gcsentinel--focus-change-func)
            (add-hook 'after-focus-change-function #'gcsentinel--focus-change-func))))
    ;; Disable
    (when gcsentinel--idle-timer
      (cancel-timer gcsentinel--idle-timer)
      (setq gcsentinel--idle-timer nil))

    (remove-hook 'minibuffer-exit-hook #'gcsentinel--minibuffer-exit-hook)
    (remove-hook 'post-command-hook #'gcsentinel--minibuffer-abort-trap)

    (if (boundp 'after-focus-change-function)
        (remove-function after-focus-change-function
                         #'gcsentinel--focus-change-func)
      (remove-hook 'after-focus-change-function #'gcsentinel--focus-change-func))))

;;; Provide

(provide 'gcsentinel)
;;; gcsentinel.el ends here

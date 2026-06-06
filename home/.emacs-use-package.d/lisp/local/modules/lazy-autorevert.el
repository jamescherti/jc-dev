;;; lazy-autorevert.el --- Description -*- lexical-binding: t -*-

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

;; A performant alternative to `global-auto-revert-mode'. Default auto-revert
;; mechanisms can cause performance degradation when managing many buffers or
;; when external tools modify files. This package updates visible windows only
;; on frame focus or file save.

;;; Code:

(require 'autorevert)

(defcustom lazy-autorevert-verbose t
  "If non-nil, print a message when a buffer is successfully reverted.
This variable dynamically overrides `auto-revert-verbose' during the
execution of the lazy auto-revert handler."
  :type 'boolean
  :group 'auto-revert)

(defcustom lazy-autorevert-debug t
  "If non-nil, print debug messages showing which buffer is being checked.
Enable this to trace the window and buffer change hooks."
  :type 'boolean
  :group 'auto-revert)

(defcustom lazy-autorevert-throttle-interval 0.3
  "Minimum time in seconds between auto-revert checks to prevent chaining."
  :type 'number
  :group 'auto-revert)

(defvar-local lazy-autorevert--last-check-time 0.0
  "Time when the current buffer was last checked.")

(defvar lazy-autorevert--last-global-check-time 0.0
  "Time when the visible buffers were last checked globally.")

(defun lazy-autorevert-buffer-handler (&rest _)
  "Auto revert current buffer, if necessary."
  (let ((current-time (float-time)))
    (when (> (- current-time lazy-autorevert--last-check-time)
             lazy-autorevert-throttle-interval)
      (setq lazy-autorevert--last-check-time current-time)
      (let* ((target-buffer (current-buffer))
             (base-buffer (or (buffer-base-buffer target-buffer) target-buffer))
             (file-name (buffer-file-name base-buffer)))
        (unless (or auto-revert-mode
                    (active-minibuffer-window)
                    (and file-name
                         auto-revert-remote-files
                         (file-remote-p file-name nil t))
                    ;; The `verify-visited-file-modtime' check acts as a fast
                    ;; path. It is an optimized C function that returns t if the
                    ;; file on disk matches the buffer's modification time,
                    ;; allowing us to bypass the handler entirely when no
                    ;; changes exist.
                    (and file-name
                         (verify-visited-file-modtime base-buffer)))
          (let ((auto-revert-mode t)
                (revert-without-query (list "."))
                (auto-revert-verbose lazy-autorevert-verbose)
                (auto-revert-use-notify nil)
                (auto-revert-stop-on-user-input nil))
            (when lazy-autorevert-debug
              (message "Auto revert %s" (buffer-name target-buffer)))
            (auto-revert-handler)))))))

(defun lazy-autorevert-visible-buffers-handler (&rest _)
  "Auto revert stale buffers in visible windows, if necessary."
  (let ((current-time (float-time)))
    (when (> (- current-time lazy-autorevert--last-global-check-time)
             lazy-autorevert-throttle-interval)
      (setq lazy-autorevert--last-global-check-time current-time)
      (walk-windows
       (lambda (win)
         (with-current-buffer (window-buffer win)
           (lazy-autorevert-buffer-handler)))
       'nomini
       'visible))))

;;;###autoload
(define-minor-mode lazy-autorevert-mode
  "A more performant alternative to `global-auto-revert-mode'.
Default auto-revert relies on heavy file watching or polling, which can tank
performance when external tools modify files or when managing hundreds of
buffers. This lazy alternative updates visible windows only on frame focus.
Hidden buffers stay untouched until you switch to them, bounding
operations to a few active viewports instead of the entire session."
  :global t
  :group 'auto-revert
  (when global-auto-revert-mode
    (global-auto-revert-mode -1))
  (let ((fn (if lazy-autorevert-mode #'add-hook #'remove-hook)))
    (funcall fn 'window-buffer-change-functions
             #'lazy-autorevert-buffer-handler)
    (funcall fn 'window-selection-change-functions
             #'lazy-autorevert-buffer-handler)
    (if (boundp 'after-focus-change-function)
        (if lazy-autorevert-mode
            (add-function :after after-focus-change-function
                          #'lazy-autorevert-visible-buffers-handler)
          (remove-function after-focus-change-function
                           #'lazy-autorevert-visible-buffers-handler))
      (funcall fn 'focus-in-hook #'lazy-autorevert-visible-buffers-handler))))

(provide 'lazy-autorevert)

;;; lazy-autorevert.el ends here

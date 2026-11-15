;;; my-xref-restore-window-start.el --- Viewport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; Version: 0.0.9
;; URL: https://github.com/jamescherti/jc-dev
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;; Preserve the window start when navigating backward and forward through
;; Xref history.  Works with global and window-local Xref history.

;;; Code:

(require 'xref)

;; Use marker identity: Xref detaches a history marker after popping it.
;; Weak keys also allow discarded history entries to be collected.
(defvar my-xref-window-starts
  (make-hash-table :test 'eq :weakness 'key)
  "Map Xref history markers to saved window-start markers.")

(defvar my-xref--pending-start nil
  "Dynamically bound source window-start marker during an Xref jump.")

(defun my-xref--capture-start ()
  "Return a marker at the selected window's start, or nil.
Only capture a start when the selected window shows the current buffer."
  (let ((window (selected-window)))
    (when (and (window-live-p window)
               (eq (window-buffer window) (current-buffer)))
      (copy-marker (window-start window) t))))

(defun my-xref--record-start (history-marker start-marker)
  "Associate HISTORY-MARKER with START-MARKER when buffers match."
  (when (and (markerp history-marker)
             (marker-buffer history-marker)
             (markerp start-marker)
             (eq (marker-buffer history-marker)
                 (marker-buffer start-marker)))
    (puthash history-marker start-marker my-xref-window-starts)))

;; Xref pushes its source marker after displaying the target.
;; Capture the source viewport before that buffer switch.
(defun my-xref--capture-origin-advice (orig-fun &rest args)
  "Call ORIG-FUN with ARGS, retaining the source viewport during a jump."
  (let ((my-xref--pending-start (my-xref--capture-start)))
    (apply orig-fun args)))

(defun my-xref--push-marker-advice (orig-fun &rest args)
  "Call ORIG-FUN with ARGS and save the actual pushed Xref marker."
  (let* ((history (xref--get-history))
         (start-marker
          (if (and (markerp my-xref--pending-start)
                   (eq (marker-buffer my-xref--pending-start)
                       (current-buffer)))
              (copy-marker my-xref--pending-start t)
            (my-xref--capture-start))))
    (prog1
        (apply orig-fun args)
      (my-xref--record-start (caar history) start-marker))))

(defun my-xref--restore-start (start-marker)
  "Restore START-MARKER in the selected window when it is accessible.
Allow redisplay to keep point visible if the window or buffer changed."
  (when (and (markerp start-marker)
             (marker-buffer start-marker))
    (let* ((window (selected-window))
           (buffer (marker-buffer start-marker))
           (position (marker-position start-marker)))
      (when (and (window-live-p window)
                 (eq (window-buffer window) buffer)
                 (with-current-buffer buffer
                   (and (<= (point-min) position)
                        (<= position (point-max)))))
        (set-window-start window position t)))))

(defun my-xref--navigate (orig-fun backward args)
  "Call ORIG-FUN with ARGS, preserving Xref history scroll positions.
If BACKWARD is non-nil, navigate backward; otherwise navigate forward."
  (let* ((history (xref--get-history))
         (target-marker (if backward (caar history) (cadr history)))
         (target-start (and (markerp target-marker)
                            (gethash target-marker my-xref-window-starts)))
         (source-start (my-xref--capture-start)))
    (prog1
        (apply orig-fun args)
      (my-xref--record-start
       (if backward (cadr history) (caar history))
       source-start)
      (when (markerp target-marker)
        (remhash target-marker my-xref-window-starts))
      (my-xref--restore-start target-start))))

(defun my-xref--go-back-advice (orig-fun &rest args)
  "Call ORIG-FUN with ARGS and restore backward Xref viewport history."
  (my-xref--navigate orig-fun t args))

(defun my-xref--go-forward-advice (orig-fun &rest args)
  "Call ORIG-FUN with ARGS and restore forward Xref viewport history."
  (my-xref--navigate orig-fun nil args))

;;;###autoload
(define-minor-mode my-xref-restore-window-start-mode
  "Restore saved window-start positions during Xref history navigation."
  :global t
  :init-value nil
  :group 'xref
  (if my-xref-restore-window-start-mode
      (progn
        (advice-add 'xref--show-defs :around
                    #'my-xref--capture-origin-advice)
        (advice-add 'xref--show-xrefs :around
                    #'my-xref--capture-origin-advice)
        (advice-add 'xref-push-marker-stack :around
                    #'my-xref--push-marker-advice)
        (advice-add 'xref-go-back :around
                    #'my-xref--go-back-advice)
        (advice-add 'xref-go-forward :around
                    #'my-xref--go-forward-advice))
    (advice-remove 'xref--show-defs #'my-xref--capture-origin-advice)
    (advice-remove 'xref--show-xrefs #'my-xref--capture-origin-advice)
    (advice-remove 'xref-push-marker-stack #'my-xref--push-marker-advice)
    (advice-remove 'xref-go-back #'my-xref--go-back-advice)
    (advice-remove 'xref-go-forward #'my-xref--go-forward-advice)
    (clrhash my-xref-window-starts)))

(provide 'my-xref-restore-window-start)

;;; my-xref-restore-window-start.el ends here

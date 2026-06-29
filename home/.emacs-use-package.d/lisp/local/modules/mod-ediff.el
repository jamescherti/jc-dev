;;; mod-ediff.el --- mod-ediff -*- lexical-binding: t -*-

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

;;; Require

(require 'cl-lib)

;;; ediff: next/previous diff `rencenter'

(defun my-ediff-recenter ()
  "Force recentering of both buffers in the current ediff session."
  (dolist (buf (mod-ediff--ediff-buffers-from-control-panel))
    (when (buffer-live-p buf)
      (let ((win (get-buffer-window buf t)))
        (when (window-live-p win)
          (with-selected-window win
            (recenter)))))))

(add-hook 'ediff-select-hook #'my-ediff-recenter)

;;; ediff: settings

(setq ediff-keep-variants t)
;; (setq ediff-make-buffers-readonly-at-startup nil) ; default nil
;; (setq ediff-confirm-copy t)

;; Ediff: Ignore all whitespace differences (-w) to reduce visual noise from
;; indentation changes or auto-formatters, keeping the focus on logic.
(setq ediff-diff-options "")

;; Ediff: Skip over regions where the only differences are whitespace (or other
;; ignored options) when navigating with 'n' and 'p'.
;; (setq ediff-ignore-similar-regions t)

;;; diff-options

(defun my-ediff-setup-elisp-options (orig-fn &rest args)
  "Apply whitespace-ignoring Ediff settings only for Emacs Lisp buffers.
ORIG-FN and ARGS is the functions and its arguments.
This wraps `ediff-setup' to dynamically bind options based on the major mode."
  (let* ((buffer-a (nth 1 args))
         (is-elisp (when (buffer-live-p buffer-a)
                     (with-current-buffer buffer-a
                       (derived-mode-p 'emacs-lisp-mode))))
         ;; Bind the variables to your preferred values if it is an Elisp buffer.
         (ediff-diff-options (if is-elisp
                                 "-w"
                               ediff-diff-options))
         (ediff-ignore-similar-regions (if is-elisp
                                           t
                                         ediff-ignore-similar-regions)))
    (ignore ediff-diff-options)
    (ignore ediff-ignore-similar-regions)
    ;; When `ediff-setup' executes, it will make these variables buffer-local
    ;; in its control buffer, capturing these let-bound values for the session.
    (apply orig-fn args)))

(advice-add 'ediff-setup :around #'my-ediff-setup-elisp-options)

;;; ediff startup: go to the next difference

(add-hook 'ediff-startup-hook 'ediff-next-difference)

;; ediff: Dynamic ediff-split-window-function
;;

(defun mod-ediff--frame-portrait-p ()
  "Return non-nil if the current frame is taller than it is wide in pixels.
This accurately detects when a monitor is rotated to a portrait orientation."
  (< (frame-pixel-width) (frame-pixel-height)))

(defun dynamic-ediff-split-window ()
  "Dynamically determine window split direction for Ediff.
Checks if the frame is in portrait mode to decide between a horizontal or
vertical split."
  (if (mod-ediff--frame-portrait-p)
      (split-window-vertically)
    (split-window-horizontally)))

(setq ediff-split-window-function #'dynamic-ediff-split-window)

;;; ediff: winner-undo

(defvar mod-ediff--inhibit-winner-undo nil)

;; Conditionally trigger `winner-undo` only if the layout remains unmutated
(defun mod-ediff-winner-undo ()
  "Ediff winner undo.
Restores the window configuration while ensuring point position is preserved
for the compared buffers."
  (when (and (not mod-ediff--inhibit-winner-undo)
             (bound-and-true-p winner-mode)
             (fboundp 'winner-undo))
    (let ((buf-points
           (delq nil
                 (mapcar (lambda (buf)
                           (when (and buf (buffer-live-p buf))
                             (cons buf (with-current-buffer buf (point)))))
                         (list (and (boundp 'ediff-buffer-A) ediff-buffer-A)
                               (and (boundp 'ediff-buffer-B) ediff-buffer-B)
                               (and (boundp 'ediff-buffer-C) ediff-buffer-C))))))
      (winner-undo)
      (dolist (bp buf-points)
        (let ((buf (car bp))
              (pt (cdr bp)))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (goto-char pt))
            (let ((win (get-buffer-window buf t)))
              (when (window-live-p win)
                (set-window-point win pt)))))))))

(add-hook 'ediff-quit-hook #'mod-ediff-winner-undo)

;;; ediff: kill control buffer

;; this function is not useful. In fact, it is an architectural anti-pattern for
;; Emacs state management and should be removed from your configuration.
;;
;; While the developer's intent was to prevent Ediff from leaking memory and
;; leaving "garbage" buffers behind, the implementation relies on brute-force
;; imperative logic rather than leveraging Ediff's built-in declarative
;; configurations.

;; (defun my/ediff-kill-control-buffer ()
;;   "Kill the Ediff control buffer and temporary buffers upon quitting.
;; Safely handles multiple concurrent Ediff sessions and numbered buffers."
;;   ;; Keep the fallback logic for the local control buffer
;;   (when (and (boundp 'ediff-control-buffer)
;;              (buffer-live-p ediff-control-buffer))
;;     (kill-buffer ediff-control-buffer))
;;
;;   ;; Check if there are any other active Ediff sessions
;;   (let ((active-ediffs (seq-filter
;;                         (lambda (buf)
;;                           (with-current-buffer buf
;;                             (eq major-mode 'ediff-control-mode)))
;;                         (buffer-list))))
;;     ;; If this is the last one (or zero left), kill the shared buffers
;;     (when (<= (length active-ediffs) 1)
;;       (dolist (buf (buffer-list))
;;         (let ((name (buffer-name buf)))
;;           ;; We are using `string-prefix-p' because Ediff sometimes creates
;;           ;; variants of these names such as *ediff-diffs<2>*.
;;           (when (or (string-prefix-p "*ediff-errors" name)
;;                     (string-prefix-p "*ediff-diff" name)
;;                     (string-prefix-p "*ediff-fine-diff" name)
;;                     (string-prefix-p "*Ediff Registry" name))
;;             (kill-buffer buf)))))))
;;
;; (add-hook 'ediff-quit-hook #'(lambda()
;;                                (when (fboundp 'my/ediff-kill-control-buffer)
;;                                  (my/ediff-kill-control-buffer))))

;;; ediff: Sync text scale when ediff starts
;; TODO patch?

(defun mod-ediff--ediff-sync-zoom-startup ()
  "Synchronize text scale in all Ediff buffers based on Buffer A."
  (when (and (boundp 'ediff-buffer-A)
             (buffer-live-p ediff-buffer-A))
    (let ((zoom-level (with-current-buffer ediff-buffer-A
                        (if (boundp 'text-scale-mode-amount)
                            text-scale-mode-amount
                          0))))
      (dolist (buf (list (and (boundp 'ediff-buffer-B) ediff-buffer-B)
                         (and (boundp 'ediff-buffer-C) ediff-buffer-C)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (text-scale-set zoom-level)))))))

(add-hook 'ediff-startup-hook #'mod-ediff--ediff-sync-zoom-startup)

(defun mod-ediff--ediff-buffers-from-control-panel ()
  "Return list of live ediff buffers A, B, and C, if bound."
  (delq nil
        (list (and (bound-and-true-p ediff-buffer-A) ediff-buffer-A)
              (and (bound-and-true-p ediff-buffer-B) ediff-buffer-B)
              (and (bound-and-true-p ediff-buffer-C) ediff-buffer-C))))

(defun mod-ediff--ediff-buffers ()
  "Return list of live ediff buffers A, B, and C for the current session.
Searches `ediff-session-registry' to locate the control buffer associated
with the current buffer, bypassing frame or window visibility limitations."
  (let ((current-buf (current-buffer))
        matched-buffers)
    (message "[EDIFF] Registry: %s" ediff-session-registry)
    (when (boundp 'ediff-session-registry)
      (catch 'found
        (dolist (session ediff-session-registry)
          (let ((ctrl-buf session))
            (when (buffer-live-p ctrl-buf)
              (with-current-buffer ctrl-buf
                (let ((buf-a (and (bound-and-true-p ediff-buffer-A) ediff-buffer-A))
                      (buf-b (and (bound-and-true-p ediff-buffer-B) ediff-buffer-B))
                      (buf-c (and (bound-and-true-p ediff-buffer-C) ediff-buffer-C)))
                  ;; Check if the current buffer is participating in this Ediff
                  ;; session (either as the control buffer itself, or as A, B,
                  ;; or C).
                  (when (memq current-buf (list ctrl-buf buf-a buf-b buf-c))
                    (setq matched-buffers (delq nil (list buf-a buf-b buf-c)))
                    (throw 'found t)))))))))
    matched-buffers))

(defun mod-ediff--setup-ediff-auto-text-scale ()
  "Add a buffer-local hook to keep text scale synchronized during Ediff sessions.
This installs `mod-ediff--ediff-auto-text-scale` on `text-scale-mode-hook` in
each Ediff buffer when the session starts, and cleans up automatically when the
session ends."
  (with-no-warnings
    (add-hook 'text-scale-mode-hook #'mod-ediff--ediff-auto-text-scale 99 t)))

(defun mod-ediff--ediff-auto-text-scale (&rest _)
  "Synchronize text scale across all Ediff buffers based on the current buffer."
  (when (and (boundp 'text-scale-mode-hook)
             (bound-and-true-p text-scale-mode-amount))
    (let ((original-buf (current-buffer))
          (original-buf-text-scale-amount text-scale-mode-amount)
          (ediff-bufs (mod-ediff--ediff-buffers)))
      (if ediff-bufs
          ;; Session is active: synchronize scale across peers
          (dolist (buf ediff-bufs)
            (when (and (buffer-live-p buf)
                       (not (eq buf original-buf)))
              (with-current-buffer buf
                ;; Temporarily shadow the hook to prevent infinite recursion
                (let ((text-scale-mode-hook
                       (remove 'mod-ediff--ediff-auto-text-scale
                               text-scale-mode-hook)))
                  (ignore text-scale-mode-hook)
                  (text-scale-set original-buf-text-scale-amount)))))
        ;; Session is dead or buffer detached: teardown hook
        (with-no-warnings
          (remove-hook 'text-scale-mode-hook #'mod-ediff--ediff-auto-text-scale t))))))

(defun mod-ediff--ediff-teardown-auto-text-scale ()
  "Remove text scale synchronization hooks when an Ediff session ends.
This function executes within the Ediff Control Buffer."
  (message "[EDIFF] BEGIN: Teardown auto text scale")
  (let ((bufs (mod-ediff--ediff-buffers-from-control-panel)))
    (dolist (buf bufs)
      (message "[EDIFF] Teardown auto text scale: %s" buf)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (with-no-warnings
            ;; TODO make something to restore initial text scale
            (remove-hook 'text-scale-mode-hook #'mod-ediff--ediff-auto-text-scale t)))))))

(add-hook 'ediff-prepare-buffer-hook #'mod-ediff--setup-ediff-auto-text-scale)
(add-hook 'ediff-cleanup-hook #'mod-ediff--ediff-teardown-auto-text-scale)

;;; ediff: Synchronize truncate-lines

(defun mod-ediff--ediff-sync-truncate-startup ()
  "Synchronize `truncate-lines' in all Ediff buffers based on Buffer A."
  (when (and (boundp 'ediff-buffer-A)
             (buffer-live-p ediff-buffer-A))
    (let ((truncate-state (with-current-buffer ediff-buffer-A truncate-lines)))
      (dolist (buf (mod-ediff--ediff-buffers-from-control-panel))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq truncate-lines truncate-state)))))))

(add-hook 'ediff-startup-hook #'mod-ediff--ediff-sync-truncate-startup)

(defun mod-ediff--sync-truncate-advice (&rest _)
  "Advice to synchronize line truncation across active Ediff buffers."
  (let ((ediff-bufs (mod-ediff--ediff-buffers)))
    ;; Only execute the synchronization if we are inside an active Ediff session
    (when ediff-bufs
      (let ((current-state truncate-lines)
            (original-buf (current-buffer)))
        (dolist (buf ediff-bufs)
          (when (and (buffer-live-p buf)
                     (not (eq buf original-buf)))

            ;; 1. Update the buffer-local variable
            (with-current-buffer buf
              (setq truncate-lines current-state))

            ;; 2. Find the window displaying the buffer and redraw it
            (let ((win (get-buffer-window buf t)))
              (when (window-live-p win)
                (with-selected-window win
                  (recenter))))))))))

(defun mod-ediff--setup-ediff-auto-truncate ()
  "Apply the synchronization advice to `toggle-truncate-lines'."
  (advice-add 'toggle-truncate-lines :after #'mod-ediff--sync-truncate-advice))

(defun mod-ediff--ediff-teardown-auto-truncate ()
  "Remove the synchronization advice when the session terminates."
  (advice-remove 'toggle-truncate-lines #'mod-ediff--sync-truncate-advice))

(add-hook 'ediff-prepare-buffer-hook #'mod-ediff--setup-ediff-auto-truncate)
(add-hook 'ediff-cleanup-hook #'mod-ediff--ediff-teardown-auto-truncate)

;;; ediff: disable/enable minor modes

(defvar mod-ediff-ediff-disabled-minor-modes '(aggressive-indent-mode)
  "List of minor modes to disable automatically during an Ediff session.")

(defvar-local mod-ediff--ediff-restorable-minor-modes nil
  "Buffer-local list storing the minor modes that were disabled.")

(defun mod-ediff--ediff-disable-minor-modes ()
  "Disable specific minor modes in Ediff buffers and record them for restoration.
This function is intended to be added to `ediff-prepare-buffer-hook'."
  (message "[EDIFF] BEGIN: Disable minor modes")
  (setq mod-ediff--ediff-restorable-minor-modes nil)
  (dolist (mode mod-ediff-ediff-disabled-minor-modes)
    (when (and (fboundp mode)
               (boundp mode)
               (symbol-value mode))
      (push mode mod-ediff--ediff-restorable-minor-modes)
      (message "[EDIFF] Disable: %s" mode)
      (funcall mode -1))))

(defun mod-ediff--ediff-restore-minor-modes ()
  "Restore minor modes that were disabled during the Ediff session.
This function executes within the Ediff Control Buffer."
  (message "[EDIFF] BEGIN: Restore minor modes")
  (let ((bufs (mod-ediff--ediff-buffers-from-control-panel)))
    (dolist (buf bufs)
      (message "[EDIFF] Check modes that should be restored: %s" buf)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (dolist (mode mod-ediff--ediff-restorable-minor-modes)
            (when (fboundp mode)
              (message "[EDIFF] Enable: %s" mode)
              (funcall mode 1)))
          (setq mod-ediff--ediff-restorable-minor-modes nil))))))

(add-hook 'ediff-prepare-buffer-hook #'mod-ediff--ediff-disable-minor-modes)
(add-hook 'ediff-cleanup-hook #'mod-ediff--ediff-restore-minor-modes)

;;; ediff: quit

(defun mod-ediff-ediff-quit-all ()
  "Gracefully terminate all active Ediff sessions unconditionally.
Bypasses the $O(N)$ buffer list scan by utilizing the `ediff-session-registry'
and suppresses all interactive confirmation prompts during teardown."
  (interactive)
  (let ((win-config (current-window-configuration)))
    (save-window-excursion
      (when (boundp 'ediff-session-registry)
        ;; Snapshot the registry. `ediff-quit' destructively mutates
        ;; `ediff-session-registry'. We must iterate over a copy.
        (let ((sessions (copy-sequence ediff-session-registry))
              (mod-ediff--inhibit-winner-undo t)
              (inhibit-redisplay t))
          ;; Mock interactive prompts. We intercept and automatically
          ;; return `t` to allow batch processing without thread blocking.
          (cl-letf (((symbol-function 'y-or-n-p) #'always)
                    ((symbol-function 'yes-or-no-p) #'always))
            (dolist (session sessions)
              (let ((ctrl-buf session))
                (when (buffer-live-p ctrl-buf)
                  (with-current-buffer ctrl-buf
                    (when (fboundp 'ediff-quit)
                      ;; Execute teardown from within the control buffer
                      ;; context. Passing t handles the
                      ;; `reverse-default-keep-variants' argument, suppressing
                      ;; further variant-saving prompts.
                      (ediff-quit t))))))

            (when (window-configuration-p win-config)
              (set-window-configuration win-config))))))))

;;; Legacy

;; (defun mod-ediff-get-all-control-buffers ()
;;   "Return a list of all live buffers currently in `ediff-mode'."
;;   (seq-filter (lambda (buf)
;;                 (with-current-buffer buf
;;                   (eq major-mode 'ediff-mode)))
;;               (buffer-list)))

;; (defun mod-ediff-get-control-buffers-from-registry ()
;;   "Return all control buffers registered in the Ediff session registry."
;;   (when (boundp 'ediff-session-registry)
;;     (delq nil
;;           (mapcar (lambda (session)
;;                     (let ((buf (car session)))
;;                       (if (buffer-live-p buf) buf nil)))
;;                   ediff-session-registry))))

;; (defun mod-ediff-get-current-control-buffer ()
;;   "Get the control buffer for the current Ediff session.
;; This works when called from within Ediff buffer A, B, or C."
;;   (when (boundp 'ediff-control-buffer)
;;     ediff-control-buffer))

;;; ediff: Synchronize text scale when the user changes it

;; TODO patch?
;; (defun mod-ediff--ediff-control-panel-window ()
;;   "Return the window displaying the *Ediff Control Panel* buffer, if any."
;;   (seq-find (lambda (win)
;;               ;; The Ediff control panel buffer name may have suffixes like
;;               ;; "<2>", e.g., "*Ediff Control Panel<2>*", if multiple sessions
;;               ;; are active.
;;               (string-prefix-p "*Ediff Control Panel"
;;                                (buffer-name (window-buffer win))))
;;             (window-list)))

;; (defun mod-ediff--ediff-auto-text-scale (&rest _)
;;   "Synchronize text scale across all Ediff buffers based on Ediff buffer A."
;;   (when (and (boundp 'text-scale-mode-hook)
;;              (bound-and-true-p text-scale-mode-amount))
;;     (let ((original-buf (current-buffer))
;;           (original-buf-text-scale-amount text-scale-mode-amount)
;;           (window (mod-ediff--ediff-control-panel-window)))
;;       (if (window-live-p window)
;;           ;; Sync
;;           (with-selected-window window
;;             (dolist (buf (mod-ediff--ediff-buffers))
;;               (when (and (buffer-live-p buf)
;;                          (not (eq buf original-buf)))
;;                 (with-current-buffer buf
;;                   ;; Prevent infinite loops
;;                   (let ((text-scale-mode-hook
;;                          (remove 'mod-ediff--ediff-auto-text-scale
;;                                  text-scale-mode-hook)))
;;                     (ignore text-scale-mode-hook)
;;                     (text-scale-set original-buf-text-scale-amount))))))
;;         ;; Remove
;;         (with-no-warnings
;;           (remove-hook 'text-scale-mode-hook #'mod-ediff--ediff-auto-text-scale t))))))

;;; Provide

(provide 'mod-ediff)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-ediff.el ends here

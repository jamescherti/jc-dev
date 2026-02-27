;;; buffer-guardian.el --- Auto save -*- lexical-binding: t -*-

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

;; Buffer guardian.

;;; Code:

(defcustom buffer-guardian-verbose nil
  "Enable verbose mode to log when a buffer is automatically saved."
  :type 'boolean
  :group 'buffer-guardian)

(defcustom buffer-guardian-save-on-focus-change t
  "Save the current buffer when Emacs loses focus."
  :type 'boolean
  :group 'buffer-guardian)

(defcustom buffer-guardian-save-on-minibuffer t
  "Save the current buffer when the minibuffer is opened."
  :type 'boolean
  :group 'buffer-guardian)

(defcustom buffer-guardian-unattended-built-in-save-some-buffers nil
  "Ensure `save-some-buffers' never prompts and always saves buffers."
  :type 'boolean
  :group 'buffer-guardian)

(defcustom buffer-guardian-save-all-buffers-interval nil
  "Interval in seconds for automatically calling `save-some-buffers'.
This allows you to periodically save all file visiting buffers at once,
repeating the operation at the specified interval.

If set to nil, this feature is disabled."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'buffer-guardian)

(defcustom buffer-guardian-save-all-buffers-idle nil
  "Seconds for automatically saving all buffers when the user is idle.
This allows you save all file visiting buffers at once, repeating the operation
at the specified interval.

If set to nil, this feature is disabled."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'buffer-guardian)

(defcustom buffer-guardian-inhibit-saving-remote-files t
  "If non-nil, `buffer-guardian` will not auto-save remote files.
When set to nil, remote files will be included in the auto-save process.
This setting is used by `buffer-guardian-predicate`."
  :type 'boolean
  :group 'buffer-guardian)

(defcustom buffer-guardian-inhibit-saving-nonexistent-files t
  "If non-nil, `buffer-guardian` will not save files that do not exist on disk.
When set to nil, buffers visiting nonexistent files can still be saved.
This setting is used by `buffer-guardian-predicate`."
  :type 'boolean
  :group 'buffer-guardian)

(defcustom buffer-guardian-hooks-auto-save-all-buffers
  '(mouse-leave-buffer-hook)
  "List of hook symbols that trigger saving of all modified buffers.

When any of these hooks run, all buffers are saved. For example, to ensure that
work is not lost when Emacs loses focus or the mouse leaves the current buffer."
  :group 'buffer-guardian
  :type '(repeat symbol))

(defvar buffer-guardian-functions-auto-save-current-buffer nil
  "List of function symbols to be advised by `buffer-guardian'.

A :before advice will be added to each function in this list so that save the
current buffer before the function executes.

This mechanism allows automatic buffer saving to be triggered by specific
commands or operations (e.g., window switching or navigation).

Set this variable to nil to disable advising altogether.")

(defvar buffer-guardian--save-all-buffers-timer nil
  "Timer object for saving all buffers.")

(defvar buffer-guardian--save-all-buffers-idle-timer nil
  "Timer object for saving all buffers when the user is idle.")

(defvar buffer-guardian--list-advised-functions nil)
(defvar buffer-guardian--bkp-save-some-buffers-default-predicate nil)

(defun buffer-guardian-predicate (&optional include-non-file-visiting)
  "Default buffer-guardian predicate.
When INCLUDE-NON-FILE-VISITING is non-nil, also include other
buffers (e.g., org src)."
  (let* ((file-name (buffer-file-name)))
    (when (buffer-modified-p)
      (cond
       ((and include-non-file-visiting
             (fboundp 'org-src-edit-buffer-p)
             (funcall 'org-src-edit-buffer-p))
        'org-src)

       ((and include-non-file-visiting
             (bound-and-true-p edit-indirect--overlay))
        'edit-indirect)

       ;; File-visiting buffer
       (file-name
        (and
         ;; Remote or local that is writable
         (if (file-remote-p file-name)
             (not buffer-guardian-inhibit-saving-remote-files)
           (file-writable-p file-name))
         ;; File exists
         (if buffer-guardian-inhibit-saving-nonexistent-files
             (file-exists-p file-name)
           t)))))))

(defun buffer-guardian-save-buffer-maybe (&optional buffer)
  "Save BUFFER if it is visiting a file that is existing on the disk.
By default, it only saves when the file exists on the disk."
  (unless buffer
    (setq buffer (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((predicate-result (buffer-guardian-predicate
                               :include-all-buffers)))
        (when predicate-result
          (cond
           ((and (eq predicate-result 'org-src)
                 (fboundp 'org-edit-src-save))
            (funcall 'org-edit-src-save))

           ((and (eq predicate-result 'edit-indirect)
                 (fboundp 'edit-indirect--commit))
            (funcall 'edit-indirect--commit))

           (predicate-result
            (let ((save-silently (not buffer-guardian-verbose))
                  (inhibit-message (not buffer-guardian-verbose)))
              (if (verify-visited-file-modtime (current-buffer))
                  (save-buffer)
                (message
                 (concat "[buffer-guardian] Warning: Automatic save skipped "
                         "for '%s' because the file was modified externally.")
                 (buffer-file-name (buffer-base-buffer)))))))

          (when buffer-guardian-verbose
            (message
             "[buffer-guardian] '%s'" (buffer-file-name (buffer-base-buffer)))))))))

(defun buffer-guardian-save-all-buffers (&optional buffer-list)
  "Save some modified buffers that are visiting files that exist on the disk.
BUFFER-LIST is the list of buffers."
  (dolist (buffer (or buffer-list (buffer-list)))
    (buffer-guardian-save-buffer-maybe buffer)))

;; TODO: add optional option to auto revert the buffer or never revert
(defun buffer-guardian-save-buffer ()
  "Save the current buffer.

If the buffer is visiting a file and has a base buffer, save that base buffer.

Before saving, check if the visited file has been modified outside of Emacs. If
so, prompt the user for confirmation and revert the buffer if confirmed. Then
save the buffer without prompting or displaying messages."
  (interactive)
  (let ((buffer (or (buffer-base-buffer) (current-buffer)))
        (file-name (buffer-file-name (buffer-base-buffer)))
        (buffer-guardian-inhibit-saving-nonexistent-files nil))
    (when buffer
      (cond
       (file-name
        (with-current-buffer buffer
          ;; Was the file modified outside of Emacs? Revert buffer
          (unless (verify-visited-file-modtime (current-buffer))
            (when (yes-or-no-p (format "Discard edits and reread from '%s'?"
                                       file-name))
              (revert-buffer :ignore-auto :noconfirm)))

          ;; Save buffer
          (buffer-guardian-save-buffer-maybe)))

       (t
        (buffer-guardian-save-buffer-maybe))))))

(defun buffer-guardian--before-advice-save-current-buffer (&rest _)
  "Save current buffers."
  (buffer-guardian-save-buffer-maybe (current-buffer)))

(defun buffer-guardian--advice-around-save-some-buffers (original
                                                         &optional
                                                         _arg pred)
  "Make `save-some-buffers' never ask questions and always use the predicate.
ORIGINAL is the function. ARG and PRED are the `save-some-buffers' arguments."
  (let ((save-silently (not buffer-guardian-verbose)))
    (funcall original
             t  ;  Non-nil means save all with no questions.
             (if (eq pred t)
                 ;; If PRED is t, the `buffer-guardian-predicate' is ignored.
                 ;; Functions like `save-buffers-kill-emacs' are problematic in
                 ;; this case as they bypass the predicate by passing t to PRED.
                 ;; We rely on the safety of the buffer-guardian predicate,
                 ;; which is why we change PRED back to nil to ensure that the
                 ;; `buffer-guardian-predicate' is always used.
                 nil
               pred))))

(defun buffer-guardian--on-focus-change ()
  "Run `buffer-guardian-save-all-buffers' when Emacs loses focus."
  (when (and buffer-guardian-save-on-focus-change
             (not (frame-focus-state)))
    (buffer-guardian-save-all-buffers)))

(defun buffer-guardian--minibuffer-setup-hook ()
  "Save the buffer whenever the minibuffer is open."
  (when buffer-guardian-save-on-minibuffer
    (let ((buffer (window-buffer (minibuffer-selected-window))))
      (when (buffer-live-p buffer)
        (buffer-guardian-save-buffer-maybe buffer)))))

;;;###autoload
(define-minor-mode buffer-guardian-mode
  "Toggle `buffer-guardian-mode'."
  :global t
  :lighter " SaveAngel"
  :group 'buffer-guardian
  (if buffer-guardian-mode
      (progn
        ;; Hook triggers
        ;; -------------
        (when buffer-guardian-hooks-auto-save-all-buffers
          (dolist (hook buffer-guardian-hooks-auto-save-all-buffers)
            (add-hook hook #'buffer-guardian-save-all-buffers)))

        (when buffer-guardian-unattended-built-in-save-some-buffers
          (setq buffer-guardian--bkp-save-some-buffers-default-predicate
                save-some-buffers-default-predicate)
          ;; Default predicate for `save-some-buffers'. This prevents
          ;; `save-some-buffers' from prompting about certain files that you'd
          ;; typically prefer not to save.
          (setq save-some-buffers-default-predicate
                #'buffer-guardian-predicate)

          ;; Advice
          ;; ------
          ;; Ensure `save-some-buffers' never prompts and always saves
          ;; file-visiting buffers. When
          ;; `buffer-guardian-inhibit-saving-nonexistent-files' is:
          ;; - nil: all file-visiting buffers are saved.
          ;; - non-nil: only file-visiting buffers that exist on disk are saved.
          (advice-add 'save-some-buffers :around
                      #'buffer-guardian--advice-around-save-some-buffers))

        ;; Minibuffer setup
        ;; ----------------
        (add-hook 'minibuffer-setup-hook
                  #'buffer-guardian--minibuffer-setup-hook)

        ;; Focus Change
        ;; ------------
        (add-function :after after-focus-change-function
                      #'buffer-guardian--on-focus-change)

        ;; Save some buffers (Disabled by default)
        ;; ---------------------------------------
        (when buffer-guardian-save-all-buffers-idle
          (setq buffer-guardian--save-all-buffers-idle-timer
                (run-with-idle-timer buffer-guardian-save-all-buffers-idle
                                     buffer-guardian-save-all-buffers-idle
                                     #'buffer-guardian-save-all-buffers)))

        (when buffer-guardian-save-all-buffers-interval
          (setq buffer-guardian--save-all-buffers-timer
                (run-with-timer buffer-guardian-save-all-buffers-interval
                                buffer-guardian-save-all-buffers-interval
                                #'buffer-guardian-save-all-buffers)))

        ;; Advise functions
        ;; ----------------
        ;; TODO: store a copy of the function in an internal variable
        (when buffer-guardian-functions-auto-save-current-buffer
          (setq buffer-guardian--list-advised-functions
                (copy-sequence buffer-guardian-functions-auto-save-current-buffer))
          (dolist (func buffer-guardian-functions-auto-save-current-buffer)
            (when (fboundp func)
              (advice-add
               func :before
               #'buffer-guardian--before-advice-save-current-buffer)))))
    ;; Hook triggers
    ;; -------------
    (when buffer-guardian-hooks-auto-save-all-buffers
      (dolist (hook buffer-guardian-hooks-auto-save-all-buffers)
        (remove-hook hook #'buffer-guardian-save-all-buffers)))

    ;; Advice
    ;; ------
    (setq save-some-buffers-default-predicate
          buffer-guardian--bkp-save-some-buffers-default-predicate)
    (advice-remove 'save-some-buffers
                   #'buffer-guardian--advice-around-save-some-buffers)

    ;; Minibuffer setup
    ;; ----------------
    (remove-hook 'minibuffer-setup-hook #'buffer-guardian--minibuffer-setup-hook)

    ;; Disable: Focus Change
    ;; ---------------------
    (remove-function after-focus-change-function
                     #'buffer-guardian--on-focus-change)

    ;; Disable: Save some buffers (timer)
    ;; ----------------------------------
    (when buffer-guardian--save-all-buffers-timer
      (cancel-timer buffer-guardian--save-all-buffers-timer)
      (setq buffer-guardian--save-all-buffers-timer nil))

    ;; Disable: Save some buffers (idle timer)
    ;; ---------------------------------------
    (when buffer-guardian--save-all-buffers-idle-timer
      (cancel-timer buffer-guardian--save-all-buffers-idle-timer)
      (setq buffer-guardian--save-all-buffers-idle-timer nil))

    ;; Advise functions
    ;; ----------------
    (when buffer-guardian--list-advised-functions
      (dolist (func buffer-guardian--list-advised-functions)
        (when (fboundp func)
          (advice-remove
           func #'buffer-guardian--before-advice-save-current-buffer))))))

(provide 'buffer-guardian)
;;; buffer-guardian.el ends here

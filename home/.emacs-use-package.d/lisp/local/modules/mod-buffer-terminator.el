;;; mod-buffer-terminator.el --- mod-buffer-terminator -*- lexical-binding: t -*-

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

;; Configure buffer terminator.

;;; Code:

(require 'buffer-guardian)
(require 'lightemacs-use-package)

(defvar mod-buffer-terminator-keep-buffer-regexp nil)
(setq mod-buffer-terminator-keep-buffer-regexp '("\\`\\(?: \\)?\\*eldoc for .*\\*\\'"
                                                 "\\`\\(?: \\)?\\*EGLOT .*\\*\\'"
                                                 "\\`\\(?: \\)?\\*straight.*\\*\\'"
                                                 "\\`\\(?: \\)?\\*elpaca.*\\*\\'"
                                                 "\\`\\(?: \\)?\\*apheleia-.*\\*\\'"
                                                 ;; "\\` \\*Minibuf-[0-9]+\\*\\'"
                                                 ;; "\\` \\*Compiler Input\\*-"
                                                 ;; "\\` \\*stderr of "
                                                 ;; "\\`\\*gptel"
                                                 ;; "\\` \\*flymake-.*\\*\\'"
                                                 ;; "\\` \\*Preview:.*\\*\\'"
                                                 ;; "\\` \\*Old buffer "  ; tab-bar
                                                 ;; "\\` \\*markdown-code-fontification:.*\\*\\'"
                                                 ;; "\\` \\*wgrep "
                                                 ;; "\\` \\*Org "
                                                 ;; "\\` \\*org-src-"
                                                 ;; "\\` \\*Echo Area [0-9]+\\*\\'"
                                                 ))

(defvar mod-buffer-terminator-always-keep nil)
(setq mod-buffer-terminator-always-keep '("*Messages*"
                                          "*tmux*"
                                          "*scratch*"
                                          "*Flymake log*"
                                          "*Compile-Log*"
                                          "*Warnings*"
                                          "*Ollama*"
                                          "*Non-Native-Compiled*"
                                          "*compile-angel:debug*"
                                          "*vc*"
                                          "*Help*"
                                          "*Ediff Registry*"

                                          "*Flymake log*"
                                          "*Async-native-compile-log*"
                                          "*Native-compile-Log*"
                                          "*Backtrace*"

                                          "*lsp-log*"
                                          "*pylsp*"
                                          "*pylsp::stderr*"

                                          ;; " *server*"
                                          ;; " *counsel*"
                                          ;; " *eldoc*"
                                          ;; " *code-converting-work*"
                                          ;; " *code-conversion-work*"
                                          ;; " *Compiler Input*"
                                          ;; " *jka-compr-wr-temp*"
                                          ;; " *elisp-flymake-byte-compile*"
                                          ;; " *consult-async*"
                                          ;; " *consult-async-stderr*"
                                          "*Org string width*"
                                          "*Async-native-compile-log*"))

(defvar-local mod-buffer-terminator--keep nil)
;; (defvar-local my-buffer-terminator--track-buffers--list-buffers nil)

(defun mod-buffer-terminator-predicate ()
  "Return :kill, :keep, or nil."
  (let* ((file-name (buffer-file-name (buffer-base-buffer)))
         ;; (buffer (current-buffer))
         )
    (cond
     (mod-buffer-terminator--keep
      :keep)

     ;; ((and (eq my-track-buffers--buffer-type 'overlay)
     ;;       (buffer-live-p my-track-buffers--other-buffer))
     ;;  ;; The buffer should never be killed if the other buffer is still alive
     ;;  :keep)

     ;; ((and (or (eq my-track-buffers--buffer-type 'overlay)
     ;;           (eq my-track-buffers--buffer-type 'original))
     ;;       (or (not (buffer-live-p buffer))
     ;;           (buffer-terminator--buffer-visible-p)
     ;;           (not (buffer-live-p my-track-buffers--other-buffer))
     ;;           (with-current-buffer my-track-buffers--other-buffer
     ;;             (buffer-terminator--buffer-visible-p))))
     ;;  ;; One of the two buffers is one of them is still visible.
     ;;  :keep)

     ;; ((and (= (length comp-files-queue) 0)
     ;;       (or (string= (buffer-name) "*Async-native-compile-log*")
     ;;           (string= (buffer-name) "*Compile-Log*")))
     ;;  :kill)

     ;; Previous buffer to the buffer in the current window
     ;; NOTE: minibuffer issue (It says I cannot switch to another buffer)
     ;; ((and (not (minibufferp buffer))
     ;;       (eq buffer (save-window-excursion
     ;;                    (previous-buffer)
     ;;                    buffer)))
     ;;  :keep)

     (file-name
      (cond
       ((and file-name (string-suffix-p "/todo.org" file-name))
        :keep)))

     (t
      (let ((buffer-name (buffer-name)))
        (cond
         ((and (string-prefix-p " " buffer-name))
          :keep)

         ;; Let buffer terminator decide
         ;; (t
         ;;  nil)
         ))))))

(setq buffer-terminator-rules-alist
      `(
        (call-function . mod-buffer-terminator-predicate)

        ;; Keep active buffers.
        ;; (This can be customized with `buffer-terminator-inactivity-timeout'
        ;; and `buffer-terminator-interval'.)
        (keep-buffer-property . active)

        ;; Keep process buffers.
        ;; (Process buffers are buffers where an active process is running.)
        (keep-buffer-property . process)

        ;; (kill-buffer-name . ("*Warnings*"
        ;;                      "*Compile-Log*"
        ;;                      "*Backtrace*"
        ;;                      "*Help*"))

        ;; Retain special buffers (DO NOT REMOVE).
        ;; DO NOT REMOVE (keep-buffer-property . special) unless you know of what
        ;; you are doing.
        ;; (keep-buffer-property . special)

        (keep-buffer-name . ,mod-buffer-terminator-always-keep)

        (keep-buffer-name-regexp . ,mod-buffer-terminator-keep-buffer-regexp)

        (keep-buffer-property . visible)

        ;; Kill
        (return . :kill)))

(defun mod-buffer-terminator-crazy ()
  "Buffer terminator crazy."
  (interactive)
  (setopt buffer-terminator-verbose t)
  (setopt buffer-terminator-inactivity-timeout 3)
  (setopt buffer-terminator-interval 1))

(defun mod-buffer-terminator-sane ()
  "Buffer terminator crazy."
  (interactive)
  (setopt buffer-terminator-inactivity-timeout 200)
  (setopt buffer-terminator-interval 100))

(mod-buffer-terminator-sane)

;; (defvar my-buffer-rename-list nil
;;   "List of buffer renames as cons cells (old-name . new-name).")
;;
;; (defvar my-buffer-rename nil
;;   "List of buffer renames as cons cells (old-name . new-name).")
;;
;; (defun my-record-buffer-rename (orig-fun &rest args)
;;   "Advise `rename-buffer` to record renames in `my-buffer-rename`."
;;   (let ((old-name (buffer-name)))
;;     (apply orig-fun args)
;;     (let ((new-name (buffer-name)))
;;       (unless (equal old-name new-name)
;;         (push (cons old-name new-name) my-buffer-rename)
;;         (unless (memq old-name my-buffer-rename-list)
;;           (push old-name my-buffer-rename-list))
;;         (unless (memq new-name my-buffer-rename-list)
;;           (push new-name my-buffer-rename-list))
;;
;;         ))))
;;
;; (advice-add 'rename-buffer :around #'my-record-buffer-rename)

;; Fix the ubiquity issue
;; (defun my-record-buffer-rename (orig-fun &rest args)
;;   "Advise `rename-buffer` to record renames in `my-buffer-rename`."
;;   (apply orig-fun args)
;;   (buffer-terminator--update-buffer-last-view-time))
;;
;; (advice-add 'rename-buffer :around #'my-record-buffer-rename)

;;; Functions

(defun mod-buffer-terminator-empty ()
  "Kill all buffers."
  (interactive)
  ;; (when (fboundp 'eglot-shutdown-all)
  ;;   (let ((inhibit-message t))
  ;;     (eglot-shutdown-all)))

  (when (fboundp 'easysession-reset)
    (funcall 'easysession-reset))
  ;; (mod-buffer-terminator-only)
  ;; (scratch-buffer)
  )

(defun mod-buffer-terminator-toggle-keep ()
  "Docstring."
  (interactive)
  (setq mod-buffer-terminator--keep (not mod-buffer-terminator--keep))
  (if mod-buffer-terminator--keep
      (message "Always keep: %s" (buffer-name))
    (message "Do not always keep: %s" (buffer-name))))

;; Helper functions
;; (defun buffer-is-visible (buffer)
;;   "Wrapper around `buffer-terminator--buffer-visible-p'.
;; BUFFER is the buffer."
;;   (interactive)
;;   (if (fboundp 'buffer-terminator--buffer-visible-p)
;;       (buffer-terminator--buffer-visible-p buffer)
;;     (error "Undeclared: buffer-terminator--buffer-visible-p")))

(defun mod-buffer-terminator-empty-all ()
  "Kill all buffers."
  (interactive)
  (mod-buffer-terminator-empty)
  (when (fboundp 'tmux-reset)
    (tmux-reset nil)))

(defun mod-buffer-terminator-buffer-list ()
  "Create a buffer listing the names of all open buffers."
  (interactive)
  (let ((list-buffer-name "*Buffer List*"))
    (with-current-buffer (get-buffer-create list-buffer-name)
      (erase-buffer)
      (dolist (buffer (sort (buffer-list)
                            (lambda (a b)
                              (string< (buffer-name a) (buffer-name b)))))
        (insert (format "%s\n" (buffer-name buffer))))
      (pop-to-buffer (current-buffer)))))

;;; buffer terminator II

(defun mod-buffer-terminator--file-buffer ()
  "Return :keep the non file-visiting buffers whose name start with a space."
  (let ((file-name (buffer-file-name (buffer-base-buffer))))
    (when (and file-name
               (buffer-modified-p)
               (not (file-exists-p file-name)))
      ;; TODO add to buffer-terminator?
      (unwind-protect
          (when (and (not buffer-terminator-protect-unsaved-file-buffers)
                     (buffer-modified-p))
            (set-buffer-modified-p nil))
        :kill))))

(defun mod-buffer-terminator--non-file-buffer-name-starts-with-space ()
  "Return :keep the non file-visiting buffers whose name start with a space."
  (let ((buffer-name (buffer-name)))
    (when (and buffer-name
               (string-prefix-p " " (buffer-name))
               (not (buffer-file-name (buffer-base-buffer))))
      :keep)))

(defun mod-buffer-terminator-kill-non-visible-buffers (&optional buffers)
  "Terminate all non visible buffers.
BUFFERS is a buffer or a list of alive buffers."
  (let* ((buffer-terminator-protect-unsaved-file-buffers nil)
         (rules `((call-function . mod-buffer-terminator--file-buffer)
                  (call-function . mod-buffer-terminator--non-file-buffer-name-starts-with-space)
                  (keep-buffer-property . process)
                  ;; (keep-buffer-property . special)
                  (keep-buffer-name . ,mod-buffer-terminator-always-keep)
                  (keep-buffer-name-regexp . ,mod-buffer-terminator-keep-buffer-regexp)
                  (keep-buffer-property . visible)
                  (return . :kill)))
         (result (with-no-warnings
                   (buffer-terminator-apply-rules rules buffers))))
    (let ((inhibit-message t))
      (dolist (buffer-info result)
        ;; (message "[MOD-BUFFER-TERMINATOR] Killed non visible: %s" pair)
        (let* ((buffer-name (cdr (assq 'buffer-name buffer-info)))
               (major-mode (cdr (assq 'major-mode buffer-info)))
               (file-name (cdr (assq 'file-name buffer-info))))
          (message "[MOD-BUFFER-TERMINATOR] Killed non visible: %s (%s)"
                   buffer-name (if file-name file-name major-mode)))))
    (message "[MOD-BUFFER-TERMINATOR] Terminated %s buffers" (length result))))

(defun mod-buffer-terminator-kill-all-buffers (&optional buffers)
  "Kill all visible buffers.
BUFFERS is a buffer or a list of alive buffers."
  (let ((rules '((call-function . mod-buffer-terminator--non-file-buffer-name-starts-with-space)
                 ;; TODO How to remove special without causing an issue
                 (keep-buffer-property . process)
                 ;; (keep-buffer-property . special)
                 (return . :kill))))
    (if (fboundp 'buffer-terminator-apply-rules)
        (buffer-terminator-apply-rules rules buffers)
      (error "Undefined: buffer-terminator-apply-rules"))))

(defun mod-buffer-terminator-find-dired-parent ()
  "Open the current directory in a `dired' buffer and select the current file."
  (interactive)
  (let* ((buffer (or (buffer-base-buffer)
                     (current-buffer)))
         (file-name (buffer-file-name buffer)))
    ;; (let ((flymake-on-save-buffer nil)
    ;;       (save-silently t))
    ;;   (save-buffer buffer))
    (unless file-name
      (setq file-name default-directory))
    (when-let* ((buffer (find-file-noselect (file-name-directory file-name))))
      (switch-to-buffer buffer nil t)
      (with-current-buffer buffer
        (when (fboundp 'dired-goto-file)
          (dired-goto-file file-name)
          (lightemacs-recenter-maybe))))))

(defun mod-buffer-terminator-find-dired-parent-kill-buffer ()
  "Open the current directory in a `dired' buffer and select the current file."
  (let* ((buffer (or (buffer-base-buffer)
                     (current-buffer)))
         (file-name (buffer-file-name buffer)))

    (mod-buffer-terminator-find-dired-parent)
    (when file-name
      (mod-buffer-terminator-kill-non-visible-buffers buffer))))

(defun mod-buffer-terminator---non-minibuffer-windows ()
  "Return a list of all windows excluding the minibuffer."
  (let (result)
    (dolist (win (window-list))
      (when win
        (let ((buffer (window-buffer win)))
          (when (and buffer (not (minibufferp buffer)))
            (push win result)))))
    result))

(defvar-local mod-buffer-terminator--protected-from-close nil
  "Non-nil means the current buffer is protected from being closed.")

(defun mod-buffer-terminator--toggle-protect-buffer-from-close ()
  "Toggle the protection flag to prevent the current buffer from being closed."
  (interactive)
  (setq mod-buffer-terminator--protected-from-close (not mod-buffer-terminator--protected-from-close))
  (message "Buffer protection enabled: %s" mod-buffer-terminator--protected-from-close))

(defun mod-buffer-terminator-close-window (&optional kill-buffer)
  "Close the current window and kill the buffer when KILL-BUFFER is set to t.
When KILL-BUFFER is t, file-visiting buffers are saved before being killed.
If the window is the last one in its tab-bar tab, the tab will also be closed.
By default, closing the last window in a tab does not close the tab."
  (interactive)
  (if mod-buffer-terminator--protected-from-close
      (user-error "You cannot close: %s" (buffer-name))
    (let* ((buffer (or (buffer-base-buffer) (current-buffer)))
           (number-of-splits (length (mod-buffer-terminator---non-minibuffer-windows))))
      ;; (with-current-buffer buffer
      ;;   (buffer-guardian-save-buffer))

      ;; Close the window/tab
      (if (and (boundp 'tab-bar-mode) tab-bar-mode)
          (let ((amount-open-tabs (length (funcall tab-bar-tabs-function))))
            (cond ((and (= 1 number-of-splits)
                        (> amount-open-tabs 1))
                   (tab-close))
                  ((> number-of-splits 1)
                   (delete-window))
                  (t
                   (scratch-buffer))))
        (delete-window))

      ;; Save and close the buffer
      (when kill-buffer
        (mod-buffer-terminator-kill-non-visible-buffers buffer)))))

(defun mod-buffer-terminator-close-window-kill-buffer ()
  "Save and kill the current buffer and close the current window.
If the window is the last one in its tab-bar tab, the tab will also be closed.
By default, closing the last window in a tab does not close the tab."
  (interactive)
  (let ((kill-buffer t))
    (mod-buffer-terminator-close-window kill-buffer)))

(defun mod-buffer-terminator-only ()
  "Kill all the other buffers."
  (interactive)
  (buffer-guardian-save-all-buffers)
  (when (and (bound-and-true-p tab-bar-mode)
             (fboundp 'tab-bar-close-other-tabs))
    (tab-bar-close-other-tabs))
  (delete-other-windows)
  (mod-buffer-terminator-kill-all-buffers))

(defun mod-buffer-terminator-only-visible ()
  "Kill all the buffers that are not currently displayed in a window or tab."
  (interactive)
  (buffer-guardian-save-all-buffers)
  (mod-buffer-terminator-kill-non-visible-buffers))

;;; Evil

(with-eval-after-load "evil"
  (define-key evil-normal-state-map (kbd "<leader>ov") #'mod-buffer-terminator-only-visible)
  (define-key evil-normal-state-map (kbd "<leader>ey") #'mod-buffer-terminator-empty)
  (define-key evil-normal-state-map (kbd "<leader>eY") #'mod-buffer-terminator-empty-all)
  (define-key evil-normal-state-map (kbd "C-w c")   #'mod-buffer-terminator-close-window)
  (define-key evil-normal-state-map (kbd "C-w C-c") #'mod-buffer-terminator-close-window))

(provide 'mod-buffer-terminator)

;; Local variables:
;; byte-compile-warnings: (not lexical free-vars)
;; End:

;;; mod-buffer-terminator.el ends here

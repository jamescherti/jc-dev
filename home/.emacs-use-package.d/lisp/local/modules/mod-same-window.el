;;; mod-same-window.el --- Force buffers to open in the same window -*- lexical-binding: t -*-

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

;;; Fix embark

(defun fix-embark-collect-window-history (&rest _)
  "Remove intermediate buffer from history.
This ensures `previous-buffer' returns to Embark Collect.

The issue that this function fixes is due to how Embark establishes context
before executing an action.

When you trigger an action in an *Embark Collect* buffer (for example, by
clicking or pressing RET), Embark does not jump directly from the collect buffer
to the target. Instead, it follows this sequence:
- Embark temporarily switches the current window back to the original buffer
  where you initially invoked the search or command.
- It executes the action from that original context.

The action runs and opens the target buffer. Because your `display-buffer-alist'
forces `display-buffer-same-window', the target buffer replaces the original
buffer in the same window.

Every time a buffer replaces another in the same window, Emacs records the
previous buffer in the window history (window-prev-buffers). Because of the
intermediate context switch, your window history stack looks like this:

- Current buffer: The destination target.
- History index 0: The original buffer (the intermediate step).
- History index 1: The *Embark Collect* buffer.

When you call `previous-buffer' the first time, you land on history index 0.
Calling it a second time takes you to history index 1 (the collect buffer)."
  (let* ((win (selected-window))
         (prev (window-prev-buffers win)))
    (when (and (>= (length prev) 2)
               (string-match-p "Embark Collect" (buffer-name (car (nth 1 prev)))))
      (set-window-prev-buffers win (cdr prev)))))

(with-eval-after-load 'embark
  (advice-add 'embark-collect-choose :after
              #'fix-embark-collect-window-history))

;;; Always current window

(defun always-current-window---display-buffer-from-compilation-p (_buffer-name _action)
  "Display buffer from compilation."
  (unless current-prefix-arg
    (with-current-buffer (window-buffer)
      (or
       (derived-mode-p 'embark-collect-mode)
       (derived-mode-p 'compilation-mode)))))

(defun current-window-only-setup ()
  "Make Emacs only use the current window."
  ;; org-mode
  (setq org-src-window-setup 'current-window) ;; Edit source in current window
  (setq org-agenda-window-setup 'current-window)

  ;; Open links in help windows (like links to files) in the current window
  (setq help-window-keep-selected t)

  ;; Compilation buffers. Also used by wgrep buffers / Embark export.
  (push '(always-current-window---display-buffer-from-compilation-p
          display-buffer-same-window
          (inhibit-same-window . nil))
        display-buffer-alist)

  ;; Setup display buffer alist using push for performance.
  (dolist (regexp '("\\*Man"
                    "\\*eat"
                    "\\*Memory-Report\\*"
                    "\\*helpful"
                    "\\*Backtrace\\*"
                    "\\*\\(Help\\|eldoc\\)\\*"
                    "\\*[Hh]elp:"
                    "\\*sdcv"

                    ;; markdown-mode. I want to edit in a separate window
                    "\\*edit-indirect "

                    "\\*Proced\\*"
                    "\\*Embark Collect"
                    "\\*Embark Export"))
    (push `(,regexp (display-buffer-same-window)) display-buffer-alist)))

(unless noninteractive
  (current-window-only-setup))

(provide 'mod-same-window)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-same-window.el ends here

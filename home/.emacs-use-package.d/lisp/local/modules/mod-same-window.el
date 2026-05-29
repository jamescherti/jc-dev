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
                    "\\*Embark Export"))
    (push `(,regexp (display-buffer-same-window)) display-buffer-alist)))

(unless noninteractive
  (current-window-only-setup))

(provide 'mod-same-window)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-same-window.el ends here

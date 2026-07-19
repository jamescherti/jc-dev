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

;;; Fix compilation buffer

;; When an action is triggered from an Embark Collect buffer, Embark does not
;; execute the command directly within that buffer. Instead, it temporarily
;; restores the window to the original source buffer to establish the correct
;; execution context. Because this context switch happens before Emacs
;; evaluates `display-buffer-alist', a standard major mode check will fail to
;; detect that the action actually originated from Embark Collect.
;;
;; To ensure the destination buffer consistently opens in the same window,
;; this function inspects the window history stack. When Embark switches back
;; to the source buffer, Emacs immediately pushes the Collect buffer to the
;; top of the `window-prev-buffers' list. By checking if the most recent
;; previous buffer was running `embark-collect-mode', we can accurately
;; identify the source of the action and enforce the same window display rule.

(defun mod-same-window---display-buffer-from-compilation-p (_buffer-name _action)
  "Display buffer from compilation or Embark Collect."
  (unless current-prefix-arg
    (or
     (derived-mode-p 'compilation-mode)
     (derived-mode-p 'embark-collect-mode)
     ;; When you select a candidate in the Embark Collect buffer, Embark
     ;; temporarily switches back to the original buffer to run the command.
     ;; Because of this context switch, standard window rules fail to see that
     ;; the action originated from the collect buffer.
     ;;
     ;; This code looks at the window history to solve the issue. When Embark
     ;; switches back to the original buffer, Emacs places the collect buffer at
     ;; the top of the 'previous buffers' list. By checking this history, we can
     ;; detect when an action comes from Embark Collect and force the
     ;; destination to open in the same window.
     (let ((prev (car (window-prev-buffers))))
       (and prev
            (buffer-live-p (car prev))
            (with-current-buffer (car prev)
              (derived-mode-p 'embark-collect-mode)))))))

(push '(mod-same-window---display-buffer-from-compilation-p
        display-buffer-same-window
        (inhibit-same-window . nil))
      display-buffer-alist)

;;; Fix embark

;; (defun mod-same-window--fix-embark-collect-window-history (&rest _)
;;   "Remove intermediate buffer from history.
;; This ensures `previous-buffer' returns to Embark Collect.
;;
;; The issue that this function fixes is due to how Embark establishes context
;; before executing an action.
;;
;; When you trigger an action in an *Embark Collect* buffer (for example, by
;; clicking or pressing RET), Embark does not jump directly from the collect buffer
;; to the target. Instead, it follows this sequence:
;; - Embark temporarily switches the current window back to the original buffer
;;   where you initially invoked the search or command.
;; - It executes the action from that original context.
;;
;; The action runs and opens the target buffer. Because your
;; `display-buffer-alist' forces `display-buffer-same-window', the target buffer
;; replaces the original buffer in the same window.
;;
;; Every time a buffer replaces another in the same window, Emacs records the
;; previous buffer in the window history (window-prev-buffers). Because of the
;; intermediate context switch, your window history stack looks like this:
;;
;; - Current buffer: The destination target.
;; - History index 0: The original buffer (the intermediate step).
;; - History index 1: The *Embark Collect* buffer.
;;
;; When you call `previous-buffer' the first time, you land on history index 0.
;; Calling it a second time takes you to history index 1 (the collect buffer)."
;;   (let* ((win (selected-window))
;;          (prev (window-prev-buffers win)))
;;     (when (and (>= (length prev) 2)
;;                (string-match-p "Embark Collect" (buffer-name (car (nth 1 prev)))))
;;       (set-window-prev-buffers win (cdr prev)))))
;;
;; (with-eval-after-load 'embark
;;   (advice-add 'embark-collect-choose :after
;;               #'mod-same-window--fix-embark-collect-window-history))

;;; Always current window

(defun mod-same-window--current-window-only-setup ()
  "Make Emacs only use the current window."
  ;; org-mode
  (setq org-src-window-setup 'current-window) ;; Edit source in current window
  (setq org-agenda-window-setup 'current-window)

  ;; Display indirect tree buffers in the current window
  (setq org-indirect-buffer-display 'current-window)

  ;; Show agenda in the current window, keeping all other windows.
  (setq org-agenda-window-setup 'current-window)

  ;; Open links in help windows (like links to files) in the current window
  (setq help-window-keep-selected t)

  ;; Setup display buffer alist using push for performance.
  (dolist (regexp '("\\*Man"
                    "\\*eat"
                    "\\*Memory-Report\\*"
                    "\\*Backtrace\\*"
                    "\\*eldoc\\*"
                    "\\*tmux"
                    ;; "\\*grep\\*"
                    "\\*edit-indirect "))
    (push `(,regexp (display-buffer-same-window)) display-buffer-alist)))

(unless noninteractive
  (mod-same-window--current-window-only-setup))

;;; Exceptions

(unless noninteractive
  (add-to-list 'display-buffer-alist
               '((or (derived-mode . occur-mode)
                     (derived-mode . Buffer-menu-mode)
                     (derived-mode . proced-mode)
                     (derived-mode . quick-sdcv-mode)
                     (derived-mode . log-view-mode)
                     (derived-mode . woman-mode)
                     (derived-mode . helpful-mode)
                     (derived-mode . help-mode)
                     (derived-mode . grep-mode)
                     (derived-mode . embark-collect-mode))
                 ;; Display buffer in the currently selected window
                 (display-buffer-same-window)
                 ;; Allow the buffer to be displayed in the same window even if
                 ;; it is already displayed there
                 (inhibit-same-window . nil)))

  (dolist (entry
           '(("\\*pathaction:"
              (display-buffer-at-bottom)
              (window-height . 0.33))
             ("\\*CPU-Profiler-Report" (display-buffer-at-bottom))
             ("\\*Memory-Profiler-Report" (display-buffer-at-bottom))
             ("\\*Calendar\\*" (display-buffer-at-bottom))))
    (push entry display-buffer-alist))

  ;; Force Org selection menus and capture buffers into a dedicated bottom side
  ;; window. This prevents Emacs from spawning new graphical frames, which
  ;; occurs when pop-up-windows is nil and Org internally requests a window
  ;; split. The regular expression explicitly traps all three stages of the
  ;; capture process:
  ;;
  ;; - *Org Select*: The initial template selection menu.
  ;; - *Capture* (and numbered variants): The temporary buffer Org uses to
  ;;   expand templates.
  ;; - CAPTURE-: The final indirect buffer where the note is edited.
  (add-to-list
   'display-buffer-alist
   '("\\`\\*\\(Org \\(Select\\|Note\\)\\|Agenda Commands\\)\\*\\'\\|\\`\\*Capture\\*\\(?:<[0-9]+>\\)?\\'\\|\\`CAPTURE-"
     ;; Use side window for placement
     (display-buffer-in-side-window)
     ;; Ensure window is dedicated to this buffer
     (dedicated . t)
     ;; Position the window at the bottom of the frame
     (side . bottom)
     ;; Set slot to 0 to be the primary window in this side
     (slot . 0)
     ;; Set the height to 33% of the total frame height
     (window-height . 0.20)
     ;; Force the window to preserve the defined height
     (preserve-size . (t . t))
     ;; Apply window parameters to hide the mode line
     (window-parameters . ((mode-line-format . none)))))

  (push `(,(rx (or "*Org Agenda*" "*Agenda Commands*"))
          display-buffer-in-side-window
          (side . right)
          (slot . 0)
          (window-parameters . ((no-delete-other-windows . t)))
          (window-width . 100)
          (dedicated . t))
        display-buffer-alist))

;;; Provide

(provide 'mod-same-window)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; env-allow-syntax-checker-package-lint: nil
;; End:

;;; mod-same-window.el ends here

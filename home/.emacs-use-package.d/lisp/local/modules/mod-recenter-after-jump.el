;;; mod-recenter-after-jump.el --- Recenter after jump -*- lexical-binding: t -*-

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

(require 'lightemacs)  ;; lightemacs-recenter-maybe

;;; Main code

(defvar lightemacs-maybe-recenter-after-jump t
  "Non-nil enables recentering the window when the point jumps out of view.
Recentering only occurs when `scroll-conservatively' is >= 101. The recenter
position can be customized using `lightemacs-maybe-recenter-after-jump-value'.")

(defvar lightemacs-maybe-recenter-after-jump-value 12
  "The line position for recentering the window when the point jumps out of view.
Only used when `lightemacs-maybe-recenter-after-jump' is non-nil and
`scroll-conservatively' is >= 101. A numeric value indicates the number of lines
from the top of the window; nil recenters in the middle.")

(defun lightemacs-default-settings--recenter-maybe ()
  "Recenter conditionally when `scroll-conservatively' is set to 101 or higher.
This ensures that conservative scrolling is preserved while maintaining point
visibility when navigation commands are executed."
  (when (and lightemacs-maybe-recenter-after-jump
             (>= scroll-conservatively 101))
    (lightemacs-recenter-maybe lightemacs-maybe-recenter-after-jump-value)))

(defun lightemacs-default-settings--recenter-maybe-adjust-arg ()
  "Recenter conditionally when `scroll-conservatively' is set to 101 or higher.
This ensures that conservative scrolling is preserved while maintaining point
visibility when navigation commands are executed."
  (when (and lightemacs-maybe-recenter-after-jump
             (>= scroll-conservatively 101))
    (lightemacs-recenter-maybe lightemacs-maybe-recenter-after-jump-value t)))

(defun lightemacs-default-settings--advice-recenter-maybe-adjust-arg (fn &rest args)
  "FN is the advised function. ARGS are the function arguments."
  (unwind-protect
      (apply fn args)
    (lightemacs-default-settings--recenter-maybe-adjust-arg)))

;; TODO use post-command-hook?

(defun lightemacs-default-settings--advice-recenter-maybe (fn &rest args)
  "FN is the advised function. ARGS are the function arguments."
  (unwind-protect
      (apply fn args)
    (lightemacs-recenter-maybe)))

(defun lightemacs-default-settings--advice-recenter-always (fn &rest args)
  "FN is the advised function. ARGS are the function arguments."
  (unwind-protect
      (apply fn args)
    (when (and (eq (current-buffer) (window-buffer))
               (not (pos-visible-in-window-p (point))))
      (recenter))))

;; (with-eval-after-load 'saveplace  ; TODO test more
;;   (add-hook 'save-place-after-find-file-hook
;;             #'lightemacs-default-settings--recenter-maybe 70))
;;
;; (with-eval-after-load 'imenu  ; TODO test more
;;   (add-hook 'imenu-after-jump-hook
;;             #'lightemacs-default-settings--recenter-maybe 70))
;;
;; (with-eval-after-load 'consult  ; TODO test more
;;   (add-hook 'consult-after-jump-hook
;;             #'lightemacs-default-settings--recenter-maybe 70))
;;
;; (with-eval-after-load 'org-agenda  ; TODO test more
;;   (add-hook 'org-agenda-after-show-hook
;;             #'lightemacs-default-settings--recenter-maybe 70))
;;
;; (with-eval-after-load 'bookmark  ; TODO test more
;;   (add-hook 'bookmark-after-jump-hook
;;             #'lightemacs-default-settings--recenter-maybe 70))

(with-eval-after-load 'evil-commands
  (advice-add 'evil-goto-last-change-reverse :around
              #'lightemacs-default-settings--advice-recenter-maybe)
  (advice-add 'evil-goto-last-change :around
              #'lightemacs-default-settings--advice-recenter-maybe))

(with-eval-after-load 'diff-hl
  (advice-add 'diff-hl-previous-hunk :around
              #'lightemacs-default-settings--advice-recenter-always)
  (advice-add 'diff-hl-next-hunk :around
              #'lightemacs-default-settings--advice-recenter-always))

(with-eval-after-load 'git-gutter
  (advice-add 'git-gutter:previous-hunk :around
              #'lightemacs-default-settings--advice-recenter-maybe)
  (advice-add 'git-gutter:next-hunk :around
              #'lightemacs-default-settings--advice-recenter-maybe))

(with-eval-after-load 'flymake
  (advice-add 'flymake-goto-next-error :around
              #'lightemacs-default-settings--advice-recenter-maybe)
  (advice-add 'flymake-goto-prev-error :around
              #'lightemacs-default-settings--advice-recenter-maybe))

(with-eval-after-load 'evil
  (advice-add 'evil-goto-mark :around
              #'lightemacs-default-settings--advice-recenter-maybe-adjust-arg)

  (advice-add 'evil-ex-search-previous :around
              #'lightemacs-default-settings--advice-recenter-maybe-adjust-arg)
  (advice-add 'evil-ex-search-next :around
              #'lightemacs-default-settings--advice-recenter-maybe-adjust-arg))

;; When the user presses C-o
(unless noninteractive
  (add-hook 'evil-jumps-post-jump-hook
            #'lightemacs-default-settings--recenter-maybe-adjust-arg 70))

(with-eval-after-load 'simple
  (add-hook 'next-error-hook
            #'lightemacs-default-settings--recenter-maybe-adjust-arg))

(with-eval-after-load 'xref
  (when (boundp 'xref-after-jump-hook)
    (let ((xref-pulse-originally-present (memq 'xref-pulse-momentarily
                                               xref-after-jump-hook)))
      (remove-hook 'xref-after-jump-hook 'recenter)
      (remove-hook 'xref-after-jump-hook 'xref-pulse-momentarily)

      (add-hook 'xref-after-return-hook
                #'lightemacs-default-settings--recenter-maybe 70)

      (add-hook 'xref-after-jump-hook
                'lightemacs-default-settings--recenter-maybe 70)
      (when xref-pulse-originally-present
        (add-hook 'xref-after-jump-hook 'xref-pulse-momentarily 71)))))

;;; Provide

(provide 'mod-recenter-after-jump)

;;; mod-recenter-after-jump.el ends here

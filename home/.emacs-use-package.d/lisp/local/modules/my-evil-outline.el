;;; my-evil-outline.el --- my-evil-outline -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/jc-dev
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
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

;;; Code:

(require 'evil)
(require 'outline)
(require 'buffer-guardian)
(require 'my-evil-intercept)

;;; outline: Restore column: move subtree up/down

(defun my-advice-outline-move-subtree-up-down (orig-fun &rest args)
  "Move the current subtree up/down past ARGS headlines of the same level.
This function ensures the last blank line is included, even when
`outline-blank-line' is set to t. It also restores the cursor position,
addressing the issue where the cursor might be reset after the operation.
ORIG-FUN is the function ARGS are the arguments."
  (interactive "p")
  ;; Pass `'(1)' instead of `1' as fallback to `apply', which requires its last
  ;; argument to be a list when `args' is nil.
  (if (bound-and-true-p outline-minor-mode)
      (apply orig-fun (or args '(1)))
    (let ((column (current-column)))
      (unwind-protect
          (apply orig-fun (or args '(1)))
        (move-to-column column)))))

(with-eval-after-load 'outline
  (advice-add 'outline-move-subtree-up
              :around #'my-advice-outline-move-subtree-up-down)
  (advice-add 'outline-move-subtree-down
              :around #'my-advice-outline-move-subtree-up-down))

;;; Restore column: next/previous heading

;; (evil-define-key 'normal 'global (kbd "[[") #'evil-backward-section-begin)
;; (evil-define-key 'normal 'global (kbd "]]") #'evil-backward-section-end)

;; Replace with

;; TODO fix this
;; (defun my-advice-outline-restore-column (orig-fun &rest args)
;;   "Advice to restore the column.
;; ORIG-FUN is the function ARGS are the arguments."
;;   (interactive "p")
;;   (if (bound-and-true-p outline-minor-mode)
;;       (apply orig-fun args)
;;     (let ((column (current-column)))
;;       (unwind-protect
;;           (apply orig-fun (or args 1))
;;         (move-to-column column)))))
;;
;; (with-eval-after-load 'outline
;;   (advice-add 'outline-next-visible-heading
;;               :around #'my-advice-outline-restore-column)
;;   (advice-add 'outline-previous-visible-heading
;;               :around #'my-advice-outline-restore-column))

;;; Provide

(provide 'my-evil-outline)

;; Local variables:
;; env-allow-syntax-checker-package-lint: nil
;; End:

;;; my-evil-outline.el ends here

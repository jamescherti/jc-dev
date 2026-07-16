;;; dir-locals-trigger.el --- Run manual logic after dir-locals -*- lexical-binding: t; -*-

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

;; A minimal package that provides a dedicated hook and helper function.
;; It lets you manually evaluate your directory-local variables and enable
;; modes after Emacs applies them.

;;; Code:

(defgroup dir-locals-trigger nil
  "Customization options for `dir-locals-trigger-mode'."
  :group 'dir-locals-trigger
  :prefix "dir-locals-trigger-")

(defcustom dir-locals-trigger-hook nil
  "Hook run immediately after directory-local variables are applied."
  :type 'hook
  :group 'local)

(defun dir-locals-trigger-mark-safe (var)
  "Mark VAR as a safe local variable for boolean values."
  (put var 'safe-local-variable #'booleanp))

(defmacro dir-locals-trigger-defvar (var &optional initvalue docstring)
  "Define VAR with INITVALUE and DOCSTRING, and mark it safe for booleans."
  (declare (indent defun) (doc-string 3))
  `(progn
     (defvar ,var ,initvalue ,docstring)
     (dir-locals-trigger-mark-safe ',var)))

(defun dir-locals-trigger--run ()
  "Execute the user-defined hook `dir-locals-trigger-hook'."
  (run-hooks 'dir-locals-trigger-hook))

;;;###autoload
(define-minor-mode dir-locals-trigger-mode
  "Global minor mode to run `dir-locals-trigger-hook'."
  :global t
  :group 'dir-locals-trigger
  (if dir-locals-trigger-mode
      (add-hook 'hack-local-variables-hook #'dir-locals-trigger--run)
    (remove-hook 'hack-local-variables-hook #'dir-locals-trigger--run)))

(provide 'dir-locals-trigger)

;;; dir-locals-trigger.el ends here

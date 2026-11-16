;;; point-manager.el --- Manage cursor movement and restrictions -*- lexical-binding: t -*-

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

;; The `point-manager' package provides a global minor mode that enforces cursor
;; constraints and manages point movement to improve the editing experience.
;;
;; Key features include:
;; - Restoring the previous column position when navigating at the end of a
;;   buffer or around empty lines.
;; - Preventing the cursor from moving into invalid areas in `dired-mode', such
;;   as the directory header or the first two columns.
;; - Automatically disabling itself in unsupported or excluded buffers
;;   (e.g., hidden buffers, special buffers, or the minibuffer).
;; - Integration with Evil mode, restricting cursor management to the normal
;;   state.

;;; Code:

(require 'my-defun) ; `my-pos-bol'

(defgroup point-manager nil
  "Point manager."
  :group 'point-manager
  :prefix "point-manager-")

(defcustom point-manager-exclude-minibuffer t
  "If non-nil, inhibit point-manager in the minibuffer."
  :type 'boolean
  :group 'point-manager)

(defcustom point-manager-exclude-hidden-buffers t
  "If non-nil, inhibit point-manager in hidden buffers.
This excludes buffers whose names begin with a space."
  :type 'boolean
  :group 'point-manager)

(defcustom point-manager-exclude-special-buffers t
  "If non-nil, inhibit point-manager in special buffers.
This excludes buffers whose names begin with an asterisk (*)."
  :type 'boolean
  :group 'point-manager)

(defcustom point-manager-excluded-modes
  '(special-mode
    minibuffer-mode
    comint-mode
    term-mode
    vterm-mode
    eshell-mode
    shell-mode
    help-mode
    compilation-mode
    magit-mode)
  "List of major modes where point-manager is inhibited."
  :type '(repeat symbol)
  :group 'point-manager)

;; (defvar point-manager-restore-column-after-commands
;;   '(evil-delete
;;     enhanced-evil-paredit-delete))

(defvar point-manager-move-to-column-force nil)
(defvar point-manager-ignore-invisible nil) ; t is slow
;; (defvar point-manager-inhibit-when-region-active t)
(defvar point-manager-dired-min-column 2)

;;; Internal variables

(defvar point-manager--pre-command-window nil
  "The window selected before the current command.")
(defvar point-manager--pre-command-buffer nil
  "The buffer current before the current command.")

(defvar-local point-manager--buffer-type nil
  "Type of the current buffer (e.g., `file' or `dired').")
(defvar-local point-manager--inhibit t
  "Non-nil means inhibit point-manager in the current buffer.")
(defvar-local point-manager--initialized nil
  "Non-nil if point-manager has been initialized in this buffer.")
;; (defvar-local point-manager--region-active-p nil)
;; (defvar-local point-manager--pre-command nil)

;;; Functions

(defun point-manager--initialize-buffer ()
  "Determine if point-manager should be active in the current buffer."
  (setq point-manager--initialized t)
  (setq point-manager--inhibit
        (or (and point-manager-exclude-minibuffer
                 (or (minibufferp) (window-minibuffer-p)))
            (let ((buffer-name (buffer-name)))
              (or
               (and point-manager-exclude-hidden-buffers
                    (string-prefix-p " " buffer-name))
               (and point-manager-exclude-special-buffers
                    (string-prefix-p "*" buffer-name))))
            (derived-mode-p point-manager-excluded-modes)))

  (unless point-manager--inhibit
    (cond
     (buffer-file-name
      (setq point-manager--buffer-type 'file))

     ((derived-mode-p 'dired-mode)
      (setq point-manager--buffer-type 'dired))

     (t
      (setq point-manager--buffer-type :unsupported)
      (setq point-manager--inhibit t)))))

(defun point-manager--reset-initialization ()
  "Reset initialization status so it is re-evaluated."
  (setq point-manager--initialized nil))

(defun point-manager--pre-command-hook (&rest _)
  "Save point and column position.
The argument _ is ignored."
  (unless point-manager--initialized
    (point-manager--initialize-buffer))

  ;; (setq point-manager--pre-command this-command)
  (unless point-manager--inhibit
    (setq point-manager--pre-command-window (selected-window)
          point-manager--pre-command-buffer (current-buffer))
    (set-window-parameter nil 'point-manager--previous-point (point))
    (set-window-parameter nil 'point-manager--previous-column (current-column))))

(defun point-manager--move-to-column (column)
  "Move to COLUMN and update the temporary goal column."
  (move-to-column column point-manager-move-to-column-force)
  (setq temporary-goal-column column))

(defun point-manager--post-command-hook (&optional _command)
  "Maintain cursor constraints in normal state.
The argument _COMMAND is the previous command (ignored)."
  (when (and (not point-manager--inhibit)
             (eq (selected-window) point-manager--pre-command-window)
             (eq (current-buffer) point-manager--pre-command-buffer)
             (let ((prev-pt (window-parameter
                             nil 'point-manager--previous-point)))
               (and prev-pt (/= (point) prev-pt)))
             (or (not (boundp 'evil-state))
                 (eq evil-state 'normal))
             (not (minibufferp))
             (not (region-active-p)))
    (let ((prev-col (window-parameter
                     nil 'point-manager--previous-column)))
      ;; Restore the previous column when moving to the end of the buffer.
      (when (and (eobp) (not (bobp)))
        (forward-char -1)
        (if point-manager-ignore-invisible
            (vertical-motion 0)
          (goto-char (my-pos-bol)))
        (point-manager--move-to-column prev-col))

      ;; Enforce Dired's header and minimum-column constraints.
      (when (eq point-manager--buffer-type 'dired)
        ;; DIRED: First line
        ;; Prevent the cursor from moving to the first line containing the
        ;; directory header. An alternative approach is available via (setq
        ;; dired-movement-style 'bounded-files). However, this does not
        ;; accommodate navigation using gg or G in Evil mode (moving to the top
        ;; and bottom of the buffer).
        (let ((header-p (= (my-pos-bol) (point-min))))
          (when header-p
            (forward-line 1)
            (point-manager--move-to-column prev-col))
          (when (and point-manager-dired-min-column
                     (< (if header-p prev-col (current-column))
                        point-manager-dired-min-column))
            (point-manager--move-to-column
             point-manager-dired-min-column)))))))

;;;###autoload
(define-minor-mode point-manager-mode
  "Global minor mode to manage cursor movement and restrictions."
  :global t
  :lighter " PointM"
  :group 'point-manager
  (if point-manager-mode
      (progn
        (add-hook 'after-change-major-mode-hook #'point-manager--reset-initialization)
        (add-hook 'pre-command-hook #'point-manager--pre-command-hook -95)
        (add-hook 'post-command-hook #'point-manager--post-command-hook 95))
    (remove-hook 'after-change-major-mode-hook #'point-manager--reset-initialization)
    (remove-hook 'pre-command-hook #'point-manager--pre-command-hook)
    (remove-hook 'post-command-hook #'point-manager--post-command-hook)))

(point-manager-mode 1)

(provide 'point-manager)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; point-manager.el ends here

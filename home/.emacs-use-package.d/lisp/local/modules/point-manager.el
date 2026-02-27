;;; point-manager.el --- point-manager -*- lexical-binding: t -*-

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

(defgroup point-manager nil
  "Point manager."
  :group 'point-manager
  :prefix "point-manager-")

;; (defvar point-manager-restore-column-after-commands
;;   '(evil-delete
;;     enhanced-evil-paredit-delete))

(defvar point-manager-move-to-column-force nil)
(defvar point-manager-ignore-invisible t)
;; (defvar point-manager-inhibit-when-region-active t)
(defvar point-manager-dired-min-column 2)

;;; Internal variables

(defvar-local point-manager--previous-column nil)
(defvar-local point-manager--previous-point nil)
(defvar-local point-manager--buffer-type nil)
(defvar-local point-manager--inhibit t)
(defvar-local point-manager--last-major-mode nil)
;; (defvar-local point-manager--region-active-p nil)
;; (defvar-local point-manager--pre-command nil)

;;; Functions

(defun point-manager--pre-command-hook (&rest _)
  "Save point and column position."
  (setq point-manager--inhibit (minibufferp))

  ;; (setq point-manager--pre-command this-command)
  (when (and
         ;; Not inhibited
         (not point-manager--inhibit)
         (or
          ;; The buffer type has never been set
          (not point-manager--buffer-type)
          ;; The major mode has been changed
          (not (eq point-manager--last-major-mode major-mode))))
    (cond
     (buffer-file-name
      (setq point-manager--last-major-mode major-mode)
      (setq point-manager--buffer-type 'file))

     ((or (eq major-mode 'dired-mode)
          (derived-mode-p 'dired-mode))
      (setq point-manager--last-major-mode major-mode)
      (setq point-manager--buffer-type 'dired))

     (t
      (setq point-manager--buffer-type :unsupported)
      (setq point-manager--inhibit t))))

  (unless point-manager--inhibit
    (setq point-manager--previous-point (point))
    (setq point-manager--previous-column (current-column))))

(defun point-manager--move-to-column (column)
  "Move to COLUMN and update the temporary goal column."
  (move-to-column column point-manager-move-to-column-force)
  (setq temporary-goal-column column))

(defun point-manager--post-command-hook (&optional _command)
  "Maintain cursor constraints in normal state.
COMMAND is the previous command."
  (when (and (not point-manager--inhibit)
             (or (not (boundp 'evil-state))
                 (eq evil-state 'normal))
             ;; POINT has been changed
             (and point-manager--previous-point
                  (/= (point) point-manager--previous-point))
             (not (minibufferp))
             (not (region-active-p)))
    ;; DELETE: Restore the column after deleting
    ;; NOTE: this fails when using corfu completionm, for some reason
    ;; (setq point-manager--region-active-p (region-active-p))
    ;; (when (and (not point-manager--region-active-p)
    ;;            point-manager-restore-column-after-commands
    ;;            (memq point-manager--pre-command
    ;;                  point-manager-restore-column-after-commands))
    ;;   (point-manager--move-to-column point-manager--previous-column))

    ;; END OF FILE
    (when (and (eobp)
               (not (bobp)))
      (ignore-errors
        (backward-char 1))

      (if point-manager-ignore-invisible
          (beginning-of-visual-line)
        (beginning-of-line))

      (point-manager--move-to-column point-manager--previous-column))

    ;; DIRED: Handle dired-mode header line and column constraints
    (when (eq point-manager--buffer-type 'dired)
      (let ((column (current-column)))
        ;; DIRED: First line
        ;; Prevent the cursor from moving to the first line containing the
        ;; directory header. An alternative approach is available via (setq
        ;; dired-movement-style 'bounded-files). However, this does not
        ;; accommodate navigation using gg or G in Evil mode (moving to the top
        ;; and bottom of the buffer).
        (when (save-excursion
                ;; First line?
                (forward-line 0)
                (bobp))
          (forward-line 1)
          (point-manager--move-to-column point-manager--previous-column)
          ;; This is used by the next check (2 columns)
          (setq column point-manager--previous-column))

        ;; DIRED: First two columns
        (when (and point-manager-dired-min-column
                   (< column point-manager-dired-min-column))
          (point-manager--move-to-column point-manager-dired-min-column))))))

;;;###autoload
(define-minor-mode point-manager-mode
  "Global minor mode to manage cursor movement and restrictions."
  :global t
  :lighter " PointM"
  :group 'point-manager
  (if point-manager-mode
      (progn
        ;; (add-hook
        ;;  'after-change-major-mode-hook #'point-manager--post-command-hook 95)
        (add-hook 'pre-command-hook #'point-manager--pre-command-hook -95)
        (add-hook 'post-command-hook #'point-manager--post-command-hook 95))
    ;; (remove-hook
    ;;  'after-change-major-mode-hook #'point-manager--post-command-hook)
    (remove-hook 'pre-command-hook #'point-manager--pre-command-hook)
    (remove-hook 'post-command-hook #'point-manager--post-command-hook)))

(point-manager-mode 1)

(provide 'point-manager)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; point-manager.el ends here

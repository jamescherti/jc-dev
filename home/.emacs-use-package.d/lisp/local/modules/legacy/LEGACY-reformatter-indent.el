;;; reformatter-indent.el --- reformatter-indent -*- lexical-binding: t -*-

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

;;; DEPRECATED: Legacy unmaintained code. Safe to remove if it causes
;;; regressions.

;; Auto indent elisp on save mode

;;; Code:

(defun reformatter-indent-buffer ()
  "Indent the entire buffer using `indent-region'."
  (interactive)
  (when (derived-mode-p 'emacs-lisp-mode)
    (save-excursion
      (widen)
      (indent-region (point-min) (point-max)))))

(define-minor-mode reformatter-indent-on-save-mode
  "Indent the entire buffer using `indent-region'."
  :global nil
  (if reformatter-indent-on-save-mode
      (add-hook 'before-save-hook #'reformatter-indent-buffer)
    (remove-hook 'before-save-hook #'reformatter-indent-buffer)))

(provide 'reformatter-indent)
;;; reformatter-indent.el ends here

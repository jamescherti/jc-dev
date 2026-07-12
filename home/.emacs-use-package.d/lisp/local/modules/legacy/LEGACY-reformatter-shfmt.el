;;; reformatter-shfmt.el --- Shfmt -*- lexical-binding: t -*-

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

;;; Code:

(require 'reformatter)
(require 'sh-script)

(defgroup reformatter-shfmt nil
  "Reformat shell scripts using shfmt."
  :group 'reformatter-shfmt)

(defun reformatter-shfmt--get-cmd-args (input-file)
  "Get shfmt arguments. INPUT-FILE is the bash/sh file."
  (append
   (list "-filename" input-file
         "-i" (number-to-string
               (cond ((boundp 'sh-basic-offset)
                      sh-basic-offset)

                     (t 4)))
         "--binary-next-line")
   ;; (when (bound-and-true-p sh-shell)
   ;;   "-ln" (cond ((eq sh-shell 'bash)
   ;;                "bash")
   ;;               (t "posix")))
   ))

(reformatter-define
 reformatter-shfmt
 :program "shfmt"
 :args (reformatter-shfmt--get-cmd-args
        (or (buffer-file-name (buffer-base-buffer)) input-file))
 :lighter " ShFmt"
 :group 'shfmt)

(provide 'reformatter-shfmt)
;;; reformatter-shfmt.el ends here

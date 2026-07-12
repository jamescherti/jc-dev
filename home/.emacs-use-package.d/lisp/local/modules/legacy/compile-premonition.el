;;; compile-premonition.el --- Load compile-premonition packages in the future -*- lexical-binding: t -*-

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

(require 'compile-angel)

(defgroup compile-premonition nil
  "Compile Emacs Lisp libraries automatically."
  :group 'compile-premonition
  :prefix "compile-premonition-")

(defcustom compile-premonition-verbose nil
  "Enable displaying messages (e.g., when files are compiled).
When set to non-nil, this option will cause messages to be shown during the
compilation process, providing feedback on the compilation status."
  :type 'boolean
  :group 'compile-premonition)

(defcustom compile-premonition-debug nil
  "Non-nil to display debug messages in the *compile-premonition:debug* buffer.
This displays a lot of messages."
  :type 'boolean
  :group 'compile-premonition)

(defvar compile-premonition-on-load-advise-eval-after-load nil
  "When non-nil, compile .el files before `eval-after-load'.")

(defvar compile-premonition-on-load-advise-autoload nil
  "When non-nil, automatically compile .el files loaded using `autoload'.")

(defmacro compile-premonition--debug-message (&rest args)
  "Display a debug message with the same ARGS arguments as `message'.
The messages are displayed in the *compile-premonition* buffer."
  (declare (indent 0) (debug t))
  `(when compile-premonition-debug
     (compile-premonition--insert-message "*compile-angel:debug*"
                                          ,(car args) ,@(cdr args))))

(defmacro compile-premonition--verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  (declare (indent 0) (debug t))
  `(progn
     (when compile-premonition-debug
       (compile-premonition--debug-message ,(car args) ,@(cdr args)))
     (when compile-premonition-verbose
       (message (concat "[compile-premonition] " ,(car args)) ,@(cdr args)))))

(defun compile-premonition--advice-before-autoload (_function
                                                    feature
                                                    &optional _docstring _interactive
                                                    _type)
  "Recompile before `autoload'. FEATURE is the file or the feature."
  (when compile-premonition-debug (compile-premonition--debug-message
                                   "AUTOLOAD: %s (%s)" feature (type-of feature)))
  (compile-angel--entry-point nil feature))

(defun compile-premonition--advice-eval-after-load (feature-or-file _form)
  "Advice to track what FEATURE-OR-FILE (symbol) is passed to `eval-after-load'."
  (compile-premonition--debug-message "EVAL-AFTER-LOAD: %s (%s)"
                                      feature-or-file (type-of feature-or-file))
  (compile-angel--entry-point feature-or-file feature-or-file))

;;;###autoload
(define-minor-mode compile-premonition-on-load-mode
  "Toggle `compile-premonition-mode'."
  :global t
  :lighter " CAngelP"
  :group 'compile-premonition
  (if compile-premonition-on-load-mode
      (progn
        (compile-angel--init)
        (compile-angel--entry-point nil "compile-premonition")
        (when compile-premonition-on-load-advise-autoload
          (advice-add 'autoload :before #'compile-premonition--advice-before-autoload))
        (when compile-premonition-on-load-advise-eval-after-load
          (advice-add
           'eval-after-load :before #'compile-premonition--advice-eval-after-load)))
    (advice-remove 'autoload #'compile-premonition--advice-before-autoload)
    (advice-remove 'eval-after-load #'compile-premonition--advice-eval-after-load)))

(provide 'compile-premonition)

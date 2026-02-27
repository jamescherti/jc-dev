;;; lazy-loader.el --- Lazy loader -*- lexical-binding: t -*-

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
;; Lazy loader.

;;; Code:

;;; Defcustom and Variables

(defgroup lazy-loader nil
  "Lazy loader."
  :group 'convenience
  :prefix "lazy-loader-")

(defcustom lazy-loader-modules nil
  "Modules to load when Emacs is unfocused or idle."
  :type '(repeat symbol)
  :group 'lazy-loader)

(defcustom lazy-loader-files nil
  "Files to load when Emacs is unfocused or idle."
  :type '(repeat string)
  :group 'lazy-loader)

(defcustom lazy-loader-buffers nil
  "Files to load when Emacs is unfocused or idle."
  :type '(repeat string)
  :group 'lazy-loader)

(defcustom lazy-loader-buffers nil
  "Alist of buffers to load when Emacs is unfocused or idle.
Each entry should be a cons cell (BUFFER-NAME . INIT-FUNC),
where BUFFER-NAME is a string and INIT-FUNC is a function
called to initialize the buffer."
  :type '(repeat (cons (string :tag "Buffer name")
                       (function :tag "Initializer function")))
  :group 'lazy-loader)

(defcustom lazy-loader-idle-delay 5
  "Number of seconds of idle time before loading modules and files."
  :type 'number
  :group 'lazy-loader)

(defcustom lazy-loader-verbose nil
  "Enable displaying verbose messages."
  :type 'boolean
  :group 'lazy-loader)

;; Functions

(defmacro lazy-loader--verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  (declare (indent 0) (debug t))
  `(progn
     (when lazy-loader-verbose
       (message (concat "[lazy-loader] " ,(car args)) ,@(cdr args)))))

(defun lazy-loader--load ()
  "Load modules and files."
  ;; Load modules
  (dolist (module lazy-loader-modules)
    (unless (featurep module)
      (lazy-loader--verbose-message "Load module: %s" module)
      (require module)))

  ;; Load files
  (dolist (file lazy-loader-files)
    (when (and (file-regular-p file)
               (file-readable-p file))
      (lazy-loader--verbose-message "Load file: %s" file)
      (find-file-noselect file)))

  ;; Load all buffers listed in `lazy-loader-buffers' by calling their
  ;; initializer functions.
  (dolist (entry lazy-loader-buffers)
    (let ((buf-name (car entry))
          (init-func (cdr entry)))
      (unless (get-buffer buf-name)
        (funcall init-func))))

  ;; Disable hooks and timers
  (lazy-loader-disable-focus-hook)
  (lazy-loader--stop-idle-timer))

;;; Focus change

(defun lazy-loader--on-focus-change ()
  "Load modules and files when Emacs loses focus."
  (when (not (frame-focus-state))
    (lazy-loader--load)))

(defun lazy-loader-disable-focus-hook ()
  "Disable lazy loading triggered by Emacs losing or gaining focus.
This prevents modules and files from being loaded automatically when the frame
focus state changes."
  (remove-function after-focus-change-function
                   #'lazy-loader--on-focus-change))

;;; Idle

(defvar lazy-loader--idle-timer nil
  "Timer used to load modules and files when Emacs is idle.")

(defun lazy-loader--start-idle-timer ()
  "Start the idle timer for lazy loading."
  (setq lazy-loader--idle-timer
        (run-with-idle-timer lazy-loader-idle-delay t
                             #'lazy-loader--load)))

(defun lazy-loader--stop-idle-timer ()
  "Stop the idle timer for lazy loading."
  (when lazy-loader--idle-timer
    (cancel-timer lazy-loader--idle-timer)
    (setq lazy-loader--idle-timer nil)))

;;; Mode

;;;###autoload
(define-minor-mode lazy-loader-mode
  "Global minor mode to load modules when Emacs loses focus."
  :global t
  :group 'lazy-loader
  (if lazy-loader-mode
      (progn
        (lazy-loader--start-idle-timer)
        (when (display-graphic-p)
          (add-function :after after-focus-change-function
                        #'lazy-loader--on-focus-change)))
    (lazy-loader--stop-idle-timer)
    (lazy-loader-disable-focus-hook)))

;;; Provide

(provide 'lazy-loader)

;;; lazy-loader.el ends here

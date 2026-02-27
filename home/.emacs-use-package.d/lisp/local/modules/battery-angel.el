;;; battery-angel.el --- Monitor battery charging state changes -*- lexical-binding: t -*-

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
;; This package provides a global minor mode to monitor changes in battery
;; charging state, triggering hooks on AC or battery transitions, with optional
;; verbose logging.

;;; Code:

(require 'battery)

;;; Defcustom and Variables

(defgroup battery-angel nil
  "Battery state monitoring and hooks on charging state changes."
  :group 'convenience
  :prefix "battery-angel-")

(defcustom battery-angel-on-ac-hook nil
  "Hooks run when the battery is plugged into AC power."
  :type 'hook
  :group 'battery-angel)

(defcustom battery-angel-on-bat-hook nil
  "Hooks run when running on battery power."
  :type 'hook
  :group 'battery-angel)

(defcustom battery-angel-interval 10
  "Seconds after which the battery status will be updated."
  :type 'number
  :group 'battery-angel)

(defcustom battery-angel-verbose nil
  "If non-nil, display messages when the battery charging state changes."
  :type 'boolean
  :group 'battery-angel)

(defvar battery-angel--charging-state nil
  "Internal variable holding the last known charging state.")

(defvar battery-angel--update-timer nil
  "Timer object for periodic battery state checking.")

(defvar battery-angel-fallback-charging-state "BAT"
  "Default charging state used when the actual state cannot be determined.
Typically represents the battery (discharging) state.")

;;; Internal macros and functions

(defmacro battery-angel--verbose-message (&rest args)
  "Display a verbose message with ARGS if `battery-angel-verbose' is non-nil."
  (declare (indent 0) (debug t))
  `(when battery-angel-verbose
     (message (concat "[battery-angel] " ,(car args)) ,@(cdr args))))

(defun battery-angel-get-charging-state ()
  "Return the current charging state as a string: \"AC\" or \"BAT\".
If the state cannot be determined,
return `battery-angel-fallback-charging-state'."
  (let ((battery-data (funcall battery-status-function)))
    (if battery-data
        (let* ((raw-status (battery-format "%L" battery-data))
               (ac-status (and (stringp raw-status)
                               (downcase raw-status))))
          (cond
           ((or (and ac-status (string-prefix-p "on-line" ac-status))
                (and ac-status (string= "ac" ac-status)))
            "AC")

           ((and ac-status (or (string-prefix-p "off-line" ac-status)
                               (string= "battery" ac-status)
                               (string= "bat" ac-status)))
            "BAT")

           (t
            (battery-angel--verbose-message
              "Warning: Unsupported ac-status: %s" ac-status)
            battery-angel-fallback-charging-state)))
      (battery-angel--verbose-message
        "Warning: battery-status-function returned: %s" battery-data)
      battery-angel-fallback-charging-state)))

(defvar battery-angel--upower-signals nil
  "Handles for UPower signal subscriptions.")

(defun battery-angel--update (&optional force)
  "Update the cached charging state and run relevant hooks on change.

This function checks the current battery charging state and compares it with the
last known state.

If the charging state has changed, or if FORCE is non-nil, the appropriate hooks
are executed:

- `battery-angel-on-ac-hook' is run when the state changes to AC power.
- `battery-angel-on-bat-hook' is run when the state changes to battery.

Additionally, verbose messages are logged via `battery-angel--verbose-message'
to indicate the initial state or any subsequent state changes."
  (let ((previous-charging-state battery-angel--charging-state)
        (current-charging-state (battery-angel-get-charging-state))
        changed)
    (when current-charging-state
      (setq battery-angel--charging-state current-charging-state)
      (cond
       (force
        (setq changed t)
        (battery-angel--verbose-message
          "FORCE: Run hooks for: %s" current-charging-state))

       ((not previous-charging-state)
        (setq changed t)
        (battery-angel--verbose-message
          "Initial charging state: %s" current-charging-state))

       ((string= previous-charging-state
                 current-charging-state)
        ;; TODO make this a debug message
        ;; (battery-angel--verbose-message
        ;;   "IGNORED: Charging state hasn't been changed (%s)"
        ;;   current-charging-state)
        t)

       (t
        (setq changed t)
        (battery-angel--verbose-message
          "Charging state changed to: %s" current-charging-state)))

      (when changed
        (if (string= current-charging-state "AC")
            (run-hooks 'battery-angel-on-ac-hook)
          (run-hooks 'battery-angel-on-bat-hook))))))

;;; upower

(defun battery-angel--upower-signal-handler (&rest _)
  "Update battery status on receiving a UPower D-Bus signal."
  (when (and battery-angel--update-timer
             (timerp battery-angel--update-timer))
    (timer-event-handler battery-angel--update-timer)))

;; TODO docstring
(defun battery-angel--upower-props-changed (_interface changed _invalidated)
  "Update status when system boots or stops running on battery.
CHANGED is TODO.
Intended as a UPower PropertiesChanged signal handler."
  (when (assoc "OnBattery" changed)
    (battery-angel--upower-signal-handler)))

(defun battery-angel--upower-unsubscribe ()
  "Unsubscribe from UPower device change signals."
  (mapc #'dbus-unregister-object battery-angel--upower-signals)
  (setq battery-angel--upower-signals ()))

(defun battery-angel--upower-subscribe ()
  "Subscribe to UPower device change signals."
  (push (dbus-register-signal :system battery-upower-service
                              battery-upower-path
                              dbus-interface-properties
                              "PropertiesChanged"
                              #'battery-angel--upower-props-changed)
        battery-angel--upower-signals)
  (dolist (method '("DeviceAdded" "DeviceRemoved"))
    (push (dbus-register-signal :system battery-upower-service
                                battery-upower-path
                                battery-upower-interface
                                method #'battery-angel--upower-signal-handler)
          battery-angel--upower-signals)))

;;; Mode

;;;###autoload
(define-minor-mode battery-angel-mode
  "Global minor mode to watch for battery charging state changes.
Runs `battery-angel-on-ac-hook' when plugged into AC power, and
`battery-angel-on-bat-hook' when running on battery."
  :global t
  :lighter " battery-angel"
  :group 'battery-angel
  (progn
    (when battery-angel--update-timer
      (cancel-timer battery-angel--update-timer)
      (setq battery-angel--update-timer nil))
    (battery-angel--upower-unsubscribe)

    (when battery-angel-mode
      ;; Mode enabled
      (when (and (eq battery-status-function #'battery-upower)
                 battery-upower-subscribe)
        (battery-angel--upower-subscribe))

      (battery-angel--update)

      (setq battery-angel--update-timer
            (run-with-timer battery-angel-interval
                            battery-angel-interval
                            #'battery-angel--update)))))

(provide 'battery-angel)
;;; battery-angel.el ends here

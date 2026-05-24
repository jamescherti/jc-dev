;;; mod-cleanup.el --- cleanup -*- lexical-binding: t -*-

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

;; Cleanup.

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

;;; Cleanup toggles

(defvar mod-cleanup-enable-native-compile t
  "Enable native compilation cache cleanup.")

(defvar mod-cleanup-projects t
  "Enable zombie projects cleanup.")

(defvar mod-cleanup-recentf t
  "Enable recentf file cleanup.")

(defvar mod-cleanup-tramp t
  "Enable Tramp connections cleanup.")

(defvar mod-cleanup-packages (eq lightemacs-package-manager 'builtin-package)
  "Enable unused packages cleanup.")

(defvar mod-cleanup-packages-list '(olivetti
                                    ws-butler
                                    spinner
                                    git-gutter
                                    posframe
                                    popper
                                    rainbow-mode
                                    with-editor
                                    lsp-mode
                                    git-timemachine
                                    focus
                                    shut-up
                                    llama
                                    lv
                                    jenkinsfile-mode
                                    basic-mode
                                    highlight-numbers
                                    ;; TODO remove on mac
                                    exec-path-from-shell
                                    groovy-mode
                                    ;; php-mode
                                    erefactor
                                    parent-mode
                                    popup
                                    xclip
                                    tempel
                                    tempel-collection
                                    stillness-mode
                                    golden-ratio
                                    tabgo
                                    ws-butler
                                    quickrun
                                    xterm-color
                                    ztree
                                    vundo
                                    flyspell-lazy
                                    eat
                                    magit
                                    magit-section
                                    ace-window)
  "Packages to delete.")

;; 5 minutes: This is the standard definition of "Away From Keyboard." If you
;; haven't touched Emacs for 5 minutes, you have likely stepped away (coffee,
;; meeting, etc.). This is the safest time to burn CPU cycles or disk I/O
;; without affecting user experience.
(defvar mod-cleanup-idle-delay (* 5 60)
  "Number of seconds of idle time before running cleanups.")

(defvar mod-cleanup-gc-threshold (* 256 1024 1024)
  "GC threshold to use during compilation.")

(defvar mod-cleanup-gc-percentage 0.5
  "GC percentage to use during compilation.")

;;; Internal variables

(defvar mod-cleanup--idle-timer nil
  "Timer object for periodic project cleanup and other idle tasks.")

(defvar mod-cleanup--native-compile-prune-done nil
  "Flag to ensure native compile cache is pruned only once per session.")

;;; Unified cleanup function

(defun mod-cleanup-perform-all (&optional on-exit)
  "Perform all enabled cleanup operations.

When ON-EXIT is non-nil, run exit-specific cleanup tasks (such as tearing down
active Tramp connections) and skip the final trailing garbage collection pass to
maximize shutdown speed.

Native compilation cache pruning is safely throttled to execute only once per
Emacs session to prevent unnecessary disk I/O."
  (interactive)
  (let ((inhibit-message t)
        (gc-cons-threshold (max gc-cons-threshold mod-cleanup-gc-threshold))
        (gc-cons-percentage (max gc-cons-percentage mod-cleanup-gc-percentage)))
    ;; project cleanup
    (when mod-cleanup-projects
      (when (and (featurep 'project)
                 (fboundp 'project-forget-zombie-projects))
        (with-demoted-errors "Error pruning zombie projects: %S"
          (project-forget-zombie-projects))))

    ;; Delete unused packages
    (when (and (featurep 'package)
               mod-cleanup-packages)
      (require 'package)
      (when (fboundp 'package-delete)
        (dolist (item mod-cleanup-packages-list)
          (let ((desc (cadr (assq item package-alist))))
            (if (not desc)
                (when init-file-debug
                  (message "Package %s not found in alist (already deleted?)"
                           item))
              (condition-case err
                  (progn
                    (package-delete desc)
                    (message "Successfully deleted: %s" item))
                (error
                 ;; This captures the actual error message from Emacs
                 (message "Failed to delete %s: %s: %s"
                          item
                          (error-message-string err)
                          desc))))))))

    ;; Recentf cleanup
    (when mod-cleanup-recentf
      (when (and (featurep 'recentf)
                 (fboundp 'recentf-cleanup))
        (recentf-cleanup)))

    ;; Tramp cleanup
    ;; Clean up Tramp connections to avoid hanging Emacs on exit.
    ;;
    ;; Here is the difference between tramp-cleanup-all-connections and the
    ;; kill-emacs-hook's tramp-dump-connection-properties:
    ;; - tramp-dump-connection-properties: This saves information about your
    ;;   connections (like hardware properties, paths, and verified settings)
    ;;   into your persistent file cache (usually ~/.emacs.d/tramp) so that
    ;;   subsequent Emacs sessions can connect to those machines faster. It does
    ;;   not close active network connections.
    ;; - tramp-cleanup-all-connections: This explicitly flushes all cached
    ;;   background processes, tears down existing SSH/SCP tunnels, closes
    ;;   socket connections, and clears internal buffers.
    (when (and mod-cleanup-tramp on-exit)
      (when (and (featurep 'tramp)
                 (fboundp 'tramp-cleanup-all-connections))
        (tramp-cleanup-all-connections)))

    ;; Prune native compilation cache
    (when (and mod-cleanup-enable-native-compile
               (not mod-cleanup--native-compile-prune-done))
      (when (and (featurep 'native-compile)
                 (fboundp 'native-comp-available-p)
                 (native-comp-available-p)
                 (fboundp 'native-compile-prune-cache))
        (setq mod-cleanup--native-compile-prune-done t)
        (message "[native-comp] Native compilation cache pruned")
        (with-demoted-errors "Error pruning native cache: %S"
          (native-compile-prune-cache)))))

  (unless on-exit
    (garbage-collect)))

(defun mod-cleanup--on-exit ()
  "Wrapper to run cleanups specifically for exit hooks."
  (mod-cleanup-perform-all t))

;;; Global minor mode

(define-minor-mode mod-cleanup-mode
  "Global minor mode to manage idle and exit cleanup operations."
  :global t
  :group 'lazy-loader
  (if mod-cleanup-mode
      (progn
        ;; Set the timer and store it in the variable
        (when mod-cleanup--idle-timer
          (cancel-timer mod-cleanup--idle-timer)
          (setq mod-cleanup--idle-timer nil))

        (setq mod-cleanup--idle-timer
              (run-with-idle-timer mod-cleanup-idle-delay t
                                   #'mod-cleanup-perform-all))

        (add-hook 'kill-emacs-hook #'mod-cleanup--on-exit 80))
    ;; Disable mode: clear timers and hooks
    (when mod-cleanup--idle-timer
      (cancel-timer mod-cleanup--idle-timer)
      (setq mod-cleanup--idle-timer nil))

    (remove-hook 'kill-emacs-hook #'mod-cleanup--on-exit)))

(add-hook 'lightemacs-after-init-hook #'mod-cleanup-mode)

;;; Provide

(provide 'mod-cleanup)
;;; mod-cleanup.el ends here

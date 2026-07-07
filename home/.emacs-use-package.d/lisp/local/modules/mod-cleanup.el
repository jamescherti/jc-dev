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

(defgroup mod-cleanup nil
  "Customization options for `mod-cleanup-mode'."
  :group 'mod-cleanup
  :prefix "mod-cleanup-")

;;; Cleanup toggles

(defvar mod-cleanup-enable-native-compile t
  "Enable native compilation cache cleanup.")

(defvar mod-cleanup-projects t
  "Enable zombie projects cleanup.")

(defvar mod-cleanup-saveplace t
  "Enable saveplace cleanup.")

(defvar mod-cleanup-url-cache t
  "Enable automated purging of stale URL cache files.")

(defvar mod-cleanup-auth-source-cache t
  "Enable aggressive clearing of decrypted auth-source memory cache.")

;; (defvar mod-cleanup-projectile t
;;   "Enable Projectile known projects tracking cleanup.")

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
                                    git-gutter-fringe

                                    avy
                                    ace-window
                                    expand-region

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
                                    savefold
                                    eat
                                    magit
                                    magit-section)
  "Packages to delete.")

;; 5 minutes: This is the standard definition of "Away From Keyboard." If you
;; haven't touched Emacs for 5 minutes, you have likely stepped away (coffee,
;; meeting, etc.). This is the safest time to burn CPU cycles or disk I/O
;; without affecting user experience.
(defvar mod-cleanup-idle-delay (* 5 60)
  "Number of seconds of idle time before running cleanups.")

(defvar mod-cleanup-gc-threshold (* 256 1024 1024)
  "GC threshold to use.")

(defvar mod-cleanup-gc-percentage 0.5
  "GC percentage to use.")

;;; Internal variables

(defvar mod-cleanup--idle-timer nil
  "Timer object for periodic project cleanup and other idle tasks.")

(defvar mod-cleanup--native-compile-prune-done nil
  "Flag to ensure native compile cache is pruned only once per session.")

;;; Unified cleanup function

(defvar package-alist)

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
                  ;; TODO use a mod-cleanup message function (verbose)
                  (message "Package %s not found in alist (already deleted?)"
                           item))
              (condition-case err
                  (progn
                    (package-delete desc)
                    ;; TODO use a mod-cleanup message function (verbose)
                    (message "Successfully deleted: %s" item))
                (error
                 ;; This captures the actual error message from Emacs
                 ;; TODO use a mod-cleanup message function (verbose)
                 (message "Failed to delete %s: %s: %s"
                          item
                          (error-message-string err)
                          desc))))))))

    ;; Recentf cleanup
    (when mod-cleanup-recentf
      (when (and (featurep 'recentf)
                 (fboundp 'recentf-cleanup))
        (with-demoted-errors "Error pruning recentf list: %S"
          (recentf-cleanup))))

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
        (with-demoted-errors "Error cleaning up Tramp connections: %S"
          (tramp-cleanup-all-connections))))

    ;; Remove non-existent files from save-place-alist
    (when (and mod-cleanup-saveplace
               (fboundp 'save-place-forget-unreadable-files))
      ;; Iterates through save-place-alist and purges cursor positions for
      ;; files that have been deleted, renamed, or are unreadable.
      (with-demoted-errors "Error pruning save-place data: %S"
        (save-place-forget-unreadable-files)))

    ;; Prune savehist minibuffer histories using built-in cons cell thresholds
    ;; (when mod-cleanup-savehist
    ;;   (when (and (featurep 'savehist)
    ;;              (boundp 'savehist-minibuffer-history-variables)
    ;;              savehist-minibuffer-history-variables)
    ;;     (require 'cl-lib)
    ;;     (let ((truncated-vars nil))
    ;;       ;; Format every tracked history symbol into a (VAR . MAX-SIZE) cons cell
    ;;       (dolist (var savehist-minibuffer-history-variables)
    ;;         (when (boundp var)
    ;;           (push (cons var mod-cleanup-savehist-limit) truncated-vars)))
    ;;
    ;;       ;; Merge with any existing custom additional variables, ensuring no duplicates
    ;;       (setq savehist-additional-variables
    ;;             (cl-union savehist-additional-variables truncated-vars :test #'equal)))))

    ;; Enforce hard limit constraints on historical tracking variables
    ;; (when mod-cleanup-histories
    ;;   (setq extended-command-history (seq-take extended-command-history 100)
    ;;         file-name-history (seq-take file-name-history 100)))

    ;; Remove dead paths from Projectile history
    ;; (when (and mod-cleanup-projectile
    ;;            (featurep 'projectile)
    ;;            (fboundp 'projectile-cleanup-known-projects))
    ;;   (projectile-cleanup-known-projects))

    ;; URL Cache pruning
    (when mod-cleanup-url-cache
      (when (and (featurep 'url-cache)
                 (fboundp 'url-cache-prune-cache))
        (with-demoted-errors "Error pruning web cache: %S"
          (url-cache-prune-cache))))

    ;; Auth-source security flush
    (when mod-cleanup-auth-source-cache
      (when (and (featurep 'auth-source)
                 (fboundp 'auth-source-forget-all-cached))
        ;; Clears internal variable caches storing decoded secrets, API keys,
        ;; and passwords read from ~/.authinfo.gpg.
        ;;
        ;; Good for security. Leaving decrypted keys in memory on an idle Emacs
        ;; instance for hours creates an unnecessary attack surface. Forcing an
        ;; absolute wipe when away from the keyboard means keys must be safely
        ;; re-authenticated upon return.
        (auth-source-forget-all-cached)))

    ;; Prune native compilation cache
    (when (and mod-cleanup-enable-native-compile
               (not mod-cleanup--native-compile-prune-done))
      (when (and (featurep 'native-compile)
                 (fboundp 'native-comp-available-p)
                 (native-comp-available-p)
                 (fboundp 'native-compile-prune-cache))
        (setq mod-cleanup--native-compile-prune-done t)
        ;; TODO use a mod-cleanup message function (verbose)
        (message "[native-comp] Native compilation cache pruned")
        (with-demoted-errors "Error pruning native compilation cache: %S"
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
  :group 'mod-cleanup
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
    (remove-hook 'kill-emacs-hook #'mod-cleanup--on-exit)
    (setq mod-cleanup--native-compile-prune-done nil)))

(add-hook 'lightemacs-after-init-hook #'mod-cleanup-mode)

;;; Provide

(provide 'mod-cleanup)
;;; mod-cleanup.el ends here

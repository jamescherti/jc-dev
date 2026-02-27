;;; mod-toggle-term.el --- mod-toggle-term -*- lexical-binding: t -*-

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

(require 'buffer-guardian)
(require 'my-defun)

(defvar toggle-term-tmux-session "emacs")
(defvar toggle-term-tmux-buffer-name "*tmux*")
(defvar toggle-term-tmux-shell "bash")

(defvar choice-terminal "vterm")

(when (string= choice-terminal "term")
  (require 'term)
  (defun toggle-term-term (command)
    "Toggle between the current buffer and the terminal.
COMMAND is the same arguments as `toggle-term'."
    (toggle-term command

                 ;; Create
                 #'(lambda(_command)
                     ;; (let ((tmux-buffer
                     ;;        (ansi-term command
                     ;;                   toggle-term-tmux-buffer-name)))
                     ;;   ;; (with-current-buffer tmux-buffer
                     ;;   ;;   (term-send-raw-string "bash")
                     ;;   ;;   (term-send-raw-string (concat command "\n")))
                     ;;   )
                     )

                 ;; Switch
                 #'(lambda()
                     (toggle-term-helper-switch-to-terminal)

                     ;; Fix issue that causes the cursor to move to the top-left
                     ;; of the screen
                     ;; (let ((term-buffer (get-buffer
                     ;;                     toggle-term-tmux-buffer-name)))
                     ;;   (when (buffer-live-p term-buffer)
                     ;;     (with-current-buffer toggle-term-tmux-buffer-name
                     ;;       )))
                     ))))

(when (string= choice-terminal "vterm")
  (defun toggle-term-vterm (command)
    "Toggle between the current buffer and the terminal.
COMMAND is the same arguments as `toggle-term'."
    (when (and (fboundp 'vterm)
               (fboundp 'vterm-send-string)
               (fboundp 'vterm-send-return))
      (toggle-term command
                   ;; Create
                   #'(lambda(command)
                       (let ((tmux-buffer (vterm toggle-term-tmux-buffer-name)))
                         (with-current-buffer tmux-buffer
                           (vterm-send-string command)
                           (vterm-send-string "\n")
                           (vterm-send-return))))

                   ;; Switch
                   #'(lambda()
                       (toggle-term-helper-switch-to-terminal)

                       ;; Fix issue that causes the cursor to move to the
                       ;; top-left of the screen
                       (let ((term-buffer (get-buffer
                                           toggle-term-tmux-buffer-name)))
                         (when (buffer-live-p term-buffer)
                           (with-current-buffer toggle-term-tmux-buffer-name
                             (when (fboundp 'vterm-reset-cursor-point)
                               (vterm-reset-cursor-point))))))))))

(when (string= choice-terminal "eat")
  ;; (require 'pkg-eat)
  (defun toggle-term-eat (command)
    "Toggle between the current buffer and the terminal.
COMMAND is the same arguments as `toggle-term'."
    (when (fboundp 'eat)
      (toggle-term command
                   ;; Create
                   #'(lambda(command)
                       (let ((tmux-buffer (eat command)))
                         (with-current-buffer tmux-buffer
                           (rename-buffer toggle-term-tmux-buffer-name))))

                   ;; Switch
                   #'(lambda()
                       (toggle-term-helper-switch-to-terminal))))))

(defvar toggle-term-switch-only nil
  "Internal variable.")

(defun toggle-term (command fn-create fn-switch)
  "Toggle terminal.
COMMAND is the command to execute.
FN-CREATE is the function that creates the terminal and switches to it.
FN-SWITCH is a function called to switch to the buffer."
  (let ((term-buf (get-buffer toggle-term-tmux-buffer-name)))
    (cond
     ;; Switch to the previous buffer because this is a terminal
     ((and term-buf
           (eq (current-buffer) term-buf))
      (when (not toggle-term-switch-only)
        (previous-buffer))
      )

     ;; Switch to the terminal
     (term-buf
      ;; (save-some-buffers t)
      (buffer-guardian-save-all-buffers)
      (funcall fn-switch)

      ;; Fix the default directory if it does not exist
      ;; TODO: Enhance this
      (when (not (file-exists-p default-directory))
        (setq default-directory (expand-file-name "~"))))

     ;; Create the terminal
     (t
      (funcall fn-create command)))))

(defun toggle-term-helper-switch-to-terminal ()
  "Switch to a terminal."
  (switch-to-buffer toggle-term-tmux-buffer-name nil t))

(defun toggle-term-gen-tmux-session-cmd (tmux-session-name cwd)
  "Generate a tmux command to start a session in the specified directory.

This function creates a command to change the working directory to CWD and
then start a tmux session with the name TMUX-SESSION-NAME. The resulting
command can be executed to initialize the tmux session.

Arguments:
  TMUX-SESSION-NAME: The name of the tmux session to start.
  CWD: The directory to which tmux should change before starting the session.

Returns: The tmux command to execute (string)."
  (concat "cd "
          (shell-quote-argument cwd)
          " && "
          "tmux-session -l "
          (shell-quote-argument tmux-session-name)))

(defun toggle-term-gen-tmux-run-cmd (command tmux-session-name cwd wait-for-key)
  "Generate a tmux command to run a given command in a tmux session.

This function creates a tmux command that changes the working directory to CWD,
and runs the specified COMMAND in the tmux session TMUX-SESSION-NAME. If
WAIT-FOR-KEY is non-nil, it prompts the user to press enter before finishing
execution.

Arguments:
  COMMAND: The command to run in the tmux session.
  TMUX-SESSION-NAME: The name of the tmux session to use.
  CWD: The directory to which tmux should change before running the command.
  WAIT-FOR-KEY: If non-nil, waits for the user to press enter before finishing.

Returns: The tmux command to execute (string)."
  (concat "cd " (shell-quote-argument cwd) " && "
          "TMUX_RUN_SESSION_NAME="
          (shell-quote-argument tmux-session-name)
          " "
          "tmux-run "
          "bash -c "
          (shell-quote-argument
           (concat
            "cd " (shell-quote-argument cwd) " && "
            command
            (if wait-for-key
                " ; echo; echo 'Press enter...'; read v"
              "")))))

(defun toggle-term-tmux ()
  "Toggle between the current buffer and the tmux terminal.
COMMAND are the same arguments as `toggle-term'.
WAIT-FOR-KEY asks the user to press enter before quitting."
  (interactive)
  (let* ((cwd (expand-file-name default-directory))
         (command (toggle-term-gen-tmux-session-cmd toggle-term-tmux-session
                                                    cwd)))
    (cond
     ((and (fboundp 'toggle-term-vterm)
           (string= choice-terminal "vterm"))
      (funcall 'toggle-term-vterm command))

     ((and (fboundp 'toggle-term-eat)
           (string= choice-terminal "eat"))
      (funcall 'toggle-term-eat command))

     ((and (fboundp 'toggle-term-term)
           (string= choice-terminal "term"))
      (funcall 'toggle-term-term command))

     (t
      (user-error "Terminal not supported: %s" choice-terminal)))))

(defun toggle-term-tmux-default-bash (&optional command wait-for-key)
  "Run a command in the tmux buffer.

This function runs a specified command in a tmux session, or defaults to the
value of `toggle-term-tmux-shell` if no command is provided. It also saves the
current working directory to `~/.bash_lastdir` and switches to the tmux
terminal. If WAIT-FOR-KEY is non-nil, it will wait for the user to press a key
before completing the execution.

Arguments:
  COMMAND: The command to run in the tmux session (optional).
  WAIT-FOR-KEY: If non-nil, waits for the user to press enter before finishing.

Returns: None. It initiates the execution of the tmux command."
  (interactive)
  (unless command
    (setq command toggle-term-tmux-shell))
  (let* ((toggle-term-switch-only t)
         (cwd (expand-file-name default-directory))
         (tmux-session-command
          (toggle-term-gen-tmux-run-cmd command
                                        toggle-term-tmux-session
                                        cwd
                                        wait-for-key)))
    ;; (message "RUN: %s" tmux-session-command)
    (toggle-term-tmux)
    (with-temp-file "~/.bash_lastdir"
      (insert cwd))
    (when (/= 0 (exec-shell-command tmux-session-command))
      (message "[toggle-term] Error with the command: %s"
               tmux-session-command))))

(defun tmux-reset (&optional switch-to-tmux)
  "Open bash in tmux.

This function kills all tmux windows except the current one, and optionally
switches to the tmux session if SWITCH-TO-TMUX is non-nil.

Arguments:
  SWITCH-TO-TMUX: If non-nil, switches to the tmux session after resetting.

Returns: None. It resets the tmux session and optionally switches to it."
  (interactive)
  (exec-shell-command "tmux kill-window -a")
  (when switch-to-tmux
    (let ((toggle-term-switch-only t))
      (toggle-term-tmux))))

;;; Git

;; Plugins
(defun ci ()
  "Git commit.

This function runs the `git-commitflow` command in the tmux session and
waits for the user to press a key before finishing execution."
  (interactive)
  (when (buffer-modified-p)
    ;; (let ((save-silently t))
    ;;   (save-buffer))
    (buffer-guardian-save-buffer))
  (toggle-term-tmux-default-bash "git-commitflow" :wait-for-key))

(defun cip ()
  "Git commit and push.

This function runs the `git cip` command in the tmux session and waits for
the user to press a key before finishing execution."
  (interactive)
  (when (buffer-modified-p)
    ;; (let ((save-silently t))
    ;;   (save-buffer))
    (buffer-guardian-save-buffer))
  (toggle-term-tmux-default-bash "git cip" :wait-for-key))

(defun gpl ()
  "Git pull.

This function runs the `gpl` command in the tmux session and waits for the
user to press a key before finishing execution."
  (interactive)
  (when (buffer-modified-p)
    ;; (let ((save-silently t))
    ;;   (save-buffer))
    (buffer-guardian-save-buffer))
  (toggle-term-tmux-default-bash
   (toggle-term-tmux-default-bash "gpl" :wait-for-key)))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "<leader>et") #'toggle-term-tmux-default-bash))

(provide 'mod-toggle-term)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-toggle-term.el ends here

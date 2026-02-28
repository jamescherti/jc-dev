;;; mod-better-grep.el --- Better Emacs grep -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(require 'grep)

(defun mod-better-grep (command-args)
  "Run Grep with user-specified COMMAND-ARGS.
The output from the command goes to the \"*grep*\" buffer.

While Grep runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<grep-mode-map>\\[compile-goto-error] in the *grep* \
buffer, to go to the lines where Grep found
matches.  To kill the Grep job before it finishes, type \\[kill-compilation].

Noninteractively, COMMAND-ARGS should specify the Grep command-line
arguments.

For doing a recursive `grep', see the `rgrep' command.  For running
Grep in a specific directory, see `lgrep'.

This command uses a special history list for its COMMAND-ARGS, so you
can easily repeat a grep command.

A prefix argument says to default the COMMAND-ARGS based on the current
tag the cursor is over, substituting it into the last Grep command
in the Grep command history (or into `grep-command' if that history
list is empty)."
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-shell-command
              "Grep: "
              ""
              'grep-history
              (if current-prefix-arg nil default))))))
  (let ((grep-arguments (concat (if current-prefix-arg
                                    default
                                  (if grep-command-position
                                      (cons grep-command grep-command-position)
                                    grep-command))
                                " "
                                command-args)))
    ;; If called non-interactively, also compute the defaults if we
    ;; haven't already.
    (when (eq grep-highlight-matches 'auto-detect)
      (grep-compute-defaults))
    (grep--save-buffers)
    ;; Setting process-setup-function makes exit-message-function work
    ;; even when async processes aren't supported.
    (compilation-start
     (if (and grep-use-null-device null-device (null-device))
         (concat grep-arguments " " (null-device))
       (concat  grep-arguments))
     #'grep-mode)))

(provide 'mod-better-grep)
;;; mod-better-grep.el ends here

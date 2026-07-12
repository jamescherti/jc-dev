;;; my-defun.el --- my-defun -*- lexical-binding: t -*-

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

(require 'tmpedit)

(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defun my-default-font ()
  "Display the default font."
  (interactive)
  (message "%s" (frame-parameter nil 'font)))

(defun save-all-new-file-buffers ()
  "Save all file-visiting buffers whose files do not exist on disk.
Prompts the operator for confirmation before creating directories and saving
each buffer."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (let ((file-path buffer-file-name))
        ;; Check if the buffer is visiting a file and that file does not exist
        (when (and file-path (not (file-exists-p file-path)))
          (if (y-or-n-p (format "File '%s' does not exist on disk. Save it? "
                                file-path))
              (let ((parent-dir (file-name-directory file-path))
                    (proceed-to-save t))
                (unless (file-exists-p parent-dir)
                  (if (y-or-n-p (format "Directory '%s' does not exist. Create it? "
                                        parent-dir))
                      (make-directory parent-dir t)
                    (setq proceed-to-save nil)
                    (message "Skipped saving '%s': missing parent directory." (buffer-name))))
                (when proceed-to-save
                  (let ((inhibit-message (not (bound-and-true-p buffer-guardian-verbose)))
                        (save-silently (not (bound-and-true-p buffer-guardian-verbose)))
                        (inhibit-interaction t))
                    (ignore inhibit-interaction)
                    (condition-case err
                        (save-buffer)
                      (inhibited-interaction
                       (message
                        (concat
                         "Error: 'save-buffer' attempted an "
                         "interactive prompt in buffer '%s'. It is expected to "
                         "be non-interactive.")
                        (buffer-name)))
                      (error
                       (when (bound-and-true-p buffer-guardian-verbose)
                         (message "Failed to save '%s': %s"
                                  (buffer-name)
                                  (error-message-string err))))))))))))))

(defun my-save-all-buffers ()
  "Save all buffers."
  (cond
   ((fboundp 'buffer-guardian-save-all-buffers)
    (buffer-guardian-save-all-buffers)
    (save-all-new-file-buffers))

   (t
    (save-some-buffers t))))

(defun my-project-root-dir (&optional path)
  "Search up the PATH for `project-root-markers'."
  (when (fboundp 'project-root)
    (when-let* ((project (project-current nil path))
                (project-root (when project
                                (project-root project))))
      (directory-file-name project-root))))

(defun my-tab-bar-switch-to-buffer (buffer)
  "Switch to the tab containing a window with the specified BUFFER."
  (let* ((target-tab (alist-get 'index (tab-bar-get-buffer-tab buffer t nil))))
    (when target-tab
      (tab-bar-select-tab (1+ target-tab))
      (pop-to-buffer buffer)
      t)))

(defun my-tab-bar-find-file (filename)
  "Switch to the tab and the window containing a file or directory named FILENAME.
If FILENAME is a directory, find the buffer of the `dired'. If the file or
directory is not open yet, open it in the current window."
  (let ((buffer (if (file-directory-p filename)
                    (dired-noselect filename)
                  (or (get-file-buffer filename)
                      (find-file-noselect filename)))))
    (when (fboundp 'my-tab-bar-switch-to-buffer)
      (when (and buffer (not (my-tab-bar-switch-to-buffer buffer)))
        (set-window-buffer nil buffer)))))

(defun my-code-checker-allowed-p (&optional file-name)
  "Return t if code checking is allowed for current buffer or specified file.

If FILE-NAME is provided and non-nil, use it as the filename. Otherwise, use the
buffer's associated file name.

Returns: boolean: t if code checking is allowed, nil otherwise."
  (if (not (boundp 'config-buffer-enable-syntax-checkers))
      (let* ((buffer (or (and (fboundp 'org-src-edit-buffer-p)
                              (fboundp 'org-src-source-buffer)
                              (org-src-edit-buffer-p)
                              (when-let* ((new-buffer (org-src-source-buffer)))
                                (when (buffer-live-p new-buffer)
                                  (with-current-buffer new-buffer
                                    (current-buffer)))))
                         ;; TODO
                         ;; (bound-and-true-p edit-indirect--overlay)
                         (buffer-base-buffer)
                         (current-buffer)))
             (file-name (if file-name
                            file-name
                          (buffer-file-name buffer)))
             (base-name (when file-name
                          (file-name-nondirectory file-name))))
        (when (or (and file-name
                       base-name
                       ;; (not (string-match-p "cookiecutter" file-name))
                       (file-in-directory-p file-name "~/src")
                       (not (file-in-directory-p file-name "~/src/forks"))
                       (not (file-in-directory-p file-name "~/src/local/emacs-worktrees"))
                       (not (file-in-directory-p file-name "~/src/other"))
                       (not (file-in-directory-p file-name tmpedit-dir))))
          (setq-local config-buffer-enable-syntax-checkers t)

          (if (and (not (string= base-name "/make.conf")) ; Gentoo
                   (not (string-suffix-p "/PKGBUILD" file-name))
                   (not (string-suffix-p ".ebuild" file-name)))
              (progn
                (setq-local my-buffer-enable-apheleia t)
                (setq-local my-buffer-enable-flymake t))
            (setq-local my-buffer-enable-apheleia nil)
            (setq-local my-buffer-enable-flymake nil))
          t))
    (when (boundp 'config-buffer-enable-syntax-checkers)
      config-buffer-enable-syntax-checkers)))

(defun my-disable-fringe-truncation-arrow ()
  "Disable the truncation arrow."
  (unless (boundp 'fringe-indicator-alist)
    (error "The fringe-indicator-alist was not declared"))
  (setq fringe-indicator-alist
        (seq-remove (lambda (item)
                      (memq (car item) '(truncation continuation)))
                    fringe-indicator-alist))
  (push '(continuation nil nil) fringe-indicator-alist)
  (push '(truncation nil nil) fringe-indicator-alist))

(defun my-treesit-language-available-p (language)
  "Check if `treesit-ready-p' exists.
LANGUAGE is the programming language."
  ;; During byte-compilation macro expansion, avoid installing legacy
  ;; non tree sitter libraries
  (cond
   ;; ((or (bound-and-true-p byte-compile-current-file)
   ;;      noninteractive)
   ;;  t)

   (t
    (require 'treesit nil t)
    (when (fboundp 'treesit-ready-p)
      (treesit-ready-p language t)))))

;; https://emacs.stackexchange.com/questions/35936/disassembly-of-a-bytecode-file
(defun disassemble-file (filename)
  "Disassemble the FILENAME Elisp file."
  (require 'cl-print)
  (require 'disass)
  (when (fboundp 'disassemble-1)
    (let ((inbuf (find-file-noselect filename)))
      (with-current-buffer inbuf
        (goto-char (point-min)))
      (with-current-buffer (get-buffer-create "*file disassembly*")
        (erase-buffer)
        (with-no-warnings  ; cl-print-compiled
          (condition-case ()
              (cl-loop with cl-print-compiled = 'disassemble
                       for expr = (read inbuf)
                       do (pcase expr
                            (`(byte-code ,(pred stringp) ,(pred vectorp) ,(pred natnump))
                             (princ "TOP-LEVEL byte code:\n" (current-buffer))
                             (disassemble-1 expr 0))
                            (_ (cl-prin1 expr (current-buffer))))
                       do (terpri (current-buffer)))
            (end-of-file nil)))
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(defun bufferfile--get-dired-file ()
  "Get the currently selected `dired' file."
  (when (fboundp 'dired-get-file-for-visit)
    (when-let* ((file (condition-case nil
                          (dired-get-file-for-visit)
                        (error nil))))
      file)))

(defun my-tab-bar-move-tab (&optional arg)
  "Move the current tab ARG positions to the right without cycling."
  (interactive "p")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (count (length tabs))
         (index (tab-bar--current-tab-index))
         (arg (or arg 1))
         (new-index (+ index arg)))
    (when (and (< new-index count) (> new-index -1))
      (tab-bar-move-tab arg))))

(defun my-tab-bar-move-tab-backward (&optional arg)
  "Move the current tab ARG positions to the left without cycling."
  (interactive "p")
  (my-tab-bar-move-tab (- (or arg 1))))

(defun my-tab-next (&optional arg)
  "Switch to the next tab ARG positions to the right without cycling."
  (interactive "p")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (count (length tabs))
         (index (tab-bar--current-tab-index))
         (arg (or arg 1))
         (new-index (+ index arg)))
    (when (< new-index count)
      (tab-bar-select-tab (+ 1 new-index)))))

(defun my-tab-previous (&optional arg)
  "Switch to the previous tab ARG positions to the left without cycling."
  (interactive "p")
  (my-tab-next (- (or arg 1))))

;; (defun my-woman (&optional topic)
;;   "Prompt for a man page name and display it using the woman package.
;; If TOPIC is provided, use that as the default."
;;   (interactive "MMan page name: ")
;;   (woman topic))

(defun remove-dash-lines (beg end)
  "Remove all lines made entirely of '-' characters in region from BEG to END.
Also removes the newline after such lines if present."
  (interactive "r")
  (save-excursion
    ;; Create a marker that moves with edits
    (let ((end-marker (copy-marker end)))
      (goto-char beg)

      ;; Use the marker instead of the integer 'end'
      (while (re-search-forward "^-+[\t ]*\\(\n\\)?\\(\n\\)?$" end-marker t)
        (replace-match "" nil nil))

      ;; Good practice: free the marker when done
      (set-marker end-marker nil))))

(defvar my-markdown-to-org-remove-headers t
  "Remove headers when converting Markdown to org.")

(defun my-markdown-to-org (beg end)
  "Convert the selected Markdown text to Org format using Pandoc.
BEG and END are the beginning and end of the selection.
The Lua filter replaces headers with bold text."
  (interactive "r")
  (save-excursion
    (let* ((tmp-lua (make-temp-file "header-to-bold" nil ".lua"))
           (lua-code-no-headers "-- Intercept every Header element
function Header (header)

  -- Replace the Header with a Paragraph containing the content wrapped in
  -- Strong (bold).
  return pandoc.Para({pandoc.Strong(header.content)})
end
")

           (lua-code-headers "-- This function intercepts every Header element
function Header (header)
  -- Replace the Header with a Paragraph containing the content wrapped
  -- in Strong (bold).
  -- pandoc.Para expects a list of inline elements.
  -- pandoc.Strong takes the original header content (list of inlines).
  return pandoc.Para({pandoc.Strong(header.content)})
end"))
      (unwind-protect
          (progn
            (with-temp-file tmp-lua
              (if my-markdown-to-org-remove-headers
                  (insert lua-code-no-headers)
                (insert lua-code-headers)))
            (shell-command-on-region
             beg end
             (format "pandoc -f markdown -t org --lua-filter=%s"
                     (shell-quote-argument tmp-lua))
             t t)
            ;; shell-command-on-region with REPLACE=t leaves the point at the
            ;; end of the inserted text. Since we inserted at BEG, the new
            ;; valid range is from BEG to (point).
            (remove-dash-lines beg (point)))
        (delete-file tmp-lua)))))

(defun my-markdown-to-org-buffer ()
  "Convert this buffer from Markdown to Org."
  (interactive)
  (my-markdown-to-org (point-min) (point-max)))

(defun my-auto-scroll-message-advice (&rest _)
  "Go to point-max after a message in *Messages* buffer."
  (walk-windows
   (lambda (window)
     (let ((window-buffer (window-buffer window)))
       ;; Only adjust point for windows showing the *Messages* buffer
       (when window-buffer
         (with-current-buffer window-buffer
           (when (string= (buffer-name window-buffer) "*Messages*")
             (goto-char (point-max))
             (goto-char (line-beginning-position))
             (set-window-point window (point-max)))))))
   ;; Exclude the minibuffer
   nil
   ;; Apply to all frames
   t))

(defun my-toggle-auto-scroll-messages-buffer ()
  "Toggle automatic scrolling of the *Messages* buffer."
  (interactive)
  (if (advice-member-p #'my-auto-scroll-message-advice 'message)
      (progn
        (advice-remove 'message #'my-auto-scroll-message-advice)
        (message "Auto-scrolling of *Messages* buffer disabled"))
    (advice-add 'message :after #'my-auto-scroll-message-advice)
    (message "Auto-scrolling of *Messages* buffer enabled")))

(defun my-shell-command-first-line-to-string (command)
  "Return the first line of output from executing COMMAND.
If the command fails or produces no output, return nil.

This function is superior to `shell-command-to-string` because it allows for the
detection of command execution failures by examining the exit status. While
`shell-command-to-string` only returns the output as a string, it does not
provide information about whether the command succeeded or failed. By using
`call-process-shell-command`, this function can capture both the output and the
exit status, enabling better error handling and more robust command execution."
  ;; Generate a unique output buffer
  (let ((output-buffer (generate-new-buffer
                        "*my-shell-command-first-line-to-string*")))
    (unwind-protect
        (progn
          (let ((exit-status (call-process-shell-command command
                                                         nil
                                                         output-buffer)))
            (when (eq exit-status 0)
              (with-current-buffer output-buffer
                (goto-char (point-min))
                ;; Get the first line as plain text without properties
                (let ((first-line (buffer-substring-no-properties
                                   (point)
                                   (line-end-position))))
                  (when (not (string-empty-p first-line))
                    (string-trim-right first-line "\n")))))))
      ;; Ensure the output buffer is killed after use
      (kill-buffer output-buffer))))

;;---------------------------------------------------------------
;; External diff tools
;;---------------------------------------------------------------
(defun my-start-process-with-files (command &rest args)
  "Prompt for two files and execute COMMAND with ARGS.
This function uses the selected files as arguments."
  (unless (executable-find command)
    (error "The command '%s' does not exist" command))
  (let ((file1 (read-file-name "Select the first file: "))
        (file2 (read-file-name "Select the second file: ")))

    (when (not (file-exists-p file1))
      (error "The first file does not exist: %s" file1))
    (when (not (file-exists-p file2))
      (error "The second file does not exist: %s" file2))

    (let ((full-command (append (list command)
                                args (list file1 file2))))
      (let ((inhibit-message t))
        (apply 'start-process command nil full-command)))))

(defun gvim-diff-files ()
  "Diff files with gvim."
  (interactive)
  (my-start-process-with-files "gvim" "-d"))

(defun meld-diff-files ()
  "Diff files with meld."
  (interactive)
  ;; Try to exec: /Applications/Meld.app/Contents/MacOS/Meld
  (if (and (eq system-type 'darwin)
           (not (executable-find "meld"))
           (executable-find "/Applications/Meld.app"))
      (my-start-process-with-files
       "open" "-a" "/Applications/Meld.app" "--args")
    (my-start-process-with-files "meld")))

;;---------------------------------------------------------------
;; save
;;---------------------------------------------------------------
(defun buffer-cwd ()
  "Return the directory of the current buffer."
  (interactive)
  (or (my-dir-config--buffer-cwd) default-directory))

;;-----------------------------------------------------------------------------
;; Commands
;;-----------------------------------------------------------------------------
(defun my-dir-config--buffer-cwd ()
  "Return the directory associated with the current buffer.
Returns:
- The directory path if the buffer is in `dired-mode', or
- The directory of the file if the buffer is visiting a file, or
- nil if neither condition is met."
  (let ((file-name (buffer-file-name (buffer-base-buffer))))
    (cond ((derived-mode-p 'dired-mode)
           (file-name-as-directory (expand-file-name default-directory)))

          (file-name
           (expand-file-name (file-name-directory file-name))))))

(defun exec-command (command &rest args)
  "Execute shell COMMAND with ARGS without displaying the output."
  (apply 'call-process command nil nil nil args))

(defun exec-shell-command (arg &optional cwd)
  "Execute shell command ARG without displaying the output.
CWD is the current working directory."
  (unless cwd
    (setq cwd (buffer-cwd)))
  (let ((default-directory cwd))
    (call-process "env" nil nil nil "sh" "-c" arg)))

;;-----------------------------------------------------------------------------
;; CLONE BUFFERS
;;-----------------------------------------------------------------------------
(defun tab-new-func-buffer-from-other-window (func)
  "Open the buffer created by the FUNC function in the other window in a new tab."
  (let* ((original-tab-index (1+ (tab-bar--current-tab-index)))
         (original-window (selected-window)))
    ;; Save the state of the other window
    (other-window 1)
    (let* ((other-window (selected-window))
           (other-window-buf (current-buffer))
           (other-window-point (point))
           (other-window-view (window-start)))
      ;; Move back to the original window
      (other-window -1)

      ;; Call the specified function (e.g., embark-dwim, ivy-call...)
      (funcall func)

      ;; Switch back to the other window
      (other-window 1)
      (unless (eq (selected-window) original-window)
        (let* ((preview-buf (current-buffer)))
          ;; Create a new tab and switch to the preview buffer
          (tab-bar-new-tab)
          (set-window-buffer nil preview-buf)

          ;; Go back to the original tab
          (tab-bar-select-tab original-tab-index)

          ;; Restore the state of the other window
          (select-window other-window)
          (set-window-buffer nil other-window-buf)
          (goto-char other-window-point)
          (set-window-start nil other-window-view t)

          ;; Switch to the original window
          (select-window original-window))))))

(defun my-tab-split ()
  "Open the current file or directory in a new tab."
  (interactive)
  ;; (tab-bar-duplicate-tab)
  (let ((buffer-list (window-prev-buffers nil))
        (buffer (current-buffer))
        (point (point))
        (window-start (window-start)))
    (tab-bar-new-tab)
    (set-window-buffer nil buffer)
    (set-window-prev-buffers nil buffer-list)
    (with-current-buffer buffer
      (goto-char point)
      (set-window-start nil window-start t))))

;;; ex init-helpers

(defvar text-editing-modes '(conf-mode prog-mode text-mode)
  "List of text editing modes.")

(defun add-functions-to-mode-hooks (modes functions)
  "Add FUNCTIONS to MODES."
  ;; If modes is not a list, make it a list
  (unless (listp modes)
    (setq modes (list modes)))

  ;; If functions is not a list, make it a list
  (unless (listp functions)
    (setq functions (list functions)))

  (dolist (current-mode modes)
    (let ((hook (intern (concat (symbol-name current-mode) "-hook"))))
      (dolist (func functions)
        (if (functionp func)  ; Check if func is a valid function
            (add-hook hook func)
          (message "Warning: `%s' is not a valid function, skipping..."
                   (symbol-name func)))))))

(defun add-hook-text-editing-modes (functions)
  "Add FUNCTIONS to hooks corresponding to `text-editing-modes`.
FUNCTIONS can be a single function or a list of functions."
  (add-functions-to-mode-hooks text-editing-modes functions))

;;; ignore-errors advice

(defun my--ignore-error-advice (orig-fn &rest args)
  "Advise function to suppress any output of the ORIG-FN function.
ARGS are the ORIG_-FN function arguments."
  (ignore-errors
    (apply orig-fn args)))

(defun my-advice-ignore-errors (fn)
  "Advise the the FN function to be quiet."
  (advice-add fn :around #'my--ignore-error-advice))

;;; Ansible format buffer

;; TODO fix window start
(defun my-ansible-lint-format-buffer ()
  "Format the current buffer with ansible-lint.
After that, restore cursor, `window-start', and hscroll."
  (interactive)
  (unless (derived-mode-p 'ansible-mode)
    (error "This only works when ansible-mode is active"))
  (if (or (derived-mode-p 'yaml-mode)
          (derived-mode-p 'yaml-ts-mode))
      (let* ((path (buffer-file-name (buffer-base-buffer)))
             (cursor-pos (point))
             (window (selected-window))
             (window-start-pos (when window
                                 (window-start)))
             (hscroll-pos (window-hscroll)))
        (let ((save-silently t))
          (save-buffer))
        (call-process "ansible-lint" nil "*ansible-lint*" nil
                      "--write" path)
        (revert-buffer :ignore-auto :noconfirm)
        (goto-char cursor-pos)
        (when (and (window-live-p window)
                   (eq (current-buffer) (window-buffer window)))
          (when window-start
            (set-window-start (selected-window) window-start-pos t))
          (when hscroll-pos
            (set-window-hscroll (selected-window) hscroll-pos))))
    (user-error "Error: Unsupported file type")))

;;; bash stdops

(defun bash-stdops-project-sre (&optional from-string to-string path)
  "Recursively replace occurrences of FROM-STRING with TO-STRING in PATH.
The symbol to be replaced is specified by FROM-STRING, and the replacement is
TO-STRING."
  (interactive)
  (let ((path (or path default-directory))
        (from-string (or from-string
                         (if (use-region-p)
                             (buffer-substring-no-properties (region-beginning)
                                                             (region-end))
                           (thing-at-point 'symbol)))))
    (save-excursion
      (when (region-active-p)
        (deactivate-mark))
      (if path
          (setq path (expand-file-name path))
        (user-error "Unable to find the path: %s" path))
      (unless from-string
        (user-error "No symbol at point"))
      (let ((to-string (or to-string
                           (read-string
                            (format "Project: %s\nText before: %s\nText after: "
                                    path from-string)
                            from-string))))
        (my-save-all-buffers)
        (let ((default-directory temporary-file-directory))
          (message "sre %s" path)
          (call-process "sre" nil t nil from-string to-string path))))))

;;; Save buffers kill Emacs

(defun my-save-buffers-kill-emacs ()
  "Handle quitting Emacs with daemon-aware frame management."
  (interactive)
  (my-save-all-buffers)
  (if (and (fboundp 'easysession-save-session-and-close-frames))
      (easysession-save-session-and-close-frames)
    (save-buffers-kill-emacs)))

;;; Interesting buffers

(defun my-interesting-buffer-p ()
  "Return t if this buffer is considered a file/directory or otherwise interesting."
  (let ((buf-name (buffer-name))
        (file-name (buffer-file-name (buffer-base-buffer))))
    (or
     ;;(string-prefix-p "*Ollama" buf-name)
     ;;(string-prefix-p "*sdcv:" buf-name)
     ;; (derived-mode-p 'helpful-mode)
     ;; (derived-mode-p 'quick-sdcv-mode)
     (and (not (string-prefix-p "*" buf-name))
          (not (string-prefix-p " " buf-name)))
     (and file-name
          (string-suffix-p "/todo.org" file-name)))))

(defun my-smart-switch-interesting-buffer (switch-func)
  "Switch to an interesting buffer using SWITCH-FUNC.
SWITCH-FUNC should be a symbol for a buffer navigation function,
such as `next-buffer' or `previous-buffer'."
  (let ((start-buffer (current-buffer))
        (found nil)
        (max-iters (length (buffer-list))))
    (funcall switch-func)
    (while (and (> max-iters 0)
                (not (eq (current-buffer) start-buffer))
                (not (setq found (my-interesting-buffer-p))))
      (funcall switch-func)
      (setq max-iters (1- max-iters)))
    ;; If we didn't find anything and aren't already back at the start, revert
    ;; safely.
    (unless (or found (eq (current-buffer) start-buffer))
      (set-window-buffer nil start-buffer))))

(defun my-smart-previous-interesting-buffer ()
  "Switch to the previous buffer that is a file, Dired, or vterm."
  (interactive)
  (my-smart-switch-interesting-buffer #'previous-buffer))

(defun my-smart-next-interesting-buffer ()
  "Switch to the next buffer that is a file, Dired, or vterm."
  (interactive)
  (my-smart-switch-interesting-buffer #'next-buffer))

(defun my-previous-interesting-buffer ()
  "Switch to the previous buffer that is a file, Dired, or vterm."
  (interactive)
  ;; (my-smart-previous-interesting-buffer)
  (previous-buffer))

(defun my-next-interesting-buffer ()
  "Switch to the next buffer that is a file, Dired, or vterm."
  (interactive)
  ;; (my-smart-next-interesting-buffer)
  (next-buffer))

;; Lastdir

(defun my-update-bash-lastdir (&rest _)
  "Update Bash lastdir."
  (let* ((directory (buffer-cwd))
         (file "~/.bash_lastdir")
         (file-lastdir (when (file-exists-p file)
                         (let ((line (let ((coding-system-for-read 'utf-8-emacs)
                                           (file-coding-system-alist nil))
                                       (with-temp-buffer
                                         (insert-file-contents file)
                                         (thing-at-point 'line)))))
                           (when line
                             line)))))
    (when (or (not file-lastdir)
              (not (string= directory file-lastdir)))
      (with-temp-buffer
        (insert (expand-file-name default-directory))
        ;; Force Emacs to read and write the exact internal byte representation
        ;; of the text without attempting any implicit encoding or decoding
        ;; conversions.
        (let ((coding-system-for-write 'utf-8-emacs)
              (write-region-annotate-functions nil)
              (write-region-post-annotation-function nil))
          (let ((inhibit-quit t))
            (write-region (point-min) (point-max) file
                          nil 'silent)))))))

;;; check parens

;; TODO buffer wizard

(defun my-check-parens-no-jump (&optional no-error)
  "Check for unbalanced parentheses in the current buffer.
This function scans the entire buffer for balanced parentheses. If an imbalance
is found, it either triggers a user error or simply shows a message, depending
on the optional argument NO-ERROR.
If NO-ERROR is non-nil, it only displays a message about the unmatched
parenthesis or quote, including the line number, column, and file name, but does
not raise an error.
If NO-ERROR is nil or omitted, a user error is raised with the same details, and
the function returns nil.
If the parentheses are balanced, the function returns t."
  (interactive)
  (condition-case data
      (progn
        (scan-sexps (point-min) (point-max))
        ;; Return t
        t)
    (scan-error
     (let* ((char (nth 2 data))
            (msg
             (format "Unmatched bracket or quote in line %s, column %s in %s"
                     (line-number-at-pos char)
                     (let ((column (save-excursion (goto-char char)
                                                   (current-column))))
                       (when (integerp column)
                         (1+ column)))
                     (let ((file-name (buffer-file-name (buffer-base-buffer))))
                       (if file-name
                           (abbreviate-file-name file-name)
                         (buffer-name))))))

       (if no-error
           (user-error msg)
         (user-error msg))
       ;; Return nil
       nil))))

;; (advice-add 'check-parens :override #'my-check-parens-no-jump)

(defun my-setup-my-check-parens-no-jump ()
  "Check parens no jump."
  (add-hook 'before-save-hook #'my-check-parens-no-jump nil t))

(add-hook 'emacs-lisp-mode-hook #'my-setup-my-check-parens-no-jump)

;; Use va( instead
;; (defun my-elisp-mode-select-sexp ()
;;   "Select the sexp at point."
;;   (interactive)
;;   (let ((bounds (bounds-of-thing-at-point 'sexp)))
;;     (if bounds
;;         (progn
;;           (goto-char (car bounds))
;;           (push-mark (point) t t)
;;           (goto-char (cdr bounds))
;;           (activate-mark))
;;       (message "No sexp at point."))))

;; apheleia does this
;; (add-hook 'emacs-lisp-mode-hook
;;           #'(lambda ()
;;               (with-eval-after-load 'evil
;;                 (evil-define-key 'normal 'global (kbd "gV")
;;                   #'my-elisp-mode-select-sexp)
;;                 (add-hook 'evil-insert-state-exit-hook
;;                           #'(lambda() (when (evil-insert-state-p)
;;                                         (my-check-parens-no-jump t)))
;;                           nil t))
;;               (add-hook 'after-save-hook #'my-check-parens-no-jump -99 t)))

;;; Complicated version my-jump-to-buffers-or-open
;; (jump to the buffer's tab TODO wizard)

(defcustom my-ephemeral-buffer-names
  '("*scratch*" "*Messages*" "*Warnings*" "*Help*")
  "List of buffer names from which file jumps should reuse the current tab."
  :type '(repeat string)
  :group 'convenience)

(defcustom my-ephemeral-major-modes
  '(help-mode compilation-mode dired-mode)
  "List of major modes from which file jumps should reuse the current tab."
  :type '(repeat symbol)
  :group 'convenience)

(defcustom my-ephemeral-file-names
  '("todo.org")
  "List of file names from which file jumps should reuse the current tab."
  :type '(repeat string)
  :group 'convenience)

(defun my-ephemeral-buffer-p ()
  "Return non-nil if the current buffer is considered transient or ephemeral.
Checks against `my-ephemeral-buffer-names', `my-ephemeral-major-modes',
and `my-ephemeral-file-names'.  Properly resolves indirect buffers to
check their base buffer's file name."
  (let ((base-file-name (buffer-file-name (or (buffer-base-buffer)
                                              (current-buffer)))))
    (or (member (buffer-name) my-ephemeral-buffer-names)
        (apply #'derived-mode-p my-ephemeral-major-modes)
        (and base-file-name
             (member (file-name-nondirectory base-file-name)
                     my-ephemeral-file-names)))))

;; NOTE: Buggy
;; (defun my-jump-to-buffers-or-open (bufs fallback-file
;;                                         &optional no-new-tab)
;;   "Jump to a visible window displaying any buffer in BUFS.
;; Searches the current frame first, then across all tabs and frames.
;; If no buffer is found, open FALLBACK-FILE.  When NO-NEW-TAB is
;; non-nil or the current buffer is ephemeral, the file opens in the
;; current tab; otherwise, it opens in a new tab.  Pulses the cursor
;; upon switching or opening."
;;   (require 'pulse)
;;   (let* ((buf-objs (delq nil (mapcar #'get-buffer bufs)))
;;          (target-window (seq-find (lambda (w)
;;                                     (memq (window-buffer w) buf-objs))
;;                                   (window-list)))
;;          found-tab-info
;;          found-buf)
;;     (if target-window
;;         (progn
;;           ;; If it is already in a window on the current frame, switch and pulse
;;           (select-window target-window)
;;           (run-with-timer 0.05 nil #'pulse-momentary-highlight-one-line (point)))
;;       ;; Otherwise, check for it in a tab across all frames
;;       (when (bound-and-true-p tab-bar-mode)
;;         (catch 'found
;;           (dolist (buf bufs)
;;             (when-let* ((tab-info (tab-bar-get-buffer-tab buf t)))
;;               (setq found-tab-info tab-info
;;                     found-buf buf)
;;               (throw 'found t)))))
;;       (if found-tab-info
;;           (let ((frame (alist-get 'frame found-tab-info))
;;                 (index (alist-get 'index found-tab-info)))
;;             ;; Focus the target frame if it's not the currently active one
;;             (when (and frame (frame-live-p frame))
;;               (select-frame-set-input-focus frame))
;;             ;; Switch to the tab using its internal index (1-based)
;;             (unless (eq (car found-tab-info) 'current-tab)
;;               (tab-bar-select-tab (1+ index)))
;;             ;; Unconditionally search for the window in this tab, select it, and
;;             ;; pulse
;;             (if-let* ((target-tab-window
;;                        (seq-find (lambda (w)
;;                                    (memq (window-buffer w) buf-objs))
;;                                  (window-list))))
;;                 (progn
;;                   (select-window target-tab-window)
;;                   (run-with-timer 0.05 nil #'pulse-momentary-highlight-one-line (point)))
;;               ;; If the expected buffer is missing after the tab switch, force it
;;               (switch-to-buffer found-buf)
;;               (run-with-timer 0.05 nil #'pulse-momentary-highlight-one-line (point))))
;;         ;; Fallback: Open a new tab, find the fallback file, and pulse
;;         (when fallback-file
;;           (let ((open-in-current-tab (or no-new-tab (my-ephemeral-buffer-p))))
;;             (when (and (bound-and-true-p tab-bar-mode)
;;                        (fboundp 'tab-bar-new-tab)
;;                        (not open-in-current-tab))
;;               (tab-bar-new-tab))
;;             (find-file fallback-file)
;;             (run-with-timer 0.05 nil #'pulse-momentary-highlight-one-line (point))))))))

;;; Simple version: my-jump-to-buffers-or-open

;; (defun my-jump-to-buffers-or-open (bufs fallback-file)
;;   "Jump to a visible window displaying any buffer in BUFS.
;; Searches the current frame first, then across all tabs and frames.  If no buffer
;; is found, open FALLBACK-FILE.  Pulses the cursor upon switching or opening."
;;   (require 'pulse)
;;   (let ((target-window (seq-find (lambda (w)
;;                                    (memq (window-buffer w) bufs))
;;                                  (window-list)))
;;         found-tab-info)
;;     (if target-window
;;         (progn
;;           ;; If it is already in a window on the current frame, switch and pulse
;;           (select-window target-window)
;;           (pulse-momentary-highlight-one-line (point)))
;;       ;; Otherwise, check for it in a tab across all frames
;;       (when (bound-and-true-p tab-bar-mode)
;;         (catch 'found
;;           (dolist (buf bufs)
;;             (when-let* ((tab-info (tab-bar-get-buffer-tab buf t)))
;;               (setq found-tab-info tab-info)
;;               (throw 'found t)))))
;;       (if found-tab-info
;;           (let ((frame (alist-get 'frame found-tab-info))
;;                 (index (alist-get 'index found-tab-info)))
;;             ;; Focus the target frame if it's not the currently active one
;;             (when (and frame (frame-live-p frame))
;;               (select-frame-set-input-focus frame))
;;             ;; Switch to the tab using its internal index (1-based)
;;             (unless (eq (car found-tab-info) 'current-tab)
;;               (tab-bar-select-tab (1+ index)))
;;             ;; Unconditionally search for the window in this tab, select it, and
;;             ;; pulse
;;             (when-let* ((target-tab-window
;;                          (seq-find (lambda (w)
;;                                      (memq (window-buffer w) bufs))
;;                                    (window-list))))
;;               (select-window target-tab-window)
;;               (pulse-momentary-highlight-one-line (point))))
;;         ;; Fallback: find the fallback file, and pulse
;;         (when fallback-file
;;           (find-file fallback-file)
;;           (pulse-momentary-highlight-one-line (point)))))))

;;; tab width

(defun my-set-tab-width (width)
  "Set the tab width.
WIDTH is the tab width."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width width)
  (setq-local standard-indent width)
  ;; (setq-local evil-shift-width width)
  )

;;; From Lightemacs

;; (defmacro lightemacs-shield-macros (&rest body)
;;   "Eval BODY while preventing premature macro expansion.
;;
;; Use this when a form contains code to be evaluated later, and that code depends
;; on a macro not yet defined. If the macro treats its arguments specially, an
;; argument resembling a macro call might be expanded too early, breaking
;; evaluation. Wrapping the outer (or higher) macro in this form avoids that
;; problem."
;;   (declare (indent 0))
;;   `(eval '(progn ,@body) lexical-binding))

;; (defmacro lightemacs-shield-macros-when-compiling (feature &rest body)
;;   "Evaluate BODY, shielding macros only if FEATURE is not yet available.
;; If FEATURE is already present, expand BODY normally.
;; During byte-compilation, attempt to load FEATURE eagerly."
;;   (declare (indent 0))
;;   (let ((available (featurep feature)))
;;     (when (bound-and-true-p byte-compile-current-file)
;;       (setq available (require feature nil 'noerror)))
;;     (if available
;;         `(progn ,@body)
;;       `(lightemacs-shield-macros
;;          (progn ,@body)))))

;;; Diff against main

(defun mod-better-vc-version-diff-main ()
  "Diff HEAD against origin/main if it exists, otherwise against origin/master."
  (interactive)
  (when (and (fboundp 'vc-git-branches)
             (fboundp 'vc-diff-internal)
             (fboundp 'vc-deduce-fileset))
    (let* ((branches (vc-git-branches))  ;; Get list of remote branches
           (target (cond
                    ((member "main" branches) "origin/main")
                    ((member "master" branches) "origin/master")
                    (t (error "Neither origin/main nor origin/master found")))))
      (vc-diff-internal t (vc-deduce-fileset t) "HEAD" target))))

;;; Provide
(provide 'my-defun)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; my-defun.el ends here

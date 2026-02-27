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
  (require 'treesit)
  (if (and (fboundp 'treesit-language-available-p))
      (treesit-language-available-p language)
    nil)
  ;; (if (and (fboundp 'treesit-ready-p))
  ;;     (treesit-ready-p language)
  ;;   nil)
  )

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

(defun my-reload-current-buffer ()
  "Completely reload the current buffer by killing and reopening it."
  (interactive)
  (let* ((file buffer-file-name))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (let* ((buffer (current-buffer))
           (window (selected-window))
           (window-hscroll (when window
                             (window-hscroll)))
           (window-start (when window
                           (window-start)))
           (point (point)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (buffer-modified-p)
            (save-buffer)))

        (kill-buffer buffer)

        (let ((new-buffer (find-file-noselect file))
              (switch-to-buffer-obey-display-actions nil))
          (switch-to-buffer new-buffer)
          (with-current-buffer new-buffer
            (goto-char point)
            ;; TODO This does not seem to work
            (when (and (window-live-p window)
                       (numberp window-hscroll)
                       (numberp window-start)
                       (eq (current-buffer) (window-buffer window)))
              (set-window-start window window-start t)
              (set-window-hscroll window window-hscroll))))))))

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

(defun my-woman (&optional topic)
  "Prompt for a man page name and display it using the woman package.
If TOPIC is provided, use that as the default."
  (interactive "MMan page name: ")
  (woman topic))

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
             (beginning-of-line)
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
(defun my-dir-config--buffer-cwd ()
  "Return the directory associated with the current buffer.
Returns:
- The directory path if the buffer is in `dired-mode', or
- The directory of the file if the buffer is visiting a file, or
- nil if neither condition is met."
  (let ((file-name (buffer-file-name (buffer-base-buffer))))
    (cond ((derived-mode-p 'dired-mode)
           default-directory)

          (file-name
           (file-name-directory file-name)))))

(defun buffer-cwd ()
  "Return the directory of the current buffer."
  (interactive)
  (or (my-dir-config--buffer-cwd) default-directory))


(defun my-previous-interesting-buffer ()
  "Switch to the previous buffer that is a file, Dired, or vterm."
  (interactive)
  (previous-buffer))

(defun my-next-interesting-buffer ()
  "Switch to the next buffer that is a file, Dired, or vterm."
  (interactive)
  (next-buffer))

;;-----------------------------------------------------------------------------
;; Commands
;;-----------------------------------------------------------------------------
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
;; Temporary ediff
;;-----------------------------------------------------------------------------
(defvar my-tmp-files-dir (expand-file-name "~")
  "Temporary ediff.")

(defvar my-tmp-file-name "tmp-file"
  "The name of the temporary file.")

(defvar my-tmp-ediff-file1-name "tmp-file1"
  "The name of the first ediff temporary file.")

(defvar my-tmp-ediff-file2-name "tmp-file2"
  "The name of the second ediff temporary file.")

(defun my-prompt-extension (extension)
  "Ask the user to enter an EXTENSION if no extension is provided."
  (let ((ext (replace-regexp-in-string "^\\.*" "" extension)))
    (if extension
        (concat (if (string= ext "") "" ".")
                ext)
      "")))

(defun my-create-file-in-parent-directory (file-path empty-file)
  "Create a file in the parent directory of FILE-PATH.

If EMPTY-FILE is non-nil, ensure that the created file is empty by erasing any
existing content or deleting and recreating it. Otherwise, just create an empty
file if it does not exist. If FILE-PATH is already visited in some Emacs buffer,
then its buffer will be cleared and saved to achieve the same result.

FILE-PATH should be a string representing the file path of the desired file.
EMPTY-FILE is a boolean that defaults to nil."
  (let* ((parent-dir (file-name-directory file-path))
         (file-name (file-name-nondirectory file-path))
         (full-path (expand-file-name file-name parent-dir)))
    (unless (file-exists-p parent-dir)
      ;; Ensure the parent directory exists.
      (make-directory parent-dir t))
    (when empty-file
      (let ((buffer (find-file-noselect full-path)))
        (when buffer
          ;; If the file is open in a buffer, empty that buffer.
          (with-current-buffer buffer
            (erase-buffer)
            (let ((save-silently t))
              (save-buffer))))))))

(defun my-get-temporary-file-path (file-name ext)
  "Get the full path to the temporary file.
FILE-NAME is the file name.
EXT is the extension."
  (expand-file-name (concat file-name ext) my-tmp-files-dir))

(defun my-temporary-file (extension)
  "Create and open a new temporary file with the given EXTENSION.
If the user inputs nothing for EXTENSION, prompt for it interactively.

This function first prompts for an optional extension. If no input is provided,
the default value is used. Then, it generates a unique filename using the
specified extension and opens that file in a new buffer. The parent directory of
this file must exist."
  (interactive "sExtension: ")
  (let* ((ext (my-prompt-extension extension))
         (file-path1 (my-get-temporary-file-path my-tmp-file-name ext)))
    (my-create-file-in-parent-directory file-path1 nil)
    (find-file file-path1)))

(defun my-temporary-diff (extension)
  "Open two files with the EXTENSION in ediff in a temporary directory."
  (interactive "sExtension: ")
  (let* ((file-suffix (my-prompt-extension extension))
         (file-path1 (my-get-temporary-file-path
                      my-tmp-ediff-file1-name file-suffix))
         (file-path2 (my-get-temporary-file-path
                      my-tmp-ediff-file2-name file-suffix)))
    (my-create-file-in-parent-directory file-path1 t)
    (my-create-file-in-parent-directory file-path2 t)
    (ediff-files file-path1 file-path2)))

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
          (switch-to-buffer preview-buf)

          ;; Go back to the original tab
          (tab-bar-select-tab original-tab-index)

          ;; Restore the state of the other window
          (select-window other-window)
          (switch-to-buffer other-window-buf)
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
    (switch-to-buffer buffer)
    (set-window-prev-buffers nil buffer-list)
    (with-current-buffer buffer
      (goto-char point)
      (set-window-start nil window-start))))

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
  (save-excursion
    (when (region-active-p)
      (deactivate-mark))
    (let ((path (or path default-directory))
          (from-string (or from-string
                           (if (use-region-p)
                               (buffer-substring-no-properties (region-beginning)
                                                               (region-end))
                             (thing-at-point 'symbol)))))
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
        (save-some-buffers t)
        (call-process "sre" nil t nil from-string to-string path)))))

;;; indentnav

(defun indentnav--keep-searching-until-empty (_initial-indentation)
  "Return t when the current line is empty."
  (not (string= (string-trim-left (buffer-substring-no-properties (line-beginning-position)
                                                                  (line-end-position)))
                "")))

(defun indentnav--keep-searching-until-indent-lower (initial-indentation)
  "Return t when the indentation is >= than INITIAL-INDENTATION.
It also returns t when it is empty."
  (or (string= (string-trim-left (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))
               "")
      (>= (current-indentation) initial-indentation)))

(defun indentnav--find-line (direction func-keep-searching)
  "Get the line number of the previous/next line with lower indentation.

If DIRECTION is positive, move forward; if negative, move backward.

The FUNC-KEEP-SEARCHING function considers lines with lower indentation,
skipping empty lines. Returns the line number of the identified line."
  (when (>= direction 0) (setq direction 1))
  (when (< direction 0) (setq direction -1))
  (save-excursion
    (let* ((initial-indentation (current-indentation)))
      (while (and (not (if (> direction 0) (eobp) (bobp)))
                  (zerop (forward-line direction))
                  (funcall func-keep-searching initial-indentation))))
    (line-number-at-pos)))

(defun indentnav--goto-line (line &optional buffer relative)
  "Go to LINE, counting from line 1 at beginning of buffer.

If optional argument BUFFER is non-nil, switch to that buffer and
move to line LINE there.

If optional argument RELATIVE is non-nil, counting starts at the beginning
of the accessible portion of the (potentially narrowed) buffer.

If the variable `widen-automatically' is non-nil, cancel narrowing and
leave all lines accessible.  If `widen-automatically' is nil, just move
point to the edge of visible portion and don't change the buffer bounds.

Prior to moving point, this function sets the mark (without
activating it), unless Transient Mark mode is enabled and the
mark is already active.

This function is usually the wrong thing to use in a Lisp program.
What you probably want instead is something like:
  (goto-char (point-min))
  (forward-line (1- N))
If at all possible, an even better solution is to use char counts
rather than line counts."
  ;; Switch to the desired buffer, one way or another.
  (if buffer
      (let ((window (get-buffer-window buffer)))
        (if window (select-window window)
          (switch-to-buffer-other-window buffer))))
  ;; Leave mark at previous position
  (or (region-active-p) (push-mark))
  ;; Move to the specified line number in that buffer.
  (let ((pos (save-restriction
               (unless relative (widen))
               (goto-char (point-min))
               (if (eq selective-display t)
                   (re-search-forward "[\n\C-m]" nil 'end (1- line))
                 (forward-line (1- line)))
               (point))))
    (when (and (not relative)
               (buffer-narrowed-p)
               widen-automatically
               ;; Position is outside narrowed part of buffer
               (or (> (point-min) pos) (> pos (point-max))))
      (widen))
    (goto-char pos)))

(defun indentnav--move (direction func-keep-searching)
  "Move to the previous/next line with lower indentation.
If DIRECTION is positive, move forward; if negative, move backward.
The FUNC-KEEP-SEARCHING function considers lines with lower indentation,
skipping empty lines. Returns the line number of the identified line."
  (let ((saved-column (current-column))
        (line-number (indentnav--find-line direction func-keep-searching)))
    (when line-number
      (indentnav--goto-line line-number)
      (move-to-column saved-column))))

;;;###autoload
(defun indentnav-backward-to-lower-indentation ()
  "Move backward to the lower indentation."
  (interactive)
  (indentnav--move -1 'indentnav--keep-searching-until-indent-lower))

;;;###autoload
(defun indentnav-forward-to-lower-indentation ()
  "Move forward to the lower indentation."
  (interactive)
  (indentnav--move 1 'indentnav--keep-searching-until-indent-lower))

;;;###autoload
;;(defun indentnav-select-same-indentation ()
;;  "Select the same indentation."
;;  (interactive)
;;  (indentnav-select-current 'indentnav--keep-searching-until-indent-lower))

;;;###autoload
(defun indentnav-backward-to-empty-line ()
  "Move backward to empty line."
  (interactive)
  (indentnav--move -1 'indentnav--keep-searching-until-empty))

;;;###autoload
(defun indentnav-forward-to-empty-line ()
  "Move forward to empty line."
  (interactive)
  (indentnav--move 1 'indentnav--keep-searching-until-empty))

;;; Byte compile

;; (defun byte-compile-if-outdated (file-or-symbol &optional no-error)
;;   "Byte-compile FILE-OR-SYMBOL into a .elc if it is missing or outdated.
;;
;; If the corresponding .elc file does not exist or is older than FILE-OR-SYMBOL,
;; this function attempts to compile FILE-OR-SYMBOL. Compilation warnings and
;; errors are suppressed, and verbose messages are displayed if
;; `lightemacs-verbose` is non-nil.
;;
;; NO-ERROR, if non-nil, suppresses raising an error when FILE-OR-SYMBOL cannot be
;; found. Returns t if the file is up to date or successfully compiled, nil or the
;; error object otherwise."
;;   (let ((el-file (if (symbolp file-or-symbol)
;;                      (locate-library (symbol-name file-or-symbol))
;;                    file-or-symbol)))
;;     (cond
;;      ((and (stringp el-file)
;;            (or (string-suffix-p ".eln" el-file)
;;                (string-suffix-p ".elc" el-file)))
;;       (lightemacs-verbose-message
;;         "[lightemacs] IGNORE: Byte-compile: Already compiled: %s"
;;         el-file))
;;
;;      ((not (file-exists-p el-file))
;;       (unless no-error
;;         (error "[lightemacs] Byte-compile: Cannot detect the .elc path for: %s"
;;                el-file)))
;;
;;      (t
;;       (let* ((elc-file (funcall
;;                         (if (bound-and-true-p byte-compile-dest-file-function)
;;                             byte-compile-dest-file-function
;;                           #'byte-compile-dest-file)
;;                         el-file)))
;;         (cond
;;          ((not elc-file)
;;           (message "WARNING: Byte-compile: Cannot detect the .elc path for: %s"
;;                    el-file))
;;
;;          ((and (file-exists-p elc-file)
;;                (not (file-newer-than-file-p el-file elc-file)))
;;           ;; Up to date
;;           t)
;;
;;          ((and lightemacs-verbose
;;                (not (file-writable-p elc-file)))
;;           (lightemacs-verbose-message
;;             (concat "IGNORE: Byte-compile: Destination .elc is read-only: %S. "
;;                     "Ensure you have write permissions to allow recompilation.")
;;             elc-file))
;;
;;          (t
;;           (let* ((noninteractive t)
;;                  (byte-compile-warnings nil)
;;                  (cond-error nil)
;;                  (success (condition-case err
;;                               (progn
;;                                 (lightemacs-verbose-message
;;                                   "Byte-compile: %S" el-file)
;;                                 (byte-compile-file el-file))
;;                             (error
;;                              ;; Return error
;;                              (setq cond-error err)))))
;;             (when (or (not success)
;;                       cond-error)
;;               (let ((delete-by-moving-to-trash nil))
;;                 (lightemacs-verbose-message "Delete .elc file: %S" elc-file)
;;                 (ignore-errors
;;                   (delete-file elc-file))))
;;
;;             (when (or cond-error
;;                       (not success))
;;               (lightemacs-verbose-message
;;                 "Error: Byte-compile: %S (Result: %s)"
;;                 el-file (cond (cond-error
;;                                (format "Error: %s"
;;                                        cond-error))
;;                               ((eq success t)
;;                                "success")
;;                               ((not success)
;;                                "error")
;;                               (t
;;                                success))))))))))))

;;; Add to load-path recursively

(defun directory-contains-extension-p (dir extension)
  "Check if DIR any file with EXTENSION."
  (catch 'found
    (dolist (name (directory-files dir nil nil t))
      (when (and (string-suffix-p extension name))
        (throw 'found t)))
    nil))

;; (defun directory-contains-extension-p (dir extension)
;;   "Return non-nil if at least one EXTENSION file exists in DIR."
;;   (let ((regex (concat (regexp-quote extension) "\\'")))
;;     (catch 'found
;;       (dolist (file (directory-files dir nil regex t 1))
;;         (when (file-regular-p (expand-file-name file dir))
;;           (throw 'found t)))
;;       nil)))

(defun optimized-normal-top-level-add-subdirs-to-load-path ()
  "Recursively add all subdirectories of `default-directory' to `load-path'.
More precisely, this uses only the subdirectories whose names
start with letters or digits; it excludes any subdirectory named `RCS'
or `CVS', and any subdirectory that contains a file named `.nosearch'."
  (let (dirs
        attrs
        (file-name-handler-alist nil)  ;; JC: OPTIMIZATION
        (pending (list default-directory)))
    ;; This loop does a breadth-first tree walk on DIR's subtree,
    ;; putting each subdir into DIRS as its contents are examined.
    (while pending
      (push (pop pending) dirs)
      (let* ((this-dir (car dirs))
             (contents (directory-files this-dir))
             (default-directory this-dir)
             (canonicalized (if (fboundp 'w32-untranslated-canonical-name)
                                (w32-untranslated-canonical-name this-dir))))
        ;; The Windows version doesn't report meaningful inode numbers, so
        ;; use the canonicalized absolute file name of the directory instead.
        (setq attrs (or canonicalized
                        (file-attribute-file-identifier
                         (file-attributes this-dir))))
        (unless (member attrs normal-top-level-add-subdirs-inode-list)
          (push attrs normal-top-level-add-subdirs-inode-list)
          (dolist (file contents)
            (and ;; (string-match "\\`[[:alnum:]]" file) ;; JC: remove this
             ;; (not (string= file ""))  ;; JC: replaced it with this
             (not (string= file "."))  ;; JC: replaced it with this
             (not (string= file ".."))  ;; JC: replaced it with this
             ;; The lower-case variants of RCS and CVS are for DOS/Windows.
             ;; JC: OPTIMIZATION: added .git
             ;; (not (member file '("RCS" "CVS" "rcs" "cvs" ".git")))
             (not (member file '(".git" ".github")))
             (not (string-match-p "test" file))
             (file-directory-p file)
             ;; TODO: fix
             ;; (directory-contains-extension-p file ".el")  ;; JC: optimization
             (let ((expanded (expand-file-name file)))
               (or (file-exists-p (expand-file-name ".nosearch" expanded))
                   (setq pending (nconc pending (list expanded))))))))))

    ;; JC: Only add dirs that contain .el files
    (normal-top-level-add-to-load-path
     (seq-filter (lambda (d) (directory-contains-extension-p d ".el"))
                 (cdr (nreverse dirs))))

    ;; (normal-top-level-add-to-load-path (cdr (nreverse dirs)))
    ))

(defun my-save-buffers-kill-emacs ()
  "Handle quitting Emacs with daemon-aware frame management."
  (interactive)
  (if (and (daemonp)
           (fboundp 'easysession-save-session-and-close-frames))
      (easysession-save-session-and-close-frames)
    (save-buffers-kill-emacs)))

;;; Provide
(provide 'my-defun)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; my-defun.el ends here

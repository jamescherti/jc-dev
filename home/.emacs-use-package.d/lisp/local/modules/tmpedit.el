;;; tmpedit.el --- tmpedit -*- lexical-binding: t -*-

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

;;-----------------------------------------------------------------------------
;; Temporary ediff
;;-----------------------------------------------------------------------------
(defvar tmpedit-dir (expand-file-name "tmpedit" user-emacs-directory)
  "Temporary edit.")

(with-eval-after-load 'le-compile-angel
  (with-eval-after-load 'compile-angel
    (when (fboundp 'le-compile-angel-exclude)
      (le-compile-angel-exclude tmpedit-dir))))

(defvar tmpedit-file-name "tmp-file"
  "The name of the temporary file.")

(defvar tmpedit-ediff-file1-name "tmp-file1"
  "The name of the first ediff temporary file.")

(defvar tmpedit-ediff-file2-name "tmp-file2"
  "The name of the second ediff temporary file.")

(defun tmpedit--prompt-extension (extension)
  "Ask the user to enter an EXTENSION if no extension is provided."
  (if (and extension (not (string= extension "")))
      (let ((ext (replace-regexp-in-string "^\\.*" "" extension)))
        (concat "." ext))
    ""))

(defun tmpedit--create-file-in-parent-directory (file-path empty-file)
  "Create a file in the parent directory of FILE-PATH.

If EMPTY-FILE is non-nil, ensure that the created file is empty by erasing any
existing content or deleting and recreating it. Otherwise, just create an empty
file if it does not exist. If FILE-PATH is already visited in some Emacs buffer,
then its buffer will be cleared and saved to achieve the same result.

FILE-PATH should be a string representing the file path of the desired file.
EMPTY-FILE is a boolean that defaults to nil."
  (let* ((full-path (expand-file-name file-path))
         (parent-dir (file-name-directory full-path)))
    (unless (file-exists-p parent-dir)
      ;; Ensure the parent directory exists.
      (make-directory parent-dir t))
    (if empty-file
        (let ((buffer (find-file-noselect full-path)))
          (when buffer
            ;; If the file is open in a buffer, empty that buffer.
            (with-current-buffer buffer
              (erase-buffer)
              (let ((save-silently t))
                (save-buffer)))))
      ;; Otherwise, just create an empty file if it does not exist
      (unless (file-exists-p full-path)
        (write-region "" nil full-path nil 'silent)))))

(defun tmpedit--get-temporary-file-path (file-name ext)
  "Get the full path to the temporary file.
FILE-NAME is the file name.
EXT is the extension."
  (expand-file-name (concat file-name ext) tmpedit-dir))

(defun tmpedit-file (extension)
  "Create and open a new temporary file with the given EXTENSION.
If the user inputs nothing for EXTENSION, prompt for it interactively.

This function first prompts for an optional extension. If no input is provided,
the default value is used. Then, it generates a unique filename using the
specified extension and opens that file in a new buffer. The parent directory of
this file must exist."
  (interactive "sExtension: ")
  (let* ((ext (tmpedit--prompt-extension extension))
         (file-path1 (tmpedit--get-temporary-file-path tmpedit-file-name ext)))
    (tmpedit--create-file-in-parent-directory file-path1 nil)
    (find-file file-path1)))

(defun tmpedit-diff (extension)
  "Open two files with the EXTENSION in ediff in a temporary directory."
  (interactive "sExtension: ")
  (let* ((file-suffix (tmpedit--prompt-extension extension))
         (file-path1 (tmpedit--get-temporary-file-path
                      tmpedit-ediff-file1-name file-suffix))
         (file-path2 (tmpedit--get-temporary-file-path
                      tmpedit-ediff-file2-name file-suffix)))
    (tmpedit--create-file-in-parent-directory file-path1 t)
    (tmpedit--create-file-in-parent-directory file-path2 t)
    (ediff-files file-path1 file-path2)))

(defun tmpedit-diff-clipboard (other-file)
  "Compare the current file-visiting buffer with OTHER-FILE using ediff."
  (interactive
   (list (read-file-name "Compare with file: " nil nil t)))
  (let ((current-file (buffer-file-name)))
    (if current-file
        (ediff-files current-file other-file)
      (user-error "Current buffer is not visiting a file"))))

(defun tmpedit-ediff-clipboard ()
  "Compare the current buffer against a temporary file created from the clipboard.
The temporary file is placed in `tmpedit-dir' using `tmpedit-file-name'."
  (interactive)
  (let ((current-file (buffer-file-name (buffer-base-buffer))))
    (unless current-file
      (user-error "Current buffer is not visiting a file"))
    (let* ((file-ext (file-name-extension current-file))
           (ext (if file-ext (concat "." file-ext) ""))
           (temp-file (tmpedit--get-temporary-file-path tmpedit-file-name ext))
           (clipboard-text (or (ignore-errors (current-kill 0 t)) "")))

      ;; Ensure parent directories exist
      (tmpedit--create-file-in-parent-directory temp-file nil)

      ;; Write the clipboard contents safely by syncing the buffer and disk
      (with-current-buffer (find-file-noselect temp-file)
        (erase-buffer)
        (insert clipboard-text)
        (save-buffer))

      ;; Compare current file against the newly created temporary file
      (ediff-files current-file temp-file))))

(provide 'tmpedit)

;;; tmpedit.el ends here

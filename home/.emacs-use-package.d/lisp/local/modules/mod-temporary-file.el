;;; mod-temporary-file.el --- mod-temporary-file -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:


;;-----------------------------------------------------------------------------
;; Temporary ediff
;;-----------------------------------------------------------------------------
(defvar my-tmp-files-dir (expand-file-name "~")
  "Temporary ediff.")

(with-eval-after-load 'le-compile-angel
  (with-eval-after-load 'compile-angel
    (when (fboundp 'le-compile-angel-exclude)
      (le-compile-angel-exclude my-tmp-files-dir))))

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

;;; my-ediff: Compare the current buffer with another file

(defun my-ediff (other-file)
  "Compare the current file-visiting buffer with OTHER-FILE using ediff."
  (interactive
   (list (read-file-name "Compare with file: " nil nil t)))
  (let ((current-file (buffer-file-name)))
    (if current-file
        (ediff-files current-file other-file)
      (user-error "Current buffer is not visiting a file"))))

(provide 'mod-temporary-file)
;;; mod-temporary-file.el ends here

;;; my-dir-locals.el --- Load additional dir-locals files -*- lexical-binding: t; -*-

;;; Commentary:

;; Load additional dir-locals files.

;;; Code:

;;; Require

(require 'seq)

;;; Main code

(defgroup my-dir-locals-mode nil
  "Customization options for `my-dir-locals-mode'."
  :group 'my-dir-locals
  :prefix "my-dir-locals-")

(defcustom my-dir-locals-files '(".my-dir-locals.el")
  "List of custom directory-local variables files.
Files later in the list take precedence when merging variables."
  :type '(repeat string)
  :group 'local)

(defun my-dir-locals--find-root ()
  "Find the closest directory containing any of `my-dir-locals-files'."
  (locate-dominating-file
   default-directory
   (lambda (dir)
     (seq-some (lambda (file)
                 (file-readable-p (expand-file-name file dir)))
               my-dir-locals-files))))

(defun my-dir-locals-get-variables ()
  "Read custom dir-locals and return them.
This function hooks into `hack-dir-local-get-variables-functions'. It reads the
custom files, merges them using native collection functions, and returns the
variables applicable to the current buffer."
  (when-let* ((dir (my-dir-locals--find-root))
              (dir-name (file-name-as-directory (expand-file-name dir))))
    (let ((variables nil)
          (original-buffer (current-buffer)))
      (dolist (file my-dir-locals-files)
        (let ((full-path (expand-file-name file dir-name)))
          (when (file-readable-p full-path)
            (let ((class-alist
                   (with-temp-buffer
                     (insert-file-contents full-path)
                     (condition-case nil
                         (let ((read-circle nil))
                           (read (current-buffer)))
                       (error nil)))))
              (when (listp class-alist)
                ;; dir-locals-collect-variables handles the major-mode inheritance.
                ;; It must be executed in the original buffer so derived-mode-p works.
                (with-current-buffer original-buffer
                  (setq variables (dir-locals-collect-variables
                                   class-alist dir-name variables))))))))
      ;; Return the cons cell expected by hack-dir-local-get-variables-functions
      (when variables
        (cons dir-name variables)))))

;;;###autoload
(define-minor-mode my-dir-locals-mode
  "Global minor mode to enable loading of custom dir-locals files."
  :global t
  (if my-dir-locals-mode
      (add-hook 'hack-dir-local-get-variables-functions #'my-dir-locals-get-variables)
    (remove-hook 'hack-dir-local-get-variables-functions #'my-dir-locals-get-variables)))

(provide 'my-dir-locals)

;;; my-dir-locals.el ends here

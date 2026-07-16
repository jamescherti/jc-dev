;;; dir-locals-trigger.el --- Run manual logic after dir-locals -*- lexical-binding: t; -*-

;;; Commentary:
;; A minimal package that provides a dedicated hook and helper function.
;; It lets you manually evaluate your directory-local variables and enable
;; modes after Emacs applies them.

;;; Code:

(defgroup dir-locals-trigger nil
  "Customization options for `dir-locals-trigger-mode'."
  :group 'dir-locals-trigger
  :prefix "dir-locals-trigger-")

(defcustom dir-locals-trigger-hook nil
  "Hook run immediately after directory-local variables are applied."
  :type 'hook
  :group 'local)

(defun dir-locals-trigger-mark-safe (var)
  "Mark VAR as a safe local variable for boolean values."
  (put var 'safe-local-variable #'booleanp))

(defun dir-locals-trigger--run ()
  "Execute the user-defined hook `dir-locals-trigger-hook'."
  (run-hooks 'dir-locals-trigger-hook))

;;;###autoload
(define-minor-mode dir-locals-trigger-mode
  "Global minor mode to run `dir-locals-trigger-hook'."
  :global t
  :group 'dir-locals-trigger
  (if dir-locals-trigger-mode
      (add-hook 'hack-local-variables-hook #'dir-locals-trigger--run)
    (remove-hook 'hack-local-variables-hook #'dir-locals-trigger--run)))

(provide 'dir-locals-trigger)
;;; dir-locals-trigger.el ends here

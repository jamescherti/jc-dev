;;; workspace-actions.el --- Restore specific actions and states -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides a global registry to define and execute predefined actions.

;;; Code:

(defgroup workspace-actions nil
  "Configuration for workspace actions."
  :group 'convenience)

(defcustom workspace-actions-alist nil
  "An alist of actions to execute.
The car is a string representing the action name.
The cdr is a function taking no arguments."
  :type '(alist :key-type string :value-type function)
  :group 'workspace-actions)

;;;###autoload
(defun workspace-actions-run (action-name)
  "Execute the action defined by ACTION-NAME in `workspace-actions-alist'."
  (interactive
   (list (completing-read "Action: " workspace-actions-alist nil t)))
  (let ((action-fn (cdr (assoc action-name workspace-actions-alist))))
    (if action-fn
        (funcall action-fn)
      (user-error "Action '%s' not found" action-name))))

(provide 'workspace-actions)

;;; workspace-actions.el ends here

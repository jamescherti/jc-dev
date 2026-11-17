;;; mod-gc.el --- mod-gc -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;;; Require

;;; gcsentinel

(use-package gcsentinel
  :ensure nil
  :commands gcsentinel-mode
  :init
  (add-hook 'lightemacs-emacs-startup-hook #'gcsentinel-mode 200))

;;; Provide

(provide 'mod-gc)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-gc.el ends here

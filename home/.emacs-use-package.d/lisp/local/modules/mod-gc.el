;;; mod-gc.el --- mod-gc -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;;; Require

(eval-and-compile
  (require 'lightemacs-use-package))

;;; gcsentinel

(lightemacs-use-package gcsentinel
  :ensure nil
  :commands gcsentinel-mode
  :init
  (add-hook 'lightemacs-emacs-startup-hook #'gcsentinel-mode 200)
  (setq gcsentinel-low-cons-threshold minimal-emacs-gc-cons-threshold))

;;; Provide

(provide 'mod-gc)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-gc.el ends here

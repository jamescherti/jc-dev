;;; mod-corfu-cape.el --- mod-corfu-cape -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;;; Require

(eval-and-compile
  (require 'lightemacs-use-package))

;;; elisp cape

;; SH
(defun mod-corfu-cape--setup-cape-sh-mode ()
  "Dabbrev is better than the default configuration for `sh-mode'."
  (setq-local completion-at-point-functions '(cape-dabbrev cape-file)))
(add-hook 'bash-ts-mode-hook #'mod-corfu-cape--setup-cape-sh-mode)
(add-hook 'sh-mode-hook #'mod-corfu-cape--setup-cape-sh-mode)

;; Elisp

(defun mod-corfu-cape--cape-elisp-setup ()
  "Configure Cape to provide real Elisp completion merged with dabbrev."
  (setq-local completion-at-point-functions (list #'elisp-completion-at-point)))
;; For some reason, without this, it only uses dabbrev
(add-hook 'emacs-lisp-mode-hook #'mod-corfu-cape--cape-elisp-setup)
(add-hook 'lisp-interaction-mode-hook #'mod-corfu-cape--cape-elisp-setup)

;;; Icons corfu

;; (lightemacs-use-package nerd-icons-completion
;;   :if (display-graphic-p)
;;   :commands nerd-icons-completion-marginalia-setup
;;   :config
;;   (with-eval-after-load 'marginalia
;;     (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))

(setq nerd-icons-font-family "Symbols Nerd Font Mono")

;;; Provide

(provide 'mod-corfu-cape)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-corfu-cape.el ends here

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

(lightemacs-use-package corfu-popupinfo
  :ensure nil ; This is part of corfu
  :commands corfu-popupinfo-mode
  :hook
  (corfu-mode . corfu-popupinfo-mode)
  :init
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (setq corfu-popupinfo-max-width 80)
  (setq corfu-popupinfo-max-height 15))

;; (lightemacs-use-package nerd-icons-completion
;;   :if (display-graphic-p)
;;   :commands nerd-icons-completion-marginalia-setup
;;   :config
;;   (with-eval-after-load 'marginalia
;;     (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))

(lightemacs-use-package nerd-icons-corfu
  :after corfu
  :if (display-graphic-p)
  :commands nerd-icons-corfu-formatter
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  ;; (setq nerd-icons-font-family "Iosevka Nerd Font Mono")
  ;; (setq nerd-icons-font-family "Jetbrains Mono")
  :init
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

;;; Provide

(provide 'mod-corfu-cape)

;;; mod-corfu-cape.el ends here

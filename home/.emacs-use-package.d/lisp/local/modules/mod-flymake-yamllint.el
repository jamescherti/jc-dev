;;; mod-flymake-yamllint.el --- mod-flymake-yamllint -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package flymake-yamllint
  :after flymake
  :commands flymake-yamllint-setup

  :preface
  (defun my-flymake-yamllint-setup ()
    "Setup Flymake yamllint."
    (when (my-code-checker-allowed-p)
      (flymake-yamllint-setup)))

  :init
  (add-hook 'yaml-mode-hook #'my-flymake-yamllint-setup)
  (add-hook 'yaml-ts-mode-hook #'my-flymake-yamllint-setup))

(provide 'mod-flymake-yamllint)
;;; mod-flymake-yamllint.el ends here

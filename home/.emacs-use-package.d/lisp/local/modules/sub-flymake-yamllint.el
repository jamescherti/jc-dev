;;; sub-flymake-yamllint.el --- sub-flymake-yamllint -*- lexical-binding: t -*-

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
    (when (bound-and-true-p env-allow-syntax-checkers)
      (flymake-yamllint-setup)))

  :init
  (add-hook 'yaml-mode-hook #'my-flymake-yamllint-setup)
  (add-hook 'yaml-ts-mode-hook #'my-flymake-yamllint-setup))

(provide 'sub-flymake-yamllint)
;;; sub-flymake-yamllint.el ends here

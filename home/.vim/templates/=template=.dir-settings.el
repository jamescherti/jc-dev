;;; .dirs-settings.el --- Dir Settings -*- no-byte-compile: t; lexical-binding: t -*-

;;; Commentary:

;;; Code:

(when (fboundp 'auto-config-buffer-all-features)
  (setq-local config-buffer-delete-whitespace "modified")
  (auto-config-buffer-all-features))

;;; .dirs-settings.el ends here

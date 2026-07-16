;;; pkg-reformatter.el --- reformatter -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/jc-dev
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; DEPRECATED: Legacy unmaintained code. Safe to remove if it causes
;;; regressions.

;;; Code:

(use-package reformatter-indent
  ;; This isn't really reformatter, but my own function
  :ensure nil
  :commands reformatter-indent-on-save-mode
  :init
  ;; (defun my-setup-reformatter-indent ()
  ;;   (when (my-code-checker-allowed-p)
  ;;     (reformatter-indent-on-save-mode)))

  (when (fboundp 'my-setup-reformatter-indent)
    (add-hook 'emacs-lisp-mode-hook #'my-setup-reformatter-indent)))

;; TODO: rename this to shfmt-reformatter
(use-package shfmt
  :ensure nil
  :commands shfmt-on-save-mode

  :preface
  ;; (defun my-setup-shfmt ()
  ;;   (when (my-code-checker-allowed-p)
  ;;     (shfmt-on-save-mode)))

  :custom
  (setq shfmt-arguments "--binary-next-line")
  (setq shfmt-respect-sh-basic-offset t)

  ;; (add-hook 'sh-mode-hook #'my-setup-shfmt)
  ;; (add-hook 'bash-ts-mode-hook #'my-setup-shfmt)
  )

;; (use-package reformatter-shfmt
;;   :ensure nil
;;   :defer t
;;   :commands reformatter-shfmt-on-save-mode
;;   :init
;;   (defun my-setup-reformatter-shfmt ()
;;     (when (my-code-checker-allowed-p)
;;       (reformatter-shfmt-on-save-mode)))
;;
;;   (when (fboundp 'my-setup-reformatter-shfmt)
;;     (add-hook 'sh-mode-hook #'my-setup-reformatter-shfmt)
;;     (add-hook 'bash-ts-mode-hook #'my-setup-reformatter-shfmt)))

(provide 'pkg-reformatter)

;;; pkg-reformatter.el ends here

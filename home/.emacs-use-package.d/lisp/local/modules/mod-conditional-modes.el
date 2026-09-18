;;; mod-conditional-modes.el --- mod-conditional-modes -*- lexical-binding: t -*-

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

;;; Code:

;;; DISABLED: env vars replacement

;; 1. Define the classes and their variable configurations.
;; The `nil` key indicates that these variables apply to all major modes.

;; (defvar env-allow-syntax-checkers nil)
;; (defvar env-allow-syntax-checker-package-lint nil)
;; (defvar env-allow-lsp nil)
;; (defvar env-allow-whitespace-cleanup nil)
;; (defvar env-allow-language-servers nil)
;; (defvar env-allow-reformatters nil)
;;
;; (dolist (var '(env-allow-syntax-checkers
;;                env-allow-syntax-checker-package-lint
;;                env-allow-lsp
;;                env-allow-whitespace-cleanup
;;                env-allow-language-servers
;;                env-allow-reformatters))
;;   (put var 'safe-local-variable #'booleanp))
;;
;; (dir-locals-set-class-variables 'env-deny-all
;;                                 '((nil . ((env-allow-syntax-checkers . nil)
;;                                           (env-allow-syntax-checker-package-lint . nil)
;;                                           (env-allow-lsp . nil)
;;                                           (env-allow-whitespace-cleanup . nil)
;;                                           (env-allow-language-servers . nil)
;;                                           (env-allow-reformatters . nil)))))
;;
;; (dir-locals-set-class-variables 'env-allow-emacs-dev
;;                                 '((nil . ((env-allow-syntax-checkers . t)
;;                                           (env-allow-syntax-checker-package-lint . t)
;;                                           (env-allow-lsp . t)
;;                                           (env-allow-whitespace-cleanup . t)
;;                                           (env-allow-language-servers . t)
;;                                           (env-allow-reformatters . t)))))
;;
;; (dir-locals-set-class-variables 'env-allow-standard
;;                                 '((nil . ((env-allow-syntax-checkers . t)
;;                                           (env-allow-syntax-checker-package-lint . nil)
;;                                           (env-allow-lsp . t)
;;                                           (env-allow-whitespace-cleanup . t)
;;                                           (env-allow-language-servers . t)
;;                                           (env-allow-reformatters . t)))))
;;
;; (dir-locals-set-directory-class "~/src/forks/" 'env-deny-all)
;; (dir-locals-set-directory-class "~/src/local/" 'env-deny-all)
;; (dir-locals-set-directory-class "~/src/emacs/" 'env-allow-emacs-dev)
;; (dir-locals-set-directory-class "~/Sync/src/" 'env-allow-standard)
;; (dir-locals-set-directory-class "~/src/" 'env-allow-standard)

;;; Auto set env-* vars

(defcustom my-custom-dir-variables
  ;; env-deny-all?
  '(("~/src/forks/"
     . ((env-allow-syntax-checkers . nil)
        (env-allow-syntax-checker-package-lint . nil)
        (env-allow-lsp . nil)
        (env-allow-whitespace-cleanup . nil)
        (env-allow-language-servers . nil)
        (env-allow-reformatters . nil)))

    ("~/src/local/"
     . ((env-allow-syntax-checkers . nil)
        (env-allow-syntax-checker-package-lint . nil)
        (env-allow-lsp . nil)
        (env-allow-whitespace-cleanup . nil)
        (env-allow-language-servers . nil)
        (env-allow-reformatters . nil)))

    ("~/src/emacs"
     . ((env-allow-syntax-checkers . t)
        (env-allow-syntax-checker-package-lint . t)
        (env-allow-lsp . t)
        (env-allow-whitespace-cleanup . t)
        (env-allow-language-servers . t)
        (env-allow-reformatters . t)))

    ("~/Sync/src"
     . ((env-allow-syntax-checkers . t)
        (env-allow-syntax-checker-package-lint . nil)
        (env-allow-lsp . t)
        (env-allow-whitespace-cleanup . t)
        (env-allow-language-servers . t)
        (env-allow-reformatters . t)))

    ("~/src/"
     . ((env-allow-syntax-checkers . t)
        (env-allow-syntax-checker-package-lint . nil)
        (env-allow-lsp . t)
        (env-allow-whitespace-cleanup . t)
        (env-allow-language-servers . t)
        (env-allow-reformatters . t))))
  "Alist mapping directories to a list of buffer-local variables.
Format: (DIRECTORY-PATH . ((VAR1 . VAL1) (VAR2 . VAL2) ...))"
  :type '(alist :key-type directory
                :value-type (alist :key-type symbol :value-type sexp))
  :group 'files)

(defun my-apply-custom-dir-variables ()
  "Apply directory-specific variables from `my-custom-dir-variables'."
  (when-let* ((path (or (buffer-file-name (buffer-base-buffer)) default-directory)))
    (catch 'break
      (dolist (entry my-custom-dir-variables)
        (when (file-in-directory-p path (expand-file-name (car entry)))
          (dolist (var-val (cdr entry))
            ;; This is the programmatic runtime equivalent of `setq-local`
            (set (make-local-variable (car var-val)) (cdr var-val)))
          (throw 'break t))))))

;;; .my-dir-locals.el

;; (require 'my-dir-locals)
;; (my-dir-locals-mode 1)

;;; dir-locals-trigger

;;; env- vars

(defvar env-deny-all nil
  "Deny all the allowed modes.")

(defvar env-allow-syntax-checker-package-lint nil
  "Allow the `package-lint' syntax checker.")

(defvar env-allow-syntax-checkers nil
  "Allow syntax checkers such as Flymake.")

(defvar env-allow-language-servers nil
  "Allow language server via dir-locals.")

(defvar env-allow-whitespace-cleanup nil
  "Allow deleting whitespace via dir-locals.")

(defvar env-allow-reformatters nil
  "Non-nil allows directory-local configuration of code reformatters.")

(defvar env-allow-lsp nil
  "Non-nil allows directory-local configuration of code reformatters.")

;;; Conditional code checker/reformatter

(defun my-code-checker-get-buffer ()
  "Get the current buffer."
  (or
   ;; NOTE: It is not necessary to enable these in org-src or edit indirect.
   ;; (and (fboundp 'org-src-edit-buffer-p)
   ;;      (fboundp 'org-src-source-buffer)
   ;;      (org-src-edit-buffer-p)
   ;;      (when-let* ((new-buffer (org-src-source-buffer)))
   ;;        (when (buffer-live-p new-buffer)
   ;;          (with-current-buffer new-buffer
   ;;            (current-buffer)))))
   ;; (and (bound-and-true-p edit-indirect--overlay)
   ;;      (buffer-live-p edit-indirect--overlay)
   ;;      (overlay-buffer edit-indirect--overlay))
   (buffer-base-buffer)
   (current-buffer)))

;;; Flymake

(defun my-code-checker-and-reformatter-ignore-p ()
  "Files where modes like Flymake and Apheleia are disabled."
  (when-let* ((buffer (my-code-checker-get-buffer))
              (file-name (buffer-file-name buffer))
              (base-name (file-name-nondirectory file-name)))
    (or (string= base-name "make.conf") ; Gentoo
        (string= base-name "PKGBUILD")
        (string= base-name ".dir-locals.el")
        (string= base-name ".my-dir-locals.el")
        (string= base-name "straight-profile.el")
        (string-suffix-p ".ebuild" base-name))))

;; Evaluate .my-dir-locals.el

(defvar my-conditional-modes-verbose t)

(defvar-local my-conditional-modes--checked nil)

;; Write the manual logic
(defun my-evaluate-dir-locals ()
  "Manually check variables and enable modes."
  (when (and (not my-conditional-modes--checked)
             (not (eq major-mode 'fundamental-mode)))
    (setq my-conditional-modes--checked t)
    (when-let* ((file-name (or buffer-file-name
                               (buffer-file-name (buffer-base-buffer)))))
      (when my-conditional-modes-verbose
        (message "[CONDITIONAL-MODES] Check %S (%S)" file-name major-mode))
      (my-apply-custom-dir-variables)

      ;; Synchronously verify so-long conditions using the public API.
      ;; `after-change-major-mode-hook` runs before `global-so-long-mode` checks
      ;; the file, so we manually evaluate the predicate to catch long lines
      ;; before initializing heavy tools like Eglot.
      (when (and (bound-and-true-p global-so-long-mode)
                 (not (bound-and-true-p so-long-detected-p))
                 (boundp 'so-long-target-modes)
                 (or (eq so-long-target-modes t)
                     (if (listp so-long-target-modes)
                         (apply #'derived-mode-p so-long-target-modes)
                       (derived-mode-p so-long-target-modes)))
                 (boundp 'so-long-predicate)
                 (fboundp so-long-predicate))
        (save-match-data
          (when (funcall so-long-predicate)
            (so-long))))

      (when (and my-conditional-modes-verbose
                 (not (or (bound-and-true-p so-long-detected-p)
                          (derived-mode-p 'so-long-mode)
                          (bound-and-true-p so-long-minor-mode))))
        (message "[CONDITIONAL-MODES] DETECTED SO-LONG: %S" (buffer-name)))

      (let ((buffer-name (buffer-name)))
        (when (and
               (not (or
                     (bound-and-true-p so-long-detected-p)
                     (derived-mode-p 'so-long-mode)
                     (bound-and-true-p so-long-minor-mode)))
               (not env-deny-all)
               (not (or (string-prefix-p " " buffer-name)
                        (string-prefix-p "*" buffer-name))))
          (when-let* ((base-name (when file-name
                                   (file-name-nondirectory file-name))))
            (when (and (fboundp 'dtrt-indent-mode)
                       (file-in-directory-p file-name "~/src/forks"))
              (dtrt-indent-mode 1))
            (when env-allow-lsp
              ;; All modes
              (when (and (fboundp 'eglot-ensure)
                         (derived-mode-p 'python-mode 'python-ts-mode))
                (when (treesit-parser-list)
                  ;; This is to avoid redundant semantic highlighting, disabling
                  ;; Eglot's :semanticTokensProvider is reasonable when Tree-sitter
                  ;; is already providing semantic fontification.
                  (make-local-variable 'eglot-ignored-server-capabilities)
                  (add-to-list 'eglot-ignored-server-capabilities
                               :semanticTokensProvider))
                (eglot-ensure)))

            ;; Formatters
            (when env-allow-reformatters
              ;; All modes
              (when (and (fboundp 'apheleia-mode)
                         (derived-mode-p 'python-mode
                                         'python-ts-mode
                                         'bash-ts-mode
                                         'sh-mode
                                         'yaml-mode
                                         'yaml-ts-mode))
                (apheleia-mode 1))

              ;; Elisp
              (when (and (derived-mode-p 'emacs-lisp-mode)
                         (fboundp 'aggressive-indent-mode))
                (aggressive-indent-mode 1)))

            ;; Flymake
            (when (and (fboundp 'flymake-mode)
                       (not (my-code-checker-and-reformatter-ignore-p)))
              (when (and env-allow-syntax-checker-package-lint
                         ;; TODO add exceptions like these to the .my-dir-locals.el
                         (not (or (file-in-directory-p file-name "~/src/emacs/lightemacs")
                                  (file-in-directory-p file-name "~/src/dotfiles/jc-dev")
                                  (string= base-name "init.el")
                                  (string= base-name "early-init.el"))))
                (add-hook 'flymake-diagnostic-functions 'package-lint-flymake nil t))

              (flymake-mode 1))

            ;; Stripspace
            (when (and (bound-and-true-p env-allow-whitespace-cleanup)
                       (fboundp 'stripspace-local-mode))
              (stripspace-local-mode 1))))))))

;; Attach your logic to the trigger hook
(add-hook 'after-change-major-mode-hook #'my-evaluate-dir-locals 80)

;;; Provide

(provide 'mod-conditional-modes)

;;; mod-conditional-modes.el ends here

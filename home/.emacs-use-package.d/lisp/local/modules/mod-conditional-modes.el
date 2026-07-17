;;; mod-conditional-modes.el --- mod-conditional-modes -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;;; .my-dir-locals.el

(require 'my-dir-locals)
(my-dir-locals-mode 1)

(require 'dir-locals-trigger)
(dir-locals-trigger-mode 1)

(dir-locals-trigger-defvar env-deny-all nil
  "Deny all the allowed modes.")

(dir-locals-trigger-defvar env-allow-syntax-checker-package-lint nil
  "Allow the `package-lint' syntax checker.")

(dir-locals-trigger-defvar env-allow-syntax-checkers nil
  "Allow syntax checkers such as Flymake.")

(dir-locals-trigger-defvar env-allow-language-servers nil
  "Allow language server via dir-locals.")

(dir-locals-trigger-defvar env-allow-whitespace-cleanup nil
  "Allow deleting whitespace via dir-locals.")

(dir-locals-trigger-defvar env-allow-reformatters nil
  "Non-nil allows directory-local configuration of code reformatters.")

;;; Conditional code checker/reformatter

(defun my-code-checker-get-buffer ()
  "Docstring."
  (or (and (fboundp 'org-src-edit-buffer-p)
           (fboundp 'org-src-source-buffer)
           (org-src-edit-buffer-p)
           (when-let* ((new-buffer (org-src-source-buffer)))
             (when (buffer-live-p new-buffer)
               (with-current-buffer new-buffer
                 (current-buffer)))))
      ;; TODO
      ;; (bound-and-true-p edit-indirect--overlay)
      (buffer-base-buffer)
      (current-buffer)))

;;; Flymake

(defun my-code-checker-and-reformatter-ignore-p ()
  "Files where modes like Flymake and Apheleia are disabled."
  (let* ((buffer (my-code-checker-get-buffer))
         (file-name (buffer-file-name buffer))
         (base-name (when file-name
                      (file-name-nondirectory file-name))))
    ;; (and (not (or
    ;;            (file-in-directory-p file-name "~/src")
    ;;            (not (file-in-directory-p file-name "~/src/forks"))
    ;;            (not (file-in-directory-p file-name "~/src/local/emacs-worktrees"))
    ;;            (not (file-in-directory-p file-name "~/src/other"))
    ;;            (not (file-in-directory-p file-name tmpedit-dir)))))
    (or (string= base-name "make.conf") ; Gentoo
        (string= base-name "PKGBUILD")
        (string= base-name ".dir-locals.el")
        (string= base-name ".my-dir-locals.el")
        (string= base-name "straight-profile.el")
        (string-suffix-p ".ebuild" file-name)

        ;; TODO
        ;; (not (string-match-p "/lisp/local/" filename))
        ;; (not (string-prefix-p "le-" basename))
        )))

;; Flymake
;;
;; This mode is loaded automatically by many other modes such as
;; `emacs-lisp-mode'. That's why this advice helps a lot controling it.
(defun my-flymake-execution-only-when-code-checker-allowed (orig-fun &rest args)
  "Execute ORIG-FUN with ARGS only if it is allowed.
This function is intended for use as :around advice."
  (when (and (bound-and-true-p env-allow-syntax-checkers)
             (not (my-code-checker-and-reformatter-ignore-p)))
    (apply orig-fun args)))

(with-eval-after-load 'le-flymake
  (advice-add 'flymake-mode :around
              #'my-flymake-execution-only-when-code-checker-allowed))

;;; dir-config

;; (lightemacs-use-package dir-config
;;   :init
;;   (setq dir-config-file-names '(".dir-settings.el"))
;;   (setq dir-config-allowed-directories '("~/src"))
;;   (dir-config-mode 1))

;; Evaluate .my-dir-locals.el

;; Write the manual logic
(defun my-evaluate-dir-locals ()
  "Manually check variables and enable modes."
  (let ((buffer-name (buffer-name)))
    (when (and (not env-deny-all)
               (not (or (string-prefix-p " " buffer-name)
                        (string-prefix-p "*" buffer-name))))
      (when-let* ((file-name (buffer-file-name (buffer-base-buffer))))
        (when env-allow-reformatters
          ;; All modes
          (when (and (fboundp 'apheleia-mode)
                     (or (derived-mode-p 'python-mode)
                         (derived-mode-p 'python-ts-mode)
                         (derived-mode-p 'yaml-mode)
                         (derived-mode-p 'yaml-ts-mode)))
            (apheleia-mode env-allow-reformatters))

          ;; Elisp
          (when (and (derived-mode-p 'emacs-lisp-mode)
                     (fboundp 'aggressive-indent-mode))
            (aggressive-indent-mode env-allow-reformatters)))

        (let ((code-checker-ignore-p (my-code-checker-and-reformatter-ignore-p)))
          (unless code-checker-ignore-p
            (when env-allow-syntax-checker-package-lint
              (add-hook 'flymake-diagnostic-functions 'package-lint-flymake nil t))

            (when (fboundp 'flymake-mode)
              (flymake-mode (bound-and-true-p env-allow-syntax-checkers))
              ;; (when (/= (bound-and-true-p flymake-mode)
              ;;           (bound-and-true-p env-allow-syntax-checkers))
              ;;   (flymake-mode (bound-and-true-p env-allow-syntax-checkers)))
              )))

        (when (and (bound-and-true-p env-allow-whitespace-cleanup)
                   (fboundp 'stripspace-local-mode))
          (stripspace-local-mode 1))))))

;; Attach your logic to the trigger hook
(add-hook 'dir-locals-trigger-hook #'my-evaluate-dir-locals)

;;; Provide

(provide 'mod-conditional-modes)

;;; mod-conditional-modes.el ends here

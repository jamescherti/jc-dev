;;; mod-flymake.el --- mod-flymake -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;;; Require

(eval-and-compile
  (require 'lightemacs-use-package))
(require 'my-defun)

;;; Flymake defaults

(setq flymake-start-on-flymake-mode (when (> (num-processors) 8)
                                      t))

(setq flymake-no-changes-timeout (if (> (num-processors) 8)
                                     0.5
                                   1))

(setq flymake-start-on-save-buffer t)

;; Suppress the display of Flymake error counters when there are no errors.
(setq flymake-suppress-zero-counters t)

;;; Flymake

(defun mod-flymake--limit-package-lint-flymake-setup-a (orig-fn &rest args)
  "Limit package lint flymake setup.
ORIG-FN and ARGS is the functions and its arguments."
  ;;(setq-local package-lint-main-file
  ;;            (file-name-nondirectory
  ;;             (buffer-file-name (buffer-base-buffer))))
  (when (my-code-checker-allowed-p)
    (let* ((filename (buffer-file-name (buffer-base-buffer)))
           (basename (if filename (file-name-nondirectory filename) "")))
      (when (and filename
                 (not (string-match-p "/lisp/local/" filename))
                 (not (string= basename ".dir-locals.el"))
                 (not (string= basename ".dir-config.el"))
                 (not (string= basename ".dir-settings.el"))
                 (not (string= basename "init.el"))
                 (not (string= basename "early-init.el"))
                 (not (string-prefix-p "le-" basename)))
        (apply orig-fn args)))))

(with-eval-after-load 'le-package-lint-flymake
  (advice-add 'package-lint-flymake-setup :around
              #'mod-flymake--limit-package-lint-flymake-setup-a))

;; ignore pckage lint: The word "emacs" is redundant in Emacs package names.

(defun mod-flymake--package-lint-ignore (orig-fun desc)
  "Bypass the \"emacs\" name check for files in a specific directory.
ORIG-FUN is the advised function.  DESC is the package description struct."
  (let ((target-dir (expand-file-name "~/src/emacs/lightemacs"))
        (file-name (buffer-file-name (buffer-base-buffer))))
    (when file-name
      (if (and (buffer-file-name (buffer-base-buffer))
               (buffer-file-name (buffer-base-buffer))
               (file-in-directory-p file-name target-dir))
          ;; Condition met: return nil to skip the original function
          nil
        ;; Condition not met: execute the original function
        (funcall orig-fun desc)))))

(with-eval-after-load 'package-lint
  ;; Apply the :around advice to the specific package-lint function
  (advice-add 'package-lint--check-package-summary :around
              #'mod-flymake--package-lint-ignore)

  (advice-add 'package-lint--check-no-emacs-in-package-name :around
              #'mod-flymake--package-lint-ignore))

;;; Flymake ansible-lint

(defun my-setup-flymake-ansible-lint-project-dir ()
  "Configure `flymake-ansible-lint' to use the project or VC root."
  (when (fboundp 'project-root)
    (setq-local flymake-ansible-lint-args
                (append flymake-ansible-lint-args
                        (let* ((project (project-current nil))
                               (project-root (when project
                                               (project-root project)))
                               (vc-root (unless project-root
                                          (vc-root-dir))))
                          (cond
                           (project-root
                            (list "--project-dir"
                                  (expand-file-name project-root)))

                           (vc-root
                            (list "--project-dir"
                                  (expand-file-name project-root)))

                           (t
                            nil)))))))


(add-hook 'ansible-mode-hook #'my-setup-flymake-ansible-lint-project-dir)

(setq flymake-ansible-lint-args
      '("--offline"
        "--skip=yamllint"
        "--skip-list"
        "run-once[play],no-free-form,trailing-whitespace,yaml[line-length]"))

(setq flymake-ansible-lint-auto-project-dir t)

;; (unless noninteractive
;;   (with-eval-after-load 'flymake
;;     (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
;;     (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)))

;; Why does it start with elisp? TODO

;;; Flymake bashate
(lightemacs-use-package flymake-bashate
  :commands flymake-bashate-setup
  :init
  (setq flymake-bashate-max-line-length 80)
  ;; To make bashate ignore specific Bashate rules, such as E003 (ensure all
  ;; indents are a multiple of 4 spaces) and E006 (check for lines longer than
  ;; 79 columns), set the following variable:
  ;; (flymake-bashate-ignore "E003,E006")
  ;;
  ;; E001: trailing whitespace
  (setq flymake-bashate-ignore "E003,E001")

  (defun my-setup-flymake-bashate ()
    (when (my-code-checker-allowed-p)
      (flymake-bashate-setup)))

  (add-hook 'bash-ts-mode-hook 'my-setup-flymake-bashate)
  (add-hook 'sh-mode-hook 'my-setup-flymake-bashate))

;;; Flymake Elisp done

(defvar lightemacs-flymake--setup-elisp-done nil
  "Non-nil once `elisp-flymake-byte-compile-load-path' has been extended.")

(defun lightemacs-flymake-initialize-elisp-path ()
  "Extend `elisp-flymake-byte-compile-load-path' with the current `load-path'.
This function ensures that the Flymake subprocess inherits the session's library
environment for accurate linting."
  (with-eval-after-load 'elisp-mode
    (unless lightemacs-flymake--setup-elisp-done
      (setq elisp-flymake-byte-compile-load-path
            (append elisp-flymake-byte-compile-load-path load-path))
      (setq lightemacs-flymake--setup-elisp-done t))))

(add-hook 'lightemacs-emacs-startup-hook
          #'lightemacs-flymake-initialize-elisp-path 99)

;;; Flymake

;; (progn
;;   (defun my/flymake-error-no-exit (_orig-fun text &rest args)
;;     "Override `flymake-error' to log the message without signaling an error.
;; ORIG-FUN, TEXT, ARGS as the same arguments as `flymake-error'."
;;     ;; Format the message like the original
;;     (let ((msg (apply #'format-message text args)))
;;       (flymake-log :error "%s" msg)
;;       ;; Instead of (error ...), just return the message
;;       msg))
;;
;;   (with-eval-after-load 'flymake
;;     (advice-add 'flymake-error :around #'my/flymake-error-no-exit)))

;; (setq flymake-fringe-indicator-position 'left-fringe)
;; (setq flymake-mode-line-format
;;       '("" flymake-mode-line-exception flymake-mode-line-counters))
;; (setq flymake-mode-line-counter-format
;;       '("" flymake-mode-line-error-counter
;;         flymake-mode-line-warning-counter
;;         flymake-mode-line-note-counter ""))
;; (setq flymake-show-diagnostics-at-end-of-line nil)
;; (setq flymake-indicator-type 'margins)

;; (setq flymake-margin-indicators-string
;;       ;; (flymake-show-diagnostics-at-end-of-line 'short)
;;       ;; Alternatives: », E, W, i, !, ?, ⚠️)
;;       `((error "!" compilation-error)
;;         (warning "?" compilation-warning)
;;         (note "i" compilation-info)))

;; TODO removed recently
;; (setq flymake-proc-compilation-prevents-syntax-check t)

;; (use-package flymake-markdownlint
;;   :commands flymake-markdownlint-setup
;;   ;; :hook
;;   ;; (markdown-mode . #'flymake-markdownlint-setup)
;;   :init
;;   (add-hook 'markdown-mode-hook #'flymake-markdownlint-setup))

;;; Flymake fixes

(defun my-flymake-proc-legacy-safe-advice (orig-fun &rest args)
  "Call `flymake-proc-legacy-flymake' safely, ignoring missing init function.

ORIG-FUN is the original `flymake-proc-legacy-flymake` function.
ARGS are the arguments passed to ORIG-FUN.

If the error message contains \"find a suitable init function\", it is
ignored and logged as a warning. All other errors are re-raised."
  (condition-case err
      (apply orig-fun args)
    ((error)
     (let ((error-message (error-message-string err))
           (inhibit-message t))
       (message "[WARNING] Flymake: %s: %s"
                (buffer-file-name (buffer-base-buffer))
                error-message))
     ;; (if (string-match-p "find a suitable init function"
     ;;                     error-message)
     ;;     (let ((inhibit-message t))
     ;;       (message "[WARNING] Flymake: %s: %s"
     ;;                buffer-file-name
     ;;                error-message))
     ;;   (signal (car err) (cdr err)))
     )))

(with-eval-after-load 'flymake-proc
  (advice-add 'flymake-proc-legacy-flymake :around
              #'my-flymake-proc-legacy-safe-advice))

;;; Provide

(provide 'mod-flymake)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-flymake.el ends here

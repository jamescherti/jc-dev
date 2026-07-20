;;; pkg-company.el --- Company -*- lexical-binding: t -*-

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

;; (use-package company-prescient
;;   :after (prescient company)
;;   :commands company-prescient-mode
;;   :config
;;   (company-prescient-mode 1))

;; (use-package consult-company
;;   :after company
;;   :config
;;   (evil-define-key 'insert 'global (kbd "C-S-SPC") #'consult-company))

(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package company
  :commands (company-complete
             company-complete-common
             global-company-mode
             company-mode)
  :init
  ;; Disable automatic popup
  (setq company-idle-delay nil)

  (setq company-tooltip-limit 6) ; Limit on how many options to display
  (setq company-tooltip-minimum-width 25)
  (setq company-tooltip-align-annotations t) ; Align annotations to the right
  (setq company-dabbrev-code-completion-styles t) ; use `completion-styles'

  (setq company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance
                               company-sort-prefer-same-case-prefix))

  ;; TODO Try these
  ;; (setq company-frontends '(company-childframe-unless-just-one-frontend
  ;;                           company-preview-if-just-one-frontend
  ;;                           company-echo-metadata-frontend))

  (setq company-frontends '(; Frontends
                            company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend
                            company-echo-metadata-frontend))

  ;; Legacy
  (setq company-async-timeout 20)
  (setq company-auto-complete nil)
  (setq company-dabbrev-code-everywhere nil)  ;; Completion from comments
  (setq company-dabbrev-code-ignore-case nil)
  (setq company-dabbrev-code-other-buffers nil)
  (setq company-dabbrev-downcase nil) ;; Don't automatically downcase completions
  (setq company-dabbrev-ignore-case nil) ;; Don't ignore case when completing
  (setq company-dabbrev-other-buffers nil)
  (setq company-etags-ignore-case nil)
  (setq company-files-exclusions '(".git/" ".DS_Store"))
  (setq company-keywords-ignore-case nil)
  (setq company-minimum-prefix-length 3)  ;; Show the menu after one key press
  (setq company-require-match nil)  ;; Allow free typing
  (setq company-selection-wrap-around nil) ;; Wrap around to beginning when you hit bottom of suggestions
  (setq company-show-numbers nil)   ;; Show numbers behind options
  (setq company-tooltip-annotation-padding 1)
  (setq company-tooltip-flip-when-above nil)
  (setq company-tooltip-width-grow-only 10)


  ;;--------------------------------------------------------------------------
  ;; Company
  ;;--------------------------------------------------------------------------
  ;; TODO
  ;; (when (string= completion-system-choice "company")
  ;;   ;; File selection
  ;;   ;; --------------
  ;;   (defun my-company-select-previous ()
  ;;     (interactive)
  ;;     (let ((company-selection 1))
  ;;       (company-select-previous)))
  ;;   (add-hook 'evil-mode-hook #'(lambda()
  ;;                                 ;; (evil-define-key 'insert 'global (kbd "C-x f") #'company-files)
  ;;                                 (evil-define-key 'insert 'global (kbd "C-x C-f") #'company-files)))
  ;;
  ;;   ;; Dabbrev code
  ;;   ;; ------------
  ;;   (defun my-company-dabbrev (backward)
  ;;     "Trigger company-dabbrev-code in backward direction."
  ;;     (interactive)
  ;;     ;; sh-mode and bash-ts-mode:
  ;;     ;; Variables inside string such as "$VAR" cannot be completed
  ;;     (cond
  ;;      ((and (derived-mode-p 'prog-mode) ; Prog mode
  ;;            (not (derived-mode-p 'sh-mode))
  ;;            (not (derived-mode-p 'bash-ts-mode)))
  ;;       (let ((company-backends '(company-dabbrev-code
  ;;                                 company-dabbrev)))
  ;;         (company-complete)))
  ;;
  ;;      (t ; Other
  ;;       (if backward
  ;;           (evil-complete-previous)
  ;;         (evil-complete-next)))))
  ;;
  ;;   (defun my-company-dabbrev-code-backwards ()
  ;;     "Trigger company-dabbrev-code in backward direction."
  ;;     (interactive)
  ;;     (my-company-dabbrev t))
  ;;
  ;;   (defun my-company-dabbrev-code-forward ()
  ;;     "Trigger company-dabbrev-code in backward direction."
  ;;     (interactive)
  ;;     (my-company-dabbrev nil))
  ;;
  ;;   (with-eval-after-load 'evil
  ;;     (evil-define-key 'insert 'global (kbd "C-n") #'my-company-dabbrev-code-forward)
  ;;     (evil-define-key 'insert 'global (kbd "C-p") #'my-company-dabbrev-code-backwards)))

  ;; (company-capf company-dabbrev) represents a grouping of CAPFs.
  ;; `company-files' shows results of its own.
  (setq company-backends '((company-capf company-dabbrev) company-files))

  (defun my-company-prog-backends ()
    "Set `prog-mode' company backends."
    (setq-local company-backends '((company-capf company-dabbrev-code)
                                   company-files)))

  (add-hook 'prog-mode-hook #'my-company-prog-backends)

  (defun my-company-text-backends ()
    "Set `text-mode' company backends."
    (setq-local company-backends '((company-dabbrev
                                    company-ispell
                                    company-capf)
                                   company-files)))

  (add-hook 'text-mode-hook #'my-company-text-backends)

  ;; (setq-default company-backends '(
  ;;                                  ;; company-semantic: This backend
  ;;                                  ;; provides code completions using the
  ;;                                  ;; Semantic Bovinator, which is a
  ;;                                  ;; component of CEDET (Collection of
  ;;                                  ;; Emacs Development Environment Tools).
  ;;                                  ;; It offers context-aware code
  ;;                                  ;; completions in various programming
  ;;                                  ;; languages.
  ;;                                  company-semantic
  ;;
  ;;                                  ;; Auto complete for cmake
  ;;                                  ;; company-cmake
  ;;
  ;;                                  ;; company-mode backend using
  ;;                                  ;; completion-at-point-functions.
  ;;                                  company-capf
  ;;
  ;;                                  ;; company-clang: This backend provides
  ;;                                  ;; code completions for C and C++ code
  ;;                                  ;; using the Clang compiler. It offers
  ;;                                  ;; accurate and context-aware completions
  ;;                                  ;; for C/C++ code.
  ;;                                  ;; company-clang
  ;;
  ;;                                  ;; company-files: This backend provides
  ;;                                  ;; file name completions in the current
  ;;                                  ;; directory and subdirectories. It's
  ;;                                  ;; useful for quickly opening or
  ;;                                  ;; inserting file paths.
  ;;                                  company-files
  ;;
  ;;                                  ;; company-mode completion backend for Emacs Lisp.
  ;;                                  ;; company-elisp
  ;;
  ;;                                  (
  ;;                                   ;; company-dabbrev-code: This backend
  ;;                                   ;; offers dynamic abbreviations for code
  ;;                                   ;; symbols in the current buffer.
  ;;                                   company-dabbrev-code
  ;;
  ;;                                   ;; company-gtags and company-etags: These
  ;;                                   ;; backends provide code completions
  ;;                                   ;; based on global tags generated by
  ;;                                   ;; tools like GNU Global or etags.
  ;;                                   company-gtags
  ;;                                   company-etags
  ;;
  ;;                                   ;; company-keywords: This backend
  ;;                                   ;; provides keyword completions, which
  ;;                                   ;; can be useful for various programming
  ;;                                   ;; languages.
  ;;                                   company-keywords
  ;;
  ;;                                   )
  ;;
  ;;                                  ;; company-dabbrev: This is another
  ;;                                  ;; dynamic abbreviation backend, similar
  ;;                                  ;; to company-dabbrev-code. It offers
  ;;                                  ;; completions based on words in the
  ;;                                  ;; current buffer.
  ;;                                  company-dabbrev
  ;;
  ;;                                  ;; BBDB is the Insidious Big Brother Database for GNU Emacs.
  ;;                                  ;; It provides an address book for email and snail mail addresses,
  ;;                                  ;; phone numbers and the like.  It can be linked with various Emacs mail
  ;;                                  ;; clients (Message and Mail mode, Rmail, Gnus, MH-E, Mu4e, VM, Notmuch,
  ;;                                  ;; and Wanderlust).  BBDB is fully customizable.
  ;;                                  ;; company-bbdb
  ;;
  ;;                                  ;; company-mode completion backend using Ispell.
  ;;                                  ;; company-ispell
  ;;
  ;;                                  ;; Try to expand text before point, using multiple methods.
  ;;                                  ;; NOTE: Seems useless.
  ;;                                  hippie-expand))

  ;; :config
  ;;
  ;; (define-key company-active-map (kbd "C-h") #'backward-delete-char)
  ;; (define-key company-active-map (kbd "C-k") #'company-show-doc-buffer)
  ;; (define-key company-active-map (kbd "C-w") #'evil-delete-backward-word)
  )

(provide 'mod-company)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; pkg-company.el ends here

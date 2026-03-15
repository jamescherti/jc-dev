;;; config.el --- config -*- no-byte-compile: t; lexical-binding: t -*-
;;

;;; Commentary:

;;; Code:

;; Author: James Cherti
;; URL: https://github.com/jamescherti/jc-dev
;;
;; Distributed under terms of the MIT license.
;;
;; Copyright (C) 2004-2026 James Cherti
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; (defun my-load-from-elc-cache-advice (orig-fun file &optional noerror nomessage nosuffix must-suffixes)
;;   "Advice to load .elc from `my-elc-cache-directory' if available and newer."
;;   (let* ((found-file (if (file-name-absolute-p file)
;;                          file
;;                        (locate-file file load-path (unless nosuffix (get-load-suffixes)))))
;;          (cached-elc (when found-file
;;                        (let ((rel (replace-regexp-in-string "^/" "" (expand-file-name found-file))))
;;                          (expand-file-name (concat (file-name-sans-extension rel) ".elc") my-elc-cache-directory)))))
;;     (if (and cached-elc
;;              (file-exists-p cached-elc)
;;              (file-newer-than-file-p cached-elc found-file))
;;         ;; Load the cached .elc file
;;         (apply orig-fun cached-elc noerror nomessage nosuffix must-suffixes)
;;       ;; Fallback to standard loading
;;       (apply orig-fun file noerror nomessage nosuffix must-suffixes))))

;; Advise `load' to check the cache directory first
;; (advice-add 'load :around #'my-load-from-elc-cache-advice)

;;; Add some paths (copy paste)
(require 'lightemacs)
;; (add-to-list 'load-path (expand-file-name "modules/"
;;                                           lightemacs-local-directory))
;; TOTO replace them with push
;; (add-to-list 'load-path lightemacs-local-modules-directory)
;; (add-to-list 'load-path lightemacs-core-directory)
;; (add-to-list 'load-path lightemacs-modules-directory)

;;; External config

(message "LOAD: config.el")

(load (expand-file-name "~/.config.el") :no-error :nosuffix)

;;; Optimization

(setq site-run-file nil)
(setq inhibit-default-init t)

;;; Ido

;; (setq ido-everywhere t
;;       ido-enable-last-directory-history t
;;       ido-max-work-directory-list 30
;;       ido-max-work-file-list 50
;;       ido-max-prospects 8
;;       ido-confirm-unique-completion nil
;;       ido-create-new-buffer 'always
;;       ido-use-virtual-buffers t)
;; (add-hook 'lightemacs-after-init-hook #'ido-mode)

;;; Emacs

(setq lightemacs-load-compiled-init-files t)

;; (setq debug-on-error t)
(when debug-on-error
  ;; TODO le-default config?
  (push 'search-failed debug-ignored-errors))

(setq enable-dir-local-variables t)
(setq enable-local-variables :safe)

(setq native-comp-jit-compilation nil)
(setq native-comp-deferred-compilation nil)
(setq package-native-compile nil)

(setq lightemacs-theme-default-font "Iosevka Term-13")
(setq lightemacs-saveplace-recenter-after-find-file t)

;;; Lightemacs

(setq minimal-emacs-frame-title-format "Devemacs")

(setq lightemacs-dired-omit-parent-directory t)
(setq lightemacs-cycle nil)

(setq native-comp-jit-compilation nil)

(setq lightemacs-modules '(le-compile-angel
                           le-pathaction

                           le-autorevert

                           le-markdown-mode
                           le-org

                           le-flavor-micro
                           le-group-evil
                           le-persist-text-scale

                           ;; le-undo-fu
                           ;; le-undo-fu-session

                           ;; le-consult-dir
                           le-consult
                           ;; le-embark-consult
                           ;; le-embark
                           ;; le-wgrep
                           le-vertico
                           le-orderless

                           ;; TODO this module does not exist
                           ;; le-pathaction

                           ;; le-evil-visualstar
                           ;; le-git-modes
                           le-xclip))

(setq lightemacs-theme-name 'tomorrow-night-deepblue)
(setq lightemacs-theme-package 'tomorrow-night-deepblue-theme)

(nconc auto-mode-alist '(("/COMMIT_EDITMSG\\'" . diff-mode)
                         ("/git-rebase-todo\\'" . diff-mode)))

;;; Changes to evil-collection

;; This snippet safely overrides the default evil-collection keybindings for
;; diff-mode by delaying execution until after the package loads. It
;; specifically targets the 'motion' state map and unbinds the 'd' key by
;; assigning it a nil value, which cancels out the 'diff-hunk-kill' command.
;; This ensures the 'd' key reverts to its standard Evil behavior for deletion
;; operations, preventing accidental deletions of diff hunks while you navigate.

;; (add-hook 'diff-mode-hook
;;           (lambda ()
;;             (evil-define-key 'motion 'local (kbd "d") nil)
;;             (evil-define-key 'motion 'local (kbd "D") nil)
;;             )
;;           t)

(with-eval-after-load 'evil-collection
  (add-hook 'evil-collection-setup-hook
            (lambda (mode &rest _rest)
              (when (eq mode 'diff-mode)
                (evil-define-key 'motion diff-mode-map "d" nil)
                (evil-define-key 'motion diff-mode-map "D" nil)))))

;; (with-eval-after-load 'diff-mode
;;   (evil-define-key 'motion diff-mode-map "D" nil)
;;   (evil-define-key 'motion diff-mode-map "d" nil))
;;
;; (with-eval-after-load 'evil-collection-diff-mode
;;   (evil-define-key 'motion diff-mode-map "D" nil)
;;   (evil-define-key 'motion diff-mode-map "d" nil))


;; (defun lightemacs-user-early-init ()
;;   "Run this after early-init."
;;   (message "[INIT] user pre-early-init")
;;   )

;; (defun lightemacs-user-init ()
;;   "Run this after init."
;;   (message "[INIT] user init"))

;; (defun lightemacs-user-post-early-init ()
;;   "Run this after early-init."
;;   (message "[INIT] user early init"))

;;; config.el ends here

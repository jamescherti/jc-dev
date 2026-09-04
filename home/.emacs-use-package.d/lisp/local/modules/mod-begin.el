;;; mod-begin.el --- mod-begin -*- lexical-binding: t -*-

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

;; mod-begin

;;; Code:

;;; Require

(eval-and-compile
  (require 'lightemacs-use-package))
(require 'my-defun)

;;; scratch

(defvar my-scratch-buffer-created-hook nil
  "Hook run once when the *scratch* buffer is created.")

(defvar-local my-scratch-init-done nil
  "Non-nil if the current scratch buffer has been initialized.")

(defun my-advice-scratch-buffer-create (orig-fn &rest args)
  "Advice when the scratch buffer is created.
ORIG-FN is the original function being advised (`get-scratch-buffer-create`).
ARGS are the arguments passed to the original function."
  (let ((result (apply orig-fn args))
        (scratch-buf (get-buffer "*scratch*")))
    (when (buffer-live-p scratch-buf)
      (with-current-buffer scratch-buf
        (unless my-scratch-init-done
          (setq my-scratch-init-done t)
          (run-hooks 'my-scratch-buffer-created-hook))))
    result))

(advice-add 'get-scratch-buffer-create :around #'my-advice-scratch-buffer-create)

;;; Local modes instead of global ones

(setq lightemacs-diff-hl-flydiff-global-target-hooks nil
      lightemacs-diff-hl-local-target-hooks nil
      lightemacs-diff-hl-global-target-hooks nil)

(setq lightemacs-electric-pair-local-target-hooks nil
      lightemacs-electric-pair-global-target-hooks nil)
(add-hook-text-editing-modes #'electric-pair-local-mode)
(add-hook 'my-scratch-buffer-created-hook 'electric-pair-local-mode)

;; I don't like it in the minibuffer, especially when searching for
;; things

(defun my-disable-electric-pair-in-evil ()
  "Temporarily remove `electric-pair-local-mode' from minibuffer setup."
  (if (memq this-command '(evil-ex
                           evil-ex-search-forward
                           evil-ex-search-backward
                           evil-search-forward
                           evil-search-backward))
      (electric-pair-local-mode -1)
    (electric-pair-local-mode 1)))

(add-hook 'minibuffer-setup-hook #'my-disable-electric-pair-in-evil t)

(setq lightemacs-evil-snipe-local-target-hooks nil
      lightemacs-evil-snipe-global-target-hooks nil)
(with-eval-after-load 'le-evil-snipe
  (add-hook-text-editing-modes 'evil-snipe-local-mode)
  (add-hook 'my-scratch-buffer-created-hook 'evil-snipe-local-mode)
  (add-hook 'minibuffer-setup-hook 'evil-snipe-local-mode))

(setq lightemacs-evil-surround-local-target-hooks nil
      lightemacs-evil-surround-global-target-hooks nil)
(with-eval-after-load 'le-evil-surround
  (add-hook-text-editing-modes 'evil-surround-mode)
  (add-hook 'my-scratch-buffer-created-hook 'evil-surround-mode)
  (add-hook 'minibuffer-setup-hook 'evil-surround-mode))

(setq lightemacs-corfu-local-target-hooks nil
      lightemacs-corfu-global-target-hooks nil)
;; This is enabled by `mod-conditional-modes'
(with-eval-after-load 'le-corfu
  (add-hook-text-editing-modes 'corfu-mode)
  (add-hook 'my-scratch-buffer-created-hook 'corfu-mode)
  (add-hook 'minibuffer-setup-hook 'corfu-mode))

;; (with-eval-after-load 'le-company
;;   (add-hook-text-editing-modes 'company-mode)
;;   (add-hook 'my-scratch-buffer-created-hook 'company-mode)
;;   ;; (add-hook 'minibuffer-setup-hook 'company-mode)
;;   )

;; NOTE: Doesn't work
;; (setq lightemacs-saveplace-target-hooks nil)
;; ;; (add-hook-text-editing-modes 'save-place-local-mode)
;; (with-eval-after-load 'saveplace
;;   (add-hook-text-editing-modes 'save-place-local-mode))

(setq lightemacs-undo-fu-session-local-target-hooks nil
      lightemacs-undo-fu-session-global-target-hooks nil)
(with-eval-after-load 'le-undo-fu-session
  (add-hook-text-editing-modes 'undo-fu-session-mode))

;; Started from mod-conditional-modes.el
(setq lightemacs-stripspace-local-target-hooks nil
      lightemacs-stripspace-global-target-hooks nil
      lightemacs-aggressive-indent-local-target-hooks nil
      lightemacs-aggressive-indent-global-target-hooks nil
      lightemacs-apheleia-local-target-hooks nil
      lightemacs-apheleia-global-target-hooks nil
      lightemacs-flymake-target-hooks nil
      lightemacs-dtrt-indent-global-target-hooks nil
      lightemacs-dtrt-indent-local-target-hooks nil
      lightemacs-buffer-terminator-target-hooks nil
      lightemacs-package-lint-flymake-target-hooks nil)

;;; Yasnippet

(setq lightemacs-yasnippet-global-target-hooks nil
      lightemacs-yasnippet-local-target-hooks nil)
(add-hook-text-editing-modes 'yas-minor-mode)
(add-hook 'my-scratch-buffer-created-hook 'yas-minor-mode)

(defun le-yasnippet-reload-if-empty ()
  "Reload all YASnippet snippets only if they are not already loaded."
  (when (and (fboundp 'yas-reload-all)
             (not (and (boundp 'yas--tables)
                       (hash-table-p yas--tables)
                       (> (hash-table-count yas--tables) 0))))
    (yas-reload-all)))

;; (defun le-yasnippet-delayed-reload-if-empty ()
;;   "Reload if empty."
;;   ;; Instead of calling (le-yasnippet-reload-if-empty) directly:
;;   (run-with-idle-timer 2 nil #'le-yasnippet-reload-if-empty))
;; (add-hook 'lightemacs-after-init-hook 'le-yasnippet-delayed-reload-if-empty)

(add-hook 'lightemacs-after-init-hook 'le-yasnippet-reload-if-empty)

;;; Default modes that I disabled

(with-eval-after-load 'simple
  (remove-hook 'post-self-insert-hook #'blink-paren-post-self-insert-function))

;; Minibuffer-nonselected-mode: This mode highlights the minibuffer contents
;; using the minibuffer-nonselected face when an active minibuffer (such as a
;; recursive prompt or a background process) remains active after focus has
;; shifted to a different window.
;;
;; Utility: This feature is primarily intended for users who frequently utilize
;; recursive minibuffers. If your workflow seldom involves recursive editing,
;; the mode may offer limited benefit. If the visual feedback triggered by
;; window switching causes distractions or conflicts with your chosen theme, it
;; can be disabled safely without impacting core functionality.
(when (bound-and-true-p minibuffer-nonselected-mode)
  (minibuffer-nonselected-mode -1))
(setq-default minibuffer-nonselected-mode nil)

(when (bound-and-true-p global-eldoc-mode)
  (global-eldoc-mode -1))
(setq-default global-eldoc-mode nil)

(when (bound-and-true-p show-paren-mode)
  (show-paren-mode -1))
(setq-default show-paren-mode nil)

;; Disable Remote File Checks if Not Needed
(setq-default tramp-mode nil)
(when (bound-and-true-p windmove-mode)
  (windmove-mode -1))
(setq-default windmove-mode nil)

;; GPM mouse support is strictly for TTY consoles.
(when (bound-and-true-p gpm-mouse-mode)
  (gpm-mouse-mode -1))
(setq-default gpm-mouse-mode nil)

;; Useless for Evil users: This mode modifies minibuffer syntax tables for regex
;; navigation. Since Evil provides its own regex tools and operators that
;; operate independently of these minibuffer-specific highlighting side-effects,
;; this mode is redundant and can interfere with custom Evil keybindings.
(when (bound-and-true-p minibuffer-regexp-mode)
  (minibuffer-regexp-mode -1))
(setq-default minibuffer-regexp-mode nil)

;; In standard (vanilla) Emacs, you do not select text by shifting into a visual
;; mode. Instead, you drop an anchor called the "mark" by pressing C-SPC, and
;; then move your cursor. The text between the mark and your cursor becomes your
;; active selection.
;;
;; By default, Emacs uses `transient-mark-mode' to highlight this selection
;; visually, making it look like a standard modern text editor.
;;
;; If you use Evil (Vim bindings), this native highlighting gets in the way.
;; Evil handles text selection through its own Visual states (v, V, C-v). If
;; Emacs is also trying to highlight text in the background based on where your
;; last mark was dropped, the two systems create conflicting visual noise.
;;
;; Disabling `transient-mark-mode' stops Emacs from painting the screen with
;; highlights. Pressing C-SPC goes back to being a silent utility: it just drops
;; an invisible location bookmark that you can jump back to later, while you
;; leave all the actual visual text selection to Evil.
(with-eval-after-load 'simple
  (when (bound-and-true-p transient-mark-mode)
    (transient-mark-mode -1))
  (setq-default transient-mark-mode nil))

;;; Packages: use-package

;; (defun my-package-pin (package repository)
;;   (setq package-pinned-packages
;;         (assq-delete-all package package-pinned-packages))
;;   (add-to-list 'package-pinned-packages (list (cons package repository))))

(defun my-update-package-pinned-packages (pinned-packages)
  "Update `package-pinned-packages\=' with the entries in PINNED-PACKAGES.
This replaces existing entries that match the provided packages and appends
any new ones."
  (when (eq lightemacs-package-manager 'use-package)
    (setq package-pinned-packages (append pinned-packages
                                          (seq-remove
                                           (lambda (pkg)
                                             (assq (car pkg) pinned-packages))
                                           package-pinned-packages)))))

(when (eq lightemacs-package-manager 'builtin-package)
  (setq my-package-pinned-packages
        '((buffer-terminator             . "melpa")
          (dir-config                    . "melpa")
          (enhanced-evil-paredit         . "melpa")
          (outline-indent                . "melpa")
          (vim-tab-bar                   . "melpa")
          (persist-text-scale            . "melpa")
          (quick-sdcv                    . "melpa")
          (inhibit-mouse                 . "melpa")
          (stripspace                    . "melpa")
          (tomorrow-night-deepblue-theme . "melpa")
          (bufferfile                    . "melpa")
          (compile-angel                 . "melpa")
          (easysession                   . "melpa")
          (flymake-ansible-lint          . "melpa")
          (flymake-bashate               . "melpa")
          (buffer-guardian               . "melpa")

          (markdown-mode                 . "melpa")

          (dumb-jump                 . "melpa")
          ;; Latest
          (vterm                         . "melpa")

          (git-gutter                    . "melpa")

          (visual-fill-column            . "melpa")

          ;; lightemacs?
          (undo-fu                       . "melpa")
          (undo-fu-session               . "melpa")

          ;; To fix the window-start bug
          (apheleia                      . "melpa-stable")

          ;; 3 months ago
          ;; (gptel                         . "melpa")
          ))

  (my-update-package-pinned-packages my-package-pinned-packages))

;;; Lazy loader

;; For file-path-todo
(require 'mod-defun nil t)

;; (lightemacs-use-package lazy-loader
;;   :ensure nil
;;   :commands lazy-loader-mode
;;   :hook (lightemacs-after-init . lazy-loader-mode)
;;   :init
;;   (setq lazy-loader-verbose 'inhibit-message)
;;   (setq lazy-loader-files (delq nil
;;                                 (list (when (bound-and-true-p file-path-todo)
;;                                         file-path-todo))))
;;   (setq lazy-loader-modules '(aggressive-indent
;;                               apheleia
;;                               compile-angel
;;                               buffer-terminator
;;                               consult
;;                               corfu
;;                               ;; evil-snipe
;;                               shell-pop
;;                               vertico
;;                               ;; sh-script
;;                               ;; smie
;;                               ;; avy
;;                               diff-hl
;;                               dired
;;                               dired-filter
;;                               embark
;;                               embark-consult
;;                               enhanced-evil-paredit
;;                               evil
;;                               evil-collection
;;                               inhibit-mouse
;;                               kirigami
;;                               kirigami-evil
;;                               kirigami-jump
;;                               lazy-autorevert
;;                               org
;;                               org-capture
;;                               org-compat
;;                               org-cycle
;;                               org-element
;;                               org-element-ast
;;                               org-entities
;;                               org-faces
;;                               org-fold
;;                               org-fold-core
;;                               org-footnote
;;                               org-ibullets
;;                               org-ibullets-autoloads
;;                               org-id
;;                               org-indent
;;                               org-keys
;;                               org-list
;;                               org-loaddefs
;;                               org-macro
;;                               org-macs
;;                               org-pcomplete
;;                               org-persist
;;                               org-refile
;;                               org-src
;;                               org-table
;;                               org-version
;;                               outline
;;                               outline-indent
;;                               vterm
;;                               yasnippet
;;                               persist-text-scale))
;;   ;; (lazy-loader-buffers
;;   ;;  '(("*tmux*" .
;;   ;;     (lambda ()
;;   ;;       (let ((buf (get-buffer-create "*tmux*")))
;;   ;;         (with-current-buffer buf
;;   ;;           (vterm-mode)
;;   ;;
;;   ;;           (vterm-send-string "tmux-session -l emacs")
;;   ;;           (vterm-send-string "\n")
;;   ;;           (vterm-send-return))
;;   ;;         buf)))))
;;   )

;; Lazy loader report for new features

(defvar lazy-loader-initial-features nil
  "A copy of the features list captured right after Emacs initialization.")

(defun lazy-loader-save-initial-features ()
  "Capture the state of loaded features post-init."
  (setq lazy-loader-initial-features (copy-sequence features)))

;; Automatically capture features after the init file finishes loading
(add-hook 'after-init-hook #'lazy-loader-save-initial-features)

(defun lazy-loader-compare-features ()
  "Compare current features against the stored post-init version.
Opens a split window showing the added and removed features."
  (interactive)
  ;; Fallback for testing in the current session if Emacs wasn't restarted
  (unless lazy-loader-initial-features
    (when (y-or-n-p "Initial features not recorded. Snapshot current features as baseline? ")
      (lazy-loader-save-initial-features)))

  (if (not lazy-loader-initial-features)
      (message "Comparison canceled.")
    (let ((added (seq-remove (lambda (f)
                               (memq f lazy-loader-initial-features))
                             features))
          (removed (seq-remove (lambda (f)
                                 (memq f features))
                               lazy-loader-initial-features))
          (buf (get-buffer-create "*Feature Diff*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "=== Emacs Feature Diff Report ===\n\n")
          (insert (format "Initial features count: %d\n"
                          (length lazy-loader-initial-features)))
          (insert (format "Current features count: %d\n\n"
                          (length features)))

          (insert "--- Added Features (Loaded since init) ---\n")
          (if added
              (dolist (f (sort added #'string-lessp))
                (insert (format "%s\n" f)))
            (insert "  (None)\n"))

          (insert "\n--- Removed Features (Unloaded since init) ---\n")
          (if removed
              (dolist (f (sort removed #'string-lessp))
                (insert (format "%s\n" f)))
            (insert "  (None)\n"))
          (special-mode)))
      ;; Pop to the buffer, which naturally splits the window
      (pop-to-buffer buf))))

;;; Themes

(lightemacs-use-package modus-themes
  :no-require t)
(lightemacs-use-package ef-themes
  :no-require t)
(lightemacs-use-package doom-themes
  :no-require t)
(lightemacs-use-package tomorrow-night-deepblue-theme
  :no-require t)

(with-eval-after-load 'le-theme
  (when (fboundp 'lightemacs-theme-create-loader)
    (lightemacs-theme-create-loader "modus-operandi" 'modus-themes)
    (lightemacs-theme-create-loader "modus-operandi-tinted" 'modus-themes)
    (lightemacs-theme-create-loader "modus-operandi-tritanopia" 'modus-themes)
    (lightemacs-theme-create-loader "modus-operandi-deuteranopia" 'modus-themes)
    (lightemacs-theme-create-loader "modus-vivendi" 'modus-themes)
    (lightemacs-theme-create-loader "modus-vivendi-tinted" 'modus-themes)
    (lightemacs-theme-create-loader "modus-vivendi-tritanopia" 'modus-themes)
    (lightemacs-theme-create-loader "tango-dark" 'modus-themes)
    (lightemacs-theme-create-loader "tango" 'modus-themes)
    (lightemacs-theme-create-loader "tsdh-light" 'modus-themes)

    (lightemacs-theme-create-loader "tomorrow-night-deepblue"
                                    'tomorrow-night-deepblue-theme)

    ;; (lightemacs-theme-create-loader "gruvbox-light-soft" 'gruvbox)
    ;; (lightemacs-theme-create-loader "gruvbox-light-medium" 'gruvbox)
    ;; (lightemacs-theme-create-loader "gruvbox-light-hard" 'gruvbox)

    (lightemacs-theme-create-loader "doom-gruvbox-light" 'doom-themes)
    (lightemacs-theme-create-loader "doom-one" 'doom-themes)
    (lightemacs-theme-create-loader "doom-1337" 'doom-themes)
    (lightemacs-theme-create-loader "doom-gruvbox" 'doom-themes)
    (lightemacs-theme-create-loader "doom-solarized-light" 'doom-themes)
    (lightemacs-theme-create-loader "doom-tomorrow-night" 'doom-themes)
    (lightemacs-theme-create-loader "doom-tomorrow-day" 'doom-themes)
    (lightemacs-theme-create-loader "doom-snazzy" 'doom-themes)
    (lightemacs-theme-create-loader "doom-ir-black" 'doom-themes)
    (lightemacs-theme-create-loader "doom-ayu-dark" 'doom-themes)
    (lightemacs-theme-create-loader "doom-acario-light" 'doom-themes)

    ;; Dark
    (lightemacs-theme-create-loader "ef-melissa-dark" 'ef-themes)
    (lightemacs-theme-create-loader "ef-symbiosis" 'ef-themes)
    ;; Yellow
    (lightemacs-theme-create-loader "ef-melissa-light" 'ef-themes)
    (lightemacs-theme-create-loader "ef-duo-light" 'ef-themes)
    ;; Blue
    (lightemacs-theme-create-loader "ef-frost" 'ef-themes)
    (lightemacs-theme-create-loader "ef-light" 'ef-themes)
    (lightemacs-theme-create-loader "ef-maris-light" 'ef-themes)
    ;; Orange
    (lightemacs-theme-create-loader "ef-day" 'ef-themes)
    ;; Green
    (lightemacs-theme-create-loader "ef-spring" 'ef-themes)
    (lightemacs-theme-create-loader "ef-elea-light" 'ef-themes)
    (lightemacs-theme-create-loader "ef-cyprus" 'ef-themes)))

;;; Provide

(provide 'mod-begin)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; env-allow-syntax-checker-package-lint: nil
;; End:

;;; mod-begin.el ends here

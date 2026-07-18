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

;;; Local modes instead of global ones

(setq lightemacs-electric-pair-local-target-hooks nil)
(setq lightemacs-electric-pair-global-target-hooks nil)
(add-hook-text-editing-modes #'electric-pair-local-mode)
(add-hook 'minibuffer-setup-hook 'electric-pair-local-mode)

(with-eval-after-load 'le-evil-snipe
  (setq lightemacs-evil-snipe-local-target-hooks nil)
  (setq lightemacs-evil-snipe-global-target-hooks nil)
  (add-hook-text-editing-modes 'evil-snipe-local-mode)
  (add-hook 'minibuffer-setup-hook 'evil-snipe-local-mode))

(with-eval-after-load 'le-evil-surround
  (setq lightemacs-evil-surround-local-target-hooks nil)
  (setq lightemacs-evil-surround-global-target-hooks nil)
  (add-hook-text-editing-modes 'evil-surround-mode)
  (add-hook 'minibuffer-setup-hook 'evil-surround-mode))

(setq lightemacs-corfu-local-target-hooks nil)
(setq lightemacs-corfu-global-target-hooks nil)
;; This is enabled by `mod-conditional-modes'
(with-eval-after-load 'le-corfu
  (add-hook-text-editing-modes 'corfu-mode)
  (add-hook 'minibuffer-setup-hook 'corfu-mode))

(setq lightemacs-saveplace-target-hooks nil)
(add-hook-text-editing-modes 'save-place-local-mode)

(with-eval-after-load 'le-undo-fu-session
  (setq lightemacs-undo-fu-session-local-target-hooks nil)
  (setq lightemacs-undo-fu-session-global-target-hooks nil)
  (add-hook-text-editing-modes 'undo-fu-session-mode))

;; Started from mod-conditional-modes.el
(setq lightemacs-stripspace-target-hooks nil)
(setq lightemacs-aggressive-indent-target-hooks nil)
(setq lightemacs-apheleia-local-target-hooks nil)
(setq lightemacs-apheleia-global-target-hooks nil)
(setq lightemacs-flymake-target-hooks nil)

(setq lightemacs-package-lint-flymake-target-hooks nil)

;; Yasnippet
(progn
  (setq lightemacs-yasnippet-global-target-hooks nil)
  (setq lightemacs-yasnippet-local-target-hooks nil)
  (with-eval-after-load 'yasnippet
    (add-hook-text-editing-modes 'yas-minor-mode))

  (defun le-yasnippet-reload-if-empty ()
    "Reload all YASnippet snippets only if they are not already loaded."
    (when (and (fboundp 'yas-reload-all)
               (not (and (boundp 'yas--tables)
                         (hash-table-p yas--tables)
                         (> (hash-table-count yas--tables) 0))))
      (yas-reload-all)))

  (add-hook 'lightemacs-after-init-hook 'le-yasnippet-reload-if-empty))

;;; Default modes that I disabled

;; Force the unimpaired mode off globally
(setq lightemacs-evil-collection-inhibit-unimpaired-mode t)

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

;;; Lazy loader

;; For file-path-todo
(require 'mod-defun nil t)

(lightemacs-use-package lazy-loader
  :ensure nil
  :commands lazy-loader-mode
  :hook (lightemacs-after-init . lazy-loader-mode)
  :init
  (setq lazy-loader-verbose t)
  (setq lazy-loader-files (delq nil
                                (list (when (bound-and-true-p file-path-todo)
                                        file-path-todo))))
  (setq lazy-loader-modules '(org
                              vterm
                              aggressive-indent
                              yasnippet
                              apheleia
                              dired

                              add-log
                              advice
                              apheleia-dp
                              apheleia-formatter-context
                              apheleia-formatters
                              apheleia-log
                              apheleia-rcs
                              apheleia-utils
                              auth-source
                              autorevert
                              avl-tree
                              backtrace
                              bibtex
                              bookmark
                              buffer-terminator
                              byte-opt
                              c++-ts-mode
                              c-ts-common
                              c-ts-mode
                              cal-loaddefs
                              cal-menu
                              calendar
                              checkdoc
                              cl-print
                              color
                              compile-angel
                              consult
                              consult-imenu
                              consult-info
                              consult-xref
                              corfu
                              corfu-history
                              corfu-popupinfo
                              cursor-sensor
                              dash
                              debug
                              diff-hl
                              diff-mode
                              dired-aux
                              dired-filter
                              dired-hacks-utils
                              dired-loaddefs
                              dired-x
                              disp-table
                              display-fill-column-indicator
                              display-line-numbers
                              dtrt-indent
                              easy-escape
                              edit-indirect
                              edmacro
                              ef-melissa-dark-theme
                              ef-themes
                              ehelp
                              eieio
                              eieio-core
                              elec-pair
                              embark
                              embark-consult
                              embark-org
                              enhanced-evil-paredit
                              evil
                              evil-collection
                              evil-collection-bookmark
                              evil-collection-buff-menu
                              evil-collection-calendar
                              evil-collection-comint
                              evil-collection-compile
                              evil-collection-consult
                              evil-collection-corfu
                              evil-collection-custom
                              evil-collection-debug
                              evil-collection-diff-hl
                              evil-collection-diff-mode
                              evil-collection-dired
                              evil-collection-eldoc
                              evil-collection-elisp-mode
                              evil-collection-embark
                              evil-collection-flymake
                              evil-collection-help
                              evil-collection-imenu
                              evil-collection-indent
                              evil-collection-info
                              evil-collection-kmacro
                              evil-collection-log-edit
                              evil-collection-log-view
                              evil-collection-markdown-mode
                              evil-collection-minibuffer
                              evil-collection-org
                              evil-collection-outline
                              evil-collection-process-menu
                              evil-collection-python
                              evil-collection-replace
                              evil-collection-sh-script
                              evil-collection-simple
                              evil-collection-tab-bar
                              evil-collection-tabulated-list
                              evil-collection-term
                              evil-collection-unimpaired
                              evil-collection-vc-dir
                              evil-collection-vc-git
                              evil-collection-vertico
                              evil-collection-vterm
                              evil-collection-xref
                              evil-collection-yaml-mode
                              evil-command-window
                              evil-commands
                              evil-common
                              evil-core
                              evil-ex
                              evil-integration
                              evil-jumps
                              evil-macros
                              evil-maps
                              evil-matchit
                              evil-matchit-autoloads
                              evil-matchit-evil-setup
                              evil-matchit-sdk
                              evil-repeat
                              evil-search
                              evil-snipe
                              evil-states
                              evil-surround
                              evil-types
                              evil-vars
                              ewoc
                              executable
                              f
                              face-remap
                              ffap
                              filenotify
                              files-x
                              find-func
                              flymake
                              flyspell
                              format-spec
                              gcmh
                              gcsentinel
                              generator
                              gptel-autoloads
                              help-fns
                              highlight-defined
                              imenu
                              inhibit-mouse
                              inline
                              iso8601
                              ispell
                              jka-compr
                              json
                              kirigami
                              kirigami-evil
                              kirigami-jump
                              kmacro
                              lazy-autorevert
                              lazy-loader
                              le-undo-fu
                              le-wgrep
                              log-edit
                              log-view
                              map
                              marginalia
                              markdown-mode
                              mb-depth
                              mode-local
                              modus-themes
                              my-config-evil
                              my-evil-outline
                              noutline
                              ob
                              ob-comint
                              ob-core
                              ob-emacs-lisp
                              ob-eval
                              ob-exp
                              ob-lob
                              ob-ref
                              ob-table
                              ob-tangle
                              oc
                              oc-basic
                              ol
                              org-capture
                              org-compat
                              org-cycle
                              org-element
                              org-element-ast
                              org-entities
                              org-faces
                              org-fold
                              org-fold-core
                              org-footnote
                              org-ibullets
                              org-ibullets-autoloads
                              org-id
                              org-indent
                              org-keys
                              org-list
                              org-loaddefs
                              org-macro
                              org-macs
                              org-pcomplete
                              org-persist
                              org-refile
                              org-src
                              org-table
                              org-version
                              outline
                              outline-indent
                              paredit
                              parse-time
                              password-cache
                              pcase
                              pcomplete
                              pcvs-util
                              persist-text-scale
                              project
                              pulse
                              python
                              quick-fasd
                              quick-fasd-autoloads
                              recentf
                              rect
                              reveal
                              s
                              savehist
                              saveplace
                              semantic/fw
                              semantic/lex
                              server
                              sh-script
                              shell
                              smie
                              stripspace
                              sub-better-evil
                              sub-evil-gptel
                              sub-org
                              tabify
                              term
                              term/xterm
                              thingatpt
                              time
                              time-date
                              time-stamp
                              track-changes
                              tramp
                              tramp-cache
                              tramp-cmds
                              tramp-compat
                              tramp-integration
                              tramp-loaddefs
                              tramp-message
                              trampver
                              tree-widget
                              treesit-fold
                              treesit-fold-parsers
                              treesit-fold-summary
                              treesit-fold-util
                              undo-fu-autoloads
                              undo-fu-session
                              url-parse
                              url-util
                              url-vars
                              vc
                              vc-dir
                              vc-dispatcher
                              vc-git
                              vertico
                              vertico-sort
                              vterm-module
                              wgrep-autoloads
                              wizard
                              wizard-autoloads
                              xdg
                              xref
                              xterm
                              yaml-mode
                              yank-media))
  ;; (lazy-loader-buffers
  ;;  '(("*tmux*" .
  ;;     (lambda ()
  ;;       (let ((buf (get-buffer-create "*tmux*")))
  ;;         (with-current-buffer buf
  ;;           (vterm-mode)
  ;;
  ;;           (vterm-send-string "tmux-session -l emacs")
  ;;           (vterm-send-string "\n")
  ;;           (vterm-send-return))
  ;;         buf)))))
  )

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
                               (seq-contains-p lazy-loader-initial-features f))
                             features))
          (removed (seq-remove (lambda (f)
                                 (seq-contains-p features f))
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

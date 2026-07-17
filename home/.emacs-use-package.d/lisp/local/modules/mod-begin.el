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

(lightemacs-use-package lazy-loader
  :ensure nil
  :commands lazy-loader-mode
  :hook
  (lightemacs-after-init . lazy-loader-mode)
  :init
  (setq lazy-loader-verbose t)
  (setq lazy-loader-files (delq nil
                                (list (when (bound-and-true-p file-path-todo)
                                        file-path-todo))))
  (setq lazy-loader-modules '(org
                              vterm
                              org-appear
                              vterm
                              aggressive-indent
                              yasnippet
                              apheleia
                              dired

                              ;; advice
                              ;; annalist
                              ;; ansi-color
                              ;; ansi-osc
                              ;; apheleia-dp
                              ;; apheleia-formatter-context
                              ;; apheleia-formatters
                              ;; apheleia-log
                              ;; apheleia-rcs
                              ;; apheleia-utils
                              ;; autorevert
                              ;; avl-tree
                              ;; bibtex
                              ;; bookmark
                              ;; buffer-terminator
                              ;; wizard
                              ;; byte-opt
                              ;; c++-ts-mode
                              ;; c-ts-common
                              ;; c-ts-mode
                              ;; cal-loaddefs
                              ;; cal-menu
                              ;; calendar
                              ;; char-fold
                              ;; color
                              ;; comint
                              ;; comp
                              ;; comp-common
                              ;; comp-cstr
                              ;; comp-run
                              ;; compile
                              ;; compile-angel
                              ;; corfu
                              ;; corfu-prescient
                              ;; cursor-sensor
                              ;; derived
                              ;; diff-mode
                              ;; dig
                              ;; dired-loaddefs
                              ;; disp-table
                              ;; display-fill-column-indicator
                              ;; display-line-numbers
                              ;; doc-view
                              ;; dom
                              ;; dtrt-indent
                              ;; easy-escape
                              ;; edit-indirect
                              ;; edmacro
                              ;; ef-melissa-dark-theme
                              ;; ef-themes
                              ;; ehelp
                              ;; elec-pair
                              ;; enhanced-evil-paredit
                              ;; epa
                              ;; epg
                              ;; epg-config
                              ;; evil
                              ;; evil-collection
                              ;; evil-collection-bookmark
                              ;; evil-collection-buff-menu
                              ;; evil-collection-calendar
                              ;; evil-collection-comint
                              ;; evil-collection-compile
                              ;; evil-collection-corfu
                              ;; evil-collection-custom
                              ;; evil-collection-diff-mode
                              ;; evil-collection-dired
                              ;; evil-collection-doc-view
                              ;; evil-collection-eldoc
                              ;; evil-collection-elisp-mode
                              ;; evil-collection-epa
                              ;; evil-collection-eww
                              ;; evil-collection-finder
                              ;; evil-collection-flymake
                              ;; evil-collection-gnus
                              ;; evil-collection-help
                              ;; evil-collection-hideshow
                              ;; evil-collection-image
                              ;; evil-collection-imenu
                              ;; evil-collection-indent
                              ;; evil-collection-kmacro
                              ;; evil-collection-markdown-mode
                              ;; evil-collection-message
                              ;; evil-collection-minibuffer
                              ;; evil-collection-outline
                              ;; evil-collection-package-menu
                              ;; evil-collection-process-menu
                              ;; evil-collection-python
                              ;; evil-collection-replace
                              ;; evil-collection-sh-script
                              ;; evil-collection-simple
                              ;; evil-collection-tab-bar
                              ;; evil-collection-tabulated-list
                              ;; evil-collection-term
                              ;; evil-collection-unimpaired
                              ;; evil-collection-vc-git
                              ;; evil-collection-vertico
                              ;; evil-collection-vterm
                              ;; evil-command-window
                              ;; evil-commands
                              ;; evil-common
                              ;; evil-core
                              ;; evil-ex
                              ;; evil-integration
                              ;; evil-jumps
                              ;; evil-macros
                              ;; evil-maps
                              ;; evil-matchit-evil-setup
                              ;; evil-repeat
                              ;; evil-search
                              ;; evil-snipe
                              ;; evil-states
                              ;; evil-surround
                              ;; evil-types
                              ;; evil-vars
                              ;; eww
                              ;; executable
                              ;; exif
                              ;; face-remap
                              ;; filenotify
                              ;; files-x
                              ;; find-func
                              ;; finder
                              ;; flymake
                              ;; format-spec
                              ;; gcmh
                              ;; gcsentinel
                              ;; generator
                              ;; gmm-utils
                              ;; gnus
                              ;; gnus-art
                              ;; gnus-cloud
                              ;; gnus-group
                              ;; gnus-int
                              ;; gnus-range
                              ;; gnus-spec
                              ;; gnus-start
                              ;; gnus-sum
                              ;; gnus-undo
                              ;; gnus-util
                              ;; gnus-win
                              ;; gnutls
                              ;; help-fns
                              ;; highlight-defined
                              ;; hl-line
                              ;; ibuf-macs
                              ;; ietf-drums
                              ;; image-mode
                              ;; imenu
                              ;; inhibit-mouse
                              ;; inline
                              ;; iso8601
                              ;; jinx
                              ;; jka-compr
                              ;; kinsoku
                              ;; kirigami-evil
                              ;; kmacro
                              ;; lazy-loader
                              ;; let-alist
                              ;; lisp-mnt
                              ;; mail-parse
                              ;; mail-prsvr
                              ;; mail-source
                              ;; mail-utils
                              ;; mailabbrev
                              ;; mailheader
                              ;; marginalia
                              ;; markdown-mode
                              ;; mb-depth
                              ;; message
                              ;; mm-bodies
                              ;; mm-decode
                              ;; mm-encode
                              ;; mm-url
                              ;; mm-util
                              ;; mm-uu
                              ;; mm-view
                              ;; mml
                              ;; mml-sec
                              ;; mml-smime
                              ;; mml2015
                              ;; modus-themes
                              ;; my-config-evil
                              ;; my-evil-outline
                              ;; nnheader
                              ;; nnimap
                              ;; nnmail
                              ;; nnoo
                              ;; nnselect
                              ;; outline-indent
                              ;; package-lint
                              ;; package-lint-flymake
                              ;; paredit
                              ;; parse-time
                              ;; pcase
                              ;; pcomplete
                              ;; persist-text-scale
                              ;; pixel-fill
                              ;; prescient
                              ;; project
                              ;; pulse
                              ;; puny
                              ;; python
                              ;; radix-tree
                              ;; range
                              ;; recentf
                              ;; rect
                              ;; reveal
                              ;; rfc2045
                              ;; rfc2047
                              ;; rfc2231
                              ;; rfc6068
                              ;; rfc822
                              ;; ring
                              ;; rx
                              ;; savehist
                              ;; saveplace
                              ;; sendmail
                              ;; server
                              ;; sh-script
                              ;; shell
                              ;; shell-pop
                              ;; shr
                              ;; smie
                              ;; smime
                              ;; stripspace
                              ;; sub-better-evil
                              ;; svg
                              ;; tabify
                              ;; term
                              ;; term/xterm
                              ;; terminal-themes
                              ;; terminal-themes-frame-color
                              ;; terminal-themes-vterm
                              ;; text-property-search
                              ;; thingatpt
                              ;; time
                              ;; time-date
                              ;; time-stamp
                              ;; track-changes
                              ;; tramp
                              ;; tramp-cache
                              ;; tramp-cmds
                              ;; tramp-compat
                              ;; tramp-integration
                              ;; tramp-loaddefs
                              ;; tramp-message
                              ;; trampver
                              ;; tree-widget
                              ;; undo-fu-session
                              ;; url-file
                              ;; url-queue
                              ;; utf7
                              ;; vc-dispatcher
                              ;; vc-git
                              ;; vertico
                              ;; vertico-prescient
                              ;; vtable
                              ;; vterm-module
                              ;; warnings
                              ;; winner
                              ;; xml
                              ;; xterm
                              ;; yaml-ts-mode
                              ;; yank-media
                              ))
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

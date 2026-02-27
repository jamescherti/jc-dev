;;; config.el --- Config -*- lexical-binding: t -*-

;;; Commentary:

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

;;; Code:

;;; Debug, native comp, and initial options

(require 'my-defun)

(setq debug-on-error t)

(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;;---------------------------------------->TMP
(defvar my-session-temp-directory
  (file-name-as-directory (make-temp-file "emacs-session-" t))
  "Unique temporary directory for the current Emacs session.")

;; Direct Emacs internal temporary file generation to the new directory
(setq temporary-file-directory my-session-temp-directory)
(setq small-temporary-file-directory my-session-temp-directory)

;; Direct subprocesses (like libgccjit) to the new directory
(setenv "TMPDIR" my-session-temp-directory)
;; (message "TMP:%s" my-session-temp-directory)

(defun my-cleanup-session-temp-directory ()
  "Remove the unique session temporary directory and its contents."
  (when (and my-session-temp-directory
             (file-directory-p my-session-temp-directory))
    (delete-directory my-session-temp-directory t)))

;; Hook the cleanup function to Emacs exit
(add-hook 'kill-emacs-hook #'my-cleanup-session-temp-directory)
;;---------------------------------------->TMP

(setq minimal-emacs-frame-title-format "Lightemacs")

;; Which kind of warnings and errors to report from async native compilation.
;; (setq native-comp-async-warnings-errors-kind 'important)
(setq native-comp-async-warnings-errors-kind 'all)

;; TODO find out why early-init and init is compiled when
;; compile-angel-native-compile-load is t
(setq lightemacs-load-compiled-init-files t)

(setq lightemacs-optional-modules '(
                                    mod-defun
                                    mod-misc
                                    buffer-guardian
                                    mod-lsp-mode

                                    smartindent
                                    battery-angel
                                    mod-project
                                    mod-evil
                                    mod-buffer-terminator
                                    mod-toggle-term
                                    mod-better-vc
                                    point-manager
                                    mod-org

                                    mod-kirigami
                                    ))

(setq lightemacs-recentf-track-switch-to-buffer t)

(setq compile-angel-optimize-regexps t)

(with-eval-after-load 'compile-angel
  ;; (setq compile-angel-verbose t)
  ;; (setq compile-angel-debug t)

  ;; Exclusions
  (push "/file-templates-auto/main.el" compile-angel-excluded-files)
  (push "/tmp-file.el" compile-angel-excluded-files)
  (push "/.dir-settings.el" compile-angel-excluded-files)

  ;; This is important because Emacs loads the early-init.elc, even if it is
  ;; older than the early-init.el file
  (push "/early-init.el" compile-angel-excluded-files))

;; TODO test this more. It does not seem stable.
;; (setq package-quickstart t)
(setq package-native-compile nil)
(setq native-comp-jit-compilation nil)

(with-eval-after-load 'compile-angel
  (if (fboundp 'compile-angel-exclude-directory)
      (compile-angel-exclude-directory "~/src/emacs/")
    (error "Undefined: compile-angel-exclude-directory")))

(setq compile-angel-enable-byte-compile t)
(setq compile-angel-enable-native-compile t)
(setq compile-angel-native-compile-load nil)
;; (setq compile-angel-exclude-core-emacs-directory
;;       ;; Emacs was compiled with native-compile aot
;;       (when (and (fboundp 'subr-native-elisp-p)
;;                  (subr-native-elisp-p (symbol-function 'find-file)))
;;         t))

(setq compile-angel-verbose t)
(setq compile-angel-debug nil)

;; To stop vterm from asking for confirmation and force it to compile the
;; module automatically, you need to set the vterm-always-compile-module
;; variable to t.
(setq vterm-always-compile-module t)

;; Delayed native compilation
;; (progn
;;   (with-no-warnings  ; Obsolete
;;     (setq native-comp-deferred-compilation native-comp-jit-compilation))
;;
;;   (defun my-delayed-native-compilation ()
;;     "Enable native compilation ten seconds after Emacs startup."
;;     (run-at-time 5 nil (lambda ()
;;                          (require 'le-compile-angel))))
;;
;;   (add-hook 'emacs-startup-hook #'my-delayed-native-compilation))

;; Native compilation ignore
;; (let ((deny-list '(
;;                    ;; "\\(?:[/\\\\]\\.dir-locals\\.el\\(?:\\.gz\\)?$\\)"
;;                    ;; "\\(?:[/\\\\]gc\\.el\\(?:\\.gz\\)?$\\)"  ; devemacs
;;                    "\\(?:[/\\\\]bind-key\\.el\\(?:\\.gz\\)?$\\)"
;;                    "\\(?:[/\\\\]cl-lib\\.el\\(?:\\.gz\\)?$\\)"  ; devemacs
;;                    "\\(?:[/\\\\]bytecomp\\.el\\(?:\\.gz\\)?$\\)"
;;                    ;; "\\(?:[/\\\\]use-package-ensure\\.el\\(?:\\.gz\\)?$\\)"
;;                    ;; "\\(?:[/\\\\]use-package-delight\\.el\\(?:\\.gz\\)?$\\)"
;;                    ;; "\\(?:[/\\\\]use-package-diminish\\.el\\(?:\\.gz\\)?$\\)"
;;                    ;; "\\(?:[/\\\\]use-package-bind-key\\.el\\(?:\\.gz\\)?$\\)"
;;                    "\\(?:[/\\\\]use-package-core\\.el\\(?:\\.gz\\)?$\\)"
;;                    "\\(?:[/\\\\]easy-mmode\\.el\\(?:\\.gz\\)?$\\)"
;;
;;                    ;; Emacs 30.2
;;                    "\\(?:[/\\\\]ansi-color\\.el\\(?:\\.gz\\)?$\\)"
;;                    "\\(?:[/\\\\]comeint\\.el\\(?:\\.gz\\)?$\\)"
;;                    "\\(?:[/\\\\]cl-macs\\.el\\(?:\\.gz\\)?$\\)"
;;                    "\\(?:[/\\\\]cl-seq\\.el\\(?:\\.gz\\)?$\\)"
;;                    "\\(?:[/\\\\]cl-extra\\.el\\(?:\\.gz\\)?$\\)"
;;
;;                    ;; Compiling:
;;                    ;; /path/to/emacs/30.1/lisp/org/org-loaddefs.el.gz...
;;                    ;; "\\(?:[/\\\\][^/\\\\]+-loaddefs\\.el\\(?:\\.gz\\)?$\\)"
;;                    ;; "\\(?:[/\\\\][^/\\\\]+-autoloads\\.el\\(?:\\.gz\\)?$\\)"
;;                    )))
;;   (setq native-comp-jit-compilation-deny-list deny-list)
;;   ;; Deprecated
;;   (with-no-warnings
;;     (setq native-comp-deferred-compilation-deny-list deny-list)
;;     (setq comp-deferred-compilation-deny-list deny-list)))

;; `native-comp-compiler-options' specifies flags passed directly to the C
;; compiler (for example, GCC or Clang) when compiling the Lisp-to-C output
;; produced by the native compilation process. These flags affect code
;; generation, optimization, and debugging information.
;;
;; `native-comp-driver-options' specifies additional flags passed to the native
;; compilation driver process, which may invoke the compiler and linker with
;; certain parameters. In most cases, these flags mirror the compiler options,
;; but they can be configured separately when the driver needs different
;; arguments (for example, link-time options or toolchain-specific parameters).
;;
;; In native-compilation mode, when a .el or .elc file is compiled to native
;; code (.eln): Emacs itself does not call gcc directly. Instead, Emacs invokes
;; the native compilation driver process (a small program written in C and
;; Lisp). This driver prepares the command line for the compiler, applies
;; system-dependent settings, and runs the compilation in an isolated process.
(setq native-comp-compiler-options '(;; Enables aggressive optimization passes
                                     ;; in GCC. Native compilation of Emacs Lisp
                                     ;; benefits from improved inlining and loop
                                     ;; transformations without affecting
                                     ;; correctness.
                                     "-O2"

                                     ;; Disables generation of debug symbols.
                                     ;; Reduces compilation time and disk usage
                                     ;; for .eln files.
                                     "-g0"

                                     ;; /usr/bin/ld: /tmp/ccps2Qse.o:
                                     ;; plugin needed to handle lto object
                                     ;; "-flto=auto"

                                     ;; Omits the frame pointer on supported
                                     ;; architectures. Frees an additional
                                     ;; register and slightly reduces call
                                     ;; overhead on x86_64 systems.
                                     "-fomit-frame-pointer"

                                     ;; Prevents assumptions that floating point
                                     ;; operations never produce NaN or
                                     ;; infinity. Emacs uses IEEE-compliant
                                     ;; behavior, and this avoids undefined
                                     ;; behavior in numerical primitives.
                                     "-fno-finite-math-only"

                                     ;; Can improve performance in loops, but
                                     ;; sometimes increases binary size.
                                     ;; "-funroll-loops"

                                     ;; Allows the compiler to assume standard C
                                     ;; aliasing rules. Emacs native code
                                     ;; adheres to these rules, enabling better
                                     ;; load and store optimizations.
                                     "-fstrict-aliasing"

                                     ;; Generates code optimized for the local
                                     ;; CPU architecture. Improves instruction
                                     ;; selection and vectorization when
                                     ;; available.
                                     ;; NOTE: Does not work.
                                     ;; "-march=native"

                                     ;; Tunes instruction scheduling for the
                                     ;; local CPU. Improves runtime behavior
                                     ;; without affecting portability of
                                     ;; bytecode.
                                     ;; NOTE: Does not work.
                                     ;;"-mtune=native"

                                     ;; Not relevant for Emacs Lisp, more for C++ code.
                                     ;; -fno-exceptions / -fno-rtti
                                     ))
(setq native-comp-driver-options (copy-sequence native-comp-compiler-options))

(load (expand-file-name "~/.config.el") :no-error :nosuffix)

;;; Options

;; Define your preferred font name here
;; (defvar my-font-choice "Iosevka Term")
;; (defconst my-font-choice "Iosevka SS08")
;; (defvar my-font-choice "Iosevka Term-13")
;; (add-to-list 'default-frame-alist `(font . ,my-font-choice))
(setq lightemacs-theme-default-font "Iosevka Term-13")

;; Check if the font exists on the system before applying it
;; NOTE doesn't work?
;; (if (find-font (font-spec :name my-font-choice))
;;     ;; TODO: Concat -13 to `my-font-choice'
;;     (add-to-list 'default-frame-alist `(font . ,my-font-choice))
;;   (message "Warning: Font '%s' not found. Using system default." my-font-choice))

;; (add-to-list 'default-frame-alist '(font . "Iosevka Term-13"))

;; TODO: Lightemacs?
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; (unless (display-graphic-p)
;;   (add-to-list 'default-frame-alist '(inhibit-double-buffering . t)))

;; On some window managers (fvwm 2.2.5 and KDE 2.1), Emacs can pause because Xt
;; waits for a `ConfigureNotify` event that the WM does not send, timing out
;; after about 5 seconds.
;; (add-to-list 'default-frame-alist '(wait-for-wm . t))

;; TODO: add to minimal emacs README.md

;; Redired elc

;; Redirect the ELN (Emacs Lisp Native) cache to a custom directory
;; (when (featurep 'native-compile)
;;   (let ((eln-cache-dir (convert-standard-filename
;;                         (expand-file-name "eln-cache"
;;                                           minimal-emacs-user-directory))))
;;     ;; Modify the load path for existing native-compiled files
;;     (when (boundp 'native-comp-eln-load-path)
;;       (setcar native-comp-eln-load-path eln-cache-dir))
;;
;;     ;; Set the target directory for future native compilations
;;     (setq native-compile-target-directory eln-cache-dir)
;;
;;     ;; Redirect the ELN cache used at startup (Emacs 29+)
;;     ;; Set the directory where Emacs looks for native-compiled .eln files during
;;     ;; early startup, before user configuration is fully loaded. This function
;;     ;; ensures Emacs does not write .eln files to the default system path (e.g.,
;;     ;; '$EMACS_LIBDIR/eln-cache/' which may be read-only or shared) and instead
;;     ;; uses a user-defined writable location. It is a no-op in earlier Emacs
;;     ;; versions where native compilation support was different.
;;     ;; (when (fboundp 'startup-redirect-eln-cache)
;;     ;;   (startup-redirect-eln-cache eln-cache-dir))
;;
;;     ))

(when (boundp 'trusted-content)
  (let ((dirs
         (list
          "~/src/dotfiles/jc-dev/"
          "~/src/emacs/")))
    (dolist (dir dirs)
      (when dir
        ;; Ensure the path ends with a slash so it registers as a directory
        (push dir trusted-content)))))

;; Prevent Emacs from loading the system-wide 'default.el' initialization file.
;;
;; By default, after loading the user's own init file (e.g., ~/.emacs or
;; ~/.emacs.d/init.el), Emacs may load a secondary initialization file named
;; 'default.el' from its data-directory. This file is distributed by Emacs
;; itself or the system package manager and may contain global default settings
;; such as enabling toolbars, setting default keybindings, or altering the
;; initial buffer state.
;;
;; Loading 'default.el' can introduce environment-dependent or
;; distribution-specific behavior, which interferes with the user's attempt to
;; fully control and isolate their Emacs configuration. It also makes
;; configuration less reproducible and predictable, particularly in environments
;; where Emacs is deployed across multiple systems or used programmatically.
;;
;; Setting 'inhibit-default-init' to non-nil disables the automatic loading of
;; 'default.el', ensuring that the Emacs session starts only with the user's
;; configuration.
(setq inhibit-default-init t)

;; Setting `site-run-file' to nil disables the loading of the site-run-file.el,
;; which is a site-wide initialization script typically provided by the system
;; or Emacs installation.
;;
;; Advantage:
;; - Prevents execution of potentially unwanted or incompatible site-wide
;;   customizations, leading to a cleaner and more predictable Emacs startup
;;   environment, especially useful in controlled or minimal setups.
;;
;; Disadvantage:
;; - May omit important default configurations or package initializations
;;   expected by the system or administrators, possibly causing missing
;;   functionality or inconsistent behavior in Emacs sessions.
(setq site-run-file nil)

;;----------------------------------------------------------------------------
;; Optimize
;;----------------------------------------------------------------------------

;; (setq vc-handled-backends nil)
;; (defun my-restore-vc-handled-backends ()
;;   "Restore VC backends."
;;   (setq vc-handled-backends '(Git)))
;; (add-hook 'emacs-startup-hook #'my-restore-vc-handled-backends 120)

;;; Lightemacs modules and parameters

(setq stripspace-verbose nil)
(setq stripspace-normalize-indentation t)
(setq stripspace-restore-column t)
(setq stripspace-only-if-initially-clean t)

(setq lightemacs-reduce-messages t)
(setq lightemacs-saveplace-recenter-after-find-file t)

(setq lightemacs-debug t)
(setq lightemacs-verbose t)

(setq lightemacs-dired-omit-parent-directory t)
(setq lightemacs-cycle nil)
(setq lightemacs-native-comp-excluded-cpus 1)

(setq lightemacs-theme-name 'tomorrow-night-deepblue)
(setq lightemacs-theme-package 'tomorrow-night-deepblue-theme)

;; Enable native-compilation and byte-compilation

(setq lightemacs-modules '(le-compile-angel  ;;moved it down
                           le-theme
                           le-default-settings
                           le-default-keybindings
                           le-gcmh

                           le-evil
                           le-evil-collection
                           le-evil-snipe
                           le-enhanced-evil-paredit  ;; using local pkg
                           le-evil-surround
                           le-goto-chg
                           le-undo-fu
                           le-undo-fu-session
                           le-vim-tab-bar
                           ;; le-evil-commentary  ; I am using my own in mod-evil

                           le-buffer-terminator
                           le-kirigami
                           le-inhibit-mouse

                           le-csv-mode

                           ;; le-treesit-fold

                           ;; le-diminish

                           ;; Uses too much CPU. TODO active it on AC mode only

                           ;; le-magit

                           le-xclip

                           le-flymake
                           le-package-lint-flymake

                           le-apheleia

                           ;; le-consult-dir
                           le-consult
                           le-embark-consult
                           le-embark
                           le-wgrep
                           le-vertico
                           le-orderless
                           le-marginalia

                           le-corfu
                           le-cape
                           le-prescient
                           le-vertico-prescient
                           le-corfu-prescient

                           le-elisp-refs
                           le-easy-escape
                           le-aggressive-indent
                           le-highlight-defined
                           le-paredit
                           le-elisp-autofmt
                           ;; le-page-break-lines

                           le-autorevert
                           ;; mod-lazy-autorevert

                           le-recentf
                           le-savehist
                           le-saveplace
                           le-winner
                           le-elec-pair

                           ;; le-which-key
                           ;; le-paren

                           le-dired
                           le-dired-filter

                           le-org
                           le-org-appear

                           le-markdown-mode
                           le-markdown-toc
                           le-edit-indirect

                           le-outline
                           le-outline-indent

                           le-yasnippet
                           ;; le-yasnippet-snippets

                           le-group-yaml
                           ;; le-yaml-mode
                           ;; le-yaml-ts-mode

                           le-avy
                           ;; le-ace-window
                           le-bufferfile
                           le-diff-hl
                           le-dtrt-indent
                           le-dumb-jump
                           le-expand-region
                           le-git-modes
                           le-helpful
                           le-indent-bars
                           le-stripspace

                           le-vterm
                           ;; le-eat

                           le-persist-text-scale

                           le-easysession

                           ;; My modules
                           ;;------------------------------

                           ;; My modules
                           ;; TODO change
                           ;; (setq lightemacs-display-line-numbers-mode-add-hook-to nil)
                           ;; le-display-line-numbers  ;; mod-misc provides its own
                           mod-defun
                           mod-misc  ; misc=begin
                           mod-misc2

                           ;; tmp-easysession
                           buffer-guardian

                           ;; mod-eglot
                           ;; TODO fix eldoc help more than one line. I only
                           ;; want 1 line
                           mod-lsp-mode

                           smartindent
                           battery-angel
                           mod-project
                           mod-evil
                           mod-buffer-terminator
                           mod-toggle-term
                           mod-better-vc
                           point-manager
                           mod-org

                           mod-kirigami
                           ;; mod-kirigami-alternative

                           ;; le-treesit-auto
                           ))

;;; Target hooks

;; (setq lightemacs-buffer-terminator-target-hooks '())

(setq lightemacs-apheleia-target-hooks '(python-mode-hook
                                         python-ts-mode-hook

                                         sh-mode-hook
                                         bash-ts-mode-hook

                                         emacs-lisp-mode-hook))

(setq lightemacs-flymake-target-hooks
      '(;; text-mode: Exceptions Configuration and Markup)
        python-mode-hook
        python-ts-mode-hook

        sh-mode-hook
        bash-ts-mode-hook

        emacs-lisp-mode-hook

        ansible-mode-hook
        yaml-ts-mode-hook
        yaml-mode-hook
        toml-ts-mode-hook
        conf-toml-mode-hook
        markdown-mode-hook))

;;; Packages

;; (defun my-package-pin (package repository)
;;   (setq package-pinned-packages
;;         (assq-delete-all package package-pinned-packages))
;;   (add-to-list 'package-pinned-packages (list (cons package repository))))

(defun my-update-package-pinned-packages (pinned-packages)
  "Update `package-pinned-packages\=' with the entries in PINNED-PACKAGES.
This replaces existing entries that match the provided packages and appends
any new ones."
  (setq package-pinned-packages (append pinned-packages
                                        (seq-remove
                                         (lambda (pkg)
                                           (assq (car pkg) pinned-packages))
                                         package-pinned-packages))))

(setq my-package-pinned-packages
      '((buffer-terminator             . "melpa")
        (enhanced-evil-paredit         . "melpa")
        (dir-config                    . "melpa")
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

        ;; To fix the window-start bug
        (apheleia                      . "melpa")

        ;; The released version is too old (1 year)
        ;; TODO: Lightemacs?
        (markdown-mode                 . "melpa")

        ;; Last release: 2024. Too old. TODO
        (lsp-mode                      . "melpa")
        (lsp-ui                        . "melpa")

        ;; 3 months ago
        ;; (gptel                         . "melpa")
        ))

(defvar my-shared-user-emacs-directory (expand-file-name "~/.emacs-data/var"))

;;; Add my packages to load path

(defvar my-package-base-directory (expand-file-name "~/src/emacs")
  "The base directory containing Emacs packages to add to `load-path\='.")

(defvar my-excluded-package-directories
  '("elispcomp"
    "lightemacs"
    "minimal-emacs.d"
    "pre-commit-elisp")
  "List of directory names to exclude from the dynamic `load-path\=' addition.")

(defvar my--package-load-path-cache nil
  "Internal cache storing the list of discovered package directories.")

(defun my-add-packages-to-load-path ()
  "Add my packages to `load-path\=' dynamically.
Iterates over `my-package-base-directory\=' and adds all subdirectories to
`load-path\=', skipping any directories listed in
`my-excluded-package-directories\='. Caches the result in
`my--package-load-path-cache\=' to avoid redundant scanning."
  ;; Build the cache if it is empty
  (unless my--package-load-path-cache
    (let ((items (condition-case nil
                     (progn (directory-files my-package-base-directory
                                             t
                                             directory-files-no-dot-files-regexp))
                   (error
                    ;; TODO add debug message
                    nil)))
          (discovered-paths nil))
      (when items
        (seq-doseq (dir items)
          (let ((dir-name (file-name-nondirectory dir)))
            (when (and (file-directory-p dir)
                       (not (member dir-name my-excluded-package-directories)))
              ;; Dynamically handle the "extensions" subdirectory if it exists
              ;; TODO optimize this by providing a list?
              (let ((ext-dir (expand-file-name "extensions" dir)))
                (when (file-directory-p ext-dir)
                  (push ext-dir discovered-paths)))

              ;; Add the base package directory
              (push dir discovered-paths)))))

      ;; Reverse the list to maintain the original alphabetical order
      (setq my--package-load-path-cache (nreverse discovered-paths))))

  ;; Apply the cached paths to load-path
  (seq-doseq (path my--package-load-path-cache)
    (add-to-list 'load-path path)))

;;; lightemacs-user-post-init

(defun my-evil-config ()
  "Setup evil."
  ;; Make `v$` exclude the final newline
  (setq evil-v$-excludes-newline t)

  ;; Prevent Evil state from being echoed, preserving Eldoc display in the
  ;; minibuffer (If set to t, Eldoc output in the minibuffer will be overridden)
  (setq evil-echo-state nil)

  ;; Enable automatic horizontal split below
  (setq evil-split-window-below t)

  ;; Enable automatic vertical split to the right
  (setq evil-vsplit-window-right t)

  ;; Enable fine-grained undo behavior
  (setq evil-want-fine-undo t)

  ;; Required by evil-collection

  ;; Do not move cursor back when exiting insert state
  (setq evil-move-cursor-back nil)

  ;; Only complete in the current buffer
  (setq evil-complete-all-buffers nil)

  (setq evil-command-window-height 8)
  (setq evil-display-shell-error-in-message nil)

  ;; Controls whether evil-collection defines Vim-unimpaired-style keybindings
  ;; (setq evil-collection-want-unimpaired-p nil)
  (setq evil-collection-calendar-want-org-bindings t)

  (setq tooltip-hide-delay 20) ;; seconds
  (setq tooltip-delay 0.4)
  (setq tooltip-short-delay 0.08)

  ;; TODO is this good?
  (setq mouse-wheel-progressive-speed nil) ; disable acceleration of scrolling
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . hscroll) ((meta))
          ((control meta) . global-text-scale)
          ((control) . text-scale)))

  (setq inhibit-mouse-button-numbers '(1 2 3))
  (setq pixel-scroll-precision-use-momentum nil))

(defun my-set-tab-width (width)
  "Set the tab width.
WIDTH is the tab width."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width width)
  (setq-local standard-indent width)
  ;; (setq-local evil-shift-width width)
  )

(defun my-setup-filetype ()
  "Setup filetype."
  (add-to-list 'auto-mode-alist '("\\.[Oo][Rr][Gg]\\.[aA][sS][cC]\\'" . org-mode))
  (defun my-org-mode-setup ()
    "When active, indent text according to outline structure."
    ;; (auto-fill-mode -1)

    ;; In org buffers we set `nobreak-char-display' to nil locally so that the
    ;; Unicode no-break space (U+00A0) is rendered just like a regular ASCII
    ;; space. This suppresses the distinct glyph or face Emacs normally applies
    ;; to NBSP, keeping the buffer free of distracting blue highlights while
    ;; preserving the character's internal no-break semantics.
    ;;
    ;; Here is an example of what is highlighted: $5 billion-valued.
    ;; When `nobreak-char-display' is non-nil, the non-breaking space after `5`
    ;; and the hyphen after n are rendered as highlighted glyphs.
    (setq-local nobreak-char-display nil)

    ;; TODO: bug. When jumping to org file from org agenda todo list,
    ;; org-indent-mode is not enabled by default.
    (when (fboundp 'org-indent-mode)
      (org-indent-mode 1))

    ;; (display-line-numbers-mode -1)

    (when (derived-mode-p 'org-mode)
      ;; It makes o not auto indent after a bullet list like * or -
      (setq-local evil-auto-indent nil)

      ;; (setq-local indent-line-function nil)
      (setq-local search-invisible nil)

      ;; Fixes a bug of jumping in org mode when scrolling many lines in my
      ;; file
      ;; TODO: bug?
      ;; (setq-local scroll-margin 1)

      ;; (toggle-truncate-lines 0)

      ;; (custom-set-faces `(org-block ((t (:height 0.7)))))
      ;; (custom-set-faces `(org-block-begin-line ((t (:height 0.6)))))
      ;; (custom-set-faces `(org-block-end-line ((t (:height 0.6 :extend t)))))
      ))

  (when (fboundp 'my-org-mode-setup)
    (add-hook 'org-mode-hook #'my-org-mode-setup))

  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-clock-report-include-clocking-task t)

  (setq sgml-basic-offset 2)  ;; HTML
  (setq css-indent-offset 2)
  (setq javascript-indent-level 2)
  (setq html-indent-offset 2)
  (setq sgml-basic-offset 2)
  (setq lua-indent-level 3)
  (setq yaml-indent-offset 2)

  ;; python
  (defun setup-python-mode ()
    "Setup `python-mode'."
    (display-fill-column-indicator-mode)
    (my-set-tab-width 4)
    (setq-local fill-column 79))

  (when (fboundp 'setup-python-mode)
    (add-hook 'python-mode-hook #'setup-python-mode)
    (add-hook 'python-ts-mode-hook #'setup-python-mode))

  ;; sh
  (setq sh-basic-offset 2)
  (defun setup-sh-mode ()
    "Setup `sh-mode'."
    (display-fill-column-indicator-mode)
    (unless (string-suffix-p ".ebuild" (buffer-file-name (buffer-base-buffer)))
      (my-set-tab-width sh-basic-offset)
      (setq-local fill-column 80)))
  (when (fboundp 'setup-sh-mode)
    (add-hook 'sh-mode-hook #'setup-sh-mode)
    (add-hook 'bash-ts-mode-hook #'setup-sh-mode)))

(defun lightemacs-user-init ()
  "This function is executed right before loading modules."
  ;; TODO: lightemacs?
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-extra-load-path
                 (expand-file-name "tree-sitter" lightemacs-var-directory)))

  (unless IS-MAC
    ;; Mac Port
    (add-to-list 'treesit-extra-load-path "/opt/local/lib"))

  (with-eval-after-load 'evil
    (require 'my-config-evil))

  (global-set-key (kbd "M-RET") 'toggle-term-tmux)
  (global-set-key (kbd "M-<enter>") 'toggle-term-tmux)
  (global-set-key (kbd "M-<return>") 'toggle-term-tmux)
  (global-set-key (kbd "M-o") 'my-previous-interesting-buffer)
  (global-set-key (kbd "M-i") 'my-next-interesting-buffer)
  ;; (global-set-key (kbd "M-=") 'global-text-scale-adjust)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C-S-k") 'my-tab-bar-move-tab-backward)
  (global-set-key (kbd "C-S-j") 'my-tab-bar-move-tab)
  (global-set-key (kbd "C-k") 'my-tab-previous)
  (global-set-key (kbd "C-j") 'my-tab-next)

  (with-eval-after-load 'le-dired-filter
    (add-hook 'lightemacs-dired-filter-setup-hook
              'dired-filter-by-git-ignored))

  ;; Prevent yasnippet from highlighting inserted fields, you need to modify the
  ;; display face that it uses for overlays. This is done by changing the
  ;; attributes of yas-field-highlight-face.
  (defun my-clear-yasnippet-field-highlight (&rest _args)
    "Clear yasnippet field highlight face to keep original syntax highlighting."
    (when (facep 'yas-field-highlight-face)
      (set-face-attribute 'yas-field-highlight-face nil
                          :inherit 'unspecified
                          :background 'unspecified
                          :foreground 'unspecified
                          :box 'unspecified
                          :underline 'unspecified)))
  ;; Apply the fix whenever a theme is loaded
  (with-no-warnings
    (advice-add 'load-theme :after #'my-clear-yasnippet-field-highlight))

  ;; Ensure it also applies when yasnippet is first loaded
  (with-no-warnings
    (add-hook 'yas-minor-mode-hook #'my-clear-yasnippet-field-highlight))

  (setq yas-snippet-dirs '())
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "yasnippet/snippets" "~/.emacs-data/etc"))
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "yasnippet/snippets-auto" "~/.emacs-data/etc"))

  (with-eval-after-load 'yasnippet
    ;; (add-hook-text-editing-modes 'yas-minor-mode-on)
    (define-key yas-minor-mode-map (kbd "C-f") 'yas-expand)

    (setq yas-prompt-functions '(yas-no-prompt))  ; Do not ask the user

    ;; (add-to-list 'yas-snippet-dirs
    ;;              (expand-file-name "yasnippet/snippets" emacs-var-dir))
    ;; (add-hook-text-editing-modes 'yas-minor-mode-on)

    ;; (define-key yas-keymap (kbd "RET") (yas-filtered-definition
    ;;                                     'yas-next-field-or-maybe-expand))

    )

  (setq hs-hide-comments-when-hiding-all nil)
  (setq hs-isearch-open t)  ;; Open both comments and code
  (add-hook 'lua-mode-hook #'hs-minor-mode)

  ;; This fixes the skipping when scrolling long org documents
  ;; NOTE: MANAGED BY MINIMAL-EMACS
  ;; (setq scroll-conservatively most-positive-fixnum)

  ;; TODO put them back
  ;;(setq eldoc-idle-delay 0.5)
  ;;(setq eldoc-echo-area-display-truncation-message t)
  ;; (setq eldoc-echo-area-prefer-doc-buffer nil)
  (setq eldoc-echo-area-use-multiline-p nil)  ;; Prevent some errors from showing

  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "•")

  (with-eval-after-load 'recentf
    (setq recentf-exclude
          (append recentf-exclude
                  (list
                   "^/\\(?:su\\|sudo\\)?:"
                   "^~/\\.emacs"
                   "^~/\\.src"
                   "^~/src/forks/"
                   "^~/\\.[a-z]*-?emacs"
                   "^/opt/local/"
                   "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$" "\\.bz$"
                   "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zpaq$" "\\.lz$" "\\.lrz$"
                   "\\.lzo$" "\\.lzma$" "\\.shar$" "\\.kgb$" "\\.zip$" "\\.Z$"
                   "\\.7z$" "\\.rar$"

                   ;; TODO lighemacs?
                   "COMMIT_EDITMSG\\'"

                   "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"

                   ;; TODO add lightemacs dir

                   "-autoloads\\.el$"
                   "autoload\\.el$"))))

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-mixed-fonts nil)
  (setq modus-themes-prompts '(bold intense))

  ;;; Simple text file
  ;; To avoid text-mode interfering with other modes like org or markdown,
  ;; I created a dedicated mode for *.txt files.
  (define-derived-mode txt-file-mode text-mode "SimpleTextFile"
    "Major mode for editing *.txt files.")
  (defun setup-txt-file-mode ()
    "Setup txt file mode."
    ;; (setq-local evil-shift-width 2)
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 2)
    (setq-local standard-indent 2))
  (setq initial-major-mode 'txt-file-mode)
  (push (cons "\\.[Tt][Xx][Tt]\\'" 'txt-file-mode) auto-mode-alist)
  (push (cons "\\.[Tt][Xx][Tt]\\.[aA][sS][cC]\\'" 'txt-file-mode) auto-mode-alist)

  (nconc auto-mode-alist
         '(;; conf-mode
           ;; ("\\.profile\\'" . conf-mode)  ; firejail profiles
           ("^/etc/[^/]+" . conf-unix-mode)

           ;; /etc/hosts and ansible /hosts

           ;; Replace with git-modes
           ;; ("/\\.gitignore" . conf-unix-mode)
           ;; ("/\\.gitattributes" . conf-space-mode)

           ;; Git

           ;; hexl-mode
           ;; ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)

           ;; txt-file-mode
           ;; ("\\.log\\'" . txt-file-mode)
           ))
  (add-to-list 'auto-mode-alist '("/\\.gitconfig\\.local\\'" . gitconfig-mode))
  (add-to-list 'auto-mode-alist '("/\\.gitignore\\.local\\'" . gitignore-mode))
  (add-to-list 'auto-mode-alist '("/\\.gitattributes\\.local\\'" . gitattributes-mode))

  (defun my-setup-conf-mode ()
    "Setup `conf-mode'."
    (setq-local evil-auto-indent nil)
    (setq-local indent-line-function #'ignore))
  (with-no-warnings
    (add-hook 'conf-mode-hook #'my-setup-conf-mode))

  (setq markdown-toc-mode-map nil)
  (setq markdown-toc-header-toc-title "## Table of Contents")
  (add-to-list 'auto-mode-alist '("\\.md\\.asc\\'" . markdown-mode))

  ;; The function that is called by default is `vc-shrink-buffer-window',
  ;; which calls `shrink-window-if-larger-than-buffer' when BUFFER is visible.
  ;; This function shrinks height of WINDOW if its buffer doesn’t need so many
  ;; lines. More precisely, shrink WINDOW vertically to be as small as possible,
  ;; while still showing the full contents of its buffer. WINDOW must be a live
  ;; window and defaults to the selected one.
  (setq vc-diff-finish-functions nil)
  (setq vc-handled-backends '(Git))
  (setq vc-git-diff-switches '("--histogram"  ; Faster algorithm
                               "--textconv"
                               "--stat"

                               ;; "--ignore-cr-at-eol"

                               ;; Ignore changes in amount of white space.
                               ;; For example these would be considered the same:
                               ;; -foo    bar
                               ;; +foo bar
                               ;; "--ignore-space-change"

                               ;; Ignore all white space.
                               ;; "--ignore-all-space"
                               "-w"

                               ;; Ignore changes whose lines are all blank.
                               ;; "--ignore-blank-lines"
                               ))
  ;; Allow completing Git revisions from all refs, not only branches.
  (setq vc-git-revision-complete-only-branches nil)

  ;; Keep related changes together when generating changelogs.
  ;;
  ;; When you run `add-change-log-entry' or generate a changelog,
  ;; Emacs groups changes that belong to the same logical change together,
  ;; rather than scattering them across separate entries.
  ;;
  ;; This results in cleaner, more coherent changelog entries,
  ;; especially useful when editing multiple related files or making
  ;; several small fixes that belong to a single change.
  (setq add-log-keep-changes-together t)

  ;; Hide "up-to-date" messages in vc-dir buffers when reverting, reducing noise
  ;; (available since Emacs 31).
  (setq vc-dir-hide-up-to-date-on-revert t)

  ;; Ignore large, commonly untracked directories (like node_modules) in VC
  ;; operations to improve performance.
  (with-eval-after-load 'tramp
    (setq vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                       vc-ignore-dir-regexp
                                       tramp-file-name-regexp
                                       "[/\\\\]node_modules")))

  (setq kirigami-preserve-visual-position t)
  (setq buffer-terminator-verbose 'inhibit-message)

  ;; Hide markers like * / _ = ~; cleaner view but markers are not visible for
  ;; editing emphasis.
  ;; TODO add again
  (setq org-hide-emphasis-markers t)

  ;; No extra indentation for source blocks. It keeps code aligned with text.
  (setq org-edit-src-content-indentation 0)

  ;; Fast todo selection without popup; efficient for experts but hides guidance
  ;; for beginners.
  (setq org-use-fast-todo-selection 'expert)

  ;; Source block settings
  (setq org-directory "~/src/wip/notes")
  (setq org-edit-src-persistent-message nil)
  (setq org-export-backends '(html texinfo md))

  ;; Lists
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))

  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (shell . t)
                                   (python . t)))

  (setq org-tag-alist '((:startgroup)
                        ;; Status
                        ("next" . ?n)
                        ("wip" . ?w)
                        ("soon" . ?o)
                        ("future" . ?f)
                        ("maybe" . ?e)
                        (:endgroup)

                        (:startgroup)
                        ;; Contexts
                        ("@home" . ?h)
                        ("@work" . ?r)
                        ("@outside" . ?u)
                        (:endgroup)

                        (:startgroup)
                        ;; Priorities
                        ("high" . ?i)
                        ("medium" . ?m)
                        ("low" . ?l)
                        (:endgroup)

                        (:startgroup)
                        ("quick" . ?q)
                        ("mediumtime" . ?t)
                        ("long" . ?g)
                        (:endgroup)))


  (setq org-src-lang-modes '(("python" . python)
                             ("sh" . sh)
                             ("bash" . sh)
                             ("elisp" . emacs-lisp)))
  ;; Tag colors
  (setq org-tag-faces
        '(("@home" . (:foreground "green" :weight bold))
          ("@work" . (:foreground "green" :weight bold))
          ("@outside" . (:foreground "green" :weight bold))
          ("@computer" . (:foreground "green" :weight bold))
          ("@phone" . (:foreground "green" :weight bold))

          ("next" . (:foreground "cyan"  :weight bold))
          ("wip" . (:foreground "cyan"  :weight bold))
          ("soon" . (:foreground "cyan"  :weight bold))
          ("future" . (:foreground "cyan"  :weight bold))
          ("maybe" . (:foreground "cyan"  :weight bold))

          ("high" . (:foreground "orange"    :weight bold))
          ("medium" . (:foreground "orange"    :weight bold))
          ("low" . (:foreground "orange"    :weight bold))

          ("quick"        . (:foreground "red"        :weight bold))
          ("medium-time"        . (:foreground "red"        :weight bold))
          ("long"        . (:foreground "red"        :weight bold))

          ;; ("meeting"   . (:foreground "yellow1"       :weight bold))
          ;; ("CRITICAL"  . (:foreground "red1"          :weight bold))
          ))

  ;; Set tag column to 0 (tags appear immediately after heading); simplifies
  ;; layout but may make long headings with tags harder to read.
  (setq org-hide-block-startup t)

  (setq project-switch-commands #'project-dired)
  (setq project-vc-extra-root-markers '(".projectile"
                                        ".dir-locals.el"
                                        "requirements.txt"
                                        "autogen.sh"
                                        ".project"))


  (setq savehist-autosave-interval 650)
  (setq tab-bar-history-limit 15)

  (setq outline-blank-line t)

  (setq vim-tab-bar-update-group-name-function #'(lambda(name)
                                                   (concat " [" name "] ")))
  (setq vim-tab-bar-show-groups nil)

  (setq bufferfile-use-vc t)
  ;; (setq bufferfile-delete-switch-to 'previous-buffer)
  (setq bufferfile-delete-switch-to 'parent-directory)

  (setq markdown-gfm-use-electric-backquote nil)
  (setq markdown-heading-scaling t)
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "TAB") #'ignore))

  (setq grep-use-null-device nil
        grep-use-null-filename-separator nil
        grep-use-headings nil)
  (with-eval-after-load 'le-core-cli-tools
    (setq grep-command
          (concat (if lightemacs--ripgrep-executable
                      lightemacs--ripgrep-executable
                    "rg")
                  ;; Lightemacs
                  " --hidden -g !.git -g !.svn -g !.hg"

                  ;; Default
                  " --null --line-buffered --color=never --max-columns=1000"
                  " --path-separator / --smart-case --no-heading"
                  " --with-filename --line-number --search-zip")))

  (add-hook 'sh-mode-hook 'outline-indent-minor-mode)
  (add-hook 'bash-ts-mode-hook 'outline-indent-minor-mode)
  (add-hook 'yaml-mode-hook 'outline-indent-minor-mode)
  (add-hook 'yaml-ts-mode-hook 'outline-indent-minor-mode)
  (add-hook 'python-mode-hook 'outline-indent-minor-mode)
  (add-hook 'python-ts-mode-hook 'outline-indent-minor-mode)

  (add-hook 'conf-mode-hook 'outline-minor-mode)
  (add-hook 'js-mode-hook 'outline-minor-mode)
  (add-hook 'js-ts-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'grep-mode-hook 'outline-minor-mode)
  (add-hook 'markdown-ts-mode-hook 'outline-minor-mode)
  (add-hook 'markdown-mode-hook 'outline-minor-mode)

  (with-eval-after-load 'savehist
    (setq savehist-autosave-interval 650)

    (defvar my-savehist-additional-variables-added nil)

    (unless my-savehist-additional-variables-added
      (setq savehist-additional-variables
            (append
             savehist-additional-variables
             '(;; Record of all interactive commands entered
               command-history

               ;; Preserve jump history (used for C-o / C-i)
               ;; evil-jumps-history

               ;; Custom
               lightemacs-theme-name)))
      (setq my-savehist-additional-variables-added t))

    ;; Moved it here because it does not work from :hook
    ;; TODO replace this with easysession or with custom le-theme
    (add-hook 'savehist-mode-hook
              #'(lambda()
                  (when (fboundp 'lightemacs-load-default-theme)
                    (lightemacs-load-default-theme)))))

  (setq easysession-save-pretty-print t)
  (setq easysession-switch-to-exclude-current t)
  (setq easysession-save-interval (* 14 60))
  (add-hook 'easysession-before-reset-hook #'(lambda()
                                               ;; Save all with no questions
                                               (save-some-buffers t)))
  (defun my-easysession-only-main-saved ()
    "Only save the main session."
    (when (and (fboundp 'easysession-get-session-name)
               (string= "main" (funcall 'easysession-get-session-name)))
      t))
  (setq easysession-save-mode-predicate 'my-easysession-only-main-saved)
  (add-hook 'easysession-new-session-hook 'easysession-reset)

  (setq flymake-start-on-flymake-mode (when (> (num-processors) 8) t))
  ;; (setq flymake-no-changes-timeout (when (> (num-processors) 8) 0.8))
  (setq flymake-no-changes-timeout 0.8)
  (setq flymake-start-on-save-buffer t)  ;; Do not enable or it will enable it on save
  (setq flymake-suppress-zero-counters t)

  (with-no-warnings
    (add-hook 'grep-mode-hook #'hs-line-mode))
  (with-eval-after-load 'icomplete
    (define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-force-complete-and-exit))

  ;; Modify all of them
  ;; ORIGINAL:               "[-–!|#%;>*·•‣⁃◦ 	]*"
  ;; This prevents fill from adding - to every new line
  (setq adaptive-fill-regexp "[  !|#%;>*·•‣⁃◦   ]*")

  ;; Display the current line and column numbers in the mode line
  ;; Non-nil if searches and matches should ignore case.
  ;; nil means case is significant.
  (setq-default case-fold-search nil)

  (setq persist-text-scale-handle-file-renames t)

  (setq vterm-timer-delay 0.001)  ;; Only works when added after :config
  (setq vterm-max-scrollback 1)

  (setq eat-enable-yank-to-terminal t
        eat-enable-directory-tracking t
        eat-enable-shell-command-history t
        eat-enable-shell-prompt-annotation t
        eat-term-scrollback-size nil)
  (add-hook 'eat-mode-hook
            #'(lambda ()
                (my-disable-fringe-truncation-arrow)
                (display-line-numbers-mode -1)
                (setq-local show-paren-mode nil)
                (setq-local line-number-mode nil)
                (setq-local column-number-mode nil)))
  (with-eval-after-load 'eat
    (with-eval-after-load 'evil-collection
      (defun evil-collection-enable-eat-toggle-send-escape ()
        (unless (bound-and-true-p evil-collection-eat-send-escape-to-eat-p)
          ;; Hide "Sending ESC to eat."
          (let ((inhibit-message t))
            (when (fboundp 'evil-collection-eat-toggle-send-escape)
              (funcall 'evil-collection-eat-toggle-send-escape)))))
      (add-hook 'eat-mode-hook 'evil-collection-enable-eat-toggle-send-escape)))

  (setq show-paren-mode nil)

  (setq icomplete-separator "\n")
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-prospects-height 10)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-with-completion-tables t)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-tidy-shadowed-file-names t)

  (add-hook 'emacs-lisp-mode-hook
            #'(lambda()
                (setq-local dabbrev-case-fold-search t)
                (setq-local case-fold-search t)))

  ;; Control whether dabbrev searches should ignore case.
  ;; Any other non-nil version means case is not significant.
  ;; nil means case is significant.
  (setq dabbrev-case-fold-search nil)

  ;; Whether dabbrev applies the abbreviations’s case pattern to the expansion.
  ;; A value of nil means preserve the expansion’s case pattern.
  (setq dabbrev-case-replace nil)

  (setq dabbrev-check-all-buffers nil)

  ;; It configures dabbrev (dynamic abbreviation expansion) to search only in the
  ;; current buffer when expanding abbreviations, instead of searching in other
  ;; buffers as well.
  ;; (setq dabbrev-check-other-buffers t)  ;; Default t

  ;; (dabbrev-abbrev-char-regexp "\\\\sw\\\\|\\\\s_")
  ;; (dabbrev-ignored-buffer-names '("*Messages*" "*Ibuffer*"))
  ;; (dabbrev-limit 1000)
  (setq dabbrev-case-distinction nil)

  (setq epg-gpg-program "gpg2")
  ;; (setq epa-pinentry-mode 'loopback)  ;; Obsolete

  (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)\\(~\\|\\.~[0-9]+~\\)?\\'")

  ;; Manually update the handler alist to recognize the new extension immediately
  (setq file-name-handler-alist
        (cons (cons epa-file-name-regexp #'epa-file-handler)
              file-name-handler-alist))

  (setq large-file-warning-threshold (* 100 1024 1024))  ; 100 Mb

  ;; Add ( and ) to modes such as yaml-ts-mode
  (defvar original-electric-pair-pairs '((40 . 41)   ;; ( and ) for yaml-ts-mode
                                         (123 . 125) ;; { and } for elisp
                                         ;; (91 . 93) ;; [ and ]
                                         ;; Default:
                                         (34 . 34) ;; Double quote
                                         (8216 . 8217)
                                         (8220 . 8221)))

  ;; comments
  (defvar original-electric-pair-text-pairs '((40 . 41) ;; ( and ) for yaml-ts-mode
                                              (123 . 125) ;; { and } for elisp
                                              (96 . 96) ;; ` and ` for elisp comments (instead of `')
                                              ;; (91 . 93) ;; [ and ]
                                              ;; Default:
                                              (34 . 34) ;; Double quote
                                              (8216 . 8217)
                                              (8220 . 8221)))

  (setq electric-pair-text-pairs original-electric-pair-text-pairs)
  (setq electric-pair-pairs original-electric-pair-pairs)

  (setq which-key-idle-delay 1.5)

  ;; It defines the "en_US" spell-check dictionary locally, telling Emacs to use
  ;; UTF-8 encoding, match words using alphabetic characters, allow apostrophes
  ;; inside words, treat non-alphabetic characters as word boundaries, and pass
  ;; -d en_US to the underlying spell-check program.
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

  ;; Configures Aspell's suggestion mode to "ultra", which provides more
  ;; aggressive and detailed suggestions for misspelled words. The language
  ;; is set to "en_US" for US English, which can be replaced with your desired
  ;; language code (e.g., "en_GB" for British English, "de_DE" for German).
  (setq ispell-extra-args '("--sug-mode=ultra"
                            "--lang=en_US"))

  ;; If non-nil, add correction to abbreviation table.
  (setq flyspell-abbrev-p nil)
  ;; (setq flyspell-use-global-abbrev-table-p t)

  (with-eval-after-load 'flyspell
    ;; Remove strings from Flyspell
    (setq flyspell-prog-text-faces (delq 'font-lock-string-face
                                         flyspell-prog-text-faces))

    ;; Remove doc from Flyspell
    (setq flyspell-prog-text-faces (delq 'font-lock-doc-face
                                         flyspell-prog-text-faces)))

  (setq ispell-program-name "aspell")
  (setq ispell-local-dictionary "en_US")

  (with-eval-after-load 'which-key
    (when (bound-and-true-p which-key-buffer-name)
      (add-to-list 'winner-boring-buffers which-key-buffer-name)))

  (with-eval-after-load 'le-aggressive-indent
    (add-hook 'lua-mode-hook 'aggressive-indent-mode))

  (my-update-package-pinned-packages my-package-pinned-packages)

  ;; Add them a second time just in case one of them gets installed
  (my-add-packages-to-load-path)

  ;; Abbrev
  (add-hook 'markdown-mode-hook #'abbrev-mode)
  (add-hook 'markdown-ts-mode-hook #'abbrev-mode)
  (add-hook 'org-mode-hook #'abbrev-mode)
  (define-abbrev-table 'global-abbrev-table
    '(("i" "I")))

  ;; For some reason, post-init ignores this sometimes
  ;; Similar to the default configuration, but without spaces surrounding pairs
  ;; such as (), [], {}
  (setq evil-surround-pairs-alist
        '((?\( . ("(" . ")"))
          (?\[ . ("[" . "]"))
          (?\{ . ("{" . "}"))

          (?\) . ("(" . ")"))
          (?\] . ("[" . "]"))
          (?\} . ("{" . "}"))

          (?# . ("#{" . "}"))
          (?b . ("(" . ")"))
          (?B . ("{" . "}"))
          (?> . ("<" . ">"))
          (?t . evil-surround-read-tag)
          (?< . evil-surround-read-tag)
          (?\C-f . evil-surround-prefix-function)
          (?f . evil-surround-function)))

  (my-evil-config)

  ;; Ensure load-path is accurate even after installing packages
  (my-add-packages-to-load-path)

  (setq user-full-name "user"
        user-mail-address "user@domain.ext")

  ;; Ignore X resources
  (advice-add #'x-apply-session-resources :override #'ignore)

  (with-eval-after-load 'ediff
    (add-hook 'ediff-startup-hook 'ediff-next-difference)
    (add-hook 'ediff-quit-hook #'(lambda()
                                   (with-eval-after-load 'winner
                                     (when (and (bound-and-true-p winner-mode)
                                                (fboundp 'winner-undo))
                                       (winner-undo))))))
  (setq ediff-keep-variants t)  ; Do not kill ediff buffers
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-confirm-copy t)
  (setq diff-default-read-only t)

  ;; This mimics the Magit's diff format by making the hunk header less cryptic,
  ;; and on GUI frames also displays insertion and deletion indicators on the
  ;; left fringe (if it's available).
  ;;
  ;; This is better for patches.
  (setq diff-font-lock-prettify t)

  ;; Use reliable file-based syntax highlighting when available and hunk-based
  ;; syntax highlighting otherwise as a fallback.
  (setq diff-font-lock-syntax 'hunk-also)

  ;; (setq diff-advance-after-apply-hunk t)

  ;; This sometimes interacts poorly with the undo mechanism
  ;; (setq diff-update-on-the-fly t)

  ;; Set this to nil if you want to do it on demand, with my `agitate' package
  ;; (setq diff-refine nil)

  (add-hook 'diff-mode-hook #'outline-minor-mode)

  (setq vertico-count 13)
  (setq consult-preview-excluded-files '("\\`/[^/|:]+:" "\\.asc\\'"
                                         "\\`/[^/|:]+:" "\\.gpg\\'"))
  (add-hook 'embark-collect-mode-hook
            #'(lambda()
                ;; TODO: What sets this to t?
                (setq make-window-start-visible nil)
                (my-disable-fringe-truncation-arrow)))

  (add-hook 'embark-collect-mode-hook
            (lambda ()
              "Disable auto-hscroll in embark-collect buffers."
              (setq-local auto-hscroll-mode nil)))

  (with-eval-after-load 'consult
    (setq consult-fd-args
          (concat (if lightemacs--fdfind-executable
                      lightemacs--fdfind-executable
                    "fd")
                  ;; This config
                  " --type f"

                  ;; Lightemacs
                  " --hidden --exclude .git --absolute-path"
                  (if (memq system-type '(cygwin windows-nt ms-dos))
                      " --path-separator=/"
                    "")

                  ;; Default
                  " --full-path --color=never")))

  (add-hook 'text-mode-hook #'(lambda () (setq-local indent-tabs-mode nil)))

  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
    (add-hook hook #'(lambda ()
                       (display-fill-column-indicator-mode)
                       (if (fboundp 'my-set-tab-width)
                           (my-set-tab-width 2)
                         (error "Undefined: my-set-tab-width")))))

  (setq read-process-output-max (* 32 1024 1024))

  (unless (display-graphic-p)
    (xterm-mouse-mode 1))

  (windmove-default-keybindings)

  ;; Conflict with XFCE C-m-h
  (global-unset-key (kbd "C-M-h"))
  (global-unset-key (kbd "C-M-l"))
  (global-set-key [next] #'ignore)
  (global-set-key [prior] #'ignore)
  (global-set-key (kbd "M-SPC") #'ignore)  ; Disable cycle spacing
  (global-set-key (kbd "C-SPC") #'ignore)  ; Disable C-SPC mark
  (global-set-key (kbd "<C-prior>") 'my-tab-previous)
  (global-set-key (kbd "<C-next>") 'my-tab-next)
  ;; (global-set-key (kbd "C-?") 'help-command)

  ;; Non-nil means show the equivalent keybinding when M-x has one.
  ;; The value can be a length of time to show the message for.
  ;; If the value is non-nil and not a number, we wait 2 seconds.
  (setq suggest-key-bindings nil)

  ;; (setq tooltip-resize-echo-area t)
  (setq-default line-spacing 0.05)
  (setq enhanced-evil-paredit-handle-paste t)
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq remote-file-name-inhibit-locks t)
  (setq history-delete-duplicates t)
  (setq comint-buffer-maximum-size 10000)
  (setq shell-kill-buffer-on-exit t)
  (setq widget-image-enable nil)
  (setq mode-line-collapse-minor-modes t)  ;; Emacs 31
  (setq abbrev-suggest t)
  (setq copy-directory-create-symlink t)
  (setq max-mini-window-height 0.33)
  (setq echo-keystrokes-help nil) ; Emacs 30
  (setq kept-old-versions 15)
  (setq kept-new-versions 15)

  ;; (setq suggest-key-bindings t)
  ;; Many X desktop environments support a feature called the clipboard manager.
  ;; If you exit Emacs while it is the current “owner” of the clipboard data, and
  ;; there is a clipboard manager running, Emacs transfers the clipboard data to
  ;; the clipboard manager so that it is not lost. In some circumstances, this may
  ;; cause a delay when exiting Emacs; if you wish to prevent Emacs from
  ;; transferring data to the clipboard manager, change the variable
  ;; x-select-enable-clipboard-manager to nil.
  (setq x-select-enable-clipboard-manager nil)

  (setq select-enable-clipboard t)
  (setq select-enable-primary nil)

  ;; (setq yank-excluded-properties
  ;;       '(category field follow-link fontified font-lock-face help-echo
  ;;                  intangible invisible keymap local-map mouse-face read-only
  ;;                  yank-handler))

  ;; When I copy paste from an org buffer into the minibuffer, it somehow
  ;; inherits the colors
  (setq yank-excluded-properties t)

  ;; Shows all options when running apropos. For more info,
  ;; (use-package calendar
  ;;   :ensure nil
  ;;   :commands calendar
  ;;   :custom
  ;;   )
  (setq calendar-week-start-day 1)

  (setq echo-keystrokes 0)  ;; Do not show keystrokes in the mini buffer

  (setq delete-pair-blink-delay 0)

  ;; Other things
  (setq tab-bar-close-tab-select 'right)

  (setq history-length 200)

  ;; (setq kill-ring-max 60)
  (setq kill-ring-max 240)
  (setq mark-ring-max 32)

  ;; testing
  (setq transient-detect-key-conflicts t)


  (setq remote-file-name-inhibit-auto-save t
        remote-file-name-inhibit-auto-save-visited t
        ;; yank-pop-change-selection t
        ;; kill-whole-line t
        ;; list-matching-lines-jump-to-current-line t
        ;; mouse-prefer-closest-glyph t
        ;; next-error-message-highlight 'keep
        ;; read-char-by-name-sort 'code
        ;; revert-buffer-quick-short-answers t
        ;; shift-select-mode 'permanent
        ;; track-eol t

        ;; The benefit of visual-order-cursor-movement t is that when editing text
        ;; containing both left-to-right and right-to-left scripts, cursor
        ;; movement aligns with how the text is visually presented on the screen.
        ;; This means pressing C-f moves the cursor to the character visually to
        ;; the right, and C-b moves it to the character visually to the left,
        ;; regardless of the underlying logical (buffer) order. It provides a
        ;; navigation model that matches human reading habits in mixed-script
        ;; documents, reducing cognitive load and making cursor movement
        ;; predictable in visually complex bidirectional contexts.
        ;; visual-order-cursor-movement t

        ;; what-cursor-show-names t
        ;; help-enable-symbol-autoload t
        ;; help-enable-completion-autoload t
        ;; help-enable-symbol-autoload t
        ;; help-window-select t
        ;; help-clean-buttons t
        ;; help-enable-variable-value-editing t
        )

  (setq completion-auto-select nil  ; Alternative: 'second-tab
        completions-detailed t
        completions-format 'vertical
        ;; completions-format 'one-column
        completions-group t
        completions-group-sort 'alphabetical)

  ;; TODO: minimal emacs?
  (setq debugger-bury-or-kill 'kill)

  (setq tramp-default-remote-shell "/bin/bash")


  ;; (setq byte-compile-warnings
  ;;       '(not
  ;;         ;; free-vars   ;; Using variables not defined with defvar (catches typos)
  ;;         unresolved  ;; Calling functions that aren't defined yet
  ;;         ;; noruntime   ;; Using functions/macros only available at compile-time
  ;;         lexical     ;; Missing "lexical-binding: t" header (the .dir-locals warning)
  ;;         ;; make-local  ;; Variables being made buffer-local in potentially odd ways
  ;;         obsolete
  ;;         ))  ;; Use of deprecated functions slated for removal

  ;; Update paths
  (setq undo-fu-session-directory
        (expand-file-name "undo-fu-session"
                          my-shared-user-emacs-directory))

  (setq savehist-file (expand-file-name "history" my-shared-user-emacs-directory))

  (setq persist-text-scale-file (expand-file-name "persist-text-scale"
                                                  my-shared-user-emacs-directory))

  (setq prescient-save-file (expand-file-name "prescient-save.el"
                                              my-shared-user-emacs-directory))

  (setq backup-directory-alist
        `(("." . ,(expand-file-name "backup" my-shared-user-emacs-directory))))
  (setq tramp-backup-directory-alist backup-directory-alist)

  (setq auto-save-list-file-prefix
        (expand-file-name "autosave/" my-shared-user-emacs-directory))
  (setq tramp-auto-save-directory
        (expand-file-name "tramp-autosave/" my-shared-user-emacs-directory))

  (setq save-place-file (expand-file-name "saveplace" my-shared-user-emacs-directory))

  (setq abbrev-file-name (expand-file-name "abbrev_defs" my-shared-user-emacs-directory))

  (setq easysession-debug t)
  (setq easysession-refresh-tab-bar t)

  (setq easysession-directory
        (expand-file-name "easysession" my-shared-user-emacs-directory))

  (setq my-project-list-file-auto
        (expand-file-name "projects-auto"
                          my-shared-user-emacs-directory))

  (setq recentf-save-file
        (expand-file-name "recentf" my-shared-user-emacs-directory)))

;;; Useful functions

(defun my-dir-config--buffer-cwd ()
  "Return the directory associated with the current buffer.
Returns:
- The directory path if the buffer is in `dired-mode', or
- The directory of the file if the buffer is visiting a file, or
- nil if neither condition is met."
  (let ((file-name (buffer-file-name (buffer-base-buffer))))
    (cond ((derived-mode-p 'dired-mode)
           default-directory)

          (file-name
           (file-name-directory file-name)))))

(defun buffer-cwd ()
  "Return the directory of the current buffer."
  (interactive)
  (or (my-dir-config--buffer-cwd) default-directory))

;;; Startup time

(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs loaded in %.2f seconds (Init only: %.2fs) with %d garbage collections."
           (time-to-seconds (time-since before-init-time))
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time 200)

;;; Ignored errors

(add-to-list 'debug-ignored-errors 'search-failed)
(add-to-list 'debug-ignored-errors 'invalid-read-syntax)  ; Wrong syntax (PDF)


(add-to-list 'debug-ignored-errors "This function supports only emacs-lisp-mode")

(add-to-list 'debug-ignored-errors "Cannot find a suitable checker")

;; (add-to-list 'debug-ignored-errors "Attempt to delete the sole visible or iconified frame")

(add-to-list 'debug-ignored-errors "Already at top level of the outline")

(add-to-list 'debug-ignored-errors "This buffer cannot use ‘imenu-default-create-index-function’")

;; Debugger entered--Lisp error: (permission-denied "Setting current directory"
;; "Permission denied" "/dir/")
(add-to-list 'debug-ignored-errors 'permission-denied)

;; Debugger entered--Lisp error: (invalid-regexp "Unmatched [ or [^")
;;   evil-ex-search-find-next-pattern(("[\"" t t) forward)
;;   evil-ex-find-next(("[\"" t t) forward t)
;;   evil-ex-search-full-pattern("[\"" nil forward)
;;   evil-ex-start-search(forward nil)
;;   evil-ex-search-forward(nil)
;;   funcall-interactively(evil-ex-search-forward nil)
;;   command-execute(evil-ex-search-forward)
(add-to-list 'debug-ignored-errors 'invalid-regexp)

;; Debugger entered--Lisp error: (error "Accessing an empty ring")
;;   error("Accessing an empty ring")
;;   ring-ref((0 0 . [nil nil nil nil nil nil nil nil nil nil]) 0)
;;   evil-repeat(nil nil)
;;   funcall-interactively(evil-repeat nil nil)
;;   command-execute(evil-repeat)
(add-to-list 'debug-ignored-errors "Accessing an empty ring")

;; goto last change
;; ----------------
;; Debugger entered--Lisp error: (error "Negative arg: Cannot reverse as the
;; first operation")
;; error("Negative arg: Cannot reverse as the first operation")
;; goto-last-change(-)
;; goto-last-change-reverse(nil)
;; evil-goto-last-change-reverse(nil)
;; funcall-interactively(evil-goto-last-change-reverse nil)
;; command-execute(evil-goto-last-change-reverse)
(add-to-list 'debug-ignored-errors "Negative arg: Cannot reverse as the first operation")

;; goto-chg
(add-to-list 'debug-ignored-errors "Buffer has not been changed")

;; Paredit
(add-to-list 'debug-ignored-errors "Mismatched parenthesis depth")
(add-to-list 'debug-ignored-errors "Mismatched character quotation")
(add-to-list 'debug-ignored-errors "Mismatched comment state:")
(add-to-list 'debug-ignored-errors "Mismatched string state:")

;; Outline next/previous heading
(add-to-list 'debug-ignored-errors 'outline-before-first-heading)  ;; (outline-back-to-heading) and (show-children)
(add-to-list 'debug-ignored-errors "No previous same-level heading")
(add-to-list 'debug-ignored-errors "No following same-level heading")

(add-to-list 'debug-ignored-errors "Bad diff region number")

;;; Other settings

;; Control ^ = Control
;; Command = Fn/Globe
;; Fn/Globe = Command
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)

;; Ignore local variables that are declared in files
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Local-Variables.html
;; Enable loading variables from .dir-locals.el
(setq enable-local-variables :safe)  ;; Also loads variables that are in comments
;; (setq enable-local-variables nil)
(setq enable-local-eval nil)

;;; ibuffer

(setq ibuffer-filter-group-name-face '(:inherit (success bold)))
(setq
 ;; ibuffer-hidden-filter-groups nil

 ;; The number of hours before a buffer is considered "old".
 ibuffer-old-time 5

 ;; If non-nil, then forward and backwards movement commands cycle.
 ibuffer-movement-cycle nil

 ibuffer-show-empty-filter-groups nil

 ;; If non-nil, don’t ask for confirmation of "dangerous" operations.
 ibuffer-expert t

 ;; If non-nil, summarize Ibuffer columns.
 ibuffer-display-summary nil

 ;; If non-nil, display Ibuffer in another window by default.
 ;; ibuffer-use-other-window nil

 ibuffer-use-header-line nil
 ;; ibuffer-default-shrink-to-minimum-size t

 )

(with-eval-after-load 'ibuffer
  (progn
    ;; Add size
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))

    (setq ibuffer-formats
          '((mark modified read-only " "
                  (name 55 55 :left :nil) " "
                  (size-h 9 -1 :right) " "
                  (mode 16 16 :left :elide) " "
                  filename-and-process))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (require 'ibuf-ext)
              (when (fboundp 'ibuffer-switch-to-saved-filter-groups)
                (ibuffer-switch-to-saved-filter-groups "default"))))

  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Org" (or (name . "^\\*Calendar\\*$")
                            (name . "^\\*Org Agenda")
                            (name . "^ \\*Agenda")
                            (mode . org-mode)
                            (name . "^diary$")
                            (mode . muse-mode)
                            (mode . org-mode)
                            (mode . org-agenda-mode)))

                 ("Emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")
                           (name . "^\\*Warnings\\*$")
                           (name . "^\\*Async-native-compile-log\\*$")
                           (name . "^\\*dashboard\\*$")
                           (name . "^\\*compilation\\*$")
                           (name . "^\\*Backtrace\\*$")
                           (name . "^\\*Packages\\*$")
                           (name . "^\\*Customize\\*$")
                           (name . "^\\*\\(Echo\\|Minibuf\\)")))

                 ("Help" (or (name . "^\\*Help\\*$")
                             (name . "^\\*Apropos\\*$")
                             (name . "^\\*info\\*$")
                             (mode . Man-mode)
                             (mode . woman-mode)))

                 ("Repl" (or (mode . gnuplot-comint-mode)
                             (mode . inferior-emacs-lisp-mode)
                             (mode . inferior-python-mode)))

                 ("Term" (or (mode . term-mode)
                             (mode . shell-mode)
                             (mode . vterm-mode)
                             (mode . compilation-mode)
                             (mode . eshell-mode)))

                 ("VC" (or (mode . diff-mode)
                           (derived-mode . log-view-mode)))

                 ("Starred" (starred-name))

                 ("Programming" (and (derived-mode . prog-mode)
                                     (not (starred-name))))

                 ("Text" (and (derived-mode . text-mode)
                              (not (starred-name))))

                 ("Dired" (or (mode . dired-mode)
                              (mode . sr-mode)))

                 ("Other" (name . ".*"))

                 ;; (add-hook 'ibuffer-mode-hook
                 ;;           (lambda ()
                 ;;             (ibuffer-switch-to-saved-filter-groups "custom")
                 ;;             (setq ibuffer-hidden-filter-groups nil)))

                 ;; ("Conf" (or (mode . yaml-mode)
                 ;;             (mode . yaml-ts-mode)
                 ;;             (mode . conf-mode)))

                 ;; ("Coq" (or
                 ;;         (mode . coq-mode)
                 ;;         (name . "\\<coq\\>")
                 ;;         (name . "_CoqProject")))
                 ;; ("code" (or (mode . emacs-lisp-mode)
                 ;;             (mode . yaml-mode)
                 ;;             (mode . yaml-ts-mode)
                 ;;             (mode . cperl-mode)
                 ;;             (mode . c-mode)
                 ;;             (mode . java-mode)
                 ;;             (mode . idl-mode)
                 ;;             (mode . web-mode)
                 ;;             (mode . lisp-mode)
                 ;;             (mode . js2-mode)
                 ;;             (mode . c++-mode)
                 ;;             (mode . lua-mode)
                 ;;             (mode . cmake-mode)
                 ;;             (mode . ruby-mode)
                 ;;             (mode . css-mode)
                 ;;             (mode . objc-mode)
                 ;;             (mode . sql-mode)
                 ;;             (mode . python-mode)
                 ;;             (mode . php-mode)
                 ;;             (mode . sh-mode)
                 ;;             (mode . json-mode)
                 ;;             (mode . scala-mode)
                 ;;             (mode . go-mode)
                 ;;             (mode . erlang-mode)))

                 ;; ("Unsaved" (modified))
                 ;; ("erc" (mode . erc-mode))
                 ;; ("Browser" (or (mode . eww-mode)
                 ;;                (mode . xwidget-webkit-mode)))
                 ;; ("Mail" (or (mode . mail-mode)
                 ;;             (mode . message-mode)
                 ;;             (derived-mode . gnus-mode)))
                 ;; ("Dict" (or (mode . fanyi-mode)
                 ;;             (mode . dictionary-mode)))
                 ;; ("Magit" (or (mode . magit-repolist-mode)
                 ;;              (mode . magit-submodule-list-mode)
                 ;;              (mode . git-rebase-mode)
                 ;;              (derived-mode . magit-section-mode)
                 ;;              (mode . vc-annotate-mode)))
                 ;; ("IRC" (or (mode . rcirc-mode)
                 ;;            (mode . erc-mode)))

                 ;; ("gnus" (or (mode . message-mode)
                 ;;             (mode . bbdb-mode)
                 ;;             (mode . mail-mode)
                 ;;             (mode . gnus-group-mode)
                 ;;             (mode . gnus-summary-mode)
                 ;;             (mode . gnus-article-mode)
                 ;;             (name . "^\\.bbdb$")
                 ;;             (name . "^\\.newsrc-dribble")))
                 )))))

;;; C-g

;; NOTE: Issue. When I press C-g while selecting text, it moves the cursor
(defun my/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'my/keyboard-quit-dwim)


;;; Setup scratch

(defvar-local my-scratch-setup-done nil)

(defun my-setup-scratch-buffer ()
  "Setup the scratch buffer."
  (let ((buffer (get-buffer "*scratch*")))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless my-scratch-setup-done
          (setq my-scratch-setup-done t)
          (setq fill-column 60))))))

(advice-add 'scratch-buffer :after #'my-setup-scratch-buffer)

;;; Display line numbers

(defun my-setup-display-line-numbers-mode ()
  "Setup `display-line-numbers-mode'."
  (cond
   ((or (derived-mode-p 'markdown-mode)
        (derived-mode-p 'org-mode))
    (setq-local display-line-numbers-type 'relative)
    (display-line-numbers-mode 1))

   (t
    ;; (setq-local display-line-numbers-type 'visual)
    (display-line-numbers-mode 1))))

;; TODO replace add hook with add-hook-text-editing-modes
;; (add-hook-text-editing-modes 'my-setup-display-line-numbers-mode)
(add-hook 'prog-mode-hook #'my-setup-display-line-numbers-mode)
(add-hook 'text-mode-hook #'my-setup-display-line-numbers-mode)
(add-hook 'conf-mode-hook #'my-setup-display-line-numbers-mode)

(add-hook 'helpful-mode-hook #'my-setup-display-line-numbers-mode)
(add-hook 'dired-mode-hook #'my-setup-display-line-numbers-mode)
(add-hook 'org-agenda-mode-hook #'my-setup-display-line-numbers-mode)

;; TODO
;; (setq lightemacs-display-line-numbers-mode-target-hooks nil)

;; Use absolute numbers; 'relative and 'visual are significantly slower
;; t=absolute
;; (setq-default display-line-numbers-type t)
(setq-default display-line-numbers-type 'visual)
(setq display-line-numbers-grow-only t)  ; t is slow. Use nil.
(setq display-line-numbers-current-absolute nil)  ;; t=line num / nil=0

;;; Sync dictionary

;; Silence: Truncate long lines disabled
;; TODO: Should we execute it after inserting anything in the speller
(defun run-sync-spell-dict-if-exists ()
  "Run sync-spell-dict command if it exists."
  (when (executable-find "sync-spell-dict")
    (shell-command "sync-spell-dict >/dev/null 2>&1 & disown")))

(add-hook 'kill-emacs-hook 'run-sync-spell-dict-if-exists)

;;; apheleia

(with-eval-after-load 'apheleia
  (setq apheleia-mode-alist nil)
  (setq apheleia-formatters nil)

  ;; Elisp
  (setf (alist-get 'lisp-indent apheleia-formatters)
        'apheleia-indent-lisp-buffer)
  (setf (alist-get 'emacs-lisp-mode apheleia-mode-alist) 'lisp-indent)

  ;; Bash shell scripts
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "--binary-next-line"
          "-filename" filepath
          (when (and apheleia-formatters-respect-indent-level
                     (boundp 'sh-basic-offset))
            (list "-i" (number-to-string sh-basic-offset)))
          "-"))

  ;; Python
  (setf (alist-get 'autopep8 apheleia-formatters)
        '("autopep8"
          "--max-line-length=79"
          "--aggressive"
          ;; "--aggressive"
          "-"))
  (setf (alist-get 'isort apheleia-formatters) '("isort" "--stdout" "-"))


  (setf (alist-get 'bash-ts-mode apheleia-mode-alist) '(shfmt))
  (setf (alist-get 'sh-mode apheleia-mode-alist) '(shfmt))

  (setf (alist-get 'python-mode apheleia-mode-alist) '())
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '())
  (setf (alist-get 'python-mode apheleia-mode-alist) '(isort autopep8))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(isort autopep8))
  )

;;; Elisp

;; (defun better-jump--setup-imenu-elisp ()
;;   "Setup imenu."
;;   (setq-local imenu-generic-expression
;;               (cl-remove-if (lambda (item)
;;                               (or (string= (car item) "Packages")
;;                                   (string= (car item) "Variables")
;;                                   (string= (car item) "Types")))
;;                             imenu-generic-expression)))

(defun better-jump--setup-imenu-elisp ()
  "Setup imenu."
  (setq-local imenu-generic-expression
              (seq-remove (lambda (item)
                            (member (car item) '("Packages"
                                                 "Variables"
                                                 "Types")))
                          imenu-generic-expression)))

(add-hook 'emacs-lisp-mode-hook #'better-jump--setup-imenu-elisp)

;;; Flymake

(defun my-path-inside-p (path1 path2)
  "Check if PATH2 is inside PATH1."
  (let ((absolute-path1 (file-truename path1))
        (absolute-path2 (file-truename path2)))
    (string-prefix-p absolute-path1 absolute-path2)))

(defun my-code-checker-allowed-p (&optional file-name)
  "Return t if code checking is allowed for current buffer or specified file.

If FILE-NAME is provided and non-nil, use it as the filename. Otherwise, use the
buffer's associated file name.

Returns: boolean: t if code checking is allowed, nil otherwise."
  (if (bound-and-true-p config-buffer-enable-syntax-checkers)
      t
    (let* ((file-name (if file-name
                          file-name
                        (buffer-file-name (buffer-base-buffer))))
           (base-name (when file-name
                        (file-name-nondirectory file-name))))
      (when (and file-name
                 base-name
                 (not (string-match-p "cookiecutter" file-name))
                 (not (string-match-p "/forks/" file-name))
                 (not (string-prefix-p "tmp-" base-name))
                 (my-path-inside-p "~/src" file-name)
                 (not (my-path-inside-p "~/src/other" file-name))
                 (not (string-suffix-p "/PKGBUILD" file-name))
                 (not (string-suffix-p ".ebuild" file-name)))
        (setq-local config-buffer-enable-syntax-checkers t)
        t))))

(defun my-limit-package-lint-flymake-setup-a (orig-fn &rest args)
  "Limit package lint flymake setup.
ORIG-FN and ARGS is the functions and its arguments."
  ;;(setq-local package-lint-main-file
  ;;            (file-name-nondirectory
  ;;             (buffer-file-name (buffer-base-buffer))))
  (when (my-code-checker-allowed-p)
    (let* ((filename (buffer-file-name (buffer-base-buffer)))
           (basename (if filename (file-name-nondirectory filename) "")))
      (when (and filename
                 (not (string= basename ".dir-locals.el"))
                 (not (string= basename ".dir-config.el"))
                 (not (string= basename ".dir-settings.el"))
                 (not (string= basename "init.el"))
                 (not (string= basename "early-init.el"))
                 (not (string-prefix-p "le-" basename)))
        (apply orig-fn args)))))

(with-eval-after-load 'le-package-lint-flymake
  (advice-add 'package-lint-flymake-setup :around
              #'my-limit-package-lint-flymake-setup-a))

;; ignore pckage lint: The word "emacs" is redundant in Emacs package names.

(defun my-package-lint-ignore (orig-fun desc)
  "Bypass the \"emacs\" name check for files in a specific directory.
ORIG-FUN is the advised function.  DESC is the package description struct."
  (let ((target-dir (expand-file-name "~/src/emacs/lightemacs")))
    (if (and (buffer-file-name (buffer-base-buffer))
             (file-in-directory-p buffer-file-name target-dir))
        ;; Condition met: return nil to skip the original function
        nil
      ;; Condition not met: execute the original function
      (funcall orig-fun desc))))

(with-eval-after-load 'package-lint
  ;; Apply the :around advice to the specific package-lint function

  (advice-add 'package-lint--check-package-summary :around
              #'my-package-lint-ignore)

  (advice-add 'package-lint--check-no-emacs-in-package-name :around
              #'my-package-lint-ignore))



;;; dired

(defun my-dired-get-file-open-command (file-path)
  "Return FILE-PATH corresponding command from `dired-guess-shell-alist-user'."
  (let* ((case-fold-search nil)
         (result (seq-find (lambda (pattern)
                             (string-match-p (car pattern) file-path))
                           dired-guess-shell-alist-user)))
    (when result
      (car (cdr result)))))

(defun my-dired-open-with-external-command ()
  "Open the current file in `dired' using an external command based on file type."
  (interactive nil dired-mode)
  (if (and (fboundp 'dired-get-file-for-visit)
           (fboundp 'dired--find-possibly-alternative-file))
      (let* ((file (dired-get-file-for-visit)))
        (let ((shell-cmd (my-dired-get-file-open-command file)))
          (if shell-cmd
              (progn
                ;; (message "[RUN] %s %s" shell-cmd file)
                (if (fboundp 'quick-fasd-add-path)
                    (quick-fasd-add-path file)
                  (message "Warning: Undefined: `quick-fasd-add-path'"))
                (call-process shell-cmd nil nil nil file))
            (dired--find-possibly-alternative-file file))))
    (error "Undefined: dired-get-file-for-visit or dired--find-possibly-alternative-file")))

(with-eval-after-load 'dired

  ;; --------------------------------------------------------------------------
  ;; Functions
  ;; --------------------------------------------------------------------------
  (defun my-dired-home ()
    "Dired home."
    (interactive)
    (dired "~/"))

  ;; --------------------------------------------------------------------------
  ;; Abbreviate dired header
  ;; https://emacs.stackexchange.com/questions/33799/is-there-any-way-to-abbreviate-dired-header
  ;;
  ;; I modified it to make it only modify the first line
  ;;
  ;; NOTE: does not work. it sometimes changes where the directory is
  ;; --------------------------------------------------------------------------
  ;; TODO: Contribution to Emacs?
  ;; (defvar dired-abbreviate-header t)
  ;;
  ;; (defun my-dired-readin-abbreviate-header (&rest _)
  ;;   "Abbreviate home directory path to '~' in the first line of the buffer."
  ;;   (when dired-abbreviate-header
  ;;     (save-excursion
  ;;       (goto-char (point-min))
  ;;       (let ((inhibit-read-only t)
  ;;             (case-fold-search nil)
  ;;             (home (expand-file-name "~"))
  ;;             (line-end (line-end-position)))
  ;;         (while (search-forward home line-end t)
  ;;           (replace-match "~" t t))))))
  ;;
  ;; (advice-add 'dired-readin :after 'my-dired-readin-abbreviate-header)

  ;; --------------------------------------------------------------------------
  ;; Using xdg-open/open/start for certain filetypes
  ;; --------------------------------------------------------------------------
  (defvar my-dired-xdg-open-cmd nil)

  (defun my-dired-xdg-open ()
    "Make Dired open the file under the cursor."
    (interactive)
    ;; Removed: (dired-get-filename nil t)
    (if (fboundp 'dired-get-file-for-visit)
        (let* ((file (dired-get-file-for-visit)))
          (when my-dired-xdg-open-cmd
            (call-process my-dired-xdg-open-cmd nil nil nil file)))
      (error "Undefined: dired-get-file-for-visit")))

  (when-let* ((cmd (cond (IS-MAC "open")
                         (IS-LINUX "xdg-open")
                         (IS-WINDOWS "start"))))
    (when cmd
      (setq my-dired-xdg-open-cmd cmd)
      (setq dired-guess-shell-alist-user
            `(("\\.\\(?:docx\\|pdf\\|odt\\|odg\\|ods\\|djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpe?g\\|webp\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|m4a\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ;; ("\\.csv\\'" ,cmd)
              ;; ("\\.html?\\'" ,cmd)
              ;; ("\\.md\\'" ,cmd)
              ))))

  ;; Advise `dired-find-file' to use `my-dired-open-with-external-command'
  ;; instead
  (advice-add 'dired-find-file :override #'my-dired-open-with-external-command))

;;; Last resort hjkl

(defun my-forward-char-same-line ()
  "Forward char can go paste the ellipsis.
This function prevents forward char from going to another line, which is
generally one of the lines that are folded."
  (interactive)
  (ignore-errors
    (if (or (derived-mode-p 'fundamental-mode)
            (region-active-p))
        (right-char)
      (when (= (line-number-at-pos)
               (save-excursion (right-char)
                               (line-number-at-pos)))
        (right-char)))))

(defun my-backward-char-same-line ()
  "Backward to the same line."
  (interactive)
  (ignore-errors
    (if (or (derived-mode-p 'fundamental-mode)
            (region-active-p))
        (left-char)
      (if (= (current-column) 0)
          ;; Prevent the cursor from changing the line
          (when (= (line-number-at-pos)
                   (save-excursion (left-char)
                                   (line-number-at-pos)))
            (left-char))
        ;; We can go to the previous line because the cursor could be next to an
        ;; Ellipsis
        (left-char)))))

(with-eval-after-load 'eldoc
  (eldoc-add-command 'my-backward-char-same-line)
  (eldoc-add-command 'my-forward-char-same-line))

;; Last resort hjkl
(when (fboundp 'my-backward-char-same-line)
  (global-set-key (kbd "M-h") #'my-backward-char-same-line))
(when (fboundp 'my-forward-char-same-line)
  (global-set-key (kbd "M-l") #'my-forward-char-same-line))

;;; Persist text scale

(defun my-window-redisplay ()
  "Redisplay window."
  (when (bound-and-true-p text-scale-mode-amount)
    (let ((amount text-scale-mode-amount))
      (when (or (not (bound-and-true-p
                      my-window-redisplay-last-text-scale-amount))
                (/= amount my-window-redisplay-last-text-scale-amount))
        (setq-local my-window-redisplay-last-text-scale-amount amount)
        (let ((window (selected-window)))
          (cond
           ((<= emacs-major-version 27)
            (run-hook-with-args 'window-size-change-functions window))
           ((> emacs-major-version 27)
            (run-hook-with-args 'window-state-change-functions window)))))))

  (run-hooks 'window-configuration-change-hook))

(defun my-persist-text-scale-adjust ()
  "Ensure the window is updated.
I have identified an issue that affects Emacs packages such as eat (terminal)
and visual-fill-column. Functions like `text-scale-increase`,
`text-scale-decrease`, and `text-scale-set` do not trigger hooks like
`window-configuration-change-hook`. As a result, the eat package does not
immediately update the window when the text scale is changed, and
visual-fill-column does not update the margin right away (it updates only after
the window is resized). This function fixes these issues."
  (when (or (derived-mode-p 'eat-mode)
            (bound-and-true-p visual-fill-column-mode))
    (my-window-redisplay)))

(with-eval-after-load 'persist-text-scale
  (defun my-persist-text-scale-function ()
    ;; TODO: Add to the official?
    ;;
    ;; Corfu context menu adjusts the text size based on the size of the
    ;; window from which the text completion is triggered. It should be
    ;; ignored.
    (let ((buffer-name (buffer-name)))
      (cond
       ((string= buffer-name " *transient*")
        :ignore)

       ;; TODO: add to the official one
       ((string-prefix-p "*Embark Export:" buffer-name)
        "c:embark-export")

       ((string-prefix-p "*sdcv:" buffer-name)
        "c:sdcv"))))

  (defvar my-window-redisplay-last-text-scale-amount nil)

  ;; Force windows update
  (progn
    (add-hook 'text-scale-mode-hook #'my-persist-text-scale-adjust))

  (setq persist-text-scale-buffer-category-function 'my-persist-text-scale-function))

;;; text scale ediff

(defun pkg-diff--ediff-control-panel-window ()
  "Return the window displaying the *Ediff Control Panel* buffer, if any."
  (seq-find (lambda (win)
              ;; The Ediff control panel buffer name may have suffixes like
              ;; "<2>", e.g., "*Ediff Control Panel<2>*", if multiple sessions
              ;; are active.
              (string-prefix-p "*Ediff Control Panel"
                               (buffer-name (window-buffer win))))
            (window-list)))

(defun pkg-diff--ediff-buffers ()
  "Return list of live ediff buffers A, B, and C, if bound."
  (when-let* ((window (pkg-diff--ediff-control-panel-window)))
    (with-selected-window window
      (delq nil
            (list (and (bound-and-true-p ediff-buffer-A) ediff-buffer-A)
                  (and (bound-and-true-p ediff-buffer-B) ediff-buffer-B)
                  (and (bound-and-true-p ediff-buffer-C) ediff-buffer-C))))))

(defun pkg-diff--ediff-auto-text-scale (&rest _)
  "Synchronize text scale across all Ediff buffers based on Ediff buffer A."
  (when (and (boundp 'text-scale-mode-hook)
             (bound-and-true-p text-scale-mode-amount))
    (let ((window (pkg-diff--ediff-control-panel-window)))
      (if (not window)
          (with-no-warnings
            (remove-hook 'text-scale-mode-hook #'pkg-diff--ediff-auto-text-scale t))
        (let ((original-buf (current-buffer))
              (original-buf-text-scale-amount text-scale-mode-amount))
          (with-selected-window window
            (when (and (bound-and-true-p ediff-buffer-A)
                       (buffer-live-p ediff-buffer-A))
              (with-current-buffer ediff-buffer-A
                (dolist (buf (pkg-diff--ediff-buffers))
                  (when (and (buffer-live-p buf)
                             (not (eq buf original-buf)))
                    (with-current-buffer buf
                      (with-no-warnings
                        (let ((text-scale-mode-hook
                               (delq 'pkg-diff--ediff-auto-text-scale
                                     text-scale-mode-hook)))
                          (text-scale-set original-buf-text-scale-amount))))))))))))))

(defun pkg-diff--setup-ediff-auto-text-scale ()
  "Add a buffer-local hook to keep text scale synchronized during Ediff sessions.
This installs `pkg-diff--ediff-auto-text-scale` on `text-scale-mode-hook` in
each Ediff buffer when the session starts, and cleans up automatically when the
session ends."
  (with-no-warnings
    (add-hook 'text-scale-mode-hook #'pkg-diff--ediff-auto-text-scale 99 t)))

(add-hook 'ediff-prepare-buffer-hook #'pkg-diff--setup-ediff-auto-text-scale)


;;; Always current window

(defun my-config-display-buffer-alist ()
  "Config display buffer alist."


  ;; Display buffer alist

  (add-to-list 'display-buffer-alist '("\\*pathaction:"
                                       (display-buffer-at-bottom)
                                       (window-height . 0.33)))

  (add-to-list 'display-buffer-alist
               `(,(rx (or "*Org Agenda*" "*Agenda Commands*"))
                 display-buffer-in-side-window
                 (side . right)
                 (slot . 0)
                 (window-parameters . ((no-delete-other-windows . t)))
                 (window-width . 100)
                 (dedicated . t)))

  (add-to-list 'display-buffer-alist '("\\*CPU-Profiler-Report"
                                       (display-buffer-at-bottom)))

  (add-to-list 'display-buffer-alist '("\\*Memory-Profiler-Report"
                                       (display-buffer-at-bottom)))

  (add-to-list 'display-buffer-alist '("\\*Calendar\\*"
                                       (display-buffer-at-bottom)))

  (add-to-list 'display-buffer-alist '("\\*tmux"
                                       (display-buffer-same-window)))

  (add-to-list 'display-buffer-alist '("\\*grep\\*"
                                       (display-buffer-same-window))))

(defun current-window-only--setup-display-buffer-alist ()
  "Setup display buffer alist."
  ;; (add-to-list 'display-buffer-alist '("\\*vc-diff\\*"
  ;;                                      (display-buffer-same-window)))

  ;; (add-to-list 'display-buffer-alist '("\\*vc-change-log\\*"
  ;;                                      (display-buffer-same-window)))

  (add-to-list 'display-buffer-alist '("\\*Man"
                                       (display-buffer-same-window)))

  (add-to-list 'display-buffer-alist '("\\*eat"
                                       (display-buffer-same-window)))

  (add-to-list 'display-buffer-alist '("\\*Memory-Report\\*"
                                       (display-buffer-same-window)))

  (add-to-list 'display-buffer-alist '("\\*helpful"
                                       (display-buffer-same-window)))

  (add-to-list 'display-buffer-alist '("\\*Backtrace\\*"
                                       (display-buffer-same-window)))

  (add-to-list 'display-buffer-alist '("\\*\\(Help\\|eldoc\\)\\*"
                                       (display-buffer-same-window)))

  (add-to-list 'display-buffer-alist '("\\*[Hh]elp:"
                                       (display-buffer-same-window)))

  (add-to-list 'display-buffer-alist '("\\*edit-indirect "
                                       (display-buffer-same-window)))

  (add-to-list 'display-buffer-alist '("\\*Proced\\*"
                                       (display-buffer-same-window)))

  ;; (add-to-list 'display-buffer-alist '("\\magit:"
  ;;                                      (display-buffer-same-window)))

  ;; This uses compile-goto-error
  (add-to-list 'display-buffer-alist '("\\*Embark Export"
                                       (display-buffer-same-window))))

(defun always-current-window---display-buffer-from-compilation-p (_buffer-name _action)
  "Display buffer from compilation."
  (unless current-prefix-arg
    (with-current-buffer (window-buffer)
      (derived-mode-p 'compilation-mode))))

(defun current-window-only-setup ()
  "Make Emacs only use the current window."
  ;; org-mode
  (setq org-src-window-setup 'current-window) ;; Edit source in current window
  (setq org-agenda-window-setup 'current-window)

  ;; Open links in help windows (like links to files) in the current window
  (setq help-window-keep-selected t)

  ;; Compilation buffers. Also used by wgrep buffers / Embark export.
  ;; (push '(always-current-window---display-buffer-from-compilation-p
  ;;         display-buffer-same-window
  ;;         (inhibit-same-window . nil))
  ;;       display-buffer-alist)

  (current-window-only--setup-display-buffer-alist))

(defun lightemacs-user-pre-init ()
  "Pre-init config."
  (my-config-display-buffer-alist)
  (current-window-only-setup))

;;; tree sitter

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        ;; TODO: add markdown to treesit auto
        (markdown
         ;; For split parsers like Markdown, the extra two fields are required:
         ;; 1. "split_parser" indicates that this language uses a parser split
         ;;    into multiple components.
         ;; 2. The directory path (e.g., "tree-sitter-markdown/src") points to
         ;;    the location of the parser source within the repository. Without
         ;;    these, treesit would not be able to find and compile the parser
         ;;    correctly.
         ;;
         ;; A split parser is a Tree-sitter parser that is divided into multiple
         ;; smaller parsers instead of being a single file or module. Each
         ;; smaller parser handles a part of the language, such as different
         ;; syntaxes or embedded languages, and together they form the complete
         ;; parser. This approach makes it easier to manage complex languages,
         ;; like Markdown, which can contain code blocks, inline formatting, and
         ;; other embedded languages. In Emacs, specifying "split_parser" and
         ;; the source directory tells treesit how to find and build all the
         ;; pieces correctly.
         "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
         "split_parser"
         "tree-sitter-markdown/src")
        ;; TODO: add markdown-inline to treesit auto
        (markdown-inline
         "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
         "split_parser"
         "tree-sitter-markdown-inline/src")
        ;; TODO: add php to treesit auto
        (php
         "https://github.com/tree-sitter/tree-sitter-php"
         "master"
         "php/src")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (glsl "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (julia "https://github.com/tree-sitter/tree-sitter-julia")
        (make "https://github.com/tree-sitter-grammars/tree-sitter-make")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (scala "https://github.com/tree-sitter/tree-sitter-scala")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
        (vue "https://github.com/tree-sitter-grammars/tree-sitter-vue")

        (heex "https://github.com/phoenixframework/tree-sitter-heex")
        (janet "https://github.com/sogaiu/tree-sitter-janet-simple")
        (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
        (latex "https://github.com/latex-lsp/tree-sitter-latex")
        (magik "https://github.com/krn-robin/tree-sitter-magik")
        (nix "https://github.com/nix-community/tree-sitter-nix")
        (nu "https://github.com/nushell/tree-sitter-nu")
        (org "https://github.com/milisims/tree-sitter-org")
        (perl "https://github.com/ganezdragon/tree-sitter-perl")
        (proto "https://github.com/mitchellh/tree-sitter-proto")
        (r "https://github.com/r-lib/tree-sitter-r")
        (sql "https://github.com/DerekStride/tree-sitter-sql")
        (surface "https://github.com/connorlay/tree-sitter-surface")
        (typst "https://github.com/uben0/tree-sitter-typst")
        (verilog "https://github.com/gmlarumbe/tree-sitter-verilog")
        (vhdl "https://github.com/alemuller/tree-sitter-vhdl")
        (wast "https://github.com/wasm-lsp/tree-sitter-wasm")
        (wat "https://github.com/wasm-lsp/tree-sitter-wasm")
        (wgsl "https://github.com/mehmetoguzderin/tree-sitter-wgsl")
        (awk "https://github.com/Beaglefoot/tree-sitter-awk")
        (bibtex "https://github.com/latex-lsp/tree-sitter-bibtex")
        (blueprint "https://github.com/huanie/tree-sitter-blueprint")
        (clojure "https://github.com/sogaiu/tree-sitter-clojure")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (dart "https://github.com/ast-grep/tree-sitter-dart")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")))

(defun my-treesit-update-language-grammar ()
  "Update language grammar."
  (interactive)
  (treesit-install-language-grammar 'markdown)
  (treesit-install-language-grammar 'markdown-inline)
  (treesit-install-language-grammar 'python)
  (treesit-install-language-grammar 'bash)
  (treesit-install-language-grammar 'yaml)
  (treesit-install-language-grammar 'json)
  (treesit-install-language-grammar 'html)
  (treesit-install-language-grammar 'lua)
  (treesit-install-language-grammar 'c)
  (treesit-install-language-grammar 'cpp)
  (treesit-install-language-grammar 'dockerfile)
  (treesit-install-language-grammar 'go)
  (treesit-install-language-grammar 'java)
  (treesit-install-language-grammar 'javascript)
  (treesit-install-language-grammar 'php)
  ;; (treesit-install-language-grammar 'toml)
  ;; (treesit-install-language-grammar 'make)
  )

(defun my-setup-yaml-mode ()
  "Config Yaml mode."
  ;; TODO put it back
  ;; (setq-local indent-line-function 'smartindent-indent-relative-to-visible)
  t
  )


;; Highlight $variables
;; This one works, but it also highlights the variable that are in comments
(defvar sh-script-extra-font-lock-keywords
  '(("\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\|[[:digit:]]+\\)"
     (2 font-lock-variable-name-face prepend))))

(defun sh-script-extra-font-lock-activate ()
  "Activate additional font-locking for variables in double-quoted strings."
  (font-lock-add-keywords nil sh-script-extra-font-lock-keywords))

(add-hook 'sh-mode-hook #'sh-script-extra-font-lock-activate)
(add-hook 'bash-ts-mode-hook #'sh-script-extra-font-lock-activate)

;; When auto-mode-alist is bypassed, use a hook function
(defun ansible-detect-and-enable-mode ()
  "Enable `ansible-mode' for YAML files in Ansible-related directories."
  ;; This works better than auto-mode-alist
  (when (and (not (derived-mode-p 'ansible-mode))
             buffer-file-name
             (string-match
              my-ansible-file-regexp
              buffer-file-name)
             (fboundp 'ansible-mode))
    (funcall 'ansible-mode)))

(defun my-config-tree-sitter ()
  "Config Tree Sitter."
  (when (my-treesit-language-available-p 'c)
    (push '(c-mode . c-ts-mode) major-mode-remap-alist))

  (when (my-treesit-language-available-p 'cpp)
    (push '(c++-mode . c++-ts-mode) major-mode-remap-alist))

  (when (my-treesit-language-available-p 'go)
    (add-to-list 'auto-mode-alist '("\.[gG][oO]\\'" . go-ts-mode)))

  (when (my-treesit-language-available-p 'java)
    (push '(java-mode . java-ts-mode) major-mode-remap-alist))

  (when (my-treesit-language-available-p 'json)
    (push '(js-json-mode . json-ts-mode) major-mode-remap-alist))

  (if (my-treesit-language-available-p 'yaml)
      (progn
        (define-derived-mode ansible-mode yaml-ts-mode "Ansible"
          "Major mode for editing Ansible files.")

        (defun my-setup-ansible-mode ()
          ;; (set-syntax-table (copy-syntax-table))

          ;; For pip_pkg==1.0.0
          (modify-syntax-entry ?= ".")

          ;; Make / a punctuation (for, for example, strings like group/package)
          (modify-syntax-entry ?/ ".")

          ;; The vertical bar (|) is used in YAML for literal block scalars.
          ;; Treating it as punctuation (instead of part of a word or symbol)
          ;; ensures it is recognized for its structural role in defining
          ;; literal block scalars rather than being incorrectly identified as
          ;; part of a symbol or key.
          (modify-syntax-entry ?| ".")

          ;; Also treat $ as punctuation, as it is commonly used for embedding
          ;; languages like Bash in Ansible files and for GitHub Actions
          ;; variables.
          ;; (modify-syntax-entry ?$ ".")

          ;; Ensures that (.), (,) and (!) are treated as part of symbols or words
          ;; within YAML documents. In YAML, these characters may be used as part
          ;; of keys in quoted strings.
          ;;
          ;; (.) is for symbols such as: ansible.builtin.command
          (modify-syntax-entry ?. "_")
          (modify-syntax-entry ?, "_")
          (modify-syntax-entry ?! "_"))
        (when (fboundp 'my-setup-ansible-mode)
          (add-hook 'ansible-mode-hook #'my-setup-ansible-mode))

        ;; Remove the auto-mode-alist entry (Useful to prevent yaml-ts-mode from
        ;; activating on ansible-mode)
        (with-eval-after-load 'yaml-ts-mode
          (setq auto-mode-alist
                (rassq-delete-all 'yaml-ts-mode auto-mode-alist))

          (push '(yaml-mode . yaml-ts-mode) major-mode-remap-alist))

        ;; Treesitter
        (add-hook 'yaml-ts-mode-hook #'my-setup-yaml-mode))
    (when (fboundp 'yaml-mode)
      (define-derived-mode ansible-mode yaml-mode "Ansible"
        "Major mode for editing Ansible files.")))

  (defvar my-ansible-file-regexp
    (rx "/"
        (group (or "tasks"
                   "handlers"
                   "vars"
                   "defaults"
                   "ansible"
                   "playbooks"))
        "/" (+ (not (any "/\\")))
        "." (regexp "[yY][aA]?[mM][lL]")
        string-end))

  (add-to-list 'auto-mode-alist (cons my-ansible-file-regexp 'ansible-mode))

  (add-hook 'yaml-mode-hook #'ansible-detect-and-enable-mode)
  (add-hook 'yaml-ts-mode-hook #'ansible-detect-and-enable-mode)

  (if (my-treesit-language-available-p 'bash)
      (progn
        (push '(shell-script-mode . bash-ts-mode) major-mode-remap-alist)
        (push '(sh-mode . bash-ts-mode) major-mode-remap-alist))
    ;; use-package sh-mode
    ;; :ensure nil
    ;; :commands shell-script-mode
    ;; :mode (("\\.sh\\'" . shell-script-mode)
    ;;        ("\\.bash\\'" . shell-script-mode)
    ;;        ("\\.pbs\\'" . shell-script-mode))
    ;; :custom
    (with-eval-after-load 'sh-script
      (when (fboundp 'sh-indent-supported)
        (sh-indent-supported (append sh-indent-supported '((bash . sh)))))))

  (if (my-treesit-language-available-p 'javascript)
      (progn
        (push '(js2-mode . js-ts-mode) major-mode-remap-alist)
        (push '(js-mode . js-ts-mode) major-mode-remap-alist)
        (add-to-list 'auto-mode-alist '("\.[jJ][sS]\\'" . js-ts-mode)))
    (progn
      (add-to-list 'auto-mode-alist '("\.[jJ][sS]\\'" . js-mode))

      ;; Not required
      ;; (use-package js2-mode
      ;;   :commands js2-mode
      ;;   ;; :mode
      ;;   ;; ("\\.js\\'" . js2-mode)
      ;;   )
      ))

  (when (my-treesit-language-available-p 'python)
    (progn
      t
      ;; (with-eval-after-load 'python
      ;;   ;; TODO: should be put it back
      ;;   ;; (setq auto-mode-alist (rassq-delete-all 'python-mode auto-mode-alist))
      ;;
      ;;   ;; Remove python-flymake error: "Cannot find suitable checker" when a Python
      ;;   ;; script is loaded before eglot and the checker isn't found
      ;;   (advice-add 'python-flymake :override #'ignore))
      )
    (push '(python-mode . python-ts-mode) major-mode-remap-alist))


  (when (my-treesit-language-available-p 'markdown)
    (add-to-list 'auto-mode-alist '("\\.[lL][uU][aA]\\'" . lua-ts-mode)))

  (when (my-treesit-language-available-p 'dockerfile)
    (add-to-list 'auto-mode-alist '("/[dD][oO][cC][kK][eE][rR]\\'"
                                    . dockerfile-ts-mode))
    (add-to-list 'auto-mode-alist '("/[dD][oO][cC][kK][eE][rR][fF][iI][lL][eE]\\'"
                                    . dockerfile-ts-mode)))
  )

(add-hook 'lightemacs-after-init-hook #'my-config-tree-sitter)

;;; ansible

(add-to-list 'display-buffer-alist '("\\*ansible-doc"
                                     (display-buffer-same-window)))

(progn
  ;; Patch sent to ansible-doc. Merged, but not released yet.
  ;; commit c6ccdf8069e8a257501394fe6900b5cf5961e625
  ;; Author: James Cherti
  ;; Date:   2025-04-15 10:32:51 -0400
  ;; Prevent ANSI color codes from being inserted into the buffer
  (defun ansible-doc--with-nocolor (orig-fun &rest args)
    "Advice around `ansible-doc-revert-module-buffer' to disable colors.
  Temporarily set the environment variable ANSIBLE_NOCOLOR=1 when
  invoking the original function ORIG-FUN with ARGS."
    (let ((process-environment (cons "ANSIBLE_NOCOLOR=1" process-environment)))
      (apply orig-fun args)))
  (with-eval-after-load 'ansible-doc
    (when (fboundp 'ansible-doc-revert-module-buffer)
      (advice-add 'ansible-doc-revert-module-buffer :around #'ansible-doc--with-nocolor))))

(defun ansible-doc-symbol ()
  "Show ansible doc of the current symbol."
  (let ((inhibit-message t)
        (symbol (thing-at-point 'symbol t)))
    (when (and symbol (fboundp 'ansible-doc))
      (ansible-doc symbol))))

(defun ansible-doc-local-setup-buffer ()
  "Setup `ansible-doc'."
  (setq-local evil-lookup-func 'ansible-doc-symbol))

(add-hook 'ansible-mode-hook 'ansible-doc-local-setup-buffer)

;;; Highlight codetags

;; TODO: do not do anything
;; FIXME: Fix this
;; BUG: there is a bug here
;; XXX: Good job
;; NOTE: This is important
;; HACK! Bad

(defvar highlight-codetags-keywords
  '(("\\<\\(TODO\\|FIXME\\|BUG\\|XXX\\)\\>" 1 font-lock-warning-face prepend)
    ("\\<\\(NOTE\\|HACK\\|DONE\\)\\>" 1 font-lock-doc-face prepend)))

(define-minor-mode highlight-codetags-local-mode
  "Highlight codetags like TODO, FIXME..."
  :global nil
  (if highlight-codetags-local-mode
      (font-lock-add-keywords nil highlight-codetags-keywords)
    (font-lock-remove-keywords nil highlight-codetags-keywords))

  (when (bound-and-true-p font-lock-mode)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

;;; Disable arrow in the fringe

(defun my-minibuffer-mode-setup ()
  "Setup minibuffer mode."
  ;; Remove the arrow when the line is wrapped
  (my-disable-fringe-truncation-arrow)
  (setq-local truncate-lines t))

(when (fboundp 'my-minibuffer-mode-setup)
  (add-hook 'minibuffer-setup-hook #'my-minibuffer-mode-setup))

(add-hook-text-editing-modes #'my-disable-fringe-truncation-arrow)

(add-hook-text-editing-modes #'highlight-codetags-local-mode)

;; Enable smerge
(defun my-enable-smerge-maybe ()
  "Enable `smerge'."
  (when (and buffer-file-name (vc-backend buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode +1)))))

(add-hook 'find-file-hook #'my-enable-smerge-maybe)

;; Switch to `fundamental-mode' when `smerge-mode' is activated.
(defun my-smerge-to-fundamental-mode ()
  "Switch to `fundamental-mode' when `smerge-mode' is activated."
  (when (and smerge-mode (not (eq major-mode 'fundamental-mode)))
    (fundamental-mode)
    (smerge-mode 1)))
(add-hook 'smerge-mode-hook #'my-smerge-to-fundamental-mode)

;;; Prune cache

;; TODO lightemacs?
;; TODO minimal-emacs.d?

(defvar my-native-compile-prune-cache-done nil
  "Flag to ensure native compile cache is pruned only once per session.")

(defun my-native-compile-prune-cache ()
  "Prune the native compile cache safely.
This function checks for native compilation support and ensures the operation
only runs once per session to avoid redundant I/O."
  (interactive)
  ;; Check if we have already run this session
  (unless my-native-compile-prune-cache-done
    ;; Check if native compilation is actually available
    (when (and (fboundp 'native-comp-available-p)
               (native-comp-available-p)
               (fboundp 'native-compile-prune-cache))
      ;; Set flag immediately to prevent re-entry
      (setq my-native-compile-prune-cache-done t)
      (message "[native-comp] Native compilation cache pruned")
      (with-demoted-errors "Error pruning native cache: %S"
        (native-compile-prune-cache)))))

;; Only register the timer if native compilation is enabled
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Run once after 5 minutes of idle time
  (add-hook 'kill-emacs-hook 'my-native-compile-prune-cache)

  ;; 5 minutes: This is the standard definition of "Away From Keyboard." If you
  ;; haven't touched Emacs for 5 minutes, you have likely stepped away (coffee,
  ;; meeting, etc.). This is the safest time to burn CPU cycles or disk I/O
  ;; without affecting user experience.
  (add-hook 'lightemacs-after-init-hook
            #'(lambda()
                (run-with-idle-timer (* 5 60) nil #'my-native-compile-prune-cache))))

;;; Prune native comp tmp files

(defun my-clean-native-comp-temp-files ()
  "Delete temporary native compilation files from the temporary directory.
This targets files matching `emacs-async-comp-*' and
`emacs-int-comp-*' (including trampoline files) ending in .el."
  (interactive)
  (let* ((target-dir temporary-file-directory)
         ;; Regex matches both emacs-async-comp- and emacs-int-comp-
         ;;
         ;; Async Compilation Files (emacs-async-comp-): These files are
         ;; generated during the standard asynchronous native compilation of
         ;; Emacs Lisp libraries.
         ;;
         ;; Internal/Trampoline Files (emacs-int-comp-): These
         ;; files—specifically those matching your example
         ;; emacs-int-comp-subr--trampoline—are generated to handle function
         ;; advice on C primitives. The emacs-int-comp- files are specific to
         ;; native compilation. They are not generated by other standard Emacs
         ;; processes. Origin: Emacs primitives (functions written in C, like
         ;; write-region or car) cannot be natively advised (modified) directly.
         ;; When you use advice-add on a C primitive, Emacs must generate a
         ;; "trampoline" function. This small bridge function calls the original
         ;; C code while allowing Lisp-level advice to intervene.
         (regexp "^emacs-\\(?:async\\|int\\)-comp-.*\\.el$")
         ;; (regexp "^emacs-async-comp-.*\\.el$")
         (files (directory-files target-dir t regexp)))
    (dolist (file files)
      ;; Check existence to avoid race conditions
      ;; Safety Check: Explicitly verify the filename starts with "emacs"
      ;; Uses file-name-nondirectory to ignore the path components.
      (if (string-prefix-p "emacs-" (file-name-nondirectory file))
          (progn
            (delete-file file)
            ;; (message "Deleted native-comp temp file: %s" file)
            )
        ;; (message "Safety check failed (skipping): %s" file)
        ))))

;; Run this function automatically when asynchronous native compilation finishes
(add-hook 'native-comp-async-all-done-hook #'my-clean-native-comp-temp-files)
;; (add-hook 'kill-emacs-hook #'my-clean-native-comp-temp-files)

;;; vc

(defun my-log-view-diff-stay (orig-fun &rest args)
  "Advice to keep focus in the log buffer when showing diffs.
ORIG-FUN - the original function being advised
ARGS - the arguments passed to the original function"
  (save-selected-window
    (apply orig-fun args)))

(with-eval-after-load 'log-view
  (advice-add 'log-view-diff :around #'my-log-view-diff-stay))

;;; Provide

(provide 'config)

;; Local variables:
;; byte-compile-warnings: (not lexical free-vars)
;; End:

;;; config.el ends here

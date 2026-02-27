;;; config.el --- Config -*- lexical-binding: t -*-

;;; Commentary:

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

;;; Code:

;;; Debug, native comp, and initial options

(setq debug-on-error t)

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

(setq lightemacs-optional-modules '(mod-defun
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

                                    mod-kirigami))

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

(defun my-config-evil ()
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
      ;; todo.org
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

;; (defun my-disable-fringe-truncation-arrow ()
;;   "Disable the truncation arrow."
;;   (unless (boundp 'fringe-indicator-alist)
;;     (error "The fringe-indicator-alist was not declared"))
;;   (setq fringe-indicator-alist
;;         (cl-remove-if (lambda (item)
;;                         (memq (car item) '(truncation
;;                                            continuation)))
;;                       fringe-indicator-alist))
;;   (push '(continuation nil nil) fringe-indicator-alist)
;;   (push '(truncation nil nil) fringe-indicator-alist))

(defun my-disable-fringe-truncation-arrow ()
  "Disable the truncation arrow."
  (unless (boundp 'fringe-indicator-alist)
    (error "The fringe-indicator-alist was not declared"))
  (setq fringe-indicator-alist
        (seq-remove (lambda (item)
                      (memq (car item) '(truncation continuation)))
                    fringe-indicator-alist))
  (push '(continuation nil nil) fringe-indicator-alist)
  (push '(truncation nil nil) fringe-indicator-alist))

(defun lightemacs-user-init ()
  "This function is executed right before loading modules."
  (with-eval-after-load 'le-dired-filter
    (add-hook 'lightemacs-dired-filter-setup-hook
              'dired-filter-by-git-ignored)

    ;; (when lightemacs-dired-omit-parent-directory
    ;;   (setq dired-omit-files (concat dired-omit-files "\\|^/home/work/\\.\\|~/\\.")))
    )

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

  (my-config-evil)

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

;;; vterm settings

;; TODO lightemacs?
(defun my-setup-vterm ()
  "Better evil integration with `vterm'."
  ;; https://www.reddit.com/r/emacs/comments/xyo2fo/orgmode_vterm_tmux/
  ;; With the first line, you can use a binding like `M-SPC ESC` (`M-SPC`
  ;; being the default alt-leader key) to switch the vterm buffer to evil
  ;; normal state. Most of the time I don't use it, and simply interact with
  ;; the vterm the same exact way I do with any other terminal.
  (with-eval-after-load 'evil-collection
    (when (and (not (bound-and-true-p evil-collection-vterm-send-escape-to-vterm-p))
               (fboundp 'evil-collection-vterm-toggle-send-escape))
      (let ((inhibit-message t))
        ;; TODO add shut-up back?
        (evil-collection-vterm-toggle-send-escape))))

  (setq-local line-number-mode nil)
  (setq-local column-number-mode nil)
  (setq-local cursor-type 'bar)
  (setq mode-line-format nil)

  (when-let* ((proc (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag proc nil)))

(with-eval-after-load 'evil
  (add-to-list 'evil-emacs-state-modes 'vterm-mode))

(add-hook 'vterm-mode-hook 'my-setup-vterm)

(setq vterm-clear-scrollback-when-clearing t)

(setq vterm-max-scrollback 100)
;; (setq vterm-set-bold-hightbright t)
;; (setq vterm-disable-bold t)
;; (setq vterm-copy-exclude-prompt t)
;; "C-x" "C-c" "C-g"
(setq vterm-keymap-exceptions '("M-RET" "C-x" "C-c" "M-x" "M-o" "C-y" "M-y"))
(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")

(defun my-vterm--send-Alt-Shift-H ()
  "Send Alt Shift H to vterm."
  (interactive)
  (when (fboundp 'vterm-send-key)
    (vterm-send-key (kbd "H") t t)))

(defun my-vterm--send-Alt-Shift-L ()
  "Send Alt Shift L to vterm."
  (interactive)
  (when (fboundp 'vterm-send-key)
    (vterm-send-key (kbd "L") t t)))

;;; server

(setq server-client-instructions nil)
(unless (daemonp)
  (add-hook 'lightemacs-after-init-hook #'server-start))

;;; mode line

(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))
(setq mode-line-percent-position nil)

;;; Modeline
(add-hook 'lightemacs-after-init-hook #'display-time-mode)
(setq display-time-mail-function #'ignore)
(setq display-time-mail-string "")
(setq display-time-mail-directory nil)
(setq display-time-use-mail-icon nil)
(setq display-time-mail-face nil)
(setq display-time-format " %Y-%m-%d  %I:%M %p")

(defun mode-line-right ()
  "Render the `mode-line-right-format'."
  (let* ((mode-line-right-format '(mode-line-front-space
                                   mode-line-misc-info
                                   mode-line-end-spaces))
         (formatted-line (format-mode-line mode-line-right-format)))
    (list (propertize
           " "
           'display
           `(space :align-to (+ 2
                                (- right
                                   (+ ,(string-width formatted-line) right-fringe
                                      right-margin)))))
          formatted-line)))

(defun my-gc-cons-threshold-mode-line ()
  "Return a short string with the current `gc-cons-threshold`."
  (format " GC:%s" (cond
                    ((= gc-cons-threshold most-positive-fixnum) "∞")
                    (t (format "%sM" (/ gc-cons-threshold 1000000))))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-modified
                "  |  "
                mode-line-buffer-identification
                "  |  "
                (vc-mode vc-mode)
                (:eval
                 (let ((project-name (my-project-name)))
                   (format "  |  Project:%s" (my-project-name))))
                "  |  "
                mode-line-position
                "  |  "
                (:eval (my-gc-cons-threshold-mode-line))
                ;; mode-line-modes
                ;; Slow eval
                (:eval (mode-line-right))))

;;; flymake fixes

;; TODO: Should this fail silently? (Patch Emacs)
(defun my-flymake-proc-legacy-safe-advice (orig-fun &rest args)
  "Call `flymake-proc-legacy-flymake' safely, ignoring missing init function.

ORIG-FUN is the original `flymake-proc-legacy-flymake` function.
ARGS are the arguments passed to ORIG-FUN.

If the error message contains \"find a suitable init function\", it is
ignored and logged as a warning. All other errors are re-raised."
  (condition-case err
      (apply orig-fun args)
    ((error)
     (let ((error-message (error-message-string err)))
       (if (string-match-p "find a suitable init function"
                           error-message)
           (let ((inhibit-message t))
             (message "[WARNING] Flymake: %s: %s"
                      buffer-file-name
                      error-message))
         (signal (car err) (cdr err)))))))

(with-eval-after-load 'flymake-proc
  (advice-add 'flymake-proc-legacy-flymake :around
              #'my-flymake-proc-legacy-safe-advice))

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

;;; markdown toc

;;; Markdown

(defun my-markdown-toc-gen-if-present ()
  "Gen table of contents if present."
  (when (and (fboundp 'markdown-toc--toc-already-present-p)
             (fboundp 'markdown-toc-generate-toc)
             (funcall 'markdown-toc--toc-already-present-p))
    ;; atomic-change-group in Emacs creates a temporary “transaction” for buffer
    ;; modifications: all changes made inside the group are treated as a single,
    ;; atomic operation for undo purposes. This means that if the group
    ;; completes successfully, all modifications are merged into one undo step,
    ;; so pressing undo will revert everything in the group at once instead of
    ;; step by step. If an error occurs inside the group, the changes are
    ;; automatically rolled back, leaving the buffer unchanged. It also ensures
    ;; that point and mark positions are preserved unless explicitly modified,
    ;; which is why it’s useful for functions like markdown-toc-generate-toc
    ;; where you want the buffer updated without losing your cursor location.
    (atomic-change-group
      (funcall 'markdown-toc-generate-toc))))

(defun my-setup-markdown-toc ()
  "Setup the markdown-toc package."
  (add-hook 'before-save-hook #'my-markdown-toc-gen-if-present 99 t))

(when (fboundp 'my-setup-markdown-toc)
  (add-hook 'gfm-mode-hook #'my-setup-markdown-toc)
  (add-hook 'markdown-ts-mode-hook #'my-setup-markdown-toc)
  (add-hook 'markdown-mode-hook #'my-setup-markdown-toc))

;;; hideshow

(with-eval-after-load 'hideshow
  ;; TODO lightemacs?
  ;; Fringe
  (define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])
  (defcustom hs-fringe-face 'hs-fringe-face
    "*Specify face used to highlight the fringe on hidden regions."
    :type 'face
    :group 'hideshow)
  (defface hs-fringe-face
    '((t (:foreground "#888" :box (:line-width 2 :color "grey75"
                                               :style released-button))))
    "Face used to highlight the fringe on folded regions"
    :group 'hideshow)
  (defcustom hs-face 'hs-face
    "*Specify the face to to use for the hidden region indicator"
    :type 'face
    :group 'hideshow)
  (defface hs-face
    '((t (:foreground "grey" :background unspecified :box nil)))
    "Face to hightlight the ... area of hidden regions"
    :group 'hideshow)

  (defun display-code-line-counts (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* (;;(marker-string "*fringe-dummy*")
             ;; (marker-length (length marker-string))
             (display-string (format "(%d)..."
                                     (count-lines (overlay-start ov)
                                                  (overlay-end ov)))))
        (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
        ;; (put-text-property 0 marker-length 'display
        ;;                    (list 'left-fringe 'hs-marker 'hs-fringe-face)
        ;;                    marker-string)
        ;; (overlay-put ov 'before-string marker-string)
        (put-text-property 0 (length display-string) 'face 'hs-face
                           display-string)
        (overlay-put ov 'display display-string))))
  (setq hs-set-up-overlay 'display-code-line-counts))

;;; auto insert if new file

;; This is called by main.el
(defun my/autoinsert-yas-expand()
  "Replace text with Yasnippet template."
  (when (fboundp 'yas-expand-snippet)
    (condition-case nil
        (progn
          (funcall 'yas-expand-snippet (buffer-string) (point-min) (point-max))
          (when (and (not (bobp))
                     (fboundp 'evil-insert-state)
                     (not (zerop (buffer-size))))
            (evil-insert-state)))
      (error
       nil))))

(defun my-auto-insert-if-new-file ()
  "Auto-insert template only if the file is newly created and does not exist."
  (when-let* ((file-name (buffer-file-name (buffer-base-buffer))))
    (when (and (not (file-exists-p file-name))
               (= (buffer-size) 0))
      ;; Execute the default auto-insert function or custom logic here. For
      ;; simplicity, we invoke `auto-insert` directly.
      (condition-case nil
          (progn
            (when (bound-and-true-p yas-minor-mode) (auto-insert)))
        ;; Ignore errors
        (error
         nil)))))

(defun config-template-system ()
  "Configure the template system."
  ;; Add the custom function to `find-file-hook`
  (add-hook 'find-file-hook 'my-auto-insert-if-new-file)

  ;; :config
  (let ((template-elisp-file (expand-file-name "main.el"
                                               auto-insert-directory)))
    (let ((inhibit-message t))
      (load template-elisp-file :no-error :no-message))))

(setq auto-insert 'other)
(setq auto-insert-query nil)
(setq auto-insert-alist nil)  ;; Will be changed by template-elisp-file
(setq auto-insert-directory (expand-file-name "file-templates-auto/"
                                              "~/.emacs-data/etc")) ;;; Or use custom, *NOTE* Trailing slash important
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion

(add-hook 'lightemacs-after-init-hook 'config-template-system)


;;===========================================================================
;; Geometry
;;===========================================================================
;; TODO Migrate this to lightemacs
(defvar my-frame-geometry-file (expand-file-name "frame-geometry"
                                                 user-emacs-directory))

(defvar my-frame-geometry-modified-p nil
  "Was the frame geometry modified.")

(defun my-frame-geometry-save ()
  "Save the current frame's geometry."
  (when (display-graphic-p)
    (let ((inhibit-message t)
          (frame (selected-frame))
          (file my-frame-geometry-file))
      (with-temp-buffer
        (let ((make-backup-files nil)
              (font (frame-parameter frame 'font))
              (left (frame-parameter frame 'left))
              (top (frame-parameter frame 'top))
              (width (frame-parameter frame 'width))
              (height (frame-parameter frame 'height))
              (pixel-width (frame-pixel-width frame))
              (pixel-height (frame-pixel-height frame)))
          (insert
           ";; -*- mode: emacs-lisp; lexical-binding: t; coding: utf-8-unix -*-\n")
          (insert ";; Frame geometry file, automatically generated "
                  "by 'my-frame-geometry*' functions.\n")
          (insert
           "(setq initial-frame-alist nil)\n"
           (format "(add-to-list 'initial-frame-alist '(font . \"%s\"))\n"
                   (replace-regexp-in-string "\"" "\\\\\"" font))
           (when top
             (format "(add-to-list 'initial-frame-alist '(top . %s))\n" top))
           (when left
             (format "(add-to-list 'initial-frame-alist '(left . %s))\n" left))
           (when width
             (format "(add-to-list 'initial-frame-alist '(width . %s))\n" width))
           (when height
             (format "(add-to-list 'initial-frame-alist '(height . %s))\n" height))
           "\n"
           (when pixel-width
             (format "(setq my-frame-geometry-pixel-width %s)\n" pixel-width))
           (when pixel-height
             (format "(setq my-frame-geometry-pixel-height %s)\n" pixel-height)))
          (when (file-writable-p file)
            (let ((save-silently t))
              (write-file file))))))))

(defun my-frame-geometry-load-initial-frame-alist ()
  "Load the previous frames geometry.
Call it from \='early-init.el\='."
  (let ((file my-frame-geometry-file)
        (inhibit-message t))
    (when (file-readable-p file)
      (load (expand-file-name file) t t t))))

(defun my-frame-geometry-set-pixel-width-height (&optional frame)
  "Set the frame width and height.
Call it from \='init.el\='.
FRAME is the frame. When FRAME is nil, the `selected-frame' function is used."
  (unless frame
    (setq frame (selected-frame)))

  (when (and (display-graphic-p)
             (boundp 'my-frame-geometry-pixel-width)
             (boundp 'my-frame-geometry-pixel-height))
    (message "Set frame size: %sx%s"
             my-frame-geometry-pixel-width
             my-frame-geometry-pixel-height)
    (set-frame-size frame
                    my-frame-geometry-pixel-width
                    my-frame-geometry-pixel-height
                    t)))

(defun my-set-frame-size-and-position (&optional frame)
  "Set position and size of FRAME when it's the first frame."
  (unless frame
    (setq frame (selected-frame)))
  (unless my-frame-geometry-modified-p
    ;; when (eq frame (selected-frame))
    (when (not (frame-parameter frame 'parent-frame))
      (when (fboundp 'my-frame-geometry-set-pixel-width-height)
        (setq my-frame-geometry-modified-p t)
        (my-frame-geometry-set-pixel-width-height frame)))))

(my-frame-geometry-load-initial-frame-alist)
;; (setq initial-frame-alist nil)

(add-hook 'kill-emacs-hook 'my-frame-geometry-save)
;; (add-hook 'after-make-frame-functions #'my-set-frame-size-and-position)

;; Issue with Emacs 31 and shut-up
;; (with-eval-after-load "shut-up"
;;   (with-no-warnings
;;     (defun my-around-my-frame-geometry-save (fn &rest args)
;;       "FN is the advised function. ARGS are the function arguments."
;;       (shut-up
;;         (apply fn args)))
;;
;;     (advice-add 'my-frame-geometry-save :around
;;                 #'my-around-my-frame-geometry-save)))

;;; quiet

;; In addition to the shut-up package, this module provides the
;; `lightemacs-shut-up-advice-add' function, which prevents a function from
;; displaying messages.

;; readme: In addition to the *shut-up* package, this module provides the
;; `lightemacs-shut-up-advice-add` function, which attaches advice to a given
;; function to suppress all of its output.

(defun lightemacs--shut-up-funcall (fn &rest args)
  "Call FN with ARGS while suppressing all output.
This function evaluates FN with the given ARGS while redirecting output that
would normally be sent to `standard-output' and suppressing messages produced by
`message'. It also overrides `write-region' and `load' with custom
implementations that prevent unintended output."
  ;; I have an issue with shut-up TODO especially with straight
  ;; (shut-up
  ;;   (apply fn args))
  (let ((inhibit-message t))
    (apply fn args)
    ;; (cl-letf
    ;;     ;; Override `standard-output' (for `print'), `message',
    ;;     ;; `write-region', `load'.
    ;;     ((standard-output #'ignore)
    ;;      ((symbol-function 'message) 'ignore)
    ;;      ;; ((symbol-function 'write-region) 'shut-up-write-region)
    ;;      ;; ((symbol-function 'write-region) 'shut-up-write-region)
    ;;      ;; ((symbol-function 'load) 'shut-up-load)
    ;;      )
    ;;   (apply fn args))
    ))

;;;###autoload
(defun lightemacs-shut-up-advice-add (fn)
  "Advise the FN function so that all its output is suppressed.
This attaches an around-advice to FN using `lightemacs--shut-up-funcall',
ensuring that when FN is invoked, it produces no messages, does not write to
`standard-output', and does not display output from `write-region' or `load'."
  (advice-add fn :around #'lightemacs--shut-up-funcall))

;;;###autoload
(defun lightemacs-shut-up-advice-remove (fn)
  "Remove the silence advice from the FN function.
This detaches the around-advice previously installed by
`lightemacs-shut-up-advice-add', restoring FN to its original behavior where
messages and output are no longer suppressed."
  (advice-remove fn #'lightemacs--shut-up-funcall))

(with-eval-after-load 'undo-fu-session
  (lightemacs-shut-up-advice-add 'undo-fu-session--recover-safe))

(with-eval-after-load 'evil
  (lightemacs-shut-up-advice-add 'evil-redo)
  (lightemacs-shut-up-advice-add 'evil-undo))

(with-eval-after-load 'sh-script
  (when (fboundp 'sh-set-shell)
    (lightemacs-shut-up-advice-add 'sh-set-shell)))

;; TODO silence it
;; (lightemacs-shut-up-advice-add 'toggle-truncate-lines)

;; (with-eval-after-load 'flyspell
;;   (lightemacs-shut-up-advice-add 'flyspell-prog-mode)
;;   (lightemacs-shut-up-advice-add 'flyspell-mode))

;; (with-eval-after-load 'recentf
;;   (lightemacs-shut-up-advice-add 'recentf-save-list)
;;   (lightemacs-shut-up-advice-add 'recentf-cleanup)
;;   (lightemacs-shut-up-advice-add 'recentf-mode))

;;; recenter after jump


(defvar lightemacs-maybe-recenter-after-jump t
  "Non-nil enables recentering the window when the point jumps out of view.
Recentering only occurs when `scroll-conservatively' is >= 101. The recenter
position can be customized using `lightemacs-maybe-recenter-after-jump-value'.")

(defvar lightemacs-maybe-recenter-after-jump-value 12
  "The line position for recentering the window when the point jumps out of view.
Only used when `lightemacs-maybe-recenter-after-jump' is non-nil and
`scroll-conservatively' is >= 101. A numeric value indicates the number of lines
from the top of the window; nil recenters in the middle.")

(require 'le-core-defun)  ;; lightemacs-recenter-maybe

(defun lightemacs-default-settings--recenter-maybe ()
  "Recenter conditionally when `scroll-conservatively' is set to 101 or higher.
This ensures that conservative scrolling is preserved while maintaining point
visibility when navigation commands are executed."
  (when (and lightemacs-maybe-recenter-after-jump
             (>= scroll-conservatively 101))
    (lightemacs-recenter-maybe lightemacs-maybe-recenter-after-jump-value)))

(defun lightemacs-default-settings--recenter-maybe-adjust-arg ()
  "Recenter conditionally when `scroll-conservatively' is set to 101 or higher.
This ensures that conservative scrolling is preserved while maintaining point
visibility when navigation commands are executed."
  (when (and lightemacs-maybe-recenter-after-jump
             (>= scroll-conservatively 101))
    (lightemacs-recenter-maybe lightemacs-maybe-recenter-after-jump-value t)))

(defun lightemacs-default-settings--advice-recenter-maybe-adjust-arg (fn &rest args)
  "FN is the advised function. ARGS are the function arguments."
  (unwind-protect
      (apply fn args)
    (lightemacs-default-settings--recenter-maybe-adjust-arg)))

;; TODO use post-command-hook?

(defun lightemacs-default-settings--advice-recenter-maybe (fn &rest args)
  "FN is the advised function. ARGS are the function arguments."
  (unwind-protect
      (apply fn args)
    (lightemacs-default-settings--recenter-maybe)))

;; TODO use a loop to add to hooks and advice functions
(with-eval-after-load 'flymake
  (advice-add 'flymake-goto-next-error :around
              #'lightemacs-default-settings--advice-recenter-maybe)
  (advice-add 'flymake-goto-prev-error :around
              #'lightemacs-default-settings--advice-recenter-maybe))

(with-eval-after-load 'evil
  (advice-add 'evil-goto-mark :around
              #'lightemacs-default-settings--advice-recenter-maybe-adjust-arg)

  (advice-add 'evil-ex-search-previous :around
              #'lightemacs-default-settings--advice-recenter-maybe-adjust-arg)
  (advice-add 'evil-ex-search-next :around
              #'lightemacs-default-settings--advice-recenter-maybe-adjust-arg)

  ;; When the user presses C-o
  (add-hook 'evil-jumps-post-jump-hook
            #'lightemacs-default-settings--recenter-maybe-adjust-arg 70))

(with-eval-after-load 'simple
  (add-hook 'next-error-hook
            #'lightemacs-default-settings--recenter-maybe-adjust-arg))

(with-eval-after-load 'xref
  (let ((xref-pulse-originally-present (memq 'xref-pulse-momentarily
                                             xref-after-jump-hook)))
    (remove-hook 'xref-after-jump-hook 'recenter)
    (remove-hook 'xref-after-jump-hook 'xref-pulse-momentarily)

    (add-hook 'xref-after-return-hook
              #'lightemacs-default-settings--recenter-maybe 70)

    (add-hook 'xref-after-jump-hook
              'lightemacs-default-settings--recenter-maybe 70)
    (when xref-pulse-originally-present
      (add-hook 'xref-after-jump-hook 'xref-pulse-momentarily 71))))

;;; Target hooks

;;(setq-local package-lint-main-file
;;            (file-name-nondirectory
;;             (buffer-file-name (buffer-base-buffer))))

(defun my-prevent-execution-only-when-code-checker-allowed (orig-fun &rest args)
  "Execute ORIG-FUN with ARGS only if it is allowed.
This function is intended for use as :around advice."
  (when (and (fboundp 'my-code-checker-allowed-p)
             (my-code-checker-allowed-p))
    (apply orig-fun args)))

(with-eval-after-load 'le-apheleia
  (advice-add 'apheleia-mode :around
              #'my-prevent-execution-only-when-code-checker-allowed))

(with-eval-after-load 'le-flymake
  (advice-add 'flymake-mode :around
              #'my-prevent-execution-only-when-code-checker-allowed))

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
           (fboundp 'dired--find-possibly-alternative-file)
           (let* ((file (dired-get-file-for-visit)))
             (let ((shell-cmd (my-dired-get-file-open-command file)))
               (if shell-cmd
                   (progn
                     ;; (message "[RUN] %s %s" shell-cmd file)
                     (if (fboundp 'quick-fasd-add-path)
                         (quick-fasd-add-path file)
                       (message "Warning: Undefined: `quick-fasd-add-path'"))
                     (call-process shell-cmd nil nil nil file))
                 (dired--find-possibly-alternative-file file)))))
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

;;; Better evil

(defun my-save-buffers-kill-emacs ()
  "Handle quitting Emacs with daemon-aware frame management."
  (interactive)
  (if (and (daemonp)
           (fboundp 'easysession-save-session-and-close-frames))
      (easysession-save-session-and-close-frames)
    (save-buffers-kill-emacs)))

(defun evilbuffer-switch-to-scratch-and-clear ()
  "Switch to the *scratch* buffer."
  (interactive)
  (scratch-buffer))

(defun evilbuffer-toggle-truncate-line ()
  "Toggle truncate line."
  (interactive)
  (if truncate-lines
      ;; When the text is wrapped
      (progn
        ;; (visual-line-mode 1)
        ;; (setq-local my-was-visual-line-fill-column-mode
        ;;             (and (boundp 'my-was-visual-line-fill-column-mode)
        ;;                  my-was-visual-line-fill-column-mode))
        ;; (when my-was-visual-line-fill-column-mode
        ;;   (visual-line-fill-column-mode)
        ;;   (setq-local my-was-visual-line-fill-column-mode nil))
        (let ((inhibit-message t))
          ;; Wrap
          (toggle-truncate-lines 0)))
    ;; When the text is truncated
    ;; (visual-line-mode -1)
    ;; (when (and (boundp 'visual-line-fill-column-mode)
    ;;            visual-line-fill-column-mode)
    ;;   (visual-line-fill-column-mode -1)
    ;;   (setq-local my-was-visual-line-fill-column-mode t))
    (let ((inhibit-message t))
      ;; Truncate
      (toggle-truncate-lines 1))))

(defun evilbuffer-erase ()
  "Delete the entire contents of the current buffer, even if it is read-only."
  (interactive)
  (let ((buffer-name (buffer-name))
        (inhibit-read-only t))
    ;; when (yes-or-no-p "Are you sure you want to erase the buffer?")
    (erase-buffer)

    (cond
     ((string-prefix-p "*Ollama" buffer-name)
      (cond
       ((derived-mode-p 'org-mode)
        (insert "* "))

       ((or (derived-mode-p 'markdown-ts-mode)
            (derived-mode-p 'markdown-mode))
        (insert "# ")))

      (when (fboundp 'evil-insert-state)
        (evil-insert-state))))))

(defun my-clear-highlights ()
  "Clear highlight and related state in the buffer."
  ;; Clear lazy highlights
  (lazy-highlight-cleanup)

  ;; Clear Evil mode highlights
  (when (fboundp 'evil-ex-nohighlight)
    (evil-ex-nohighlight))

  ;; Update font lock to clear new highlights after functions are evaluated
  (when (and (bound-and-true-p font-lock-mode)
             (fboundp 'font-lock-update))
    (font-lock-update)))

(defun evilbuffer-clear-highlights ()
  "Clear."
  (interactive)

  (save-window-excursion
    (save-excursion
      (cond
       ((string= (buffer-name) "*Messages*")
        (when (y-or-n-p "Delete the content of the *Messages* buffer?")
          (let ((inhibit-read-only t))
            (erase-buffer))))

       ;; For some reason,
       (t
        (unless (derived-mode-p 'embark-collect-mode)
          ;; Do not clear highlights during `embark-collect-mode' to prevent
          ;; disruptive color changes.
          (my-clear-highlights))

        ;; If the current character lacks a font-lock face, ensure the entire
        ;; buffer is fontified. This addresses an Org mode issue where point is
        ;; inside a source block, but the #+BEGIN_SRC line is above the window
        ;; start and thus not yet fontified.
        ;; (when (and (bound-and-true-p font-lock-mode)
        ;;            (not (get-text-property (point) 'face)))
        ;;   (font-lock-ensure))
        )))))

(defun my-evil-disable-remove-spaces ()
  "Disable automatic removal of trailing spaces in `evil-mode'."
  (setq-local evil-maybe-remove-spaces nil))

(defun my-evil-save ()
  "Save."
  (if (fboundp 'buffer-guardian-save-buffer)
      (buffer-guardian-save-buffer)
    (save-buffer)))

(defun my-goto-end-of-buffer (&rest _)
  "Go to the end of the buffer."
  (interactive)
  (let ((column (current-column)))
    (unwind-protect
        (progn
          (goto-char (point-max)))
      ;; Small customization to never be on eobp
      ;; NOTE: this is handeled by a post-command-hook
      ;; (when (eobp)
      ;;   ;; Move the cursor before `eobp' (last empty line).
      ;;   (backward-char 1))

      ;; (beginning-of-visual-line)
      ;; (move-to-column column)
      ;; (recenter -1)

      (lightemacs-recenter-maybe -1 t))))

;; TODO: Article? Bug report?
(defun my-uncomment-and-join-region ()
  "Join the selected lines."
  (interactive)
  (let* ((beg (if (use-region-p)
                  (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
                (line-beginning-position)))
         (end (if (use-region-p)
                  (let ((rend (region-end)))
                    (if (eq (char-before rend) ?\n)
                        (1- rend)  ; Exclude the final newline
                      rend))
                (save-excursion
                  (goto-char beg)
                  (forward-line 1)
                  (line-end-position))))
         ;; (contents (buffer-substring-no-properties beg end))
         ;; (is-comment nil)
         (original-point nil)
         (beg-line nil)
         (end-line nil))
    (save-excursion
      (when (and beg end)
        (when (> beg end)
          (let ((old-beg beg))
            (setq beg end)
            (setq end old-beg))))

      (setq original-point (if (use-region-p)
                               beg
                             (point)))

      (setq beg-line (save-excursion
                       (goto-char beg)
                       (line-number-at-pos)))
      (setq end-line (save-excursion
                       (goto-char end)
                       (line-number-at-pos)))

      ;; (unless (string= (buffer-substring-no-properties beg end)
      ;;                  contents)
      ;;   (setq is-comment t))

      (deactivate-mark)

      (when (/= beg-line end-line)
        (goto-char beg)
        (save-excursion
          (forward-line 1)
          (uncomment-region (line-beginning-position) end)))

      (save-excursion
        (let ((count (- end-line beg-line)))
          (goto-char beg)
          (dotimes (i count)
            (when (= (1+ i) count)
              (beginning-of-line))
            (join-line 1)))))

    (goto-char original-point)))

(with-eval-after-load 'evil
  (defun evileval-buffer ()
    "Evaluate the current buffer and display a message."
    (interactive)
    (unless (derived-mode-p 'emacs-lisp-mode)
      (user-error "This function supports only emacs-lisp-mode"))
    (eval-buffer)
    ;; (when (featurep package-name)
    ;;   (unload-feature package-name t))
    ;; (load (buffer-file-name (buffer-base-buffer)))
    (message "Buffer evaluated!"))

  (defun evileval-region ()
    "Evaluate the current region and display a message."
    (interactive)
    (unless (or (derived-mode-p 'markdown-mode)
                (derived-mode-p 'org-mode)
                (derived-mode-p 'emacs-lisp-mode))
      (error "This function supports only emacs-lisp-mode"))
    (if (use-region-p)
        (progn
          (eval-region (region-beginning) (region-end))
          (message "Region evaluated!"))
      (message "No region selected!")))
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal 'global (kbd "<leader>er") 'evileval-region)
    (evil-define-key 'normal 'global (kbd "<leader>eb") 'evileval-buffer))

  ;; Goto end buffer
  (define-key evil-normal-state-map "G" 'my-goto-end-of-buffer)

  ;; Enhanced versions of `evil-shift-right' and `evil-shift-left' for visual
  ;; mode. These functions shift the selected region to the right or left, then
  ;; temporarily exit normal mode to ensure the visual selection is restored
  ;; correctly. This provides a smoother experience when indenting multiple
  ;; lines in Evil visual mode, preserving the selection and allowing repeated
  ;; shifts without losing the highlighted region.
  (defun lightemacs-evil-shift-right ()
    "Shift the selected region to the right, preserving the selection."
    (interactive)
    (when (and (fboundp 'evil-shift-right)
               (fboundp 'evil-normal-state)
               (fboundp 'evil-visual-restore))
      (evil-shift-right evil-visual-beginning evil-visual-end 1 nil)
      (evil-normal-state)
      (evil-visual-restore)))

  (defun lightemacs-evil-shift-left ()
    "Shift the selected region to the left, preserving the selection."
    (interactive)
    (when (and (fboundp 'evil-shift-left)
               (fboundp 'evil-normal-state)
               (fboundp 'evil-visual-restore))
      (evil-shift-left evil-visual-beginning evil-visual-end 1 nil)
      (evil-normal-state)
      (evil-visual-restore)))
  (if (fboundp 'evil-define-key)
      (evil-define-key 'visual 'global
        (kbd ">") 'lightemacs-evil-shift-right
        (kbd "<") 'lightemacs-evil-shift-left))

  ;; The `evil-search-next` and `evil-search-previous` functions can sometimes
  ;; leave the buffer window scrolled horizontally. This advice adds an around
  ;; advice to these functions that resets the horizontal scroll position
  ;; (`set-window-hscroll`) to 0 when navigating using search, so the user is
  ;; presented with the correct starting point for their next search.

  (defun my-advice-search-next (orig-fun &rest args)
    "Advice for search-next to reset horizontal scroll position.
  ORIG-FUN is the function and ARGS the arguments."
    (when (/= 0 (window-hscroll))
      (set-window-hscroll nil 0))
    (apply orig-fun args))

  (defun my-advice-search-previous (orig-fun &rest args)
    "Advice for search-previous to reset horizontal scroll position.
  ORIG-FUN is the function and ARGS the arguments."
    (when (/= 0 (window-hscroll))
      (set-window-hscroll nil 0))
    (apply orig-fun args))

  (with-no-warnings
    (advice-add 'search-next :around #'my-advice-search-next)
    (advice-add 'search-previous :around #'my-advice-search-previous))

  ;; Join the current line and the next one while preserving the cursor position.
  (defun evilcursor-join-normal ()
    "Join the current line and the next one while preserving the cursor position."
    (interactive)
    (when (fboundp 'evil-join)
      (save-excursion
        (evil-join (line-beginning-position) (line-end-position)))))
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal 'global (kbd "J") 'evilcursor-join-normal)
    (evil-define-key 'visual 'global (kbd "J") 'evil-join))

  ;; M-[ and M-]: Previous and next section
  (defun my-evil-forward-section-end ()
    "Move to the next section."
    (interactive)
    (execute-kbd-macro (read-kbd-macro "]]")))
  (defun my-evil-backward-section-end ()
    "Move to the previous section."
    (interactive)
    (execute-kbd-macro (read-kbd-macro "[[")))
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal 'global (kbd "<leader>j") #'my-uncomment-and-join-region)
    (evil-define-key 'visual 'global (kbd "<leader>j") #'my-uncomment-and-join-region)
    (when (display-graphic-p)
      ;; Only on display-graphic-p because the M-[ issue occurs because modern
      ;; terminals use "Escape sequences" beginning with the ESC [ characters (the
      ;; byte-level equivalent of M-[) to communicate complex input like mouse
      ;; movements, pixel-perfect clicks, and bracketed pastes. When you bind a
      ;; custom command to M-[ in your Emacs configuration, you effectively
      ;; "highjack" the prefix of these incoming messages; Emacs consumes the first
      ;; two characters to trigger your function, leaving the remaining coordinate
      ;; or paste data orphaned and uninterpreted. Consequently, the terminal’s
      ;; attempt to say "the mouse clicked at these coordinates" is chopped up, and
      ;; the tail end of that technical string—the "gibberish" you see—is dumped
      ;; directly into your buffer as literal text. Would you like me to provide a
      ;; snippet to identify exactly which command is currently "stealing" that M-[
      ;; prefix?
      (evil-define-key '(insert motion) 'global (kbd "M-[")
        'my-evil-backward-section-end))
    (evil-define-key '(insert motion) 'global (kbd "M-]")
      'my-evil-forward-section-end))

  ;; Do not fail when the kill-ring is empty
  ;; To prevent the p command in Evil mode from failing when the paste ring
  ;; (akin to the clipboard in Vim) is empty, you can redefine the paste
  ;; function to check if the ring is empty before attempting to paste. Here’s
  ;; how you can do it using Emacs Lisp:
  (defun ignore-empty-ring-errors (orig-func &rest args)
    "Ignore errors related to the empty ring when calling ORIG-FUNC with ARGS."
    (condition-case nil
        (apply orig-func args)
      (error (message "Nothing to paste!") nil)))
  (with-no-warnings
    (advice-add 'evil-paste-after :around #'ignore-empty-ring-errors)
    (advice-add 'evil-paste-before :around #'ignore-empty-ring-errors))

  (define-key evil-normal-state-map (kbd "C-s") #'my-evil-save)
  (define-key evil-insert-state-map (kbd "C-s") #'my-evil-save)
  (define-key evil-visual-state-map (kbd "C-s") #'my-evil-save)

  (defun evilclipboard-select-pasted ()
    "Visually select last pasted text."
    (interactive)
    (when (and (fboundp 'evil-goto-mark)
               (fboundp 'evil-visual-char))
      (evil-goto-mark ?\[)
      (evil-visual-char)
      (evil-goto-mark ?\])))
  (with-no-warnings
    (define-key evil-normal-state-map (kbd "gp") #'evilclipboard-select-pasted)
    (define-key evil-normal-state-map (kbd "<leader>gp") #'evilclipboard-select-pasted))

  (setopt evil-want-Y-yank-to-eol t)

  (when (fboundp 'evil-set-leader)
    (evil-set-leader 'normal (kbd ","))
    (evil-set-leader 'visual (kbd ",")))

  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal 'global (kbd "<leader>ev") #'tab-bar-switch-to-tab)
    (evil-define-key '(visual normal insert) 'global (kbd "M-p") #'project-switch-project)

    (evil-define-key 'insert 'global (kbd "M-H") 'evil-backward-word-begin)
    (evil-define-key 'insert 'global (kbd "M-L") 'evil-forward-word-begin)

    (with-eval-after-load 'icomplete
      (evil-define-key 'normal icomplete-fido-mode-map (kbd "j") 'icomplete-forward-completions)
      (evil-define-key 'normal icomplete-fido-mode-map (kbd "k") 'icomplete-backward-completions)
      (evil-define-key 'normal icomplete-fido-mode-map (kbd "<down>") 'icomplete-forward-completions)
      (evil-define-key 'normal icomplete-fido-mode-map (kbd "<up>") 'icomplete-backward-completions)
      (evil-define-key 'insert icomplete-fido-mode-map (kbd "M-j") 'icomplete-forward-completions)
      (evil-define-key 'insert icomplete-fido-mode-map (kbd "M-k") 'icomplete-backward-completions))

    (with-eval-after-load 'eat
      ;; Causes problems
      ;; (evil-define-key 'insert eat-mode-map (kbd "C-c")
      ;;   #'eat-self-input)
      (evil-define-key 'insert eat-mode-map (kbd "M-<left>") 'eat-self-input)
      (evil-define-key 'insert eat-mode-map (kbd "M-<right>") 'eat-self-input)
      (evil-define-key 'insert eat-mode-map (kbd "M-j") 'eat-self-input)
      (evil-define-key 'insert eat-mode-map (kbd "M-k") 'eat-self-input)
      (evil-define-key 'insert eat-mode-map (kbd "M-H") 'eat-self-input)
      (evil-define-key 'insert eat-mode-map (kbd "M-L") 'eat-self-input))

    (with-eval-after-load 'dired
      (evil-define-key 'normal dired-mode-map (kbd "~") 'my-dired-home))

    (evil-define-key 'normal 'global (kbd "<leader>im") 'inhibit-mouse-mode)
    (evil-define-key 'normal 'global (kbd "<leader>ib") #'ibuffer))
  ;;; Automatic removal of spaces
  (add-hook 'evil-insert-state-entry-hook #'my-evil-disable-remove-spaces)

  (define-key evil-normal-state-map (kbd "C-l") #'evilbuffer-clear-highlights)
  (define-key evil-insert-state-map (kbd "C-l") #'evilbuffer-clear-highlights)
  (define-key evil-visual-state-map (kbd "C-l") #'evilbuffer-clear-highlights)
  (with-eval-after-load 'messages-buffer-mode
    (define-key messages-buffer-mode-map (kbd "C-l") #'evilbuffer-clear-highlights))

  (define-key evil-normal-state-map (kbd "<leader>wr") #'evilbuffer-toggle-truncate-line)
  (define-key evil-normal-state-map (kbd "<leader>eB") #'evilbuffer-erase)

  ;; TODO patch evil: this should restore point with :restore-point t
  (when (fboundp 'evil-fill-and-move)
    (with-no-warnings
      (evil-define-operator my-evil-fill-and-move-operator (beg end)
        "Fill text and move point to the end of the filled region BEG and END.
This enhancement prevents the cursor from moving."
        :move-point nil
        :type line
        :restore-point t
        (save-excursion
          (evil-fill-and-move beg end))))
    (define-key evil-normal-state-map "gq" 'my-evil-fill-and-move-operator))

  ;; It seems to only work when declared as default
  (defun my-setup-evil-mode ()
    "Removed: `git-rebase-mode' erc-mode circe-server-mode circe-chat-mode."
    ;; circe-query-mode sauron-mode
    (dolist (mode '(vterm-mode
                    eat-mode
                    ;;custom-mode
                    ;; eshell-mode
                    ;; term-mode
                    ))
      (add-to-list 'evil-emacs-state-modes mode)))
  (add-hook 'evil-mode-hook 'my-setup-evil-mode)

  (when (daemonp)
    (global-set-key (kbd "C-x C-c") 'my-save-buffers-kill-emacs))

  (define-key evil-normal-state-map (kbd "C-q") 'my-save-buffers-kill-emacs)

  ;; (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Make goto mark use ' to restore the column
  (define-key evil-motion-state-map "`" 'evil-goto-mark-line)
  (define-key evil-motion-state-map "'" 'evil-goto-mark)

  ;; Useful to insert a quote instead of two quotes when packages such as electric quote are activated
  (define-key evil-insert-state-map (kbd "C-\"") (lambda () (interactive) (insert "\"")))

  (global-set-key (kbd "M-c") nil)  ;; Change the word to uppercase in Emacs
  (global-set-key (kbd "M-v") nil)  ;; scroll the buffer up by one screenful
  (global-set-key (kbd "M-z") nil)  ;; Zap to char (delete text from the current cursor position up to a specific character)

  (define-key evil-insert-state-map (kbd "C-a") nil)
  (define-key evil-insert-state-map (kbd "A-DEL") 'evil-delete-backward-word)
  (define-key evil-insert-state-map (kbd "C-<Backspace>") 'evil-delete-backward-word)
  (define-key evil-normal-state-map (kbd "gdp") 'delete-pair)
  (define-key evil-normal-state-map (kbd "<leader>p") 'delete-pair)
  (define-key evil-normal-state-map (kbd "<leader>k") 'describe-key)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-b") 'move-beginning-of-line)


  (define-key evil-visual-state-map (kbd "u") 'ignore) ;; Disable lower/upper case region
  (define-key evil-visual-state-map (kbd "U") 'ignore) ;; Disable lower/upper case region
  (define-key evil-visual-state-map (kbd "C-c") 'evil-yank)

  (define-key evil-normal-state-map (kbd "<leader>gs") 'global-text-scale-adjust)
  (define-key evil-normal-state-map (kbd "<leader>cf") 'my-temporary-file)
  (define-key evil-normal-state-map (kbd "<leader>ce") 'my-temporary-diff)
  (define-key evil-normal-state-map (kbd "<leader>t")  'my-tab-split)
  (define-key evil-normal-state-map (kbd "<leader>T")  'tab-bar-change-tab-group)
  (define-key evil-normal-state-map (kbd "<leader>em") 'toggle-menu-bar-mode-from-frame)
  (define-key evil-normal-state-map (kbd "<leader>ww") 'my-wip)
  (define-key evil-normal-state-map (kbd "<leader>W")  'my-wip)

  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "gs") #'evilbuffer-switch-to-scratch-and-clear))

  ;; (when (fboundp 'my-dabbrev-completion-backwards)
  ;;   (setq evil-complete-next-func #'my-dabbrev-completion-backwards))
  ;;
  ;; (when (fboundp 'my-dabbrev-completion-forward)
  ;;   (setq evil-complete-previous-func #'my-dabbrev-completion-forward))
  ;; TODO use cape-dabbrev
  ;; (define-key evil-insert-state-map (kbd "C-p") #'my-dabbrev-completion-backwards)
  ;; (define-key evil-insert-state-map (kbd "C-n") #'my-dabbrev-completion-forward)

  (defun my-dabbrev-completion-forward-all-buffers (arg)
    (with-no-warnings
      (let ((dabbrev-check-all-buffers t))
        (dabbrev-completion arg)))))

(with-eval-after-load 'vertico
  (when (fboundp 'evil-define-key)
    ;; TODO move somewhere else like mod-vertico-evil
    (evil-define-key '(insert normal) 'global (kbd "S-<return>") 'embark-dwim)
    (evil-define-key 'insert vertico-map (kbd "C-f") 'vertico-scroll-up)
    (evil-define-key 'normal vertico-map (kbd "C-f") 'vertico-scroll-up)
    (evil-define-key 'insert vertico-map (kbd "C-b") 'vertico-scroll-down)
    (evil-define-key 'normal vertico-map (kbd "C-b") 'vertico-scroll-down)
    (evil-define-key 'normal vertico-map (kbd "gg") 'vertico-first)
    (evil-define-key 'normal vertico-map (kbd "G") 'vertico-last)
    (evil-define-key 'normal vertico-map (kbd "<escape>") nil)
    (evil-define-key 'insert vertico-map (kbd "<tab>") 'vertico-insert)

    (evil-define-key 'insert vertico-map (kbd "M-k") 'vertico-previous)
    (evil-define-key 'insert vertico-map (kbd "M-j") 'vertico-next)

    ;; Useful for vertico-buffer
    (evil-define-key 'normal vertico-map (kbd "M-k") 'vertico-previous)
    (evil-define-key 'normal vertico-map (kbd "M-j") 'vertico-next))

  ;; I do not like it
  ;; (define-key minibuffer-local-map (kbd "<up>") #'previous-history-element)
  ;; (define-key minibuffer-local-map (kbd "<down>") #'next-history-element)
  (define-key minibuffer-local-map (kbd "C-<up>") #'previous-history-element)
  (define-key minibuffer-local-map (kbd "C-<down>") #'next-history-element)

  ;; Open in a NEW TAB
  (keymap-set vertico-map "C-t"
              #'(lambda()
                  ;; Open embark-dwim in a new tab
                  (when (fboundp 'tab-new-func-buffer-from-other-window)
                    (tab-new-func-buffer-from-other-window 'embark-dwim))))

  ;; TODO part of this is sub-better-evil.el

  (add-hook
   'embark-after-export-hook
   #'(lambda()
       ;; wgrep
       (if (or t (< emacs-major-version 31))
           (when (fboundp 'wgrep-change-to-wgrep-mode)
             (wgrep-change-to-wgrep-mode)
             (with-eval-after-load 'evil
               (when (fboundp 'wgrep-finish-edit)
                 ;; TODO: save and restore the cursor: wgrep-finish-edit
                 (when (fboundp 'evil-define-key)
                   (evil-define-key 'normal 'local (kbd "C-s")
                     #'(lambda()
                         (interactive)
                         (lightemacs-save-window-start
                           (lightemacs-save-window-hscroll
                             (save-mark-and-excursion
                               (wgrep-finish-edit)
                               (wgrep-change-to-wgrep-mode))))))))))

         ;; Emacs >= 31
         (when (fboundp 'grep-change-to-grep-edit-mode)
           (grep-change-to-grep-edit-mode)
           (with-eval-after-load 'evil
             (when (fboundp 'grep-edit-save-changes)
               ;; TODO: save and restore the cursor: wgrep-finish-edit
               ;; (evil-define-key 'normal 'local (kbd "C-s")
               ;;   #'(lambda()
               ;;       (interactive)
               ;;       (grep-edit-save-changes)))
               ))))))

  (with-eval-after-load 'evil
    (when (fboundp 'evil-define-key)
      (evil-define-key 'normal 'global (kbd "<leader>ee") 'embark-dwim)
      (evil-define-key 'normal 'global (kbd "<leader>ew") 'embark-act))))



(with-eval-after-load 'vterm
  (with-eval-after-load 'evil
    (when (fboundp 'evil-define-key)
      (evil-define-key 'insert vterm-mode-map (kbd "M-H") 'my-vterm--send-Alt-Shift-H)
      (evil-define-key 'insert vterm-mode-map (kbd "M-L") 'my-vterm--send-Alt-Shift-L)))

  ;; Useful for nano
  ;;(define-key vterm-mode-map (kbd "C-c") #'vterm--self-insert)
  ;;(define-key vterm-mode-map (kbd "C-g") #'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-x") 'vterm--self-insert)

  (define-key vterm-mode-map (kbd "C-c C-c") 'vterm--self-insert)

  (with-eval-after-load 'evil
    (define-key vterm-mode-map (kbd "M-j") 'vterm--self-insert)
    (define-key vterm-mode-map (kbd "M-k") 'vterm--self-insert)
    (when (fboundp 'evil-define-key)
      (evil-define-key 'insert vterm-mode-map (kbd "M-j") 'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "M-k") 'vterm--self-insert)))

  (define-key vterm-mode-map (kbd "M-H") 'my-vterm--send-Alt-Shift-H)
  (define-key vterm-mode-map (kbd "M-L") 'my-vterm--send-Alt-Shift-L))

;;; Paste with current indentation

(defun string-get-indentation (str)
  "Get the indentation of STR (leading spaces and tabs)."
  (replace-regexp-in-string "^\\([ \t]*\\).*$" "\\1" str))

(defun evil-clipboard--string-unindent (input-str)
  "Unindent INPUT-STR by removing the minimal common indentation from all lines."
  (let ((lines (split-string input-str "\n"))
        (min-indent nil))

    ;; Find the minimum indentation
    (dolist (line lines)
      (let* ((indent (string-get-indentation line))
             (length-indent (length indent)))
        (when (and
               ;; The line must not be empty
               (not (string= (string-trim-left line) ""))
               ;; And the indent should be lower than the one that has been found
               ;; previously
               (or (not min-indent)
                   (< length-indent min-indent)))
          (setq min-indent length-indent))))

    (unless min-indent
      (setq min-indent 0))

    ;; Remove the minimum indentation from all lines
    (let ((result ""))
      (dolist (line lines)
        (setq result
              (concat result
                      (if (string= result "") "" "\n")
                      ;; (substring line min-indent)
                      (if (>= (length line) min-indent)
                          (substring line min-indent)
                        line)
                      )))
      result)))

(defun evilclipboard-paste-with-current-indentation ()
  "Paste text from the clipboard with the current line's indentation."
  (interactive)
  (if (and (fboundp 'evil-get-register)
           (fboundp 'evil-visual-paste)
           (fboundp 'evil-paste-before)
           (fboundp 'evil-set-register))
      (let* ((original-register-contents (evil-get-register ?a t))
             (new-indentation (make-string (current-column) ?\s))
             (kill-ring-content (ignore-errors (current-kill 0)))
             (text (if kill-ring-content
                       (evil-clipboard--string-unindent (string-trim-right
                                                         (substring-no-properties kill-ring-content)
                                                         "\n"))
                     ""))
             (text-to-paste (string-trim-left
                             (replace-regexp-in-string "^" new-indentation text))))
        (evil-set-register ?a text-to-paste)
        (if (use-region-p)
            (evil-visual-paste 1 ?a)
          (progn
            (evil-paste-before 1 ?a)
            (when evil-move-cursor-back
              (forward-char 1))
            (when original-register-contents
              (evil-set-register ?a original-register-contents)))))
    (error "Undefined required functions")))

(defun evilclipboard-paste-with-current-indentation-restore-point ()
  "Paste text from the clipboard with the current line's indentation.
This function also restores window start and point when pasting multiple lines."
  (interactive)
  (if (fboundp 'evil-paste-before)
      (if (minibufferp)
          (let ((evil-move-cursor-back nil))
            (evil-paste-before 1))
        (lightemacs-save-window-hscroll
          (lightemacs-save-window-start
            (save-mark-and-excursion
              (evilclipboard-paste-with-current-indentation)))))
    (error "Undefined required functions")))

;; (define-key evil-insert-state-map (kbd "C-a p") #'evilclipboard-paste-with-current-indentation-restore-point)
;; (define-key evil-insert-state-map (kbd "C-a C-p") #'evilclipboard-paste-with-current-indentation-restore-point)

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key 'insert 'global (kbd "C-v") #'evilclipboard-paste-with-current-indentation-restore-point)))

;;; Copy with without indentation

(defun evilclipboard-evil-yank-region-unindented ()
  "Copy the region, un-indented by the length of its minimum indent.
If numeric prefix argument PAD is supplied, indent the resulting
text by that amount."
  (interactive)
  (if (and (fboundp 'evil-yank)
           (fboundp 'evil-get-register)
           (fboundp 'evil-set-register))
      (when (use-region-p)
        (evil-yank (region-beginning) (region-end))
        (dolist (register '(?\" ?*))
          (let ((original-contents (evil-get-register register t)))
            (when original-contents
              (evil-set-register
               register (evil-clipboard--string-unindent
                         (substring-no-properties original-contents)))))))
    (error "Undefined required functions")))

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key 'visual 'global (kbd "C") #'evilclipboard-evil-yank-region-unindented)))

;;; evilwindow: split and select

(defun evilwindow-split-and-select-new-window (split-direction)
  "Split the window in the specified direction then switch to the new window.
SPLIT-DIRECTION is the direction (v or h).
By using this function, Emacs and Evil can mimic the behavior of Vim when the
user presses Ctrl-w v and Ctrl-w s. It prohibits Emacs from altering the cursor
position when the user presses Ctrl-v or Ctrl-s to create a new split and
guarantees that the new window is selected, as in Vim."
  ;; Save current buffer's cursor position and view
  (when (and (fboundp 'evil-window-vsplit)
             (fboundp 'evil-window-split))
    (let ((buf (current-buffer))
          (pos (point))
          (view (window-start)))
      ;; Split the window
      (if (equal split-direction "v")
          (evil-window-vsplit)
        (evil-window-split))
      ;; Restore cursor and view of previous window
      (with-current-buffer buf
        (goto-char pos)
        (set-window-start nil view t))
      ;; Go back to the previous window
      ;; (other-window 1)
      )))

(defun evilwindow-split-select-below ()
  "Split the window horizontally then switch to the new window."
  (interactive)
  (evilwindow-split-and-select-new-window "h"))

(defun evilwindow-split-select-right ()
  "Split the window vertically then switch to the new window."
  (interactive)
  (evilwindow-split-and-select-new-window "v"))

(defun evilwindow-edit-fold-and-open-first-fold (file-path)
  "Edit, fold, and open the first fold of FILE-PATH."
  (when (and (fboundp 'kirigami-close-folds)
             (fboundp 'kirigami-open-fold)
             (fboundp 'evil-goto-line))
    (let ((file-buffer (find-buffer-visiting file-path)))
      (if file-buffer (switch-to-buffer file-buffer)
        (find-file file-path)
        (kirigami-close-folds)
        (kirigami-open-fold)
        (evil-goto-line 1)))))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-w v") #'evilwindow-split-select-right)
  (define-key evil-normal-state-map (kbd "C-w s") #'evilwindow-split-select-below))

;;; evil: browse-url

(defun my-evil-browse-url-copy-to-clipboard (url &optional _args)
  "Copy the URL to the clipboard instead of opening it."
  (when (fboundp 'evil-set-register)
    (dolist (register '(?\" ?*))
      (evil-set-register register url))
    (message "URL copied to clipboard: %s" url)))

(with-eval-after-load 'evil
  ;; Override the browse-url function
  (setq browse-url-browser-function 'my-evil-browse-url-copy-to-clipboard))

;;; evil consult

(defun my-consult-fd-project ()
  "Run `consult-fd` in the root directory of the current project."
  (interactive)
  (require 'consult)
  (let* ((project (project-current nil "."))
         (project-root (when (and project
                                  (fboundp 'project-root))
                         (project-root project)))
         (consult-fd-args (concat (if (boundp 'consult-fd-args)
                                      consult-fd-args
                                    "")
                                  " --threads "
                                  (number-to-string (num-processors)))))
    (if (fboundp 'consult-fd)
        (consult-fd project-root)
      (error "Undefined: consult-fd"))))

(defun my-consult-imenu ()
  "Call `consult-imenu'."
  (interactive)
  (transient-mark-mode -1)
  ;; (when (fboundp 'evil-normal-state)
  ;;   (evil-normal-state))
  (if (fboundp 'consult-imenu)
      (consult-imenu)
    (error "Undefined: consult-imenu")))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "<leader>ff") 'my-consult-imenu)
  (define-key evil-normal-state-map (kbd "<leader>m") 'consult-recent-file)
  (define-key evil-normal-state-map (kbd "<leader>b") 'consult-recent-file)
  ;; (define-key evil-normal-state-map (kbd "<leadrr>B") #'switch-to-buffer)
  (define-key evil-normal-state-map (kbd "<leader>B") 'consult-buffer)
  (define-key evil-normal-state-map (kbd "M-/") 'consult-line)

  (define-key evil-normal-state-map (kbd "C-p") 'my-consult-fd-project)

  ;; (define-key evil-normal-state-map (kbd "C-p") #'consult-fd)
  )

(defun my-consult-grep-dir (&optional dir)
  "Execute ripgrep in the current directory, using the selection if available.
DIR is the directory."
  (interactive)
  (require 'consult)
  (let ((selection (when (use-region-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))))
        (consult-ripgrep-args (concat (if (fboundp 'consult-ripgrep-args)
                                          consult-ripgrep-args
                                        "")
                                      " --threads "
                                      (number-to-string (num-processors)))))
    (when selection
      (save-excursion
        (deactivate-mark)))

    (save-some-buffers t)
    ;; (buffer-guardian-save-all-buffers)
    (when (fboundp 'consult-ripgrep)
      (consult-ripgrep (or dir (buffer-cwd)) selection)
      (error "Undefined: consult-ripgrep"))))

(defun my-consult-grep-project ()
  "Run `consult-fd` in the root directory of the current project."
  (interactive)
  (require 'consult)
  (let* ((project (project-current nil "."))
         (project-root (when (and project (fboundp 'project-root))
                         (project-root project))))
    (my-consult-grep-dir (or project-root (buffer-cwd)))))

(with-eval-after-load 'evil
  (when (and (fboundp 'evil-define-key)
             (fboundp 'evil-define-key*))
    ;; (evil-define-key 'normal 'global (kbd "gR") 'my-consult-grep-project)
    ;; (evil-define-key 'normal 'global (kbd "gr") 'my-consult-grep-dir)
    (evil-define-key 'normal 'global (kbd "<leader>gR") 'my-consult-grep-dir)
    (evil-define-key 'normal 'global (kbd "<leader>gr") 'my-consult-grep-project)))

;;; evil org

;; TODO put this back?
;; (when (fboundp 'indentnav-backward-to-empty-line)
;;   (evil-define-key 'normal 'local (kbd "{") #'indentnav-backward-to-empty-line))
;; (when (fboundp 'indentnav-forward-to-empty-line)
;;   (evil-define-key 'normal 'local (kbd "}") #'indentnav-forward-to-empty-line))

;; Disable org cycle
;; TODO put this back
;; (evil-define-key 'normal 'local (kbd "<tab>") #'ignore)

(defun my-evil-delete-to-heading-star ()
  "Delete everything before the cursor except the first '*' and space.

In Org mode with Evil insert state, this function deletes everything before the
cursor except the first Org heading star (`*`), the space following it, and any
word after the space that contains at least two uppercase characters."
  (interactive)
  (when (fboundp 'evil-delete-back-to-indentation)
    (let ((cur-point (point)))
      (save-excursion
        (if (re-search-backward
             "^\\(*+\\)\\( +\\)\\([A-Z]\\{2,\\}\\)?\\( +\\)?"
             (line-beginning-position) t)
            (let ((delete-start (match-end 0)))
              (delete-region delete-start cur-point))
          (evil-delete-back-to-indentation))))))

(with-eval-after-load 'org
  (with-eval-after-load 'evil
    (when (fboundp 'evil-set-initial-state)
      (evil-set-initial-state 'org-agenda-mode 'motion)
      (evil-set-initial-state 'org-agenda-mode 'normal))

    (when (fboundp 'evil-define-key)
      (evil-define-key 'normal 'global (kbd "<leader>oa") 'org-agenda)
      (evil-define-key 'normal 'global (kbd "<leader>oc") 'org-capture)
      (evil-define-key 'normal 'global (kbd "<leader>os") 'org-schedule)
      (evil-define-key 'normal 'global (kbd "<leader>Z") 'my-org-agenda-switch-to-todos)

      (evil-define-key 'motion org-agenda-keymap (kbd "/") 'org-agenda-filter)
      (evil-define-key '(normal motion) org-agenda-keymap (kbd "RET")
        'my-org-agenda-goto-in-same-window)
      (evil-define-key '(normal) org-agenda-keymap (kbd "<leader>cd") 'org-agenda-todo)
      (evil-define-key '(normal) org-agenda-keymap (kbd "<tab>") 'org-agenda-set-tags)
      (evil-define-key '(normal motion) org-agenda-keymap (kbd "<leader>oo") 'org-agenda-set-tags)
      ;; (evil-define-key '(normal motion) org-agenda-keymap (kbd "<tab>") #'org-agenda-goto)
      (evil-define-key '(normal motion) org-agenda-keymap (kbd "C-l")
        #'(lambda()
            (interactive)
            (when (fboundp 'org-agenda-filter-remove-all)
              (org-agenda-filter-remove-all))

            (when (fboundp 'evilbuffer-clear-highlights)
              (evilbuffer-clear-highlights))))

      (evil-define-key 'insert org-mode-map (kbd "C-u") 'my-evil-delete-to-heading-star)

      ;; Equivalent to C-c C-q
      (evil-define-key 'normal org-mode-map (kbd "<leader>oo") 'org-set-tags-command)
      (evil-define-key 'normal org-mode-map (kbd "<leader>xx") 'org-babel-execute-maybe)
      (evil-define-key 'normal org-mode-map (kbd "<leader>cd") 'my-org-todo-and-toggle)
      ;; (evil-define-key 'normal org-mode-map (kbd "<leader>xx") #'org-edit-src-code)
      ;; (evil-define-key 'normal org-src-mode-map (kbd "<leader>xx") #'org-edit-src-exit)
      )))

;;; cape: complete before point

;;------------------------------------------------------------------------------
;; CAPE COMPLETE BEFORE POINT
;;------------------------------------------------------------------------------
;; TODO lightemacs?

;; This has been changed
(defun my-cape--dabbrev-bounds ()
  "Return bounds of abbreviation using only text before point.

This variant restricts the abbreviation bounds to the symbol
fragment preceding point.  It identifies the start of the current
word (a sequence of characters matching `dabbrev-abbrev-char-regexp`)
and uses point as the end boundary.  This ensures that dabbrev
completions are based solely on the fragment already typed, not
on text following the cursor."
  (unless (boundp 'dabbrev-abbrev-char-regexp)
    (require 'dabbrev))
  (let ((re (or dabbrev-abbrev-char-regexp "\\sw\\|\\s_"))
        (limit (minibuffer-prompt-end)))
    (when (or (looking-at re)
              (and (> (point) limit)
                   (save-excursion (forward-char -1) (looking-at re))))
      (cons (save-excursion
              (while (and (> (point) limit)
                          (save-excursion (forward-char -1) (looking-at re)))
                (forward-char -1))
              (when dabbrev-abbrev-skip-leading-regexp
                (while (looking-at dabbrev-abbrev-skip-leading-regexp)
                  (forward-char 1)))
              (point))
            ;; This is the part that I modified
            (point)

            ;; TODO contrib to cape?
            ;;
            ;; This is the part that I removed
            ;;
            ;; The following code scans forward from point over all characters
            ;; matching re (typically word or symbol characters) to determine
            ;; the end of the current abbreviation, returning it as a point
            ;; value while leaving the cursor in place due to save-excursion;
            ;; in the original cape-dabbrev, this ensures that the completion
            ;; can replace the entire symbol under the cursor, including any
            ;; text after point, whereas fixing END to (point) restricts
            ;; completions to only the fragment typed before the cursor.
            ;;
            ;; It is not useless. it is useful because it allows cape-dabbrev
            ;; (and dabbrev-like completions) to identify the full extent of the
            ;; symbol at point, so that when a completion is chosen, it can
            ;; replace the entire word under the cursor rather than just the
            ;; part typed so far; this behavior is important in typical Emacs
            ;; completion, where completing a symbol mid-word should overwrite
            ;; the rest of the word, but if the goal is to restrict completions
            ;; to only the text before the cursor, scanning forward becomes
            ;; unnecessary and can be replaced by simply using (point) as the
            ;; end.
            ;;
            ;; (save-excursion
            ;;   (while (looking-at re)
            ;;     (forward-char 1))
            ;;   (point))
            ))))

(with-eval-after-load 'cape
  (advice-add 'cape--dabbrev-bounds :override #'my-cape--dabbrev-bounds))

;; (defun my-cape-dabbrev-backwards ()
;;   "Cape-dabbrev backwards."
;;   (interactive)
;;   (let ((dabbrev-backward-only t))
;;     (when (fboundp 'cape-dabbrev)
;;       (cape-dabbrev t))))
;;
;; (evil-define-key 'insert 'global (kbd "C-p") 'my-cape-dabbrev-backwards)

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key 'insert 'global (kbd "C-p") 'cape-dabbrev)
    (evil-define-key 'insert 'global (kbd "C-n") 'cape-dabbrev)))


;;; cape: evil

(defun my-minibuffer-setup-dabbrev-evil ()
  "Bind `C-p' and `C-n' to dabbrev completion in minibuffer using evil."
  (when (fboundp 'evil-define-key)
    (evil-define-key 'insert 'local
      (kbd "C-p") 'cape-dabbrev
      (kbd "C-n") 'cape-dabbrev))

  ;; (when (and (boundp 'completion-at-point-functions)
  ;;            (listp completion-at-point-functions))
  ;;   (add-hook 'completion-at-point-functions #'cape-dabbrev nil t))
  )

(with-eval-after-load 'evil
  (with-eval-after-load 'cape
    (when (fboundp 'evil-define-key)
      (evil-define-key 'insert 'global (kbd "C-x C-f") 'cape-file))))

(with-eval-after-load 'cape
  ;; Emulate Vim's C-x C-f
  ;; TODO lightemacs?
  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-dabbrev-evil)

  ;; Use symbol occurence to sort candidates
  ;;
  ;; To achieve this, you must rely on the natural search order of dabbrev
  ;; (which scans from the cursor outwards) and prevent corfu from re-sorting
  ;; the candidates. By default, corfu sorts candidates by length or
  ;; alphabetically, which overrides the proximity-based order returned by the
  ;; backend.
  ;;
  ;; You can use cape-capf-properties to strictly disable sorting for
  ;; cape-dabbrev while keeping the default sorting for other backends.
  (if (fboundp 'cape-capf-properties)
      (add-to-list 'completion-at-point-functions
                   (cape-capf-properties 'cape-dabbrev :sort nil))
    (error "Undeclared: cape-capf-properties")))

;;; evil corfu

(with-eval-after-load 'corfu
  ;; (add-hook 'evil-mode-hook
  ;;           #'(lambda()
  ;;               (with-eval-after-load "evil"
  ;;                 ;; Emulate Vim's C-x C-f
  ;;                 (evil-define-key 'insert 'global (kbd "C-x C-f") #'cape-file))))
  (with-eval-after-load "evil"
    (when (fboundp 'evil-define-key)
      (evil-define-key 'insert 'global (kbd "C-SPC") #'completion-at-point))

    (unless (display-graphic-p)
      (define-key key-translation-map (kbd "C-@") (kbd "C-SPC")))

    ;; Setup auto complete
    ;; (defun pkg-corfu-setup-auto-complete ()
    ;;   "Setup Corfu auto complete."
    ;;   ;; (corfu-mode)
    ;;   (evil-define-key 'insert 'local (kbd "C-SPC")
    ;;     (if (my-code-checker-allowed-p) #'completion-at-point #'ignore)))
    ;; (add-hook-text-editing-modes #'pkg-corfu-setup-auto-complete)
    ;; (dolist (hook '(inferior-python-mode-hook
    ;;                 lisp-interaction-mode-hook))
    ;;   (add-hook hook #'pkg-corfu-setup-auto-complete))
    ;; (define-key minibuffer-mode-map (kbd "C-SPC") 'completion-at-point)

    ))

;;; Move region

(defvar move-region-skip-invisible t)

(defun move-region (n)
  "Move the current region up or down by N lines."
  ;; (interactive "r\np")
  (interactive)
  (when (use-region-p)
    ;; (region-to-linewise-region)
    (let ((start (region-beginning))
          (end (region-end)))
      ;; Exit visual state explicitly before performing the operation. If we
      ;; do not do this, Evil will automatically restore the original point
      ;; and mark after the command finishes, overriding any cursor movement
      ;; or region updates made within the function.
      (when (fboundp 'evil-exit-visual-state)
        (evil-exit-visual-state))

      (let ((line-text (delete-and-extract-region start end)))
        (if move-region-skip-invisible
            (forward-visible-line n)
          (forward-line n))
        (let ((start (point))
              ;; (length-text (length line-text))
              )
          (insert line-text)
          ;; (backward-char)

          (setq end (point))
          (set-mark end)

          (goto-char start)

          ;; Ensure that the region remains active after the command finishes.
          ;; In Emacs, the variable `deactivate-mark` controls whether the
          ;; region is deactivated automatically at the end of a command. By
          ;; default, it is set to `t` by many interactive commands, which
          ;; causes the region to be cleared after execution. Setting
          ;; `deactivate-mark` to `nil` prevents this automatic deactivation.
          ;; This is necessary even when `activate-mark` is called explicitly,
          ;; because without setting `deactivate-mark` to `nil`, the region may
          ;; appear briefly and then vanish immediately after the function
          ;; returns, giving the false impression that the selection failed.
          (setq deactivate-mark nil)
          (activate-mark))))))

(defun move-region-up (&rest _)
  "Move the current line up by N lines."
  (interactive)
  (move-region -1))

(defun move-region-down (&rest _)
  "Move the current line down by N lines."
  (interactive)
  (move-region 1))

(with-eval-after-load 'evil
  (define-key evil-visual-state-map (kbd "M-j") #'move-region-down)
  (define-key evil-visual-state-map (kbd "M-k") #'move-region-up))

;;; markdown mode

(defun my-setup-markdown-mode ()
  "Setup markdown modes."
  ;; In gptel buffers we set `nobreak-char-display' to nil locally so that the
  ;; Unicode no-break space (U+00A0) is rendered just like a regular ASCII
  ;; space. This suppresses the distinct glyph or face Emacs normally applies
  ;; to NBSP, keeping the buffer free of distracting blue highlights while
  ;; preserving the character's internal no-break semantics.
  ;;
  ;; Here is an example of what is highlighted: $5 billion-valued.
  ;; When `nobreak-char-display' is non-nil, the non-breaking space after `5`
  ;; and the hyphen after n are rendered as highlighted glyphs.
  (setq-local nobreak-char-display nil)

  ;; visual-line-mode is slow?
  (visual-line-mode 1)

  (let ((inhibit-message t))
    (toggle-truncate-lines 0))

  ;; (setq-local evil-auto-indent nil)

  ;; (evil-define-key 'normal 'local (kbd "TAB") #'ignore)
  ;; (evil-define-key 'normal 'local (kbd "<tab>") #'ignore)

  ;; DONE already added to markdown-mode (TODO not released yet)
  ;; Make / a punctuation (for, for example, strings like group/package) I
  ;; sometimes have `var' in Yaml files
  ;; (set-syntax-table (copy-syntax-table))
  (modify-syntax-entry ?' ".")
  (modify-syntax-entry ?* ".") ; Things like *word* (italic)
  (modify-syntax-entry ?> ".")
  (modify-syntax-entry ?< ".")
  ;; (modify-syntax-entry ?- ".")  ;; Annoying for editing elisp README.md
  (modify-syntax-entry ?_ ".")

  ;; RET can sometimes check and uncheck boxes. This is not something I want.
  ;; NOTE: THIS IS WRONG. DO NOT ACTIVATE.
  ;; (evil-define-key 'normal 'local (kbd "RET") #'ignore)

  ;; TODO reenable?
  ;; (when (fboundp 'indentnav-backward-to-empty-line)
  ;;   (evil-define-key 'normal 'local (kbd "{") #'indentnav-backward-to-empty-line))
  ;; (when (fboundp 'indentnav-forward-to-empty-line)
  ;;   (evil-define-key 'normal 'local (kbd "}") #'indentnav-forward-to-empty-line))

  ;; (when (fboundp 'indentnav-backward-to-empty-line)
  ;;   (evil-define-key 'normal 'local (kbd "{") #'evil-backward-paragraph))
  ;;
  ;; (when (fboundp 'indentnav-forward-to-empty-line)
  ;;   (evil-define-key 'normal 'local (kbd "}") #'evil-forward-paragraph))

  )

(add-hook 'markdown-mode-hook #'my-setup-markdown-mode)
(add-hook 'markdown-ts-mode-hook #'my-setup-markdown-mode)
(add-hook 'gfm-mode-hook #'my-setup-markdown-mode)

;;; Code that replaces evil visualstar

(defun le-evil--search-regexp (regex direction count)
  "Search for REGEX.
Inspired from `evil-ex-start-word-search'.
The search matches the COUNT occurrence of the word. The DIRECTION argument
should be either `forward' or `backward', determining the search direction."
  (if (and (fboundp 'evil-ex-make-search-pattern)
           (fboundp 'evil-push-search-history)
           (fboundp 'evil-ex-delete-hl)
           (fboundp 'evil-ex-search-next))
      (progn
        (setq evil-ex-search-count count
              evil-ex-search-direction direction
              evil-ex-search-pattern
              (let (evil-ex-search-vim-style-regexp)
                (evil-ex-make-search-pattern regex))
              evil-ex-search-offset nil
              evil-ex-last-was-search t)
        ;; Update search history unless this pattern equals the previous pattern
        (unless (equal regex (car evil-ex-search-history))
          (push regex evil-ex-search-history))
        (evil-push-search-history regex (eq direction 'forward))
        (evil-ex-delete-hl 'evil-ex-search)
        (evil-ex-search-next count))
    (error "Undefined required evil functions")))

(defun evilbuffer-search-symbol (&optional direction)
  "Search for the symbol at point using Evil search.
The DIRECTION argument can be either \='forward' or \='backward, determining the
search direction (default: \='forward)."
  (interactive)
  (if (and (fboundp 'evil-visual-state-p)
           (fboundp 'evil-exit-visual-state)
           (fboundp 'evil-ex-search))
      (progn
        ;; Both `save-window-excursion' and `save-excursion' are used here to ensure
        ;; that performing the search does not move the cursor or change the visible
        ;; portion of the buffer (`window-start'). This allows Evil's search functions
        ;; to update highlights and internal search state while leaving the user's
        ;; point and window view completely unchanged.
        (save-window-excursion ; Preserve window-start
          (save-excursion ; Preserve point and mark
            (let* ((visual-p (evil-visual-state-p))
                   (count 1)
                   (direction (or direction 'forward))
                   (text (if visual-p
                             (let ((selection (buffer-substring-no-properties
                                               (region-beginning)
                                               (region-end))))
                               (evil-exit-visual-state)
                               selection)
                           (thing-at-point 'symbol t)))
                   (current-pattern (and evil-ex-search-pattern
                                         (car evil-ex-search-pattern)))
                   (regex (when text
                            (if visual-p
                                (regexp-quote text)
                              (concat "\\_<" (regexp-quote text) "\\_>")))))
              (when regex
                (if (and current-pattern
                         (string= regex (and evil-ex-search-pattern
                                             (car evil-ex-search-pattern))))
                    ;; Instead of calling le-evil--search-regexp for the same
                    ;; keyword, just highlight the current keyword
                    (condition-case err
                        (progn
                          (evil-ex-search 1))
                      (search-failed
                       nil))
                  (progn
                    (if (eq direction 'forward)
                        (goto-char (point-min))
                      (goto-char (point-max)))

                    ;; let ((lightemacs-maybe-recenter-after-jump nil))
                    (le-evil--search-regexp regex
                                            direction
                                            count))
                  t))))))
    (error "Undefined required functions")))

(defun le-evil--search-symbol-backwards ()
  "Search for the symbol at point using Evil search.
The DIRECTION argument can be either `forward' or `backward', determining the
search direction (default: \='forward)."
  (interactive)
  (evilbuffer-search-symbol 'backward))

(with-eval-after-load 'evil
  ;; Key mappings
  (define-key evil-normal-state-map (kbd "*") #'evilbuffer-search-symbol)
  (define-key evil-visual-state-map (kbd "*") #'evilbuffer-search-symbol)
  (define-key evil-normal-state-map (kbd "#") #'le-evil--search-symbol-backwards)
  (define-key evil-visual-state-map (kbd "#") #'le-evil--search-symbol-backwards)
  (define-key evil-visual-state-map (kbd "?") #'le-evil--search-symbol-backwards))

;;; Provide

(provide 'config)

;; Local variables:
;; byte-compile-warnings: (not lexical free-vars)
;; End:

;;; config.el ends here

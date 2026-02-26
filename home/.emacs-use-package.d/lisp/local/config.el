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

(setq compile-angel-optimize-regexps t)

;; TODO test this more. It does not seem stable.
;; (setq package-quickstart t)
(setq package-native-compile nil)
(setq native-comp-jit-compilation nil)

(with-eval-after-load 'compile-angel
  (compile-angel-exclude-directory "~/src/emacs/"))

(setq compile-angel-enable-byte-compile nil)
(setq compile-angel-enable-native-compile t)
(setq compile-angel-native-compile-load nil)
;; (setq compile-angel-exclude-core-emacs-directory
;;       ;; Emacs was compiled with native-compile aot
;;       (when (and (fboundp 'subr-native-elisp-p)
;;                  (subr-native-elisp-p (symbol-function 'find-file)))
;;         t))

(setq compile-angel-verbose t)
(setq compile-angel-debug t)

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
          ;; TODO Both paths?
          "~/src/dotfiles/jc-dev"
          (expand-file-name "~/src/dotfiles/jc-dev")
          "~/src/emacs/"
          (expand-file-name "~/src/emacs/"))))
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

(setq buffer-terminator-verbose 'inhibit-message)

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

(defun lightemacs-user-post-init ()
  "This function is executed right before loading modules."
  ;; Ensure load-path is accurate even after installing packages
  (my-add-packages-to-load-path)

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

;;; lightemacs-user-init

(defun lightemacs-user-init ()
  "This function is executed right before loading modules."
  (my-update-package-pinned-packages my-package-pinned-packages)

  ;; Add them a second time just in case one of them gets installed
  (my-add-packages-to-load-path)

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
          (?f . evil-surround-function))))

;;; Startup time

(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs loaded in %.2f seconds (Init only: %.2fs) with %d garbage collections."
           (time-to-seconds (time-since before-init-time))
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time 200)

;;; Local variables

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'config)

;; config.el ends here

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

(let ((user-dir (file-truename lightemacs-user-directory)))
  (cond
   ((string= user-dir
             (file-truename "~/.emacs-elpaca.d/"))
    (setq lightemacs-package-manager 'elpaca))
   ((string= user-dir
             (file-truename "~/.emacs-straight.d/"))
    (setq lightemacs-package-manager 'straight))))


;; TODO test this more. It does not seem stable.
(when (eq lightemacs-package-manager 'builtin-package)
  (setq package-quickstart t))

(setq native-comp-jit-compilation nil)

(defun lightemacs-user-post-early-init ()
  "Post early init."
  (setq native-comp-async-report-warnings-errors t)
  ;; (when (eq lightemacs-package-manager 'builtin-package)
  ;;   (setq use-package-compute-statistics t))
  )

(setq package-native-compile t)
(when (eq lightemacs-package-manager 'straight)
  ;; TODO compile angel readme?
  (setq straight-disable-native-compile t)
  (setq straight-disable-compile t)

  ;; Causes issues
  ;; (setq straight-disable-autoloads t)
  )

;;; byte-compile

(defvar my-elc-cache--emacs-lisp-directory
  ;; Directory where Emacs's own *.el and *.elc Lisp files are installed.
  (if (bound-and-true-p lisp-directory)
      ;; Always use `file-truename'
      (file-truename lisp-directory)
    (when-let* ((library-path (locate-library "simple")))
      ;; Always use `file-truename'
      (file-name-directory (file-truename library-path)))))

;; TODO lightemacs?
(defvar my-elc-cache-directory
  ;; This has to be `file-truename'
  (file-truename (expand-file-name "elc-cache/" user-emacs-directory))
  "Directory to store byte-compiled .elc files.")

(defun my-elc-cache-dest-file (filename)
  "Determine the cache destination for the byte-compiled FILENAME."
  (let* ((true-file (file-truename filename))
         ;; FIX: Temporarily disable the hook variable to prevent infinite
         ;; recursion
         (byte-compile-dest-file-function nil)
         ;; Use Emacs's native compiler functions to safely handle .gz and .elc
         (default-dest (if (fboundp 'byte-compile-default-dest-file)
                           (byte-compile-default-dest-file filename)
                         ;; Fallback for older Emacs versions
                         (byte-compile-dest-file filename))))
    ;; Ignore Emacs's built-in files
    (if (or
         ;; (and my-elc-cache--emacs-lisp-directory
         ;;      (string-prefix-p my-elc-cache--emacs-lisp-directory true-file))
         ;; early-init has no way to guess the elc-cache path before it is
         ;; loaded before config.el
         ;; (string-suffix-p "/init.el" true-file)
         ;; (string-suffix-p "/early-init.el" true-file)
         ;; (string-suffix-p "/config.el" true-file)
         (not (string-prefix-p (file-truename "~/src/") true-file))
         ;; Ignore files already in the cache directory to prevent recursive
         ;; paths
         ;; (string-prefix-p my-elc-cache-directory true-file)
         )
        ;; Return the normal destination
        default-dest
      ;; Map the third-party file into the cache directory
      (let* ((expanded-dest (expand-file-name default-dest))
             ;; Strip the leading slash so it appends properly to the cache
             ;; directory
             (relative (replace-regexp-in-string "^/" "" expanded-dest))
             (dest (expand-file-name relative my-elc-cache-directory)))
        (make-directory (file-name-directory dest) t)
        dest))))

;; TODO
;; The provided Emacs Lisp snippet successfully intercepts
;; the byte-compiler and writes the .elc files to your
;; custom elc-cache/ directory. However, Emacs is still
;; loading the .el files because of a disconnect between the
;; compiler and the loader. Setting
;; byte-compile-dest-file-function only changes the output
;; destination for the compilation process. It does not
;; update the load-path or instruct the load function on
;; where to find those newly relocated .elc files. When
;; Emacs evaluates (require 'feature) or (load "library"),
;; it searches the directories listed in your load-path
;; variable. Because the nested directories inside your
;; elc-cache/ are not in the load-path, Emacs cannot see the
;; compiled files. It falls back to the original
;; directories, finds the uncompiled .el files, and loads
;; them instead. To make Emacs use your out-of-tree .elc
;; files, you need to bridge this gap. You have two main
;; approaches: Modify the Load Path: You can write a
;; function that recursively finds all directories inside
;; my-elc-cache-directory and prepends them to your
;; load-path. This allows the default Emacs load function to
;; find the cached .elc files before checking the original
;; source directories. Advise the Load Function: You can use
;; advice-add on the load function to intercept every load
;; request. The advice would check if a matching .elc file
;; exists in your cache directory and, if so, dynamically
;; rewrite the file path before passing it to the original
;; load function. As a side note regarding your
;; configuration, since you have enabled native compilation
;; ((setq native-comp-jit-compilation t)), Emacs
;; automatically handles out-of-tree caching for natively
;; compiled .eln files via native-comp-eln-load-path.
;; Keeping .elc files alongside their .el sources is the
;; standard design in Emacs, which is why out-of-tree
;; byte-compilation requires extra configuration to work
;; correctly.
;;
;; Redirect the byte compiler output
(setq byte-compile-dest-file-function #'my-elc-cache-dest-file)

;;; Other settings

;; Fix autoload modus-themes (straight)
;; (autoload 'modus-themes-declare "modus-themes" nil nil 'macro)

;; TODO lightemacs package manager variable to disable autoloads
;; Buggy?
;; (with-eval-after-load 'elpaca
;;   (setq elpaca-build-steps (remove 'elpaca--generate-autoloads-async
;;                                    elpaca-build-steps)))

(setq compile-angel-verbose t)
(setq compile-angel-debug t)
;; (setq compile-angel-enable-byte-compile nil)
;; (setq compile-angel-enable-native-compile nil)
;; (setq compile-angel-on-load-mode-compile-once nil)

;; Experimental
;; (setq compile-angel-reload-compiled-version t)
;; (setq compile-angel-native-compile-load t)

(require 'seq)

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

(setq lightemacs-recentf-track-switch-to-buffer t)

(defvar my-src-dir-prefix (file-name-as-directory (expand-file-name "~/src/")))
;; (defun my-compile-angel-predicate (el-file)
;;   "Compile Angel predicate.
;; EL-FILE is the *.el file."
;;   (if (string-prefix-p my-src-dir-prefix (file-truename el-file))
;;       (progn
;;         :continue
;;         ;; :native-comp
;;         )
;;     :continue))
;; (setq compile-angel-predicate-function #'my-compile-angel-predicate)

(with-eval-after-load 'compile-angel
  ;; Exclusions
  (push "/file-templates-auto/main.el" compile-angel-excluded-files)
  (push "/tmp-file.el" compile-angel-excluded-files)
  (push "/.dir-settings.el" compile-angel-excluded-files)

  ;; This is important because Emacs loads the early-init.elc, even if it is
  ;; older than the early-init.el file
  (push "/early-init.el" compile-angel-excluded-files))

;; I am using the predicate instead
;; (with-eval-after-load 'compile-angel
;;   (if (fboundp 'compile-angel-exclude-directory)
;;       (compile-angel-exclude-directory "~/src/emacs/")
;;     (error "Undefined: compile-angel-exclude-directory")))

;; (setq compile-angel-exclude-core-emacs-directory
;;       ;; Emacs was compiled with native-compile aot
;;       (when (and (fboundp 'subr-native-elisp-p)
;;                  (subr-native-elisp-p (symbol-function 'find-file)))
;;         t))

(setq lightemacs-dtrt-indent-excluded-modes '(emacs-lisp-mode
                                              python-mode
                                              python-ts-mode))

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
;;   (add-hook 'lightemacs-emacs-startup-hook #'my-delayed-native-compilation))

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
                                     ;;
                                     ;; Aggressive enough for performance but
                                     ;; stable; avoid -O3 or -Ofast since they
                                     ;; can break numeric primitives in Emacs
                                     ;; Lisp code.
                                     "-O2"

                                     ;; Disables generation of debug symbols.
                                     ;; Reduces compilation time and disk usage
                                     ;; for .eln files.
                                     "-g0"

                                     ;; This also causes instability:
                                     ;; /usr/bin/ld: /tmp/ccps2Qse.o:
                                     ;; plugin needed to handle lto object
                                     ;;
                                     ;; This is a known issue with older
                                     ;; versions of libgccjit failing to parse
                                     ;; the "native" expansion correctly.
                                     ;; Hardcoding "-march=skylake" and
                                     ;; "-mtune=skylake" is a valid workaround.
                                     ;; However, passing these to both the
                                     ;; compiler and the driver is unnecessary.
                                     ;; Passing CPU architecture flags to
                                     ;; native-comp-driver-options is sufficient
                                     ;; for the assembler and linker phases to
                                     ;; optimize the binary output.
                                     ;;
                                     ;; Commented out. If your Emacs build
                                     ;; already enables LTO
                                     ;; (--enable-link-time-optimization), you
                                     ;; do not need this here. Let the main
                                     ;; build handle LTO across modules.
                                     ;; Otherwise, enabling it here may cause
                                     ;; linking issues.
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
                                     ;;
                                     ;; Emacs relies on IEEE floating-point
                                     ;; behavior. Never enable -ffast-math or
                                     ;; similar, as it can break native
                                     ;; compilation semantics.
                                     ;;
                                     ;; This is the default behavior of GCC
                                     ;; unless you explicitly p
                                     "-fno-finite-math-only"

                                     ;; Can improve performance in loops, but
                                     ;; sometimes increases binary size.
                                     ;;
                                     ;; Usually unnecessary for Emacs Lisp;
                                     ;; increases binary size for negligible
                                     ;; benefit. Leave disabled.
                                     ;; "-funroll-loops"

                                     ;; Allows the compiler to assume standard C
                                     ;; aliasing rules. Emacs native code
                                     ;; adheres to these rules, enabling better
                                     ;; load and store optimizations.
                                     ;;
                                     ;; Standard C aliasing rules are respected
                                     ;; by Emacs; allows minor optimizations.
                                     ;;
                                     ;; This is is automatically enabled by
                                     ;; "-O2". You can safely remove it.
                                     ;; "-fstrict-aliasing"

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

(message "LOADING config.el")
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
          "~/src/emacs/"
          )))
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
;; (add-hook 'lightemacs-emacs-startup-hook #'my-restore-vc-handled-backends 120)

;;; Lightemacs modules and parameters

(setq stripspace-verbose nil)
(setq stripspace-normalize-indentation t)
(setq stripspace-restore-column t)
(setq stripspace-only-if-initially-clean t)

(setq lightemacs-reduce-messages t)
(setq lightemacs-saveplace-recenter-after-find-file t)

(setq lightemacs-debug t)
(setq lightemacs-verbose t)

(when (<= emacs-major-version 31)
  (setq package-install-upgrade-built-in t))

(setq lightemacs-dired-omit-parent-directory t)
(setq lightemacs-cycle nil)
(setq lightemacs-native-comp-excluded-cpus 1)

(setq lightemacs-theme-name 'tomorrow-night-deepblue)
(setq lightemacs-theme-package 'tomorrow-night-deepblue-theme)

;; Enable native-compilation and byte-compilation

(setq lightemacs-modules '(le-compile-angel  ;;moved it down
                           le-pathaction
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
                           ;; le-evil-commentary  ; I am using my own

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

                           mod-misc

                           ;; My modules
                           ;;------------------------------

                           ;; My modules
                           ;; TODO change
                           ;; (setq lightemacs-display-line-numbers-mode-add-hook-to nil)
                           ;; le-display-line-numbers  ;; mod-misc provides its own

                           ;; Legacy (DEPRECATED)
                           ;; mod-defun

                           ;; New
                           ;; tmp-easysession
                           buffer-guardian

                           mod-eglot
                           ;; TODO fix eldoc help more than one line. I only
                           ;; want 1 line
                           ;; mod-lsp-mode

                           smartindent
                           battery-angel
                           mod-project
                           mod-buffer-terminator
                           mod-toggle-term
                           point-manager

                           mod-kirigami
                           ;; mod-kirigami-alternative

                           ;; le-treesit-auto
                           ))

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

(when (eq lightemacs-package-manager 'straight)
  (setq straight-recipe-overrides nil)
  (add-to-list 'straight-recipe-overrides
               '(bufferwizard
                 :type git :host github
                 :repo "jamescherti/bufferwizard.el")))

;; (add-to-list 'straight-recipe-overrides
;;              '(compile-angel :local-repo "~/src/emacs/compile-angel.el"))
;; Tell straight.el to treat it as a built-in Emacs package

;; NOTE DISABLED to avoid .elc conflicts
;; (dolist (item '(be-quiet
;;                 bufferfile
;;                 buffer-terminator
;;                 bufferwizard
;;                 compile-angel
;;                 cursorcolumn
;;                 dir-config
;;                 easysession
;;                 enhanced-evil-paredit
;;                 flymake-ansible-lint
;;                 flymake-bashate
;;                 inhibit-mouse
;;                 kirigami
;;                 org-ibullets
;;                 outline-indent
;;                 outline-yaml
;;                 pathaction
;;                 persist-text-scale
;;                 quick-fasd
;;                 quick-sdcv
;;                 stripspace
;;                 tomorrow-night-deepblue-theme
;;                 ultisnips-mode
;;                 vim-tab-bar))
;;   (when (file-exists-p (expand-file-name (concat (symbol-name item) ".el")
;;                                          "~/src/emacs"))
;;     (when (eq lightemacs-package-manager 'straight)
;;       (add-to-list 'straight-recipe-overrides
;;                    (list item :type 'built-in)))))

(defun my-add-packages-to-load-path ()
  "Add my packages to `load-path\=' dynamically.
Iterates over `my-package-base-directory\=' and adds all subdirectories to
`load-path\=', skipping any directories listed in
`my-excluded-package-directories\='. Caches the result in
`my--package-load-path-cache\=' to avoid redundant scanning."
  ;; (let ((default-directory (expand-file-name "~/src/forks/evil-collection/")))
  ;;   (add-to-list 'load-path default-directory)
  ;;   (push default-directory load-path)
  ;;   (normal-top-level-add-subdirs-to-load-path))

  ;; (let ((default-directory (expand-file-name "~/src/forks/evil/")))
  ;;   (add-to-list 'load-path default-directory)
  ;;   (push default-directory load-path)
  ;;   (normal-top-level-add-subdirs-to-load-path))

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
    ;; (push path load-path)
    (add-to-list 'load-path path)
    ))

;;; display buffer alist

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

;;; Always current window

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

;; (defun lightemacs-user-post-init ()
;;   "User post init."
;;   (my-add-packages-to-load-path))


;;; pre/post modules

(defun lightemacs-user-pre-modules ()
  "Pre-modules."
  (my-add-packages-to-load-path)
  (my-config-display-buffer-alist)
  (current-window-only-setup))

(defun lightemacs-user-post-modules ()
  "Post-modules."
  (my-add-packages-to-load-path))

;;; Provide

(provide 'config)

;; Local variables:
;; byte-compile-warnings: (not lexical free-vars)
;; End:

;;; config.el ends here

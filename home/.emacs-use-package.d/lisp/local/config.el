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

;;; Debug on error

(setq debug-on-error t)

;;; Profiling

;; (profiler-start 'cpu)
;; (add-hook 'emacs-startup-hook #'(lambda() (when (fboundp 'profiler-stop)
;;                                             (profiler-stop)))
;;           300)

;;; Native compilation settings

(setq lightemacs-native-comp-excluded-cpus 2)

(setq native-comp-jit-compilation nil)
(setq compile-angel-enable-native-compile t)

(setq lightemacs-load-compiled-init-files t)
(setq lightemacs-recentf-track-switch-to-buffer t)

;; Experimental
;; TODO make them a default
(setq compile-angel-cache-file-truename t)

(setq compile-angel-verbose t)
(setq compile-angel-debug nil)

(setq buffer-terminator-verbose nil)
(setq buffer-terminator-debug nil)

;; (setq compile-angel-enable-byte-compile nil)
;; (setq compile-angel-enable-native-compile nil)
(setq compile-angel-on-load-mode-compile-once nil)

;; Experimental
;; (setq compile-angel-reload-compiled-version t)
;; (setq compile-angel-native-compile-load t)

;; Which kind of warnings and errors to report from async native compilation.
;; (setq native-comp-async-warnings-errors-kind 'important)
(setq native-comp-async-warnings-errors-kind 'all)

(setq native-comp-async-report-warnings-errors t)

;; (when (eq lightemacs-package-manager 'straight)
;;   ;; TODO compile angel readme?
;;   (setq straight-disable-native-compile t)
;;   (setq straight-disable-compile t)
;;   )

;; native-comp-speed controls the Emacs Lisp frontend. It dictates how
;; aggressively the Emacs Lisp compiler optimizes your code at the semantic
;; level before translating it into an intermediate representation.
;;
;; Setting native-comp-speed to 3 changes the behavior and assumptions of the
;; Emacs Lisp compiler itself.
;;
;; Semantic Assumptions: At speed 3, Emacs makes aggressive assumptions about
;; the Lisp environment. For instance, it assumes that functions and macros
;; will not be redefined at runtime.
;;
;; Type Checking: It may omit certain runtime type checks to execute
;; instructions faster.
;;
;; Risk Level: Because it actively alters Lisp semantics, speed 3 is considered
;; "unsafe." Heavily dynamic code that relies on advising or redefining
;; functions on the fly might break or behave unpredictably.
(setq native-comp-speed 2)

(setq vterm-module-cmake-args
      "-DCMAKE_C_FLAGS='-O2 -march=native -mtune=native' -DCMAKE_SHARED_LINKER_FLAGS='-Wl,-O2 -Wl,--as-needed' -DUSE_SYSTEM_LIBVTERM=yes")

;; `native-comp-compiler-options' specifies flags passed directly to the C
;; compiler (for example, GCC or Clang) when compiling the Lisp-to-C output
;; produced by the native compilation process. These flags affect code
;; generation, optimization, and debugging information.
;;
;; This is different from `native-comp-speed'. `native-comp-compiler-options'
;; controls the GCC backend. It passes command-line flags directly to libgccjit,
;; instructing the underlying C compiler how to optimize the generated machine
;; code.
;;
;; Machine-Level Optimizations: This enables standard GCC optimizations like
;; loop unrolling, vectorization, and aggressive inlining at the machine code
;; level.
;;
;; Semantic Preservation: Unlike changing the `native-comp-speed', passing -O3
;; here does not change the logic or the rules of your Emacs Lisp code. It
;; simply asks the C compiler to produce the most highly optimized machine
;; instructions for the abstract syntax tree it was given.
;;
;; Risk Level: It is generally safer for your Lisp code's behavior. However, it
;; will significantly increase the CPU time required for Emacs to compile
;; packages in the background.
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
                                     ;;
                                     ;; NOTE:
                                     ;; Redundancy of optimization flags: Emacs
                                     ;; manages libgccjit optimization levels
                                     ;; internally via the native-comp-speed
                                     ;; variable. By default, native-comp-speed
                                     ;; is set to 2, which maps to standard
                                     ;; optimizations with some Emacs-specific
                                     ;; safety guards. Passing -O2 manually
                                     ;; through native-comp-compiler-options is
                                     ;; redundant and bypasses the native
                                     ;; compiler's built-in logic.
                                     "-O2"

                                     ;; Using -g0 disables the generation of
                                     ;; debug symbols for .eln files, which
                                     ;; reduces their size on disk and speeds up
                                     ;; the compilation process itself.
                                     "-g0"

                                     ;; -fno-omit-frame-pointer The Emacs
                                     ;; developers recommend using this flag to
                                     ;; disable omit-frame-pointer. Although
                                     ;; enabling omit-frame-pointer frees up a
                                     ;; general-purpose CPU register, it does
                                     ;; not yield significant performance gains
                                     ;; on modern architectures and can lead to
                                     ;; bugs that are difficult to debug.
                                     ;; According to Eli Zaretskii, an Emacs
                                     ;; developer: “See bug#76180. This is in
                                     ;; the context of the igc branch, where
                                     ;; omit-frame-pointer is particularly
                                     ;; troublesome, though it also causes
                                     ;; problems in other situations. For
                                     ;; further details, see etc/DEBUG in the
                                     ;; Emacs source tree.”
                                     ;; "-fno-omit-frame-pointer"
                                     ;;
                                     ;; These are .eln files, not Emacs C
                                     ;; source.
                                     ;; "-fomit-frame-pointer"
                                     ;;
                                     ;; NOTE: Removing this makes backtraces
                                     ;; less reliable. More importantly, if you
                                     ;; are compiling the highly experimental
                                     ;; igc (MPS garbage collector) branch of
                                     ;; Emacs, removing this flag will cause
                                     ;; crashes. If you are on standard Emacs
                                     ;; (version 29 or 30), removing it is safe
                                     ;; and faster.
                                     ;; "-fno-omit-frame-pointer"

                                     ;; The -fno-finite-math-only flag prevents
                                     ;; the compiler from assuming that
                                     ;; floating-point operations never produce
                                     ;; NaN or infinity values. This flag is
                                     ;; mostly defensive because GCC does not
                                     ;; enable -ffinite-math-only at -O2 or -O3
                                     ;; by default, but it can help avoid unsafe
                                     ;; assumptions if additional aggressive
                                     ;; optimization flags are introduced later.
                                     "-fno-finite-math-only"

                                     ;; By default, when GCC compiles a shared
                                     ;; library, it adheres strictly to the ELF
                                     ;; standard, which assumes that externally
                                     ;; visible symbols might be interposed at
                                     ;; runtime (e.g., via LD_PRELOAD). This
                                     ;; assumption acts as a strict barrier that
                                     ;; inhibits optimizations such as function
                                     ;; inlining across boundaries, constant
                                     ;; propagation, and devirtualization.
                                     ;;
                                     ;; -fno-semantic-interposition tells GCC to
                                     ;; assume that functions defined within the
                                     ;; shared object will not be overridden
                                     ;; externally, removing this barrier.
                                     ;;
                                     ;; Benefit: Small. While .eln files *are*
                                     ;; compiled as shared objects (which is
                                     ;; where this flag technically applies),
                                     ;; interposition is not a major performance
                                     ;; bottleneck in this context. Emacs Lisp
                                     ;; compiled output is not call-heavy in the
                                     ;; same way native C libraries are;
                                     ;; execution time is dominated by dynamic
                                     ;; dispatch, garbage collection checks, and
                                     ;; calls back to C primitives. Breaking the
                                     ;; interposition optimization barrier here
                                     ;; is technically correct but only
                                     ;; beneficial in very narrow cases.
                                     ;;
                                     ;; Drawback: None. Elisp functions compiled
                                     ;; into a .eln file are not going to be
                                     ;; interposed at the ELF/C linker level.
                                     ;;
                                     ;; -fno-semantic-interposition: This flag
                                     ;; tells the compiler it can safely inline
                                     ;; functions within a shared library
                                     ;; because those symbols will not be
                                     ;; overridden (interposed) by the host
                                     ;; application. While theoretically
                                     ;; beneficial for .eln shared objects,
                                     ;; libgccjit handles internal Elisp symbol
                                     ;; resolution strictly. The measurable
                                     ;; performance gain here is virtually zero.
                                     "-fno-semantic-interposition"

                                     ;; -fgraphite-identity & -floop-nest-optimize
                                     ;;
                                     ;; - Benefit: Zero. Elisp loops (like while
                                     ;;   or dolist) translate into C code that
                                     ;;   is heavily interrupted by dynamic
                                     ;;   type-checking, garbage collection
                                     ;;   probes, and maybe_quit signals to keep
                                     ;;   the UI responsive. Graphite is
                                     ;;   designed for pure, predictable
                                     ;;   mathematical loops (like matrix
                                     ;;   calculations). It will find nothing to
                                     ;;   optimize in Elisp loops.
                                     ;;
                                     ;; - Drawback: Pure wasted CPU cycles
                                     ;;   during package compilation. libgccjit
                                     ;;   will spend time attempting to build
                                     ;;   polyhedral models of your Elisp loops,
                                     ;;   fail to find optimization
                                     ;;   opportunities, and abandon the effort.
                                     ;; TODO
                                     ;; "-fgraphite-identity"
                                     ;; "-floop-nest-optimize"

                                     ;; -fdevirtualize-at-ltrans: This flag is
                                     ;; used during Link Time Optimization (LTO)
                                     ;; to devirtualize C++ virtual function
                                     ;; calls. Because the core of Emacs is
                                     ;; written entirely in C and Emacs Lisp,
                                     ;; there are no C++ classes or virtual
                                     ;; methods to optimize. This flag doesn't
                                     ;; do anything for your build.
                                     ;; "-fdevirtualize-at-ltrans"

                                     ;; Benefits:
                                     ;; - Practically zero. Emacs native
                                     ;; compilation uses libgccjit to compile
                                     ;; Emacs Lisp into independent shared
                                     ;; libraries (.eln files) that are loaded
                                     ;; dynamically at runtime.
                                     ;; - Because there is no large-scale
                                     ;; linking of multiple object files into a
                                     ;; single binary, whole-program
                                     ;; optimization techniques do not apply
                                     ;; here.
                                     ;; Tradeoffs:
                                     ;; - Introduces a high probability of build
                                     ;; failures. The libgccjit environment is
                                     ;; delicate and sensitive to injected
                                     ;; compiler and linker flags.
                                     ;; - Forcing linker plugins into the async
                                     ;; native compilation threads can cause the
                                     ;; generation of .eln files to crash or
                                     ;; hang indefinitely.
                                     ;; - It wastes system resources by adding
                                     ;; unnecessary steps to the compilation of
                                     ;; single-file shared objects.
                                     ;; "-fuse-linker-plugin"
                                     ))

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
;;
;; I did not add the following to `native-comp-driver-options':
;;
;; -Wl,--sort-common:
;; ------------------
;; This flag instructs the linker to sort "common" symbols (uninitialized global
;; variables in C) by their alignment to reduce memory padding. The issue: The
;; GCC intermediate representation (IR) generated by Emacs for Lisp code does
;; not produce the kind of raw, uninitialized C global variables that benefit
;; from this sorting. Passing this flag forces the linker to perform a sorting
;; pass on empty or near-empty symbol tables for every single .el file you
;; compile. It is wasted CPU time.
;;
;; -Wl,-z,now and -Wl,-z,relro:
;; ----------------------------
;; These two flags are used together to enable "Full Relocation Read-Only." This
;; is a security hardening technique. It forces the dynamic linker to resolve
;; all dynamically linked functions at startup (now) and then marks the Global
;; Offset Table (GOT) as read-only (relro). This prevents attackers from
;; exploiting buffer overflows to overwrite function pointers in the GOT. The
;; issue: Emacs .eln files are not standalone executables; they are dynamically
;; loaded plugins that serve as caches for compiled Lisp code.
;;
;; Security mismatch: You do not have buffer overflows in safe Emacs Lisp code
;; that would allow an attacker to execute a GOT overwrite exploit against an
;; .eln file. Hardening .eln files against memory corruption exploits is
;; completely unnecessary.
;;
;; Performance penalty: Forcing -z,now means that when Emacs loads an .eln file
;; via dlopen, the OS dynamic linker must instantly resolve every external
;; symbol in that file before Emacs can resume execution. While Emacs resolves
;; most of these anyway to wire up the Lisp environment, forcing this strictly
;; at the linker level can introduce micro-stutters during .eln loading.
;;
;; Compilation overhead: It adds extra linking steps to the asynchronous
;; libgccjit worker threads.
;;
;; ---------------------------------------------------------------------------
;; Optimize the linker output for natively compiled Elisp (.eln files):
(setq native-comp-driver-options '(;; -Wl,-z,pack-relative-relocs: Compresses
                                   ;; relocation tables to reduce file size and
                                   ;; slightly improve load times.
                                   "-Wl,-z,pack-relative-relocs"

                                   ;; -Wl,-O2: Applies standard linker-level
                                   ;; optimizations (like string merging) to the
                                   ;; generated shared object.
                                   "-Wl,-O2"

                                   ;; -Wl,--as-needed: Prevents the linker from
                                   ;; recording dependencies on libraries that
                                   ;; are not actually used by the code.
                                   "-Wl,--as-needed"))

;; TODO minimal-emacs

;; Disable native compilation for dynamically generated files
;;
;; These files change frequently during package updates or are too small to
;; benefit from native compilation.
;;
;; TODO custom.el If your configuration forces the Emacs customize interface to
;; write to a separate file (e.g., (setq custom-file (expand-file-name
;; "custom.el" user-emacs-directory))), you should exclude it. Emacs routinely
;; overwrites this file with raw data (custom variables and face definitions).
;; libgccjit cannot optimize data parsing.
(let ((deny-list `(;; Native compilation translates Lisp into C code to optimize
                   ;; logic paths, loops, and math. A .dir-locals.el file is not
                   ;; an executable program; it is a static data structure (an
                   ;; association list) used to apply project-specific
                   ;; variables. Compiling static data provides zero benefit
                   ;; because the Lisp reader still has to parse the data
                   ;; structure regardless of whether it is compiled or not.
                   "\\(?:[/\\\\]\\.dir-locals\\.el\\(?:\\.gz\\)?$\\)"

                   ;; The *-autoloads.el and *-loaddefs.el files are
                   ;; automatically generated indices. Every time a package is
                   ;; installed or updated, these files are overwritten. If you
                   ;; do not block them, Emacs will spawn background GCC
                   ;; processes for every update. This wastes CPU cycles and
                   ;; fills your eln-cache directory with obsolete shared
                   ;; libraries (.eln files) that will simply be discarded on
                   ;; the next package update.
                   "\\(?:[/\\\\][^/\\\\]+-autoloads\\.el\\(?:\\.gz\\)?$\\)"
                   "\\(?:[/\\\\][^/\\\\]+-loaddefs\\.el\\(?:\\.gz\\)?$\\)"

                   ;; Just like autoloads, package.el dynamically generates a
                   ;; -pkg.el file for every package you install. These files
                   ;; contain a single (define-package ...) form holding
                   ;; metadata (version number, author, dependencies). Because
                   ;; it is 100% static data, compiling it is a waste of CPU and
                   ;; disk I/O.
                   "\\(?:[/\\\\][^/\\\\]+-pkg\\.el\\(?:\\.gz\\)?$\\)"

                   ;; TODO Useless?
                   ;; "\\(?:[/\\\\][^/\\\\]+-tests?\\.el\\(?:\\.gz\\)?$\\)"
                   ;; "\\(?:[/\\\\]\\.dir-settings\\.el\\(?:\\.gz\\)?$\\)"

                   ;; emacs-data directory
                   ,(concat "^"
                            (regexp-quote (abbreviate-file-name
                                           (expand-file-name "~/.emacs-data")))
                            ".*\\.el\\(?:\\.gz\\)?$")

                   ;; var directory
                   ,(concat "^"
                            (regexp-quote (expand-file-name "~/.emacs-data"))
                            ".*\\.el\\(?:\\.gz\\)?$")

                   ;; TODO this will exclude straight/elpa packages
                   ;; ,(concat "^"
                   ;;          (regexp-quote (abbreviate-file-name
                   ;;                         (expand-file-name lightemacs-var-directory)))
                   ;;          ".*\\.el\\(?:\\.gz\\)?$")
                   ;; ,(concat "^"
                   ;;          (regexp-quote (expand-file-name lightemacs-var-directory))
                   ;;          ".*\\.el\\(?:\\.gz\\)?$")
                   )))
  (setq native-comp-jit-compilation-deny-list deny-list)
  ;; Backwards compatibility for deprecated variable names that were replaced by
  ;; `native-comp-jit-compilation-deny-list'.
  (with-no-warnings
    (if (boundp 'native-comp-deferred-compilation-deny-list)
        (setq native-comp-deferred-compilation-deny-list deny-list)
      (when (boundp 'comp-deferred-compilation-deny-list)
        (setq comp-deferred-compilation-deny-list deny-list)))))

;;; Package manager

(let ((user-dir (file-truename lightemacs-user-directory)))
  (cond
   ((string= user-dir
             (file-truename "~/.emacs-elpaca.d/"))
    (setq lightemacs-package-manager 'elpaca))
   ((string= user-dir
             (file-truename "~/.emacs-straight.d/"))
    (setq lightemacs-package-manager 'straight))))

;;; Minimalist

(when (boundp 'trusted-content)
  (let ((dirs (list "~/src/dotfiles/jc-dev/"
                    "~/src/emacs/"
                    "~/src/forks/")))
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

;; Only support Git
(setq vc-handled-backends '(Git))

;;; Load ~/config.el

;; (message "LOADING config.el")
(load (expand-file-name "~/.config.el") :no-error :no-message :nosuffix)

;;; Temporary dir

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
(add-hook 'kill-emacs-hook #'my-cleanup-session-temp-directory 90)

;;; Byte-compile

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
;; TODO doesn't work for natively compiling my packages in src dir
;; (push my-elc-cache-directory load-path)
;; (setq byte-compile-dest-file-function #'my-elc-cache-dest-file)

;;; Auto detect architecture

(defvar my-auto-detect-cpu-architecture t)

(defvar my-cpu-architecture-cache-file (locate-user-emacs-file
                                        "cpu-arch.cache")
  "File path to persist the detected CPU architecture.")

(defun my-get-cpu-architecture ()
  "Return the CPU architecture detected via GCC target help output.
The detected value is returned as a string.
If GCC is not available or no architecture information can be
extracted, the function returns nil.
The result is cached to `my-cpu-architecture-cache-file' to avoid
subsequent GCC invocations."
  (let ((cache-file (expand-file-name my-cpu-architecture-cache-file)))
    (if (file-readable-p cache-file)
        ;; This forces Emacs to read and write the exact internal byte
        ;; representation of the text without attempting any implicit encoding
        ;; or decoding conversions.
        (let ((coding-system-for-read 'utf-8-emacs)
              ;; Prevent Emacs from trying to guess the file encoding based on
              ;; the filename or extension, forcing it to respect
              ;; coding-system-for-read.
              (file-coding-system-alist nil))
          (with-temp-buffer
            (insert-file-contents cache-file)
            (goto-char (point-min))
            (read (current-buffer))))
      (when (executable-find "gcc")
        (with-temp-buffer
          (let* ((default-directory temporary-file-directory)
                 (exit-code (call-process "gcc" nil t nil "-march=native"
                                          "-Q" "--help=target")))
            (when (zerop exit-code)
              (goto-char (point-min))
              (when (re-search-forward
                     "^[[:space:]]*-march=[[:space:]]+\\([^[:space:]]+\\)" nil t)
                (let ((arch (match-string 1)))
                  (make-directory (file-name-directory cache-file) t)
                  (with-temp-buffer
                    (prin1 arch (current-buffer))
                    ;; Force Emacs to read and write the exact internal byte
                    ;; representation of the text without attempting any implicit
                    ;; encoding or decoding conversions.
                    (let ((coding-system-for-write 'utf-8-emacs)
                          (write-region-annotate-functions nil)
                          (write-region-post-annotation-function nil))
                      (let ((inhibit-quit t))
                        (write-region (point-min) (point-max)
                                      cache-file nil
                                      'silent))))
                  arch)))))))))

;; Auto detect the CPU architecture
(when my-auto-detect-cpu-architecture
  (when-let* ((cpu-architecture (my-get-cpu-architecture)))
    (add-to-list 'native-comp-compiler-options
                 (format "-march=%s" cpu-architecture))
    (add-to-list 'native-comp-compiler-options
                 (format "-mtune=%s" cpu-architecture))))

;;; Lightemacs settings

(setq minimal-emacs-inhibit-redisplay-during-startup t)
(setq minimal-emacs-inhibit-message-during-startup nil)
(setq minimal-emacs-frame-title-format "Lightemacs")

(setq lightemacs-easysession-load-session-on-startup t)

(setq lightemacs-dtrt-indent-excluded-modes '(emacs-lisp-mode
                                              python-mode
                                              python-ts-mode))

;; Define your preferred font name here
(setq lightemacs-theme-default-font "Iosevka Term")

(setq lightemacs-dtrt-indent-global-target-hooks '())
(setq lightemacs-dtrt-indent-local-target-hooks '(prog-mode-hook
                                                  text-mode-hook
                                                  conf-mode-hook))
(setq lightemacs-reduce-messages t)
(setq lightemacs-saveplace-recenter-after-find-file t)

(setq lightemacs-debug t)
(setq lightemacs-verbose t)

(when (< emacs-major-version 31)
  (setq package-install-upgrade-built-in t))

(setq lightemacs-dired-omit-parent-directory t)
(setq lightemacs-cycle nil)

(unless noninteractive
  (setq lightemacs-theme-name 'tomorrow-night-deepblue)
  (setq lightemacs-theme-package 'tomorrow-night-deepblue-theme))

;;; Conditional modes (conditional code checking, reformatter...)

;; Managed dynamically
(setq lightemacs-stripspace-target-hooks nil)
(setq lightemacs-aggressive-indent-target-hooks nil)
(setq lightemacs-apheleia-target-hooks nil)
(setq lightemacs-flymake-target-hooks nil)

;; (setq lightemacs-stripspace-target-hooks '(prog-mode-hook))
;; (setq lightemacs-aggressive-indent-target-hooks '(emacs-lisp-mode-hook))
;; (setq lightemacs-apheleia-target-hooks '(python-mode-hook
;;                                          python-ts-mode-hook
;;
;;                                          sh-mode-hook
;;                                          bash-ts-mode-hook
;;
;;                                          emacs-lisp-mode-hook))
;; (setq lightemacs-flymake-target-hooks
;;       '(;; text-mode: Exceptions Configuration and Markup)
;;         python-mode-hook
;;         python-ts-mode-hook
;;
;;         sh-mode-hook
;;         bash-ts-mode-hook
;;
;;         emacs-lisp-mode-hook
;;
;;         ansible-mode-hook
;;         yaml-ts-mode-hook
;;         yaml-mode-hook
;;         ;; toml-ts-mode-hook
;;         ;; conf-toml-mode-hook
;;         ;; markdown-mode-hook
;;         ))



;;; Lightemacs modules

(setq lightemacs-modules '(mod-conditional-modes
                           mod-begin
                           mod-same-window
                           mod-dired
                           mod-flymake
                           mod-gc
                           mod-yasnippet

                           mod-kirigami
                           ;; le-kirigami ; replaced with mod-kirigami

                           le-compile-angel
                           le-flymake
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
                           ;; le-undo-fu ; replaced with undo-redo
                           le-undo-fu-session
                           le-vim-tab-bar
                           ;; le-evil-commentary  ; I am using my own module

                           le-buffer-terminator
                           le-inhibit-mouse

                           ;; le-csv-mode

                           le-quick-sdcv

                           le-outline
                           le-outline-indent
                           le-hideshow
                           le-treesit-fold

                           ;; xclip (Emacs handles the clipboard natively with
                           ;; your (setq select-enable-clipboard t) setting; you
                           ;; don't need this unless you are running in a very
                           ;; specific terminal-only environment without
                           ;; X11/Wayland forwarding)
                           ;; le-xclip

                           le-package-lint-flymake
                           le-flymake-ansible-lint

                           le-apheleia

                           le-consult
                           le-embark-consult
                           le-embark
                           le-vertico
                           le-orderless
                           le-marginalia

                           le-corfu
                           le-cape
                           mod-corfu-cape

                           ;; le-corfu-prescient
                           ;; le-prescient
                           ;; le-vertico-prescient

                           le-elisp-refs
                           le-easy-escape
                           le-aggressive-indent
                           le-highlight-defined
                           le-paredit
                           le-elisp-autofmt
                           ;; le-page-break-lines

                           ;; le-autorevert

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
                           mod-org

                           le-edit-indirect

                           le-yasnippet
                           ;; le-yasnippet-snippets

                           le-markdown-mode
                           ;; le-maybe-markdown-ts

                           le-maybe-yaml-ts

                           ;; le-avy
                           ;; le-ace-window
                           ;; le-expand-region

                           le-bufferfile

                           ;; TODO This adds support for indirect buffers:
                           ;;      https://github.com/dgutov/diff-hl/pull/276
                           ;; le-diff-hl
                           tmp-diff-hl

                           le-dtrt-indent

                           le-dumb-jump
                           le-git-modes
                           ;; le-helpful
                           le-indent-bars
                           le-stripspace

                           le-vterm
                           le-term

                           le-persist-text-scale

                           ;; TODO support project dirs like shell-pop
                           ;; mod-toggle-term

                           mod-misc
                           mod-ediff
                           tmpedit
                           mod-cleanup
                           mod-filetype

                           sub-project
                           mod-buffer-terminator
                           le-buffer-guardian
                           mod-eglot
                           smartindent
                           ;; mod-lsp-mode
                           point-manager
                           ;; battery-angel

                           le-easysession
                           le-server))

;;; Frame, disable cus-edit and x-apply-session-resources

(defun lightemacs-user-post-early-init ()
  "Post early init."
  ;; TODO: Lightemacs?
  (let ((no-border '(internal-border-width . 0)))
    (add-to-list 'default-frame-alist no-border)
    (add-to-list 'initial-frame-alist no-border))

  ;; Ignore X resources
  (advice-add #'x-apply-session-resources :override #'ignore)
  ;; (when (eq lightemacs-package-manager 'builtin-package)
  ;;   (setq use-package-compute-statistics t))
  )

(add-hook 'lightemacs-post-early-init-hook #'lightemacs-user-post-early-init)

;; TODO remove from devemacs and my emacs and add this to lightemacs
(with-eval-after-load 'cus-edit
  ;; Prevent Emacs from writing custom settings to any file
  (advice-add 'custom-save-all :override #'ignore))

;;; Package defaults

(with-eval-after-load 'le-gcmh
  (setq gcmh-high-cons-threshold (* 600 1024 1024)))

(setq stripspace-verbose nil)
(setq stripspace-normalize-indentation t)
(setq stripspace-restore-column t)
(setq stripspace-only-if-initially-clean t)
(setq stripspace-use-virtual-overlay nil)

;; If you want the lowest possible latency and do not care about system-level
;; Compose keys, or if you are willing to map them entirely within Emacs using
;; iso-transl, you can tell Emacs to rip out the GTK input context completely.
;;
;; This prevents Emacs passing keystrokes through GtkIMContext. It handles the
;; raw Wayland/X11 event symbols directly. This gives you terminal-level input
;; latency in a GUI frame.
;;
;; Disabling pgtk-use-im-context-on-new-connection bypasses GTK input method
;; handling in PGTK Emacs and can slightly reduce keyboard input latency. This
;; may improve perceived responsiveness, but it can disable GTK-provided
;; compose/dead-key and IME functionality. Compose behavior can be partially
;; restored inside Emacs using iso-transl.
;;
;; If you want to change it after connection, use the pgtk-use-im-context
;; function.
;;
;; If you go this route but still need Compose functionality, you can map the
;; raw <Multi_key> inside Emacs:
;; (require 'iso-transl)
;; (define-key global-map (kbd "<Multi_key>") iso-transl-ctl-x-8-map)
;; Completely disable GTK input method context overhead
;;
;; When an external IM module is active, the event flow for typing a standard
;; English letter looks like this:
;; - Wayland/X11 registers the key.
;; - GTK intercepts the key.
;; - GTK sends a D-Bus message to the IM daemon.
;; - The daemon evaluates if the key is part of a complex sequence.
;; - The daemon sends a D-Bus message back to GTK saying the key is unhandled.
;; - GTK finally passes the key to Emacs.
;;
;; Forcing GTK_IM_MODULE=none removes the D-Bus overhead. Setting
;; pgtk-use-im-context-on-new-connection to nil goes a step further by skipping
;; GTK's internal input filtering entirely, handing the raw event straight to
;; the Emacs event loop.
(setq pgtk-use-im-context-on-new-connection nil)

(setq buffer-guardian-override-save-some-buffers t)
(setq buffer-guardian-verbose nil)
(setq buffer-guardian-save-all-buffers-interval (* 60 30))
(setq buffer-guardian-save-all-buffers-idle (* 4 60))

(setq server-client-instructions nil)

;;; Other settings

(defvar my-src-dir-prefix (file-name-as-directory (expand-file-name "~/src/")))

;;; Straight

(defvar my-straight-default-profile (expand-file-name
                                     "~/.emacs-data/etc/straight-profile.el")
  "The default straight profile.")
(setq straight-profiles
      `((nil . ,my-straight-default-profile)))

(defun my-copy-straight-profile-advice (orig-fun &rest args)
  "Advise `straight-freeze-versions' to copy the profile.
ORIG-FUN and ARGS is the advised function and its arguments.
This uses an around advice to trap errors and verify file timestamps."
  (condition-case err
      (let ((result (apply orig-fun args))
            (source (expand-file-name my-straight-default-profile))
            (destination
             (expand-file-name
              "~/src/dotfiles/jc-dev/home/.emacs-data/etc/straight-profile.el")))
        ;; (message "%s:%s"
        ;;          (file-exists-p source)
        ;;          (file-newer-than-file-p source destination))
        (when (and (file-regular-p source)
                   (file-newer-than-file-p source destination))
          (copy-file source destination t)
          (message "Copied %s to %s" source destination))
        result)
    (error
     (message "straight-freeze-versions failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

(with-eval-after-load 'straight
  (when (fboundp 'straight-freeze-versions)
    (advice-add 'straight-freeze-versions :around #'my-copy-straight-profile-advice)))

(setq straight-recipes-gnu-elpa-use-mirror nil)

;; Changing straight-recipes-emacsmirror-use-mirror to nil makes straight use
;; epkgs
(setq straight-recipes-emacsmirror-use-mirror nil)

(setq lightemacs-straight-bootstrap-url "https://raw.githubusercontent.com/jamescherti/straight.el/main/install.el")

(setq straight-recipes-org-url "https://github.com/jamescherti/org-mode")

;; (choice
;;           (const :tag "GNU repository"
;;                  "https://git.savannah.gnu.org/git/emacs/nongnu.git")
;;           (const :tag "GitHub mirror (HTTPS)"
;;                  "https://github.com/emacsmirror/nongnu_elpa.git")
;;           (const :tag "GitHub mirror (SSH)"
;;                  "git@github.com:emacsmirror/nongnu_elpa.git")
;;           (string :tag "Custom value"))
(setq straight-recipes-nongnu-elpa-url "https://github.com/emacsmirror/nongnu_elpa.git")

;; (defcustom straight-recipes-gnu-elpa-url
;;   (if (eq straight-vc-git-default-protocol 'ssh)
;;       "git@github.com:emacsmirror/gnu_elpa.git"
;;     "https://github.com/emacsmirror/gnu_elpa.git")
;;   "URL of the Git repository for the GNU ELPA package repository."
;;   :type '(choice
;;           (const :tag "GNU repository"
;;                  "https://git.savannah.gnu.org/git/emacs/elpa.git")
;;           (const :tag "GitHub mirror (HTTPS)"
;;                  "https://github.com/emacsmirror/gnu_elpa.git")
;;           (const :tag "GitHub mirror (SSH)"
;;                  "git@github.com:emacsmirror/gnu_elpa.git")
;;           (string :tag "Custom value"))
;;   :set (straight--set "
;; Must be set before bootstrap."))
(setq straight-recipes-gnu-elpa-url "https://github.com/emacsmirror/gnu_elpa.git")

(setq straight-initial-recipe-repositories
      (list
       '(org-elpa :local-repo nil)
       '(melpa :type git :host github
               :repo "melpa/melpa"
               :build nil)

       (if straight-recipes-gnu-elpa-use-mirror
           '(gnu-elpa-mirror :type git :host github
                             :repo "emacs-straight/gnu-elpa-mirror"
                             :build nil)
         `(gnu-elpa :type git
                    :repo ,straight-recipes-gnu-elpa-url
                    :depth (full single-branch)
                    :local-repo "elpa"
                    :build nil))
       `(nongnu-elpa :type git
                     :repo ,straight-recipes-nongnu-elpa-url
                     :depth (full single-branch)
                     :local-repo "nongnu-elpa"
                     :build nil)

       ;; el-get is included purely as a legacy fallback mechanism. Because
       ;; el-get was one of the earliest package managers to rely on a massive
       ;; crowd-sourced repository of recipes, straight.el included it so users
       ;; could install obscure, older packages that were never ported to modern
       ;; repositories like MELPA or GNU ELPA.
       ;;
       ;; If you remove el-get from straight-initial-recipe-repositories, the
       ;; only operational differences are:
       ;; - straight.el will look up un-reciped packages across your remaining
       ;;   repositories (MELPA, GNU ELPA, NonGNU ELPA, and Emacsmirror). This
       ;;   covers well over 99% of modern Emacs packages.
       ;; - If you ever happen to request a package that only exists in the old
       ;; el-get database, straight.el will fail to find a recipe and throw an
       ;; error.
       ;;
       ;; '(el-get :type git :host github
       ;;          :repo "dimitri/el-get"
       ;;          :build nil)

       ;; Emacsmirror was created to provide a comprehensive track of every
       ;; Emacs Lisp package ever hosted on GitHub or elsewhere, regardless of
       ;; whether the authors officially packaged them for a package manager.
       ;; However, in modern Emacs workflows:
       ;; - MELPA already indexes almost every actively maintained package.
       ;; - GNU ELPA and NonGNU ELPA handle core and officially curated
       ;;   community packages.
       ;; - If an obscure package only exists on Emacsmirror, it is far cleaner
       ;; and more performant to point directly to its upstream Git repo using
       ;; an explicit recipe plist in your config rather than cloning the
       ;; massive Emacsmirror metadata repository.
       ;;
       ;; (if straight-recipes-emacsmirror-use-mirror
       ;;     '(emacsmirror-mirror :type git :host github
       ;;                          :repo "emacs-straight/emacsmirror-mirror"
       ;;                          :build nil)
       ;;   '(emacsmirror :type git :host github
       ;;                 :repo "emacsmirror/epkgs"
       ;;                 :nonrecursive t
       ;;                 :build nil))
       ))

(when (eq lightemacs-package-manager 'straight)
  (setq straight-recipe-overrides
        '((nil
           .
           (;; Built-in
            (seq :type built-in)
            (transient :type built-in)
            (let-alist :type built-in)
            (use-package :type built-in)
            (bind-key :type built-in)

            (compat
             :type git
             :host github
             :repo "emacs-compat/compat"
             :branch "main")

            (indent-bars
             :type git :host github
             :repo "jdtsmith/indent-bars"
             :branch "main")

            ;; (modus-themes :type built-in)

            ;; TODO fix this
            (ef-themes
             :type git :host github
             :repo "protesilaos/ef-themes"
             :branch "main")

            ;; Forks of maintained packages
            (doom-themes
             :type git :host github
             :repo "jamescherti/doom-themes")
            (diff-hl
             :type git :host github
             :repo "jamescherti/diff-hl")
            (dtrt-indent
             :type git :host github
             :repo "jamescherti/dtrt-indent")
            (evil
             :type git :host github
             :repo "jamescherti/evil")
            (evil-collection
             :type git :host github
             :repo "jamescherti/evil-collection")
            (yasnippet
             :type git :host github
             :repo "jamescherti/yasnippet")
            (git-gutter
             :type git :host github
             :repo "jamescherti/git-gutter")

            ;; No tags (forked)
            (vterm
             :type git :host github
             :repo "jamescherti/emacs-libvterm")

            ;; Forks of unmaintained packages
            (s
             :type git :host github
             :repo "jamescherti/s.el")
            (undo-fu
             :type git :host github
             :repo "jamescherti/emacs-undo-fu")
            (undo-fu-session
             :type git :host github
             :repo "jamescherti/emacs-undo-fu-session")
            (flymake-yamllint
             :type git :host github
             :repo "jamescherti/flymake-yamllint")
            (evil-surround
             :type git :host github
             :repo "jamescherti/evil-surround")
            (wgrep
             :type git :host github
             :repo "jamescherti/Emacs-wgrep")
            (aggressive-indent-mode
             :type git :host github
             :repo "jamescherti/aggressive-indent-mode")
            (ansible-doc
             :type git :host github
             :repo "jamescherti/ansible-doc")
            ;; (dired-subtree
            ;;  :type git :host github
            ;;  :repo "jamescherti/dired-hacks")
            ;; (dired-narrow
            ;;  :type git :host github
            ;;  :repo "jamescherti/dired-hacks")
            (dired-filter
             :type git :host github
             :repo "jamescherti/dired-hacks")
            (easy-escape
             :type git :host github
             :repo "jamescherti/easy-escape")
            (edit-indirect
             :type git :host github
             :repo "jamescherti/edit-indirect")
            (elisp-refs
             :type git :host github
             :repo "jamescherti/elisp-refs")
            (evil-snipe
             :type git :host github
             :repo "jamescherti/evil-snipe")
            (f
             :type git :host github
             :repo "jamescherti/f.el")
            (flymake-quickdef
             :type git :host github
             :repo "jamescherti/flymake-quickdef")
            (gcmh
             :type git :host github
             :repo "jamescherti/gcmh")
            (goto-chg
             :type git :host github
             :repo "jamescherti/goto-chg")
            (helpful
             :type git :host github
             :repo "jamescherti/helpful")
            (highlight-defined
             :type git :host github
             :repo "jamescherti/highlight-defined")
            (lua-mode
             :type git :host github
             :repo "jamescherti/lua-mode")
            (org-appear
             :type git :host github
             :repo "jamescherti/org-appear")
            (paredit
             :type git :host github
             :repo "jamescherti/paredit")
            (vimrc-mode
             :type git :host github
             :repo "jamescherti/vimrc-mode")

            ;; Straight
            (straight
             :type git :host github
             :repo "jamescherti/straight.el"))))))

(defun my-straight-list-unpinned-packages ()
  "Find and display packages not present in straight's profile lockfiles."
  (interactive)
  (require 'subr-x)
  (when (and (fboundp 'straight--lockfile-read-all)
             (fboundp 'hash-table-keys))
    (let* ((lockfile-repos (mapcar #'car (straight--lockfile-read-all)))
           (active-packages (hash-table-keys straight--recipe-cache))
           (unpinned-packages nil))
      (dolist (package active-packages)
        (let* ((recipe (gethash package straight--recipe-cache))
               (local-repo (plist-get recipe :local-repo)))
          (when (and local-repo
                     (not (member local-repo lockfile-repos)))
            (push package unpinned-packages))))
      (if unpinned-packages
          (message "Unpinned packages: %s"
                   (mapconcat #'identity (sort unpinned-packages #'string-lessp) ", "))
        (message "All local packages are accounted for in the profile lockfiles.")))))

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
  "Add my packages to `load-path' dynamically.
Iterates over `my-package-base-directory' and adds all subdirectories to
`load-path', skipping any directories listed in
`my-excluded-package-directories'. Caches the result in
`my--package-load-path-cache' to avoid redundant scanning."
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
        (dolist (dir items)
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

  (let ((local-path (expand-file-name "~/src/forks/diff-hl")))
    (when (file-exists-p local-path)
      (push local-path load-path)))

  (dolist (path my--package-load-path-cache)
    ;; (push path load-path)
    (push path load-path)))

(defun lightemacs-user-before-modules ()
  "Pre-modules."
  (my-add-packages-to-load-path))

(defun lightemacs-user-after-modules ()
  "Post-modules."
  (my-add-packages-to-load-path))

(add-hook 'lightemacs-before-modules-hook #'lightemacs-user-before-modules)
(add-hook 'lightemacs-after-modules-hook #'lightemacs-user-after-modules)

;;; Frame geometry

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

(unless noninteractive
  (my-frame-geometry-load-initial-frame-alist)
  ;; (setq initial-frame-alist nil)
  (add-hook 'kill-emacs-hook 'my-frame-geometry-save))

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

;;; ~/.emacs-data directory

(defvar my-shared-user-emacs-directory (expand-file-name "~/.emacs-data/var"))
(setq tmpedit-dir (expand-file-name "tmpedit" my-shared-user-emacs-directory))

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
(setq auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
         ;; Redirect TRAMP (remote) file auto-saves to the local machine
         ;; (prefixed with "tramp-") to prevent Emacs from hanging due to
         ;; network latency during auto-save operations.
         ,(file-name-concat auto-save-list-file-prefix "tramp-\\2-") sha1)
        ("\\`/\\([^/]+/\\)*\\([^/]+\\)\\'"
         ;; Redirect absolute file paths auto-saves to the
         ;; `auto-save-list-file-prefix' directory. This appends the base
         ;; filename to the prefix, avoiding #file.txt# files across the system.
         ,(file-name-concat auto-save-list-file-prefix "\\2-") sha1)))
(when (memq system-type '(windows-nt cygwin ms-dos))
  (push `("\\`\\(/\\|[a-zA-Z]:/\\|//\\)\\([^/]+/\\)*\\([^/]+\\)\\'"
          ,(file-name-concat auto-save-list-file-prefix "\\3-") sha1)
        auto-save-file-name-transforms))

(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" my-shared-user-emacs-directory))

(setq save-place-file (expand-file-name "saveplace" my-shared-user-emacs-directory))

(setq abbrev-file-name (expand-file-name "abbrev_defs" my-shared-user-emacs-directory))

(setq easysession-directory
      (expand-file-name "easysession" my-shared-user-emacs-directory))

(setq project-list-file (when (boundp 'lightemacs-var-directory)
                          (expand-file-name "projects" my-shared-user-emacs-directory)))
(setq my-project-list-file-auto
      (expand-file-name "projects-auto"
                        my-shared-user-emacs-directory))

(setq recentf-save-file
      (expand-file-name "recentf" my-shared-user-emacs-directory))

;;; Compile angel

(with-eval-after-load 'compile-angel
  (when (fboundp 'compile-angel-exclude-file)
    (compile-angel-exclude-file
     (expand-file-name "prescient-save.el"
                       my-shared-user-emacs-directory))))

(with-eval-after-load 'compile-angel
  ;; Exclusions
  (push "/file-templates-auto/main.el" compile-angel-excluded-path-suffixes)
  (push "/tmp-file.el" compile-angel-excluded-path-suffixes)
  (push "/.dir-settings.el" compile-angel-excluded-path-suffixes))

;;; easysession

;; just open easysession.el dir 5 times in 5 tabs,
;; then open the easysession.el file
;; do not switch to other tabs
;; quit emacs, launch it, then the other tabs bufers will not be loaded
(setq easysession-buffer-list-function 'easysession-visible-buffer-list)

;; (setq easysession-save-pretty-print t)

;; (setq easysession-switch-to-save-session nil)
;; (setq easysession-mode-line-misc-info t)
;; (setq easysession-setup-load-session nil)

;; Change default to this
(setq easysession-fontify t)
(setq easysession-quiet t)

(setq easysession-switch-to-exclude-current t)
(setq easysession-save-interval (* 14 60))
(add-hook 'easysession-before-reset-hook
          #'(lambda()
              ;; Save all with no questions
              (when (fboundp 'my-save-all-buffers)
                (my-save-all-buffers))))

(defun my-easysession-only-main-saved ()
  "Only save the main session."
  (when (and (fboundp 'easysession-get-session-name)
             (string= "main" (funcall 'easysession-get-session-name)))
    t))
(setq easysession-save-mode-predicate 'my-easysession-only-main-saved)

(add-hook 'easysession-new-session-hook 'easysession-reset)

;;; Provide

(provide 'config)

;; Local variables:
;; byte-compile-warnings: (not lexical free-vars)
;; End:

;;; config.el ends here

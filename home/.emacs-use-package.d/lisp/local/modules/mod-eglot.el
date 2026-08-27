;;; mod-eglot.el --- mod-eglot -*- lexical-binding: t -*-

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

;;; Code:

;;; Require

(eval-and-compile
  (require 'lightemacs-use-package))
(require 'my-defun)

;;; Defaults

;; 0.5 seconds (The Default)
;; ---------------------------
;; Best for: Developers who rely heavily on instantaneous autocompletion, live
;; error squiggles, and real-time hover documentation.
;;
;; When to use: If your language server is incredibly fast (e.g., gopls or
;; rust-analyzer), your hardware is modern, and you don't experience any
;; stuttering while typing.
;;
;; 0.75 to 1.0 seconds (The Sweet Spot)
;; ----------------------------------------
;; Best for: Fast, bursty typists who want to eliminate micro-stutters without
;; sacrificing too much responsiveness.
;;
;; When to use: This is often the recommended middle ground. It gives you enough
;; time to finish typing a variable name or method call before Emacs fires the
;; textDocument/didChange payload to the server. It drastically cuts down on
;; the server trying to parse half-finished, invalid syntax trees while you are
;; still typing.
;;
;; 2.0 seconds or higher (High Performance/Low Overhead)
;; -------------------------------------------------------
;; Best for: Developers working on massive, monolithic codebases, or using
;; extremely resource-heavy language servers (like clangd on large projects).
;;
;; When to use: If your editor constantly freezes while typing, or if you notice
;; your laptop fans spinning up just from regular editing. At this value, Eglot
;; acts more like a "pause to check my work" tool rather than a real-time
;; linter.
(setq eglot-send-changes-idle-time 0.25)

;; ## Disable Automatic Code Action Probing
;; By default, Eglot asynchronously polls the language server for available code
;; actions (`:textDocument/codeAction`) whenever the cursor idles. This
;; mechanism is responsible for displaying visual indicators, such as a
;; lightbulb, in the editor's fringe or margin.
;;
;; Disabling these automatic indications significantly reduces continuous
;; process communication and background overhead during routine editing.
;;
;; Note: Disabling this feature only removes the automatic UI indicators. You
;; retain the ability to manually request and execute code actions at any time
;; by invoking `M-x eglot-code-actions`.
(setq eglot-code-action-indications nil)

(with-eval-after-load 'jsonrpc
  (when (fboundp 'jsonrpc--log-event)
    (fset 'jsonrpc--log-event #'ignore))
  (remove-hook 'jsonrpc-event-hook 'jsonrpc--log-event))

(setq eglot-server-programs
      ;; FIXME: Maybe this info should be distributed into the major modes
      ;; themselves where they could set a buffer-local `eglot-server-program'
      ;; which would allow deprecating this database.
      ;; FIXME: With `derived-mode-add-parents' in Emacs≥30, some of
      ;; those entries can be simplified, but we keep them for when
      ;; `eglot.el' is installed via GNU ELPA in an older Emacs.
      `(((python-mode python-ts-mode) . ("pylsp"))))

;; Allow edits without confirmation
(setq eglot-confirm-server-edits nil)

(setq eglot-watch-files-outside-project-root nil)
(setq eglot-stay-out-of '(yasnippet company))
(setq eglot-connect-timeout 40)
(setq eglot-max-file-watches 5000)

;;; Disable capabilities

(setq eglot-ignored-server-capabilities
      '(;; Delegating code formatting to dedicated tools such as Apheleia can
        ;; separate formatting from the language server and provide asynchronous
        ;; formatting workflows. The tradeoff is additional formatter
        ;; configuration and the loss of the language server's built-in
        ;; formatting:
        :documentOnTypeFormattingProvider

        ;; Inlay hints (eglot-inlay-hints-mode) insert automatically determined
        ;; types and parameter names directly into the buffer. Disabling them
        ;; removes the associated visual annotations and any work required to
        ;; maintain them:
        :inlayHintProvider

        ;; When the cursor rests on a symbol, the LSP server can highlight other
        ;; occurrences of that symbol (eglot-highlight-eldoc-function).
        ;; Disabling it will stop Eglot from highlighting other occurrences of
        ;; the symbol under the cursor:
        :documentHighlightProvider

        ;; eldoc
        ;; Sends textDocument/hover to fetch documentation for the symbol at
        ;; point. Eglot parses the returned Markdown and renders it for the echo
        ;; area or ElDoc buffer.
        :hoverProvider

        ;; eldoc
        ;; Sends textDocument/codeAction strictly to check if actions are
        ;; available (like refactors or quickfixes). Eglot uses this to draw the
        ;; lightbulb icon in the mode-line, fringes, or margins.
        :codeActionProvider

        ;; Eglot implements this via eglot-semantic-tokens-mode, which disables
        ;; Emacs's default regex-based font-lock and delegates syntax
        ;; highlighting to the LSP server. When the buffer changes, Eglot issues
        ;; a textDocument/semanticTokens/full/delta request.
        ;;
        ;; The server responds with large arrays of integers representing token
        ;; coordinates. In eglot--semtok-font-lock-1, Eglot iterates through
        ;; this array (for i from 0 below (length data) by 5). For every token,
        ;; it must convert LSP line/character coordinates to Emacs buffer
        ;; positions using eglot-move-to-linepos-function (which calculates
        ;; UTF-16 code units), and then apply eglot--semtok-names and
        ;; eglot--semtok-faces text properties. This creates significant Elisp
        ;; execution overhead, garbage collection pressure from heavy JSON
        ;; payloads, and blocking UI work during scrolling and editing.
        ;; :semanticTokensProvider

        ;; eldoc
        ;; Sends textDocument/signatureHelp to parse the surrounding function
        ;; call and identify the active parameter.
        ;; :signatureHelpProvider

        ;; Flymake
        ;; If the server supports "pull" diagnostics, Eglot actively polls the
        ;; server (textDocument/diagnostic) whenever the Flymake idle timer
        ;; triggers (typically after 0.5 seconds of inactivity). Disabling it
        ;; stops Eglot from polling for diagnostics. (Note: Many servers also
        ;; push diagnostics asynchronously; disabling this only stops the
        ;; explicit pull requests).
        ;; :diagnosticProvider

        ;; Eglot itself does not call this in the background, but if you use an
        ;; external auto-completion framework (like company-mode or corfu), that
        ;; framework will invoke Eglot's completion backend on a very short idle
        ;; timer or as you type. This results in heavy textDocument/completion
        ;; traffic. Disabling it stops Eglot from participating in
        ;; auto-completion entirely.
        ;; :completionProvider
        ))

;;; Python

(let ((has-ruff (executable-find "ruff")))
  (setq-default eglot-workspace-configuration
                `(:pylsp (:plugins
                          (:ruff (;; Core
                                  :enabled ,(if has-ruff t :json-false)
                                  :lineLength 79
                                  :formatEnabled :json-false ; Use Apheleia

                                  ;; Rule Selection
                                  ;; By default, Ruff only checks 'E' and 'F'
                                  ;; rules. 'W' (warnings), and 'UP' (pyupgrade)
                                  ;;
                                  ;; NOTE: Removed "I" (false positives)
                                  :extendSelect ["W" "UP"]

                                  ;; UP035: Deprecation of imports from typing
                                  ;; (e.g., typing.List, typing.Dict).
                                  ;;
                                  ;; Why ignore:
                                  ;; - Maintains compatibility with codebases
                                  ;;   targeting Python < 3.9 where built-in
                                  ;;   collection types cannot be parameterized
                                  ;;   directly without from __future__ import
                                  ;;   annotations.
                                  ;; - Prevents multiple diagnostics from firing
                                  ;;   on the same import line (e.g., from
                                  ;;   typing import Dict, List), which causes
                                  ;;   overlapping Flymake overlays.
                                  ;;
                                  ;; When to remove:
                                  ;; - Remove once all target environments are
                                  ;;   on Python 3.9+ and codebases migrate to
                                  ;;   standard PEP 585 generics (e.g.,
                                  ;;   list[str], dict[str, int]).
                                  ;; :ignore ["UP035"]

                                  ;; Target your specific Python version
                                  ;; :targetVersion "py310"

                                  ;; File Management
                                  ;; Exclude specific files from being linted
                                  ;; :exclude ["__about__.py" "docs/"]

                                  ;; Advanced: Per-file ignores (Dictionary/Plist
                                  ;; translation) E.g., Ignore missing docstrings
                                  ;; (D100) in __init__.py
                                  ;; :perFileIgnores (:__init__.py ["D100"])

                                  ;; Advanced: Custom Severities
                                  ;; E.g., Make 'I' (isort) violations show as
                                  ;; Info instead of Warning
                                  ;; :severities (:I "I")

                                  ;; Code Actions
                                  ;; :unsafeFixes :json-false
                                  ;; :unfixable ["F401"]
                                  )

                                 ;; Syntax checkers
                                 :pylint (:enabled t)

                                 ;; Old, slow linters
                                 :mccabe (:enabled ,(if has-ruff :json-false t))
                                 :flake8 (:enabled ,(if has-ruff :json-false t))
                                 :pyflakes (;; pyflakes
                                            :enabled ,(if has-ruff :json-false t)
                                            :ignore ["W293"])
                                 :pycodestyle (;; This is also executed by flake8
                                               :enabled :json-false
                                               ;; :match "(?!test_).*\\.py"
                                               ;; :maxLineLength 79
                                               ;; :convention "pep257"
                                               ;; :ignore ["W293"]
                                               ;; :hangClosing :json-false
                                               )
                                 :pydocstyle (;; pydocstyle options
                                              :enabled ,(if has-ruff :json-false t)
                                              ;; :ignore ["W293"]
                                              ;; ,(if eglot-code-checker
                                              ;;      t
                                              ;;    :json-false)
                                              ;; string (one of: 'pep257',
                                              ;; 'numpy', 'google', None)
                                              ;; :convention "google"

                                              ;; 213: Multi-line docstring
                                              ;; summary should start in the
                                              ;; second line.
                                              ;;
                                              ;; 202: no blank lines allowed
                                              ;; after function docstring.
                                              :ignore ["W213" "W202"])

                                 ;; Disable old formatters (Handled by Apheleia)
                                 :yapf (:enabled :json-false)
                                 :isort (:enabled ,(if has-ruff :json-false t))
                                 :autopep8 (:enabled ,(if has-ruff :json-false t))

                                 :jedi_completion
                                 (:enabled t
                                           ;; Controls whether Jedi (the
                                           ;; autocompletion engine used by pylsp)
                                           ;; automatically imports certain
                                           ;; modules to provide better
                                           ;; autocompletion.
                                           ;; NOTE: Removed just to test
                                           ;; :auto_import_modules ["os"
                                           ;;                       "re"
                                           ;;                       "sys"
                                           ;;                       "subprocess"
                                           ;;                       "pathlib"
                                           ;;                       "logging"
                                           ;;                       "shlex"
                                           ;;                       "typing"]

                                           ;; Resolve documentation and detail
                                           ;; eagerly.
                                           :eager :json-false

                                           :include_class_objects :json-false
                                           :include_function_objects :json-false
                                           :include_params :json-false

                                           ;; How many labels and snippets (at most)
                                           ;; should be resolved?
                                           ;; :resolve_at_most 40
                                           )

                                 ;; NOTE: Removed because it causes on Arch: Debugger
                                 ;; entered--Lisp error: (wrong-type-argument plistp [])
                                 ;;
                                 ;; Enables or disables the preloading of
                                 ;; specified Python modules when the language
                                 ;; server starts. When enabled, the preload
                                 ;; plugin loads specified modules at the start of
                                 ;; the language server session, making them
                                 ;; readily available in memory. This is intended
                                 ;; to speed up language server operations, like
                                 ;; autocompletion or code analysis, by reducing
                                 ;; the need to load these modules on demand.
                                 ;; :preload ( :enabled t
                                 ;;            :modules ["os"
                                 ;;                      "re"
                                 ;;                      "sys"
                                 ;;                      "subprocess"
                                 ;;                      "pathlib"])

                                 :rope_autoimport (:enabled :json-false))))))

;; (setq-default eglot-workspace-configuration
;;               `(:pylsp (:plugins
;;                         (; Improve syntax
;;
;;                          :ruff (;; Core
;;                                 :enabled t
;;                                 ;; :formatEnabled :json-false ; Use Apheleia
;;                                 :lineLength 79
;;
;;                                 ;; Rule Selection
;;                                 ;; By default, Ruff only checks 'E' and 'F'
;;                                 ;; rules.
;;                                 ;; Let's add 'I' (isort), 'W' (warnings), and
;;                                 ;; 'UP' (pyupgrade)
;;                                 ;;
;;                                 ;; NOTE: Removed "I" (false positives)
;;                                 :extendSelect ["W" "UP"]
;;
;;                                 ;; UP035: Deprecation of imports from typing
;;                                 ;; (e.g., typing.List, typing.Dict).
;;                                 ;;
;;                                 ;; Why ignore:
;;                                 ;; - Maintains compatibility with codebases
;;                                 ;;   targeting Python < 3.9 where built-in
;;                                 ;;   collection types cannot be parameterized
;;                                 ;;   directly without from __future__ import
;;                                 ;;   annotations.
;;                                 ;; - Prevents multiple diagnostics from firing
;;                                 ;;   on the same import line (e.g., from typing
;;                                 ;;   import Dict, List), which causes
;;                                 ;;   overlapping Flymake overlays.
;;                                 ;;
;;                                 ;; When to remove:
;;                                 ;; - Remove once all target environments are on
;;                                 ;;   Python 3.9+ and codebases migrate to
;;                                 ;;   standard PEP 585 generics (e.g.,
;;                                 ;;   list[str], dict[str, int]).
;;                                 :ignore ["UP035"]
;;
;;                                 ;; Target your specific Python version
;;                                 ;; :targetVersion "py310"
;;
;;                                 ;; File Management
;;                                 ;; Exclude specific files from being linted
;;                                 ;; :exclude ["__about__.py" "docs/"]
;;
;;                                 ;; Advanced: Per-file ignores (Dictionary/Plist
;;                                 ;; translation) E.g., Ignore missing docstrings
;;                                 ;; (D100) in __init__.py
;;                                 ;; :perFileIgnores (:__init__.py ["D100"])
;;
;;                                 ;; Advanced: Custom Severities
;;                                 ;; E.g., Make 'I' (isort) violations show as
;;                                 ;; Info instead of Warning
;;                                 ;; :severities (:I "I")
;;
;;                                 ;; Code Actions
;;                                 ;; :unsafeFixes :json-false
;;                                 ;; :unfixable ["F401"]
;;                                 )
;;
;;                          ;; Syntax checkers
;;                          :pylint (:enabled t)
;;
;;                          ;; Old, slow linters
;;                          :mccabe (:enabled :json-false)
;;                          :flake8 (:enabled :json-false)
;;                          :pyflakes (:enabled :json-false :ignore ["W293"])
;;                          :pycodestyle (;; This is also executed by flake8
;;                                        :enabled :json-false
;;                                        ;; :match "(?!test_).*\\.py"
;;                                        ;; :maxLineLength 79
;;                                        ;; :convention "pep257"
;;                                        ;; :ignore ["W293"]
;;                                        ;; :hangClosing :json-false
;;                                        )
;;                          :pydocstyle (:enabled :json-false
;;                                                ;; :ignore ["W293"]
;;                                                ;; ,(if eglot-code-checker
;;                                                ;;      t
;;                                                ;;    :json-false)
;;                                                ;; string (one of: 'pep257',
;;                                                ;; 'numpy', 'google', None)
;;                                                ;; :convention "google"
;;
;;                                                ;; 213: Multi-line docstring
;;                                                ;; summary should start in the
;;                                                ;; second line.
;;                                                ;;
;;                                                ;; 202: no blank lines allowed
;;                                                ;; after function docstring.
;;                                                :ignore ["W213",
;;                                                         "W202"])
;;
;;                          ;; Disable old formatters (Handled by Apheleia)
;;                          :yapf (:enabled :json-false)
;;                          :isort (:enabled :json-false)
;;                          :autopep8 (:enabled :json-false)
;;
;;                          :jedi_completion
;;                          (:enabled t
;;                                    ;; Controls whether Jedi (the
;;                                    ;; autocompletion engine used by pylsp)
;;                                    ;; automatically imports certain
;;                                    ;; modules to provide better
;;                                    ;; autocompletion.
;;                                    ;; NOTE: Removed just to test
;;                                    ;; :auto_import_modules ["os"
;;                                    ;;                       "re"
;;                                    ;;                       "sys"
;;                                    ;;                       "subprocess"
;;                                    ;;                       "pathlib"
;;                                    ;;                       "logging"
;;                                    ;;                       "shlex"
;;                                    ;;                       "typing"]
;;
;;                                    ;; Resolve documentation and detail
;;                                    ;; eagerly.
;;                                    :eager :json-false
;;
;;                                    :include_class_objects :json-false
;;                                    :include_function_objects :json-false
;;                                    :include_params :json-false
;;
;;                                    ;; How many labels and snippets (at most)
;;                                    ;; should be resolved?
;;                                    ;; :resolve_at_most 40
;;                                    )
;;
;;                          ;; NOTE: Removed because it causes on Arch: Debugger
;;                          ;; entered--Lisp error: (wrong-type-argument plistp [])
;;                          ;;
;;                          ;; Enables or disables the preloading of
;;                          ;; specified Python modules when the language
;;                          ;; server starts. When enabled, the preload
;;                          ;; plugin loads specified modules at the start of
;;                          ;; the language server session, making them
;;                          ;; readily available in memory. This is intended
;;                          ;; to speed up language server operations, like
;;                          ;; autocompletion or code analysis, by reducing
;;                          ;; the need to load these modules on demand.
;;                          ;; :preload ( :enabled t
;;                          ;;            :modules ["os"
;;                          ;;                      "re"
;;                          ;;                      "sys"
;;                          ;;                      "subprocess"
;;                          ;;                      "pathlib"])
;;
;;                          :rope_autoimport (:enabled :json-false)))))

;;; Eglot use-package

(lightemacs-use-package eglot
  :ensure nil
  :commands (eglot
             eglot-rename
             eglot-managed-p
             eglot-format
             eglot-ensure
             eglot-rename
             eglot-format-buffer)

  :config
  ;; Remove eglot from the modeline
  (setq mode-line-misc-info
        (assq-delete-all 'eglot--managed-mode mode-line-misc-info))

  :preface
  (defun my-eglot-format-buffer ()
    "Eglot format buffer."
    (interactive)
    (when (and (fboundp 'eglot-managed-p)
               (eglot-managed-p)
               (fboundp 'eglot-format-buffer))
      (let ((inhibit-message t))
        (eglot-format-buffer)))))

;;; Eglot: Python

;;; Eglot: quiet

(defun my-eglot--message-filter (orig-fun format &rest args)
  "Suppress selected Eglot messages from the minibuffer.
ORIG-FUN is the original `eglot--message` function.
FORMAT and ARGS are the message format string and its arguments."
  (let ((message-string (apply #'format format args)))
    (let ((inhibit-message (or (string-prefix-p "Connected" message-string)
                               (string-prefix-p "Waiting" message-string)
                               (string-prefix-p "Reconnected" message-string))))
      (apply orig-fun format args)))
  ;; (let ((message-string (apply #'format format args)))
  ;;   (unless (or (string-prefix-p "Connected" message-string)
  ;;               (string-prefix-p "Waiting" message-string)
  ;;               (string-prefix-p "Reconnected" message-string))
  ;;     (apply orig-fun format args)))
  )

(advice-add 'eglot--message :around #'my-eglot--message-filter)

;;; Python: remove flymake

;; Debugger entered--Lisp error: (error "Can't find state for python-flymake in 'flymake--state'")
;; error("Can't find state for %s in `flymake--state'" python-flymake)
;; flymake--handle-report(python-flymake backend-token7 nil)
;; apply(flymake--handle-report python-flymake backend-token7 nil)
;; #f(compiled-function (&rest args) #<bytecode 0xc7a15d2072c6e28>)(nil)
;; python--flymake-parse-output(#<buffer allowed_paths.py> #<process python-flymake> #f(compiled-function (&rest args) #<bytecode 0xc7a15d2072c6e28>))
;; #f(compiled-function (proc event) #<bytecode 0xe03bcdda4f319e9>)(#<process python-flymake> "finished\n")
(defun my-remove-python-flymake ()
  "Remove `python-flymake' from `flymake-diagnostic-functions'."
  (remove-hook 'flymake-diagnostic-functions 'python-flymake t))

(add-hook 'python-mode-hook #'my-remove-python-flymake)
(add-hook 'python-ts-mode-hook #'my-remove-python-flymake)

(with-eval-after-load 'python
  ;; Remove python-flymake error: "Cannot find suitable checker" when a Python
  ;; script is loaded before eglot and the checker isn't found
  (advice-add 'python-flymake :override #'ignore))

;;; Cape

;; Configure buffer-local completion backend settings when Eglot manages a
;; buffer.
;; 1. Unhooks `python-completion-at-point' to avoid conflicts with LSP
;; completion.
;; 2. Replaces standard `eglot-completion-at-point' with a cache-busted variant
;; via `cape-capf-buster'. This forces Corfu/Emacs to fetch updated completion
;; candidates from the language server on each keypress rather than reusing
;; cached results.

(defun my-eglot-capf-cleanup ()
  "Prioritize Eglot and remove conflicting Python completions."
  (when (eglot-managed-p)
    ;; Remove legacy python-completion-at-point from the local list
    (remove-hook 'completion-at-point-functions 'python-completion-at-point t)

    (if (fboundp 'cape-capf-buster)
        (progn
          ;; Remove the default Eglot CAPF
          (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t)
          ;; Ensure Eglot is at the front and wrapped for cache-busting
          (add-hook 'completion-at-point-functions
                    (cape-capf-buster 'eglot-completion-at-point)
                    nil t))
      (error "Undefined: cape-capf-buster"))))

(add-hook 'eglot-managed-mode-hook #'my-eglot-capf-cleanup)

;;; Provide

(provide 'mod-eglot)

;; Local variables:
;; byte-compile-warnings: (not lexical free-vars)
;; env-allow-syntax-checker-package-lint: nil
;; End:

;;; mod-eglot.el ends here

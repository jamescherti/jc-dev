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

(with-eval-after-load 'jsonrpc
  (defun jsonrpc--log-event (&rest _)))

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
      '(;; Formatting (Handled by external tools like Apheleia)
        :documentFormattingProvider
        :documentRangeFormattingProvider
        :documentOnTypeFormattingProvider

        ;; NOTE: UI noise and performance degradation
        ;; Disable inlay hints (e.g. inferred types, parameter names) Inlay
        ;; hints are small, non-intrusive annotations inserted into the code
        ;; by the LSP server. They provide helpful context such as inferred
        ;; variable types, function return types, or parameter names in
        ;; function calls, especially in languages like TypeScript or Rust.
        ;; These hints do not change the actual source code but are visually
        ;; rendered in the editor. Disabling this prevents the display of such
        ;; annotations in the buffer.
        :inlayHintProvider

        ;; NOTE: UI noise and performance degradation
        ;; Disables highlighting other instances of the symbol at point in the
        ;; current buffer (e.g., all usages of a variable are no longer
        ;; visually highlighted). Usage: This affects the automatic
        ;; highlighting when the cursor is on a symbol. Normally, all
        ;; occurrences of that symbol in the buffer are highlighted. Disabling
        ;; this stops that behavior.
        :documentHighlightProvider

        ;; NOTE: UI noise and performance degradation
        ;; Disables inline annotations like test coverage, reference counts,
        ;; or result indicators that appear above/below code lines.
        :codeLensProvider

        ;; NOTE: UI noise and performance degradation
        ;; Disable detection and interaction with links in documents
        :documentLinkProvider

        ;; NOTE: UI noise and performance degradation
        ;; Disables rendering of inline color swatches next to color values in
        ;; code (e.g., "#ff0000" showing a red box).
        :colorProvider

        ;; :hoverProvider  ; For showing the definition and documentation.
        ;; :completionProvider  ; Completion. DO NOT DISABLE IT.

        ;; :signatureHelpProvider  ; For showing the function signature/arguments.

        ;; Disable "go to definition" feature
        ;; :definitionProvider

        ;; Disable support for "go to type definition"
        ;; :typeDefinitionProvider

        ;; Disable support for finding implementation locations This
        ;; capability allows the LSP client (like Eglot) to query the server
        ;; for the actual implementation(s) of an interface, abstract method,
        ;; or symbol. For example, if the cursor is on a function declaration
        ;; or interface, this enables jumping directly to the concrete
        ;; implementation(s). Disabling it will prevent Eglot from offering
        ;; this navigation feature.
        ;; :implementationProvider

        ;; Disables ability to jump to a symbol's declaration location (e.g.,
        ;; jumping to where a variable was declared).
        ;; :declarationProvider

        ;; Disables showing all references to the symbol at point (e.g., all
        ;; usages of a function or variable in the project).
        ;; :referencesProvider

        ;; Disables the document-wide symbol tree view used for navigation or
        ;; structural outline (e.g., class and function tree in sidebar).
        ;; Usage: This impacts commands or UI elements that display a tree or
        ;; list of all symbols (functions, classes, variables) in the current
        ;; buffer. Disabling this removes that outline view.
        ;;
        ;; TODO: :documentSymbolProvider: Enable this if you use imenu. It
        ;; populates the buffer's index of classes, methods, and functions,
        ;; allowing for rapid structural navigation.
        ;; :documentSymbolProvider

        ;; Disables workspace-wide symbol search (e.g., `M-x
        ;; xref-find-apropos` or project-wide function/class name search).
        ;;
        ;; TODO: :workspaceSymbolProvider: Enable this if you want
        ;; project-wide navigation. It feeds xref-find-apropos, letting you
        ;; search for symbols across the entire repository.
        ;; :workspaceSymbolProvider

        ;; Usage: This prevents displaying available quick fixes or
        ;; refactorings that normally appear as code actions or lightbulb
        ;; hints in the editor. Disabling this means you won't get automatic
        ;; fix suggestions from the server.
        ;;
        ;; TODO: :codeActionProvider: You should definitely enable this. It
        ;; provides the quick fixes, such as organizing imports or fixing lint
        ;; errors. Without it, you lose a significant portion of Ruff's
        ;; utility.
        ;; :codeActionProvider

        ;; Disable rename symbol functionality
        ;; :renameProvider

        ;; Disables visual fold range markers (e.g., foldable region
        ;; indicators in the fringe or gutter).
        ;; :foldingRangeProvider

        ;; Disables execution of commands exposed by the server (e.g., special
        ;; refactoring or custom commands via `M-x eglot-execute-command`).
        ;;
        ;; TODO: :executeCommandProvider: Enable this alongside code actions.
        ;; Many LSP servers require command execution to apply the code
        ;; actions they suggest.
        ;; :executeCommandProvider
        ))

;;; Python

(setq-default eglot-workspace-configuration
              `(:pylsp (:plugins
                        (; Improve syntax

                         :ruff (;; Core
                                :enabled t
                                ;; :formatEnabled :json-false ; Use Apheleia
                                :lineLength 79

                                ;; Rule Selection
                                ;; By default, Ruff only checks 'E' and 'F'
                                ;; rules.
                                ;; Let's add 'I' (isort), 'W' (warnings), and
                                ;; 'UP' (pyupgrade)
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
                                ;;   on the same import line (e.g., from typing
                                ;;   import Dict, List), which causes
                                ;;   overlapping Flymake overlays.
                                ;;
                                ;; When to remove:
                                ;; - Remove once all target environments are on
                                ;;   Python 3.9+ and codebases migrate to
                                ;;   standard PEP 585 generics (e.g.,
                                ;;   list[str], dict[str, int]).
                                :ignore ["UP035"]

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
                         :mccabe (:enabled :json-false)
                         :flake8 (:enabled :json-false)
                         :pyflakes (:enabled :json-false :ignore ["W293"])
                         :pycodestyle (;; This is also executed by flake8
                                       :enabled :json-false
                                       ;; :match "(?!test_).*\\.py"
                                       ;; :maxLineLength 79
                                       ;; :convention "pep257"
                                       ;; :ignore ["W293"]
                                       ;; :hangClosing :json-false
                                       )
                         :pydocstyle (:enabled :json-false
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
                                               :ignore ["W213",
                                                        "W202"])

                         ;; Disable old formatters (Handled by Apheleia)
                         :yapf (:enabled :json-false)
                         :isort (:enabled :json-false)
                         :autopep8 (:enabled :json-false)

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

                         :rope_autoimport (:enabled :json-false)))))

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

  ;; :config
  ;; Remove eglot from the modeline
  ;; (setq mode-line-misc-info
  ;;       (assq-delete-all 'eglot--managed-mode mode-line-misc-info))

  ;; (advice-add 'eglot--message :around
  ;;             (lambda(orig-fun format &rest args)
  ;;               ;; This code provides an Emacs Lisp function to suppress
  ;;               ;; specific Eglot messages from being shown in the minibuffer.
  ;;               ;; "Suppress specific eglot messages from being shown in the
  ;;               ;; minibuffer."
  ;;               (let ((message-string (apply #'format format args)))
  ;;                 (unless (or (string-prefix-p "Connected" message-string)
  ;;                             (string-prefix-p "Waiting" message-string)
  ;;                             (string-prefix-p "Reconnected" message-string))
  ;;                   (apply orig-fun format args)))))

  ;; :preface
  ;; (defun my-eglot-format-buffer ()
  ;;   "Eglot format buffer."
  ;;   (when (and (fboundp 'eglot-managed-p)
  ;;              (eglot-managed-p)
  ;;              (fboundp 'eglot-format-buffer))
  ;;     (let ((inhibit-message t))
  ;;       (eglot-format-buffer))))
  )

;;; Eglot: Python

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

;;; mod-eglot.el --- mod-eglot -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/jc-dev
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;; Require

(eval-and-compile
  (require 'lightemacs-use-package))
(require 'my-defun)

;;; use-package eglot

(lightemacs-use-package eglot
  :ensure nil
  :commands (eglot
             eglot-managed-p
             eglot-format
             eglot-ensure
             eglot-format-buffer))

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
(setq eglot-send-changes-idle-time 0.5)

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
      `(((python-mode python-ts-mode) . ("pylsp"))))

;; Allow edits without confirmation
(setq eglot-confirm-server-edits nil)

(setq eglot-watch-files-outside-project-root nil)
(setq eglot-stay-out-of '(yasnippet company))
(setq eglot-connect-timeout 40)
(setq eglot-max-file-watches 5000)

;;; Python Eglot: Global Configuration

;; Use ruff when it is available because it is fast (written in Rust). When ruff
;; is not available, fall back to flake8 and its individual underlying tools.
;;
;; URL: https://www.jamescherti.com/emacs-python-dev-using-eglot-pylsp-ruff-pylint-flake8/
;;
;; To sup up:
;; - When ruff is available: Ruff, and Pylint.
;; - When Ruff is not available: Flake8, isort, and Pylint.
;;
;; Documentation:
;; https://github.com/python-lsp/python-lsp-server/blob/develop/CONFIGURATION.md
;; https://github.com/python-lsp/python-lsp-ruff
;; https://github.com/chantera/python-lsp-isort

(defun my-python-eglot-config (&optional _server)
  "Return pylsp workspace settings for the current Eglot project.

Prefer Ruff for linting and formatting.  Fall back to Flake8 for
linting when available, and to autopep8 for formatting.  Tool
discovery assumes that Emacs and pylsp use the same environment.
Ruff integration requires python-lsp-ruff in that environment."
  (let* ((remote (file-remote-p default-directory))
         (has-ruff (executable-find "ruff" remote))
         (has-flake8 (and (not has-ruff) (executable-find "flake8" remote)))
         (ruff-enabled (if has-ruff t :json-false))
         (fallback-lint (if (or has-ruff has-flake8) :json-false t))
         (fallback-format (if has-ruff :json-false t)))
    `(:pylsp
      (;; Formatter to use for reformatting signatures in docstrings.
       :signature
       (;; Signature
        ;; :formatter can be "ruff", "black", or null.
        :formatter ,(if has-ruff "ruff" "black")
        :include_docstring t
        :line_length 88)
       :plugins
       (;; Plugin: https://github.com/python-lsp/python-lsp-ruff
        :ruff (;; Ruff configuration
               :enabled ,ruff-enabled
               :formatEnabled ,ruff-enabled

               ;; Add 'W' (pycodestyle warnings), 'UP' (pyupgrade),
               ;; and 'D' (pydocstyle).
               ;; :extendSelect ["W" "UP" "D"]

               ;; I: Sort imports. Combine all selected rules in one
               ;; :extendSelect vector when enabling several groups.
               ;; :extendSelect ["I"]
               ;; :format ["I"]

               ;; Ignore specific rules
               ;;   D213: Multi-line docstring summary should start on
               ;;         the second line.
               ;;   D202: No blank lines allowed after function
               ;;         docstring.
               ;; :ignore ["D213" "D202"]
               )

        ;; Pylint is disabled independently of Ruff and Flake8.
        :pylint (:enabled :json-false)

        :pylsp_mypy
        (;; Type checking (Requires: pylsp-mypy)
         :enabled t
         ;; Disabling live mode prevents type-checking updates on
         ;; every edit. Save the file to refresh mypy diagnostics.
         :live_mode :json-false)

        ;; Flake8 is a wrapper tool that bundles pyflakes,
        ;; pycodestyle, and mccabe.
        :flake8 (:enabled ,(if has-flake8 t :json-false))

        ;; Flake8 runs these checkers. Ruff implements overlapping rules.
        ;; Disable the standalone pylsp checkers when either is active.
        ;; Rule coverage depends on the selected Ruff or Flake8 rules.
        :mccabe (:enabled ,fallback-lint)
        :pyflakes (;; pyflakes catches logical errors
                   ;; (unused imports, undefined names...)
                   :enabled ,fallback-lint)

        :pycodestyle (;; pycodestyle checks style and PEP 8 conventions
                      :enabled ,fallback-lint

                      ;; Ignore specific rules
                      ;; :ignore ["W293"]
                      )

        :pydocstyle (;; pydocstyle enforces PEP 257 docstring conventions.
                     ;; Select Ruff's D rules or install flake8-docstrings
                     ;; for equivalent checks when this plugin is disabled.
                     ;; https://github.com/pycqa/flake8-docstrings
                     :enabled ,fallback-lint

                     ;; Ignore specific rules
                     ;;   D213: Multi-line docstring summary should start on
                     ;;         the second line.
                     ;;   D202: No blank lines allowed after function
                     ;;         docstring.
                     ;; :ignore ["D213" "D202"]
                     )

        ;; Formatting: isort
        ;; https://github.com/chantera/python-lsp-isort
        :isort (:enabled ,fallback-format)

        ;; Formatting: autopep8
        ;; https://github.com/python-lsp/python-lsp-server
        :autopep8 (:enabled ,fallback-format)
        :yapf (:enabled :json-false)

        ;; Code completion
        :jedi_completion (;; Jedi configuration
                          :enabled t

                          ;; Resolving documentation details eagerly
                          ;; :eager t

                          ;; Add class objects as a separate completion item
                          :include_class_objects t

                          ;; Add function objects as a separate completion item
                          :include_function_objects t

                          ;; Include function parameters in completion snippets.
                          :include_params t

                          ;; Fuzzy matching for typos/abbreviations
                          ;; :fuzzy t

                          ;; Modules for which labels and snippets should be cached.
                          ;; :cache_for ["pandas", "numpy", "tensorflow", "matplotlib"]

                          ;; Maximum number of completion items to resolve eagerly.
                          ;; :resolve_at_most 25
                          ))))))

(with-eval-after-load 'eglot
  (setq-default eglot-workspace-configuration #'my-python-eglot-config))

;;; Remove eglot from the modeline

;; Remove eglot from the modeline
(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (assq-delete-all 'eglot--managed-mode mode-line-misc-info)))

;;; Function: my-eglot-format-buffer

(defun my-eglot-format-buffer ()
  "Eglot format buffer."
  (interactive)
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p)
             (fboundp 'eglot-format-buffer))
    (let ((inhibit-message t))
      (eglot-format-buffer))))

;;; Eglot: Code Actions: Auto Code Actions (fixes)

(defun my-eglot-fix-all-and-organize-imports-before-save ()
  "Apply available LSP fix-all and import-organization actions before saving."
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p)
             (fboundp 'eglot-server-capable)
             (eglot-server-capable :codeActionProvider)
             (fboundp 'eglot-current-server)
             (fboundp 'eglot-code-actions)
             (fboundp 'eglot-execute))
    (when-let* ((server (eglot-current-server)))
      (save-restriction
        (widen)
        (dolist (action-kind '("source.fixAll" "source.organizeImports"))
          (dolist (action (eglot-code-actions (point-min) (point-max)
                                              action-kind nil))
            (eglot-execute server action)))))))

(defun my-setup-eglot-fix-on-save ()
  "Enable automatic LSP fixes in buffers managed by Eglot.

Add `my-eglot-fix-all-and-organize-imports-before-save' to the buffer-local
`before-save-hook' when Eglot manages the current buffer. Otherwise, remove it."
  (if (and (fboundp 'eglot-managed-p)
           (eglot-managed-p))
      (add-hook 'before-save-hook
                #'my-eglot-fix-all-and-organize-imports-before-save -20 t)
    (remove-hook 'before-save-hook
                 #'my-eglot-fix-all-and-organize-imports-before-save t)))

(add-hook 'eglot-managed-mode-hook #'my-setup-eglot-fix-on-save)

;;; Eglot: Format

;; Removed:
;; Sort imports (matches "source.organizeImports.ruff")
;; (dolist (action (eglot-code-actions (point-min) (point-max)
;;                                     "source.organizeImports" nil))
;;   (eglot-execute server action))

;; (add-hook 'before-save-hook #'my-eglot-ruff-fix-and-format-before-save)

;; Description: Configure Emacs Eglot to auto format code on save.
;; Author: James Cherti
;; License: MIT
;; URL: https://www.jamescherti.com/emacs-eglot-auto-format-buffer-on-save/

(defun my-eglot-format-buffer-safely ()
  "Run formatting safely to avoid blocking saves."
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p)
             (fboundp 'eglot-server-capable)
             (eglot-server-capable :documentFormattingProvider)
             (fboundp 'eglot-format-buffer))
    (ignore-errors
      (eglot-format-buffer))))

(defun my-setup-eglot-format-on-save ()
  "Enable format-on-save for Eglot managed buffers.

Add `eglot-format-buffer' to the buffer-local `before-save-hook' when Eglot
manages the current buffer. Otherwise, remove it. Run formatting at depth -10,
before hooks at the default depth."
  (if (and (fboundp 'eglot-managed-p)
           (eglot-managed-p))
      (add-hook 'before-save-hook #'my-eglot-format-buffer-safely -10 t)
    (remove-hook 'before-save-hook #'my-eglot-format-buffer-safely t)))

(add-hook 'eglot-managed-mode-hook #'my-setup-eglot-format-on-save)

;;; Eglot: quiet

;; TODO Lightemacs?
(defun my-eglot--message-filter (orig-fun format &rest args)
  "Suppress selected Eglot messages from the minibuffer.
ORIG-FUN is the original `eglot--message` function.
FORMAT and ARGS are the message format string and its arguments."
  (let ((message-string (apply #'format format args)))
    (let ((inhibit-message (or (string-prefix-p "Connected" message-string)
                               (string-prefix-p "Waiting" message-string)
                               (string-prefix-p "Reconnected" message-string)
                               (string-suffix-p "politely to terminate" message-string))))
      (apply orig-fun format args))))

(advice-add 'eglot--message :around #'my-eglot--message-filter)

;;; Cape

;; Configure buffer-local completion backend settings when Eglot manages a
;; buffer.
;;
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

;;; Disable Eglot capabilities

(setq eglot-ignored-server-capabilities
      '(;; Delegating code formatting to dedicated tools such as Apheleia can
        ;; separate formatting from the language server and provide asynchronous
        ;; formatting workflows. The tradeoff is additional formatter
        ;; configuration and the loss of the language server's built-in
        ;; formatting:
        ;; :documentOnTypeFormattingProvider

        ;; Inlay hints (eglot-inlay-hints-mode) insert automatically determined
        ;; types and parameter names directly into the buffer. Disabling them
        ;; removes the associated visual annotations and any work required to
        ;; maintain them:
        ;; :inlayHintProvider

        ;; When the cursor rests on a symbol, the LSP server can highlight other
        ;; occurrences of that symbol (eglot-highlight-eldoc-function).
        ;; Disabling it will stop Eglot from highlighting other occurrences of
        ;; the symbol under the cursor:
        :documentHighlightProvider

        ;; eldoc
        ;; Sends textDocument/hover to fetch documentation for the symbol at
        ;; point. Eglot parses the returned Markdown and renders it for the echo
        ;; area or ElDoc buffer.
        ;; :hoverProvider

        ;; eldoc
        ;; Sends textDocument/codeAction strictly to check if actions are
        ;; available (like refactors or quickfixes). Eglot uses this to draw the
        ;; lightbulb icon in the mode-line, fringes, or margins.
        ;; :codeActionProvider

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

;;; DISABLED: Python: Remove flymake

;; Debugger entered--Lisp error: (error "Can't find state for python-flymake in 'flymake--state'")
;; error("Can't find state for %s in `flymake--state'" python-flymake)
;; flymake--handle-report(python-flymake backend-token7 nil)
;; apply(flymake--handle-report python-flymake backend-token7 nil)
;; #f(compiled-function (&rest args) #<bytecode 0xc7a15d2072c6e28>)(nil)
;; python--flymake-parse-output(#<buffer allowed_paths.py> #<process python-flymake> #f(compiled-function (&rest args) #<bytecode 0xc7a15d2072c6e28>))
;; #f(compiled-function (proc event) #<bytecode 0xe03bcdda4f319e9>)(#<process python-flymake> "finished\n")

;; (defun my-remove-python-flymake ()
;;   "Remove `python-flymake' from `flymake-diagnostic-functions'."
;;   (remove-hook 'flymake-diagnostic-functions 'python-flymake t))
;;
;; (add-hook 'python-mode-hook #'my-remove-python-flymake)

;; (with-eval-after-load 'python
;;   ;; Remove python-flymake error: "Cannot find suitable checker" when a Python
;;   ;; script is loaded before eglot and the checker isn't found
;;   (advice-add 'python-flymake :override #'ignore))

;;; Provide

(provide 'mod-eglot)

;; Local variables:
;; byte-compile-warnings: (not lexical free-vars)
;; env-allow-syntax-checker-package-lint: nil
;; End:

;;; mod-eglot.el ends here

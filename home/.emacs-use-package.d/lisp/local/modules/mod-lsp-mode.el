;;; mod-lsp-mode.el --- Lsp mode -*- lexical-binding: t -*-

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

(require 'lightemacs-module)
(require 'mod-defun)
(require 'le-corfu)
(require 'le-cape)

(defun evil-lookup-lsp ()
  "Display LSP documentation for the symbol at point."
  (interactive)
  (if (fboundp 'lsp-ui-doc-glance)
      (lsp-ui-doc-glance)
    (lsp-describe-thing-at-point)))

(defun my-setup-lsp-mode ()
  "Setup `lsp-mode'."
  (when (and (fboundp 'lsp-deferred)
             (my-code-checker-allowed-p))
    (lsp-deferred))

  (setq-local evil-lookup-func #'evil-lookup-lsp))

(lightemacs-use-package lsp-mode
  :commands (lsp-mode
             lsp-describe-thing-at-point
             lsp-deferred
             lsp-rename
             lsp-document-highlight
             lsp-modeline-diagnostics-mode
             lsp-completion-at-point
             lsp-goto-type-definition)

  :hook ((python-mode . my-setup-lsp-mode)
         (python-ts-mode . my-setup-lsp-mode))

  :custom
  ;; Prevents automatic download suggestions for LSP servers.
  ;; Benefit: avoids pop-ups and unplanned server installs.
  ;;   Example: opening a Python project in Emacs with no pylsp installed will
  ;;            not trigger a prompt to download the server automatically.
  ;; Drawback: user must manually install/update the server.
  ;;   Example: if pylsp is missing or outdated, LSP features like
  ;;            completion, diagnostics, and go-to-definition will not work
  ;;            until the user manually installs or updates python-lsp-server.
  (lsp-enable-suggest-server-download nil)

  ;; Disables additional text edits during completion (like auto-imports).
  ;; Benefit: completion is less intrusive and predictable.
  ;;   Example: typing datetime. will only complete methods like now or
  ;;            today without automatically inserting import datetime at the
  ;;            top of the file.
  ;; Drawback: may require manual import insertion.
  ;;   Example: if Path from pathlib is completed as Path(), Emacs/LSP
  ;;            will not insert from pathlib import Path, so the user must add
  ;;            it manually.
  (lsp-completion-enable-additional-text-edit t)

  ;; Disable hover info in the minibuffer or tooltip.
  ;; Benefit: reduces distractions and lag.
  ;; Drawback: loses quick access to type info, docstrings, and signatures.
  (lsp-eldoc-enable-hover nil)

  ;; Show function signatures automatically in minibuffer or after typing trigger chars.
  ;; Benefit: convenient inline argument guidance.
  ;; Drawback: may clutter minibuffer or trigger frequently.
  (lsp-signature-auto-activate '(:on-trigger-char
                                 :on-server-request
                                 :after-completion))


  ;; Limit signature doc lines to 1.
  ;; Benefit: prevents large docstrings from overflowing minibuffer.
  ;; Drawback: may hide useful documentation.
  (lsp-signature-doc-lines 1)
  ;; (lsp-signature-render-documentation nil) ; Docstring under signature

  ;; Enable imenu indexing.
  ;; Benefit: quick navigation to classes, methods, functions, enums.
  ;; Drawback: slight startup overhead for large files.
  (lsp-enable-imenu t)
  (lsp-imenu-index-symbol-kinds '(Class Method Function Enum))

  ;; Disable automatic edits before save.
  ;; Benefit: avoids unexpected formatting changes.
  ;;   Example: saving a Python file with misaligned indentation or trailing
  ;;            whitespace will not automatically reformat or trim lines,
  ;;            preserving manual edits.
  ;; Drawback: code may not be automatically formatted to style guidelines.
  ;;   Example: a function with inconsistent spacing or missing blank lines
  ;;            between methods will remain unformatted, potentially violating
  ;;            PEP8 standards.
  (lsp-before-save-edits nil)

  ;; Disable formatting while typing.
  ;; Benefit: reduces distractions and latency.
  ;;   Example: typing a Python function like:
  ;;            def add(a,b): return a+b
  ;;            will not trigger automatic spacing or line breaks as you type,
  ;;            so the cursor stays stable and typing feels responsive.
  ;; Drawback: real-time formatting benefits are lost.
  ;;   Example: the same function remains unformatted until manually saved or
  ;;            formatted with a tool, so missing spaces around operators or
  ;;            inconsistent indentation are not corrected on the fly.
  (lsp-enable-on-type-formatting t)

  (lsp-enable-indentation nil)

  ;; Disable automatic code actions.
  ;; Benefit: avoids unintended modifications.
  ;;   Example: if a Python file contains import os but the code never uses it,
  ;;            LSP will not automatically remove the unused import while typing
  ;;            or saving, preventing unexpected changes to the file.
  ;; Drawback: useful quick fixes are no longer applied automatically.
  ;;   Example: LSP would normally suggest and apply fixes like adding missing
  ;;            imports, correcting variable names, or removing unused
  ;;            variables, but these must now be applied manually by the user.
  (lsp-auto-execute-action t)

  ;; Enable diagnostics (linting) via LSP.
  ;; Benefit: highlights errors, warnings, and potential issues.
  ;; Drawback: can produce visual noise if too many warnings.
  ;; Linting (t = prefer Flymake)
  (lsp-diagnostics-provider :flymake)  ;; new

  ;; TODO what does this do?
  ;; (lsp-inlay-hint-enable t)
  ;; (lsp-update-inlay-hints-on-scroll nil)  ; Bad performance.

  ;; Make TCP connection is already slow; only try to connect once
  ;; Connection timeout for TCP LSP servers.
  ;; Benefit: faster failure detection.
  ;; Drawback: may fail to connect on slow networks.
  (lsp-tcp-connection-timeout 1.0)

  ;; Disable visual breadcrumbs in header line.
  ;; Benefit: reduces clutter.
  ;;   Example: opening a Python class like:
  ;;            class MyClass:
  ;;                def my_method(self):
  ;;            will not display "MyClass > my_method" in the header line,
  ;;            keeping the top of the window clean.
  ;; Drawback: loses file/class/method path at glance.
  ;;   Example: when navigating deep nested classes or functions,
  ;;            the header line no longer shows the current context,
  ;;            so you must check the code structure manually.
  (lsp-headerline-breadcrumb-enable nil)

  ;; Visual clutter
  ;; Disable code lenses (inline annotations for references/actions).
  ;; Benefit: reduces visual noise.
  ;;   Example: in a Python file with a function `def add(a, b):`, LSP will not
  ;;            display inline text like "3 references" or "Run tests" above the
  ;;            function, keeping the buffer cleaner.
  ;; Drawback: loses inline hints like references count.
  ;;   Example: you cannot see at a glance how many times `add()` is called in
  ;;            the project without running a separate find-references command.
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable nil)

  ;; Visual clutter
  (lsp-lens-enable nil)

  ;; Visual clutter
  ;; Disable modeline diagnostics.
  ;; Benefit: reduces modeline clutter.
  ;;   Example: the modeline will not show "3 errors, 2 warnings" for a Python
  ;;   file.
  ;; Drawback: no quick visible error count.
  (lsp-modeline-diagnostics-enable nil)

  ;; Visual clutter
  ;; Disable modeline code actions.
  ;; Benefit: reduces modeline clutter.
  ;;   Example: quick-fix indicators like "Add import" or "Rename variable" will
  ;;   not appear.
  ;; Drawback: quick action indicators are lost.
  (lsp-modeline-code-actions-enable nil)

  ;; Delay before triggering idle LSP actions.
  ;; Benefit: reduces CPU usage.
  ;;   Example: LSP will wait 0.6s after typing before showing hover info or
  ;;   symbol highlights in Python files.
  ;; Drawback: slight lag in hover info, symbol highlighting.
  (lsp-idle-delay 0.4)

  ;; Disable full eldoc rendering for all symbols.
  ;; Benefit: reduces minibuffer clutter.
  ;;   Example: hovering over `os.path.join` will not show the full docstring in
  ;;   the minibuffer.
  ;; Drawback: less documentation visible for symbols.
  (lsp-eldoc-render-all nil) ; clangd docs looks ugly on eldoc-box!

  ;; TODO
  ;; (lsp-modeline-diagnostics-scope :file)

  ;; Disable folding, snippet support
  ;; Disable code folding.
  ;; Benefit: reduces complexity in buffer management.
  ;;   Example: Python class or function blocks cannot be folded with LSP commands.
  ;; Drawback: manual folding unavailable.
  (lsp-enable-folding nil)

  ;; Disable snippet support.
  ;; Benefit: avoids conflicts with other completion frameworks.
  ;;   Example: completion for `def` will not expand into a full function
  ;;   template with placeholders.
  ;; Drawback: snippet-based completions are unavailable.
  (lsp-enable-snippet nil)

  ;; (lsp-auto-register-remote-clients nil)

  ;; Disable workspace status in modeline.
  ;; Benefit: reduces clutter.
  ;;   Example: LSP server status like "Indexing workspace..." will not appear in the modeline.
  ;; Drawback: loses server connection and indexing status info.
  (lsp-modeline-workspace-status-enable nil)

  ;; Might fix the watcher exhaustion.
  ;; Your operating system has a limit on the number of files a single user or
  ;; process can monitor for changes simultaneously. When Emacs (likely via
  ;; lsp-mode, dired, or git-gutter) tries to register a new file watch after
  ;; this limit is reached, the kernel rejects the request with "Resource
  ;; temporarily unavailable."
  (lsp-enable-file-watchers t)  ;; new
  (lsp-file-watch-threshold 1000)

  ;; Enable Flake8 for linting.
  ;; Benefit: highlights syntax and style issues.
  ;;   Example: writing `def foo():pass` triggers Flake8 warning for missing
  ;;   whitespace and docstring.
  ;; Drawback: may produce too many warnings in large projects.
  (lsp-pylsp-plugins-flake8-enabled t)

  (lsp-pylsp-plugins-autopep8-enabled nil)
  (lsp-pylsp-plugins-isort-enabled nil)
  (lsp-pylsp-plugins-mccabe-enabled nil)
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-pydocstyle-enabled nil)
  (lsp-pylsp-plugins-pyflakes-enabled nil)
  (lsp-pylsp-plugins-pylint-enabled nil)
  (lsp-restart 'auto-restart)

  ;; Automatically guess project root.
  ;; Benefit: easier workspace setup.
  ;;   Example: LSP automatically treats the folder with `setup.py` as the
  ;;   project root.
  ;; Drawback: may guess incorrectly for complex projects.
  (lsp-auto-guess-root t)

  ;; Completion

  ;; Show completion item kinds (function, variable, class, etc.).
  ;; Benefit: improves context in completions.
  ;;   Example: `os.path.` completion shows which items are functions, classes,
  ;;   or modules.
  ;; Drawback: minor visual clutter.
  (lsp-completion-show-kind t)

  ;; Show detailed completion info (signature, documentation).
  ;; Benefit: faster development by providing context.
  ;;   Example: completing `datetime.datetime.` shows function signatures like
  ;;   `now() -> datetime`.
  ;; Drawback: may slow down completion popup rendering.
  (lsp-completion-show-detail t)

  ;; Enable Rope for refactoring (rename, extract, etc.).
  ;; Benefit: safe and efficient variable/method renaming.
  ;;   Example: renaming `foo` to `bar` updates all references across the Python
  ;;   project.
  ;; Drawback: depends on Rope support and may be slower in large projects.
  (lsp-pylsp-plugins-rope-enabled t)

  ;; Jedi
  ;;
  ;; Disable Jedi fuzzy completion.
  ;; Benefit: reduces unwanted suggestions.
  ;;   Example: completing `os.p` only suggests `path` exactly, not `pardir` or
  ;;   unrelated matches.
  ;; Drawback: less flexible completion matching.
  (lsp-pylsp-plugins-jedi-completion-fuzzy nil)

  ;; TODO
  ;; (lsp-pylsp-plugins-jedi-completion-include-class-objects nil)
  ;; (lsp-pylsp-plugins-jedi-completion-include-params nil)
  ;; (lsp-pylsp-plugins-jedi-completion-eager t)
  ;; (lsp-pylsp-plugins-jedi-completion-include-function-objects nil)

  ;; Shutdown server when last buffer closes.
  ;; Benefit: reduces resource usage.
  ;;   Example: closing the last Python buffer stops `pylsp`, freeing memory.
  ;; Drawback: workspace state is lost on closure.
  (lsp-keep-workspace-alive nil)

  ;; Highlighting
  ;; Highlight occurrences of symbol at point.
  ;; Benefit: easier code navigation.
  ;;   Example: placing cursor on `my_var` highlights all other occurrences in
  ;;   the Python file.
  ;; Drawback: minor performance cost.
  (lsp-enable-symbol-highlighting t)

  ;; Disable text document color features.
  ;; Benefit: reduces visual clutter and CPU use.
  ;;   Example: string literals like `"red"` are not colored based on their
  ;;   value.
  ;; Drawback: loses color information for color literals.
  (lsp-enable-text-document-color t)

  ;; Do not trim trailing whitespace automatically.
  ;; Benefit: preserves manual whitespace formatting.
  ;;   Example: lines with extra spaces at the end of Python lines remain
  ;;   unchanged.
  ;; Drawback: may leave inconsistent whitespace.
  (lsp-trim-trailing-whitespace nil)

  ;; Do not trim final newlines automatically.
  ;; Benefit: preserves file endings.
  ;;   Example: files ending with multiple newlines retain them on save.
  ;; Drawback: may violate style guides.
  (lsp-trim-final-newlines nil)

  ;; Highlighting
  ;; Enable semantic tokens (classes, functions, variables, unreachable code
  ;; highlighting).
  ;; Benefit: improves code readability and shows dead code paths.
  ;;   Example: in Python, unreachable code under `if False:` is dimmed, and
  ;;   function/class/variable tokens have semantic colors.
  ;; Drawback: minor performance cost on large files.
  (lsp-semantic-tokens-enable t) ; when t, hides unreachable ifdefs

  ;; Prevent server from overriding token faces.
  ;; Benefit: preserves Emacs syntax highlighting and theme.
  ;;   Example: Python keywords and function names use standard font-lock faces
  ;;   instead of server-suggested styles.
  ;; Drawback: loses server-applied styles like deprecated or readonly markers.
  (lsp-semantic-tokens-apply-modifiers t) ;; don't override token faces

  ;; Disable logging of LSP communication.
  ;; Benefit: reduces performance overhead.
  ;;   Example: editing Python files does not generate verbose debug logs in
  ;;   `*lsp-log*`.
  ;; Drawback: harder to debug LSP server issues.
  (lsp-log-io nil)  ; if set to true can cause a performance hit

  ;; :config
  ;; Suppress messages from LSP server connection
  ;; TODO
  ;; (advice-add 'lsp--info :around
  ;;             (lambda (orig-fun &rest args)
  ;;               (let ((msg (apply #'format args)))
  ;;                 (unless (or (string-prefix-p "Connected" msg)
  ;;                             (string-prefix-p "Waiting" msg)
  ;;                             (string-prefix-p "Reconnected" msg))
  ;;                   (apply orig-fun args)))))

  :init
  (setq lsp--show-message nil)

  (when (and (getenv "LSP_USE_PLISTS")
             (string= (getenv "LSP_USE_PLISTS") "true"))
    (setq lsp-use-plists t)))

;;; cape

;;(require 'le-corfu)
;;(require 'le-cape)

;; https://github.com/minad/corfu/wiki#advanced-example-configuration-with-orderless
(setq lsp-completion-provider :none) ; We use Corfu

(defun my/orderless-dispatch-flex-first (_pattern index _total)
  "Dispatch flex first. INDEX is the index."
  (and (eq index 0) 'orderless-flex))

(defun my/lsp-mode-setup-completion ()
  "Setup `lsp-mode' completion."
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless))
  ;; Optionally configure the first word as flex filtered.
  (setq-local orderless-style-dispatchers
              (list #'my/orderless-dispatch-flex-first))

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions
              (list (cape-capf-buster 'lsp-completion-at-point))))

(add-hook 'lsp-completion-mode-hook #'my/lsp-mode-setup-completion)

;; (use-package lsp-ui
;;   ;; :after lsp-mode
;;   :commands (lsp-ui-doc-toggle
;;              lsp-ui-mode)
;;   :hook
;;   (lsp-mode . lsp-ui-mode)
;;   ;; :custom
;;   ;; (lsp-modeline-code-actions-enable nil)
;;   ;; (lsp-ui-doc-enable t)
;;   ;; (lsp-ui-doc-include-signature t)
;;   ;; (lsp-ui-doc-max-height 30)
;;   ;; (lsp-ui-doc-position 'bottom)
;;   ;; (lsp-ui-doc-show-with-cursor nil)  ;; disable cursor hover (keep mouse hover)
;;   ;; (lsp-ui-doc-show-with-mouse nil)  ;;  disable mouse hover (keep cursor hover)
;;   ;; (lsp-ui-doc-use-childframe t) ;; nil = no more problems!
;;   ;; (lsp-ui-imenu-auto-refresh t)
;;   ;; (lsp-ui-peek-always-show nil)
;;   ;; (lsp-ui-peek-enable nil)
;;   ;; (lsp-ui-peek-fontify 'always)
;;   ;; (lsp-ui-sideline-delay 0.05)
;;   ;; (lsp-ui-sideline-enable nil)  ;;  Sideline code actions * disable whole sideline via
;;   ;; (lsp-ui-sideline-show-code-actions nil)
;;   ;; (lsp-ui-sideline-show-diagnostics t)
;;   ;; (lsp-ui-sideline-show-hover nil)
;;   )

(provide 'mod-lsp-mode)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-lsp-mode.el ends here

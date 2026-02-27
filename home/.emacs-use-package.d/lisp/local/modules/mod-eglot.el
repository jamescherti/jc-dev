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

(require 'my-defun)

;;; fix ignore empty

(defun my/jsonrpc--continue-ignore-empty (orig-fun conn id &optional cont result error)
  "Ignore empty JSON-RPC results to prevent plistp errors in Eglot.

ORIG-FUN is the original `jsonrpc--continue` function.
CONN is the JSON-RPC connection.
ID is the message ID.
CONT is the continuation function.
RESULT is the result from the server.
ERROR is the error (if any)."
  (if (and (vectorp result) (zerop (length result)))
      nil
    (funcall orig-fun conn id cont result error)))

(with-eval-after-load 'jsonrpc
  (advice-add 'jsonrpc--continue :around #'my/jsonrpc--continue-ignore-empty))

;;; eglot

;;   (when (fboundp 'jsonrpc--log-event)
;;     (fset #'jsonrpc--log-event #'ignore)))

;; (defun my-eglot-format-buffer ()
;;   "Eglot format buffer."
;;   (when (and
;;          (fboundp 'eglot-managed-p)
;;          (eglot-managed-p))
;;     (let ((inhibit-message t))
;;       (when (fboundp 'eglot-format-buffer)
;;         (eglot-format-buffer)))))

(defun my-setup-eglot-mode ()
  "Setup `eglot-mode'."
  (when (my-code-checker-allowed-p)
    (eglot-ensure)

    ;; Apheleia takes care of this
    ;; (when (fboundp 'my-eglot-format-buffer)
    ;;   (add-hook 'before-save-hook #'my-eglot-format-buffer 90 t))

    ))

;; TODO lightemacs?
;; The LSP server assumes that the candidates are retrieved on every change to
;; the buffer and we can rely on the LSP server to do the heavy lifting and
;; provide a continuously update list of completions. The problem is that LSP
;; servers are unaware of Emacs completion-styles, therefore the candidates from
;; the language server are only post-filtered by the completion style. Corfu
;; retrieves the candidate completion table once at the beginning of a
;; completion session and doesn’t reload it while the word is being typed. This
;; is advantageous for most completion-at-point-functions since it opens up
;; caching opportunities. Eglot completion function doesn’t refresh the
;; completion table itself. This is problematic, as language servers often only
;; provide limited list of completions. Therefore, when using auto completion
;; and typing slowly, possible completion candidates may be missing, because the
;; initial list was missing the intended string. This behavior can be changed
;; with the cache buster from Cape, which ensures that the completion table is
;; refreshed such that the candidates are always obtained again from the server.
;;
;; Retrieving the candidates on every key press can be seen as a disadvantage,
;; since it decreases performance. This matters in particular if your language
;; server returns many candidates (or even a complete set of candidates) right
;; away on the first invocation of completion. Depending on your language server
;; you may or may not want the cape-wrap-buster. See also the next section about
;; requesting more candidates from the Lsp server by default.
;;
;; Corfu supports the Orderless completion style. As soon as you press M-SPC, a
;; space is inserted and Orderless filtering starts. The candidates are not
;; updated again from the server. Instead the existing candidates are only
;; post-filtered with Orderless. This feature works always, with or without
;; cape-wrap-buster. Note that this feature is Orderless-specific. See the next
;; section.
(with-eval-after-load 'eglot
  (with-eval-after-load 'cape
    (advice-add 'eglot-completion-at-point :around 'cape-wrap-buster)))

(lightemacs-use-package eglot
  :ensure nil
  :commands (eglot
             eglot-rename
             eglot-managed-p
             eglot-format
             eglot-ensure
             eglot-rename
             eglot-format-buffer)

  :hook ((python-mode . my-setup-eglot-mode)
         (python-ts-mode . my-setup-eglot-mode))

  :config
  ;; Remove eglot from the modeline
  (setq mode-line-misc-info
        (assq-delete-all 'eglot--managed-mode mode-line-misc-info))

  :init
  ;; (setq eglot-prefer-plaintext nil)

  ;; Allow edits without confirmation
  (setq eglot-confirm-server-edits nil)
  ;; (setq eglot-confirm-server-edits '((eglot-rename . nil)
  ;;                                    (t . maybe-summary)))

  (setq eglot-code-action-indications nil) ;; EMACS-31 -- annoying as hell
  ;; (setq eglot-code-action-indications '(margin))  ;; no need to spam eldoc

  (setq eglot-stay-out-of '(yasnippet))
  ;; (setq eglot-stay-out-of '(flymake))

  (setq eglot-connect-timeout 40)
  ;; (setq eglot-send-changes-idle-time 0.5)

  ;; (setq eglot-ignored-server-capabilities
  ;;       '(;:hoverProvider  ; For showing the definition and documentation.
  ;;         :completionProvider  ; Completion
  ;;
  ;;         ;; If you remove this, it will cause an error
  ;;         ;; Debugger entered--Lisp error: (wrong-type-argument plistp [])
  ;;         ;;   plist-member([] :signatures)
  ;;         ;;   #f(compiled-function (jsonrpc-lambda-elem12) #<bytecode 0x14ea63506aea3ca5>)([])
  ;;         ;;   jsonrpc--continue(#<eglot-lsp-server eglot-lsp-server-1323b2317330> 3 (3 :textDocument/signatureHelp #f(compiled-function (jsonrpc-lambda-elem12) #<bytecode 0x14ea63506aea3ca5>) #f(compiled-function (jsonrpc-lambda-elem3) #<bytecode 0x1ff00ae7cafeec70>) [nil 26700 21814 516821 nil #f(compiled-function () #<bytecode 0x1b6df269801a5fe1>) nil nil 28000 nil]) [] nil)
  ;;         ;;   jsonrpc-connection-receive(#<eglot-lsp-server eglot-lsp-server-1323b2317330> (:jsonrpc "2.0" :id 3 :result []))
  ;;         ;;   #f(compiled-function (conn msg) #<bytecode -0x1fc66ff688233035>)(#<eglot-lsp-server eglot-lsp-server-1323b2317330> (:jsonrpc "2.0" :id 3 :result []))
  ;;         ;;   apply(#f(compiled-function (conn msg) #<bytecode -0x1fc66ff688233035>) (#<eglot-lsp-server eglot-lsp-server-1323b2317330> (:jsonrpc "2.0" :id 3 :result [])))
  ;;         ;;   timer-event-handler([t 26700 21804 803197 nil #f(compiled-function (conn msg) #<bytecode -0x1fc66ff688233035>) (#<eglot-lsp-server eglot-lsp-server-1323b2317330> (:jsonrpc "2.0" :id 3 :result [])) nil 238000 nil])
  ;;         :signatureHelpProvider  ; For showing the function signature/arguments.
  ;;
  ;;         ;; Disable "go to definition" feature
  ;;         ;; :definitionProvider
  ;;
  ;;         ;; Disable support for "go to type definition"
  ;;         ;; :typeDefinitionProvider
  ;;
  ;;         ;; Disable support for finding implementation locations This
  ;;         ;; capability allows the LSP client (like Eglot) to query the server
  ;;         ;; for the actual implementation(s) of an interface, abstract method,
  ;;         ;; or symbol. For example, if the cursor is on a function declaration
  ;;         ;; or interface, this enables jumping directly to the concrete
  ;;         ;; implementation(s). Disabling it will prevent Eglot from offering
  ;;         ;; this navigation feature.
  ;;         ;; :implementationProvider
  ;;
  ;;         ;; Disables ability to jump to a symbol's declaration location (e.g.,
  ;;         ;; jumping to where a variable was declared).
  ;;         ;; :declarationProvider
  ;;
  ;;         ;; Disables showing all references to the symbol at point (e.g., all
  ;;         ;; usages of a function or variable in the project).
  ;;         ;; :referencesProvider
  ;;
  ;;         ;; Disables highlighting other instances of the symbol at point in the
  ;;         ;; current buffer (e.g., all usages of a variable are no longer
  ;;         ;; visually highlighted).
  ;;         ;; Usage: This affects the automatic highlighting when the cursor is
  ;;         ;; on a symbol. Normally, all occurrences of that symbol in the buffer
  ;;         ;; are highlighted. Disabling this stops that behavior.
  ;;         :documentHighlightProvider
  ;;
  ;;         ;; Disables the document-wide symbol tree view used for navigation or
  ;;         ;; structural outline (e.g., class and function tree in sidebar).
  ;;         ;; Usage: This impacts commands or UI elements that display a tree or
  ;;         ;; list of all symbols (functions, classes, variables) in the current
  ;;         ;; buffer. Disabling this removes that outline view.
  ;;         :documentSymbolProvider
  ;;
  ;;         ;; Disables workspace-wide symbol search (e.g., `M-x
  ;;         ;; xref-find-apropos` or project-wide function/class name search).
  ;;         :workspaceSymbolProvider
  ;;
  ;;         ;; Usage: This prevents displaying available quick fixes or
  ;;         ;; refactorings that normally appear as code actions or lightbulb
  ;;         ;; hints in the editor. Disabling this means you won't get automatic
  ;;         ;; fix suggestions from the server.
  ;;         :codeActionProvider
  ;;
  ;;         ;; Disables inline annotations like test coverage, reference counts,
  ;;         ;; or result indicators that appear above/below code lines.
  ;;         :codeLensProvider
  ;;
  ;;         ;; Disables detection of URLs or other hyperlinks in documents
  ;;         ;; (clickable links will not be rendered).
  ;;         ;; :documentFormattingProvider
  ;;
  ;;         ;; :documentRangeFormattingProvider
  ;;
  ;;         ;; Disable formatting triggered by typing specific characters (like `}`)
  ;;         ;; :documentOnTypeFormattingProvider
  ;;
  ;;         ;; Disable rename symbol functionality
  ;;         ;; :renameProvider
  ;;
  ;;         ;; Disable detection and interaction with links in documents
  ;;         :documentLinkProvider
  ;;
  ;;         ;; Disables rendering of inline color swatches next to color values in
  ;;         ;; code (e.g., "#ff0000" showing a red box).
  ;;         :colorProvider
  ;;
  ;;         ;; Disables visual fold range markers (e.g., foldable region
  ;;         ;; indicators in the fringe or gutter).
  ;;         ;; :foldingRangeProvider
  ;;
  ;;         ;; Disables execution of commands exposed by the server (e.g., special
  ;;         ;; refactoring or custom commands via `M-x eglot-execute-command`).
  ;;         :executeCommandProvider
  ;;
  ;;         ;; Disable inlay hints (e.g. inferred types, parameter names) Inlay
  ;;         ;; hints are small, non-intrusive annotations inserted into the code
  ;;         ;; by the LSP server. They provide helpful context such as inferred
  ;;         ;; variable types, function return types, or parameter names in
  ;;         ;; function calls, especially in languages like TypeScript or Rust.
  ;;         ;; These hints do not change the actual source code but are visually
  ;;         ;; rendered in the editor. Disabling this prevents the display of such
  ;;         ;; annotations in the buffer.
  ;;         :inlayHintProvider))

  ;; (when (string= choice-code-formatter "eglot")
  ;;   (add-hook 'before-save-hook
  ;;             #'(lambda()
  ;;                 (when (eglot-managed-p)
  ;;                   (be-quiet (eglot-format))))
  ;;             99 t))

  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "<leader>ef") #'eglot-format-buffer))

  ;; https://github.com/python-lsp/python-lsp-server/blob/develop/CONFIGURATION.md
  (setq-default eglot-workspace-configuration
                `(:pylsp (:plugins
                          (; Improve syntax
                           :isort (:enabled :json-false)
                           ;; Note autopep uses some pycodestyle settings
                           :autopep8 (:enabled :json-false)

                           ;; Syntax checkers
                           :pylint (:enabled t)
                           ;; Executed by flake8
                           :pycodestyle (:enabled :json-false
                                                  :match "(?!test_).*\\.py"
                                                  :maxLineLength 79
                                                  :convention "pep257"
                                                  :ignore ["W293"]
                                                  :hangClosing nil)
                           :flake8 (:enabled t)
                           :pyflakes (:enabled :json-false :ignore ["W293"])
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
                                     :eager t

                                     :include_class_objects :json-false
                                     :include_function_objects :json-false
                                     :include_params :json-false

                                     ;; How many labels and snippets (at most)
                                     ;; should be resolved?
                                     ;; :resolve_at_most 40
                                     )

                           ;; NOTE: Removed because it causes on Arch:
                           ;; Debugger entered--Lisp error: (wrong-type-argument plistp [])
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
                           :mccabe (:enabled :json-false)
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
                                                          "W202"]
                                                 )
                           :yapf (:enabled :json-false)
                           :rope_autoimport (:enabled :json-false))))))

(advice-add 'eglot--message :around
            #'(lambda(orig-fun format &rest args)
                ;; This code provides an Emacs Lisp function to suppress specific Eglot
                ;; messages from being shown in the minibuffer.
                ;; "Suppress specific eglot messages from being shown in the minibuffer."
                (let ((message-string (apply #'format format args)))
                  (unless (or (string-prefix-p "Connected" message-string)
                              (string-prefix-p "Waiting" message-string)
                              (string-prefix-p "Reconnected" message-string))
                    (apply orig-fun format args)))))

(provide 'mod-eglot)

;; Local variables:
;; byte-compile-warnings: (not lexical free-vars)
;; End:

;;; mod-eglot.el ends here

;;; mod-filetype.el --- mod-filetype -*- lexical-binding: t -*-

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

;; Config evil.

;;; Code:

;;; Require

(require 'my-defun)
(eval-and-compile
  (require 'lightemacs-use-package))

;;; Tree-sitter Fallback Helpers

;; Enable native Tree-sitter mode redirection globally for Emacs 31+
;; We use `setopt' instead of `setq' because this variable
;; requires its custom `:set' function to execute the actual remaps.
;; (with-eval-after-load 'treesit
;;   (when (>= emacs-major-version 31)
;;     (if (fboundp 'setopt)
;;         (setopt treesit-enabled-modes t)
;;       (customize-set-variable 'treesit-enabled-modes t))))

;; (with-eval-after-load 'markdown-ts-mode-maybe
;;   (defun markdown-ts-mode-maybe ()
;;     "Enable `markdown-ts-mode' when its grammars are available.
;; Also propose to install the grammars when `treesit-enabled-modes'
;; is t or contains the mode name."
;;     (when (fboundp 'markdown-mode)
;;       (markdown-mode 1))))

(defvar mod-filetype--ts-lang-cache nil
  "Cache for tree-sitter language availability checks.")

(defun mod-filetype--ts-lang-available-p (lang)
  "Return non-nil if Tree-sitter LANG is available, caching the result."
  (let ((cached (assq lang mod-filetype--ts-lang-cache)))
    (if cached
        (cdr cached)
      (let ((available (my-treesit-language-available-p lang)))
        (push (cons lang available) mod-filetype--ts-lang-cache)
        available))))

(defun my-remap-ts-mode (base-mode ts-mode lang)
  "Remap BASE-MODE to TS-MODE if Tree-sitter LANG is available."
  (when (and (< emacs-major-version 31)
             (mod-filetype--ts-lang-available-p lang))
    (push (cons base-mode ts-mode) major-mode-remap-alist)))

(defun my-auto-mode-ts (regex ts-mode fallback-mode lang)
  "Map REGEX to TS-MODE if Tree-sitter LANG is available, else use FALLBACK-MODE."
  (if (mod-filetype--ts-lang-available-p lang)
      (if (and fallback-mode treesit-enabled-modes)
          (push (cons regex fallback-mode) auto-mode-alist)
        (push (cons regex ts-mode) auto-mode-alist))
    (when fallback-mode
      (push (cons regex fallback-mode) auto-mode-alist))))

;;; Filetype defaults

(setq sgml-basic-offset 2)  ;; HTML
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq javascript-indent-level 2)
(setq html-indent-offset 2)
(setq lua-indent-level 2)
(setq lua-ts-indent-offset 2)
(setq yaml-indent-offset 2)

;;; Filetype defaults
(setq typescript-ts-mode-indent-offset 2) ;; TypeScript (Tree-sitter)
(setq json-ts-mode-indent-offset 2)       ;; JSON (Tree-sitter)
;; (setq c-basic-offset 2)                   ;; C/C++
;; (setq c-ts-mode-indent-offset 2)          ;; C/C++ (Tree-sitter)

;;; typescript

(lightemacs-use-package typescript-ts-mode
  :if (and (>= emacs-major-version 29)
           (my-treesit-language-available-p 'typescript))
  :ensure nil
  :commands typescript-ts-mode
  :mode "\\.ts\\'")

;;; txt-file-mode

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

(add-hook 'txt-file-mode-hook #'setup-txt-file-mode)

(push (cons "\\.[Tt][Xx][Tt]\\'" 'txt-file-mode) auto-mode-alist)
(push (cons "\\.[Tt][Xx][Tt]\\.[aA][sS][cC]\\'" 'txt-file-mode) auto-mode-alist)

;;; conf-mode

(defun my-setup-conf-mode ()
  "Setup `conf-mode'."
  (setq-local evil-auto-indent nil)
  (setq-local indent-line-function #'ignore))
(with-no-warnings
  (add-hook 'conf-mode-hook #'my-setup-conf-mode))

;;; Elisp

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
  (add-hook hook #'(lambda ()
                     (display-fill-column-indicator-mode)
                     (if (fboundp 'my-set-tab-width)
                         (my-set-tab-width 2)
                       (error "Undefined: my-set-tab-width")))))

;;; Vimrc mode

(lightemacs-use-package vimrc-mode
  :commands vimrc-mode
  :mode
  ("/\\.l?vim\\(rc\\)?\\([^/]*\\)?\\'" . vimrc-mode)
  :init
  (add-hook 'vimrc-mode-hook #'(lambda ()
                                 (setq-local indent-tabs-mode nil)
                                 (if (fboundp 'my-set-tab-width)
                                     (my-set-tab-width 2)
                                   (error "Undefined: my-set-tab-width")))))

;;; Tree-sitter defaults

;; ■ Warning (treesit): Cannot activate tree-sitter, because language grammar
;; for yaml is unavailable (not-found): ...
(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-types '(treesit))
  (add-to-list 'warning-suppress-log-types '(treesit)))

(unless IS-MAC
  ;; Mac Port
  (add-to-list 'treesit-extra-load-path "/opt/local/lib"))

(setq treesit-auto-install-grammar nil)

(with-suppressed-warnings ((free-vars treesit-language-source-alist))
  (defvar mod-filetype--treesit-language-source-alist
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
       ;; For split parsers like Markdown, the extra two fields are
       ;; required:
       ;; 1. "split_parser" indicates that this language uses a parser
       ;;    split into multiple components.
       ;; 2. The directory path (e.g., "tree-sitter-markdown/src") points
       ;;    to the location of the parser source within the repository.
       ;;    Without these, treesit would not be able to find and compile
       ;;    the parser correctly.
       ;;
       ;; A split parser is a Tree-sitter parser that is divided into
       ;; multiple smaller parsers instead of being a single file or
       ;; module. Each smaller parser handles a part of the language, such
       ;; as different syntaxes or embedded languages, and together they
       ;; form the complete parser. This approach makes it easier to
       ;; manage complex languages, like Markdown, which can contain code
       ;; blocks, inline formatting, and other embedded languages. In
       ;; Emacs, specifying "split_parser" and the source directory tells
       ;; treesit how to find and build all the pieces correctly.
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
      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
      (glsl "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
      (julia "https://github.com/tree-sitter/tree-sitter-julia")
      (make "https://github.com/tree-sitter-grammars/tree-sitter-make")
      (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
      (rust "https://github.com/tree-sitter/tree-sitter-rust")
      (scala "https://github.com/tree-sitter/tree-sitter-scala")
      (toml "https://github.com/tree-sitter/tree-sitter-toml")
      (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
      (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
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
      (go "https://github.com/tree-sitter/tree-sitter-go")
      (gowork "https://github.com/omertuc/tree-sitter-go-work")
      (gomod "https://github.com/camdencheek/tree-sitter-go-mod"))))

;;; Bug fix: `treesit--install-language-grammar-out-dir-history'

;; TODO BUG emacs?
(with-eval-after-load 'savehist
  ;; Prevent savehist from persisting this variable because it accumulates
  ;; every directory path selected during treesit grammar installation,
  ;; including temporary or incorrect paths that are not valid locations for
  ;; tree-sitter .so files.
  (add-to-list 'savehist-ignored-variables
               'treesit--install-language-grammar-out-dir-history))

;; TODO minimal emacs
;; Prevent savehist from polluting the history file with temporary or invalid
;; directory paths entered during tree-sitter grammar installations.
;; (setq savehist-ignored-variables
;;       '(treesit--install-language-grammar-out-dir-history))

(with-eval-after-load 'treesit
  (setq treesit--install-language-grammar-out-dir-history
        (list (expand-file-name "tree-sitter" lightemacs-var-directory))))

;;; Functions

(defun mod-filetype-install (package lang-keys &optional install-fn)
  "Load PACKAGE and prepare the tree-sitter sources for LANG-KEYS.
LANG-KEYS can be a single symbol or a list of symbols.
INSTALL-FN is an optional function to call to install the parsers.
It adds the configuration from `mod-filetype--treesit-language-source-alist'
to `treesit-language-source-alist' if missing, then installs the grammars
only if they are not already available."
  (when package
    (require package nil t))
  (let ((keys (if (listp lang-keys) lang-keys (list lang-keys)))
        (missing-grammar nil))
    (dolist (key keys)
      (unless (assq key treesit-language-source-alist)
        (let ((recipe (assq key mod-filetype--treesit-language-source-alist)))
          (when recipe
            (push recipe treesit-language-source-alist))))
      ;; Flag if at least one grammar from the list needs installation
      (unless (treesit-language-available-p key)
        (setq missing-grammar t)))
    (when missing-grammar
      (if (and install-fn (fboundp install-fn))
          (funcall install-fn)
        (dolist (key keys)
          (unless (treesit-language-available-p key)
            (treesit-install-language-grammar key)))))))

(defun my-treesit-update-language-grammar ()
  "Update language grammar."
  (interactive)
  ;; (mod-filetype-install PACKAGE lang-key)
  (mod-filetype-install 'markdown-mode '(markdown markdown-inline)
                        'markdown-ts-mode-install-parsers)

  (mod-filetype-install 'js-ts-mode 'javascript)
  (mod-filetype-install 'typescript-ts-mode 'typescript)
  (mod-filetype-install 'json-ts-mode 'json)

  (mod-filetype-install 'php-ts-mode 'php 'php-ts-mode-install-parsers)
  (mod-filetype-install 'css-ts-mode 'css)

  ;; html-ts-mode: Designed strictly for parsing standard HTML. It uses only the
  ;; tree-sitter-html grammar. If you have <style> or <script> tags, the code
  ;; inside them will only receive basic HTML highlighting rather than
  ;; language-specific formatting.
  (mod-filetype-install 'html-ts-mode 'html)

  ;; mhtml-ts-mode ("Multiple HTML"): Designed for mixed-language web
  ;; development. It leverages multiple Tree-sitter parsers (html, javascript,
  ;; and css) simultaneously. It shifts context on the fly, providing smart
  ;; indentation, code completion, and exact syntax highlighting for JavaScript
  ;; and CSS code blocks nested right inside the HTML.
  (mod-filetype-install 'mhtml-ts-mode 'html
                        'mhtml-ts-mode-install-parsers)

  (mod-filetype-install 'toml-ts-mode 'toml)
  (mod-filetype-install 'python-ts-mode 'python)

  (mod-filetype-install 'yaml-ts-mode 'yaml)
  (mod-filetype-install 'bash-ts-mode 'bash)
  (mod-filetype-install 'lua-ts-mode 'lua)
  (mod-filetype-install 'c-ts-mode 'c)
  (mod-filetype-install 'c-ts-mode 'cpp)
  (mod-filetype-install 'dockerfile-ts-mode 'dockerfile))

;;; jinja2

;; TODO make a plugin

(defvar jinja2-highlight-font-lock-keywords
  '(("{%\\(?:.\\|\n\\)*?%}" 0 'font-lock-preprocessor-face t)
    ("{{\\(?:.\\|\n\\)*?}}" 0 'font-lock-preprocessor-face t))
  "Font-lock keywords for Jinja2 highlighting.")

(define-minor-mode jinja2-highlight-mode
  "A minor mode to highlight Jinja2 template tags."
  :lighter nil
  (if jinja2-highlight-mode
      (font-lock-add-keywords nil jinja2-highlight-font-lock-keywords)
    (font-lock-remove-keywords nil jinja2-highlight-font-lock-keywords))

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (font-lock-ensure)))

(defun jinja2-autodetect-mode ()
  "Determine the major mode for a .j2 file and enable `jinja2-highlight-mode'."
  (interactive)
  (when buffer-file-name
    (let ((buffer-file-name (file-name-sans-extension buffer-file-name)))
      (set-auto-mode)))
  (jinja2-highlight-mode 1))

;; (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-autodetect-mode))
(push '("\\.j2\\'" . jinja2-autodetect-mode) auto-mode-alist)

;;; Yaml and Ansible

(defvar treesit-yaml-available (my-treesit-language-available-p 'yaml))

;; Must be evaluated before Org is loaded
(with-eval-after-load 'org
  (if (and (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'yaml))
      (push (cons "yaml" 'yaml-ts) org-src-lang-modes)
    (push (cons "yaml" 'yaml) org-src-lang-modes)))

(with-suppressed-warnings ((free-vars flymake-yamllint-arguments)
                           (free-vars yaml-ts-mode-yamllint-options))
  (setq flymake-yamllint-arguments
        (list "-c" (expand-file-name "~/.yamllint_global.yml")))
  (setq yaml-ts-mode-yamllint-options
        (copy-sequence flymake-yamllint-arguments)))

;;; Ansible

;; (lightemacs-use-package ansible
;;   :commands ansible-mode
;;   :config
;;   (setq ansible-playbook-font-lock
;;         `(;;(,ansible-section-keywords-regex    (1 ansible-section-face t))
;;           (,ansible-task-keywords-regex       (1 font-lock-keyword-face t))
;;           (,ansible-keywords-regex            (1 font-lock-keyword-face t))
;;           ("^ *- \\(name\\):\\([^#\n]*\\)"
;;            (1 font-lock-keyword-face t)
;;            (2 font-lock-string-face t))
;;           ("\\({{\\)\\([^}]+\\)\\(}}\\)"
;;            (1 font-lock-builtin-face t)
;;            (2 font-lock-function-name-face t)
;;            (3 font-lock-builtin-face t))
;;           ("\\({%\\)\\([^}]+\\)\\(%}\\)"
;;            (1 font-lock-builtin-face t)
;;            (2 font-lock-function-name-face t)
;;            (3 font-lock-builtin-face t))
;;           ("\\({#\\)\\([^}]+\\)\\(#}\\)"
;;            (1 font-lock-comment-delimiter-face t)
;;            (2 font-lock-comment-face t)
;;            (3 font-lock-comment-delimiter-face t)))))

(if treesit-yaml-available
    (progn
      ;; (with-eval-after-load 'mod-cleanup
      ;;   (push 'flymake-yamllint mod-cleanup-packages-list)
      ;;   (push 'yaml-mode mod-cleanup-packages-list))

      ;; Remove the auto-mode-alist entry (Useful to prevent yaml-ts-mode from
      ;; activating on ansible-mode)
      (with-eval-after-load 'yaml-ts-mode
        (setq auto-mode-alist
              (rassq-delete-all 'yaml-ts-mode auto-mode-alist))
        (my-remap-ts-mode 'yaml-mode 'yaml-ts-mode 'yaml)))
  ;; non tree sitter
  (require 'sub-flymake-yamllint))

;; NOTE: My Ansible
(if treesit-yaml-available
    ;; Non tree-sitter
    (define-derived-mode ansible-mode yaml-ts-mode "Ansible"
      "Major mode for editing Ansible files.")
  ;; Non tree-sitter
  (when (fboundp 'yaml-mode)
    (define-derived-mode ansible-mode yaml-mode "Ansible"
      "Major mode for editing Ansible files.")))

(defun my-setup-ansible-mode ()
  "Set up `ansible-mode'."
  (set-syntax-table (copy-syntax-table))

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
  (modify-syntax-entry ?$ ".")

  ;; Fixes symbols in comments (e.g. "This is xyz, a variable...")
  (modify-syntax-entry ?, ".")

  ;; Ensures that (.), (,) and (!) are treated as part of symbols or words
  ;; within YAML documents. In YAML, these characters may be used as part
  ;; of keys in quoted strings.
  ;;
  ;; (.) is for symbols such as: ansible.builtin.command
  (modify-syntax-entry ?. "_")
  (modify-syntax-entry ?! "_"))

(add-hook 'ansible-mode-hook #'my-setup-ansible-mode)

;;; Ansible: Auto detect

(defvar my-ansible-file-regexp nil)
(setq my-ansible-file-regexp (rx "/"
                                 (group (or "tasks"
                                            "handlers"
                                            "vars"
                                            "defaults"
                                            "ansible"
                                            "host_vars"
                                            "group_vars"
                                            "playbooks"))
                                 "/" (* nonl)
                                 "." (regexp "[yY][aA]?[mM][lL]")
                                 string-end))

;; When auto-mode-alist is bypassed, use a hook function
(defvar-local my-inhibit-ansible-detect-and-enable-ansible-mode nil)

(defun my-ansible-detect-and-enable-ansible-mode ()
  "Enable `ansible-mode' for YAML files in Ansible-related directories."
  (unless my-inhibit-ansible-detect-and-enable-ansible-mode
    (let ((file-name (buffer-file-name (buffer-base-buffer)))
          (my-inhibit-ansible-detect-and-enable-ansible-mode t))
      (cond
       ((and (fboundp 'ansible-mode)
             file-name
             (string-match my-ansible-file-regexp (expand-file-name file-name)))
        (ansible-mode)
        ;; (remove-hook 'flymake-diagnostic-functions 'yaml-ts-mode-flymake 'local)
        (jinja2-highlight-mode 1))

       ((and treesit-yaml-available
             (fboundp 'yaml-ts-mode)
             (not (derived-mode-p 'yaml-ts-mode)))
        (yaml-ts-mode))

       ((and (not treesit-yaml-available)
             (fboundp 'yaml-mode)
             (not (derived-mode-p 'yaml-mode)))
        (yaml-mode))))))

;; (add-to-list 'auto-mode-alist
;;              (cons my-ansible-file-regexp
;;                    'my-ansible-detect-and-enable-ansible-mode))

(add-hook 'yaml-ts-mode-hook #'my-ansible-detect-and-enable-ansible-mode)
(add-hook 'yaml-mode-hook #'my-ansible-detect-and-enable-ansible-mode)

;;; Yaml-ts-mode: tab-width

(defun my-setup-yaml-mode ()
  "Config Yaml mode."
  ;; This patch has been merged in Emacs 32
  (when (< emacs-major-version 32)
    (setq-local tab-width 2)))

(add-hook 'yaml-ts-mode-hook #'my-setup-yaml-mode)

;;; Ansible: ansible-doc

(lightemacs-use-package ansible-doc
  :commands ansible-doc
  :init
  ;; (add-to-list 'display-buffer-alist '("\\*ansible-doc"
  ;;                                      (display-buffer-same-window)))
  (push '("\\*ansible-doc" (display-buffer-same-window)) display-buffer-alist))


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

(defun my-ansible-doc-local-setup-buffer ()
  "Setup `ansible-doc'."
  (setq-local evil-lookup-func 'ansible-doc-symbol))

(add-hook 'ansible-mode-hook 'my-ansible-doc-local-setup-buffer)

;;; Bash

(setq sh-basic-offset 2)
(defun setup-sh-mode ()
  "Setup `sh-mode'."
  (display-fill-column-indicator-mode)
  (unless (string-suffix-p ".ebuild" (buffer-file-name (buffer-base-buffer)))
    (my-set-tab-width sh-basic-offset)
    (setq-local fill-column 80)))

(add-hook 'sh-mode-hook #'setup-sh-mode)
(add-hook 'bash-ts-mode-hook #'setup-sh-mode)

;; use-package sh-mode
;; :ensure nil
;; :commands shell-script-mode
;; :mode (("\\.sh\\'" . shell-script-mode)
;;        ("\\.bash\\'" . shell-script-mode)
;;        ("\\.pbs\\'" . shell-script-mode))
;; :custom
(with-eval-after-load 'sh-script
  (when (fboundp 'sh-indent-supported)
    (sh-indent-supported (append sh-indent-supported '((bash . sh))))))

;;; css

;; (push '("\.[Cc][sS][sS]\\'" . css-ts-mode) auto-mode-alist)

;;; HTML

;; (lightemacs-use-package web-mode
;;   :commands web-mode
;;   ;; :mode "\\.html?\\'"
;;   ;; :mode "\\.css\\'"
;;   ;; :mode "\\.phtml\\'"
;;   ;; :mode "\\.tpl\\.php\\'"
;;   ;; :mode "\\.[agj]sp\\'"
;;   ;; :mode "\\.as[cp]x\\'"
;;   ;; :mode "\\.erb\\'"
;;   ;; :mode "\\.mustache\\'"
;;   ;; :mode "\\.djhtml\\'"
;;   ;; :mode "\\.php3\\'"
;;   ;; :mode "\\.php\\'"
;;   ;; :custom
;;   ;; (web-mode-enable-auto-pairing t)
;;   ;; ;; Code folding
;;   ;; (web-mode-enable-current-element-highlight t)
;;   ;; ;; (web-mode-enable-current-column-highlight t)
;;   ;; ;; (web-mode-enable-css-colorization t)
;;   ;; ;; (web-mode-enable-block-face t)
;;   ;; ;; (web-mode-enable-part-face t)
;;   ;; ;; (web-mode-enable-comment-interpolation t)
;;   ;; ;; (web-mode-enable-heredoc-fontification t)
;;   ;; (web-mode-markup-indent-offset 2)
;;   ;; (web-mode-css-indent-offset 2)
;;   ;; (web-mode-code-indent-offset 2)
;;   )

;;; Python

(defun setup-python-mode ()
  "Setup `python-mode'."
  (display-fill-column-indicator-mode)
  (my-set-tab-width 4)
  (setq-local fill-column 79))

(when (fboundp 'setup-python-mode)
  (add-hook 'python-mode-hook #'setup-python-mode)
  (add-hook 'python-ts-mode-hook #'setup-python-mode))

;;; jinja2-mode and csv-mode

;; (lightemacs-use-package jinja2-mode
;;    :commands jinja2-mode
;;    :mode ("\\.j2\\'" . jinja2-mode))

;;; ultisnips-mode

(lightemacs-use-package ultisnips-mode
  :commands ultisnips-mode
  :mode ("\\.snippets\\'" . ultisnips-mode))

;;; Jenkinsfile

(lightemacs-use-package groovy-mode
  :commands groovy-mode)

(lightemacs-use-package jenkinsfile-mode
  :commands jenkinsfile-mode
  :mode
  (("/Jenkinsfile[^/]*\\'" . jenkinsfile-mode)
   ("/Jenkinsfile\\'" . jenkinsfile-mode)))

;;; BASIC

(lightemacs-use-package basic-mode
  :commands basic-qb45-mode
  :init
  ;; Djgpp and Rhide
  (add-to-list 'file-coding-system-alist '("/legacy/.*\\.C\\'" . cp437-dos))
  (add-to-list 'file-coding-system-alist '("/legacy/.*\\.H\\'" . cp437-dos))
  (add-to-list 'file-coding-system-alist '("\\.[bB][aA][sS]\\'" . cp437-dos))
  ;; *.BAS files
  (add-to-list 'auto-mode-alist '("\\.[bB][aA][sS]\\'" . basic-qb45-mode)))

;;; auto-mode-alist

(nconc auto-mode-alist
       '(;; conf-mode
         ;; ("\\.profile\\'" . conf-mode)  ; firejail profiles
         ("^/etc/[^/]+" . simple-conf-mode)

         ("/known_hosts\\'" . conf-space-mode)

         ("/COMMIT_EDITMSG\\'" . diff-mode)
         ("\\.[Oo][Rr][Gg]\\.[aA][sS][cC]\\'" . org-mode)

         ;; Gentoo (/etc/portage files)
         ("package\\.\\(?:license\\|mask\\|use\\|accept_keywords\\)/.+\\'" . conf-unix-mode)
         ("package\\.\\(?:env\\|unmask\\)\\'" . conf-unix-mode)

         ;; /etc/hosts and ansible /hosts

         ;; Git

         ;; hexl-mode
         ;; ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)

         ;; txt-file-mode
         ("\\.ignore\\'" . conf-unix-mode)
         ("\\.fdignore\\'" . conf-unix-mode)
         ("\\.rgignore\\'" . conf-unix-mode)

         ("\\.log\\'" . txt-file-mode)))

(push '("/\\.gitconfig\\.local\\'" . gitconfig-mode) auto-mode-alist)
(push '("/\\.gitattributes\\.local\\'" . gitattributes-mode) auto-mode-alist)

;;; Markdown

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

  (let ((inhibit-message t))
    (toggle-truncate-lines 0)))

;;; Setup markdown mode

(add-hook 'markdown-mode-hook #'my-setup-markdown-mode)
(push '("\\.md\\.asc\\'" . markdown-mode) auto-mode-alist)

(setq markdown-nested-imenu-heading-index nil)

;; Uncomment this if you use Roam, Obsidian, or Logseq, as it enables proper
;; fontification and navigation for [[WikiLinks]].
(setq markdown-enable-wiki-links t)

;; Uncomment this if you work with static site generators (like Hugo or
;; Jekyll) or Pandoc. It ensures the YAML frontmatter at the top of your
;; markdown files is correctly syntax-highlighted.
(setq markdown-use-pandoc-style-yaml-metadata t)

;; Lock list indentation to 2 spaces. When you hit Tab to nest a list item
;; under a dash, it aligns perfectly with a 2-space structure, matching
;; standard configuration habits (like your YAML spacing).
(setq markdown-list-indent-width 2)

(setq markdown-disable-tooltip-prompt t)
(setq markdown-split-window-direction 'right)

;; Automates your formatting standard. When you press M-RET
;; (markdown-insert-list-item), Emacs will insert the dash automatically
;; rather than the default asterisk.
(setq markdown-unordered-list-item-prefix "- ")

;; Enables Previewing: Without configuring markdown-command, features like
;; markdown-preview (C-c C-c p) or markdown-export will fail if Emacs cannot
;; find a default compiler on your system path.
;;
;; Advanced Syntax: multimarkdown supports robust extensions that standard
;; Markdown lacks, such as native tables, footnotes, and metadata blocks.
(setq markdown-command "multimarkdown")

(setq markdown-fontify-whole-heading-line t)

(setq markdown-gfm-use-electric-backquote nil)
(setq markdown-header-scaling t)

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "TAB") #'ignore))

(defun my-markdown-toc-gen-if-present ()
  "Gen table of contents if present."
  (when (and (fboundp 'markdown-toc--toc-already-present-p)
             (fboundp 'markdown-toc-generate-toc)
             (markdown-toc--toc-already-present-p))
    (markdown-toc-generate-toc)))

(defun my-setup-markdown-toc ()
  "Setup the markdown-toc package."
  (when (fboundp 'my-markdown-toc-gen-if-present)
    (add-hook 'before-save-hook #'my-markdown-toc-gen-if-present 99 t)))

(when (fboundp 'my-setup-markdown-toc)
  (add-hook 'markdown-mode-hook #'my-setup-markdown-toc))

(setq markdown-toc-mode-map nil)
(setq markdown-toc-header-toc-title "## Table of Contents")

;;; custom-modes: Simple conf mode

(defvar simple-conf-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; The # character starts a comment (< is the comment start syntax class).
    (modify-syntax-entry ?# "<" table)
    ;; A newline (\n) ends the comment (> is the syntax class for comment end).
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `simple-conf-mode'.")

(defvar simple-conf-mode-font-lock-keywords
  '(("#.*$" . font-lock-comment-face))
  "Font lock keywords for `simple-conf-mode'.")

(define-derived-mode simple-conf-mode nil "SimpeConfMode"
  "Major mode to highlight only # comments."
  (setq font-lock-defaults '(simple-conf-mode-font-lock-keywords))
  (set-syntax-table simple-conf-mode-syntax-table))

;;; Deferred Fallbacks Loader

(defun my-load-treesit-fallbacks ()
  "Initialize tree-sitter mode remaps and fallbacks after UI is drawn."
  (my-remap-ts-mode 'c-mode 'c-ts-mode 'c)
  (my-remap-ts-mode 'c++-mode 'c++-ts-mode 'cpp)
  (my-remap-ts-mode 'js-json-mode 'json-ts-mode 'json)
  (my-remap-ts-mode 'conf-toml-mode 'toml-ts-mode 'toml)

  (if (my-treesit-language-available-p 'php)
      (progn
        (my-remap-ts-mode 'php-mode 'php-ts-mode 'php)
        (my-auto-mode-ts "\\.[pP][hH][pP]\\'" 'php-ts-mode 'php-mode 'php)
        (my-auto-mode-ts "\\.[pP][hH][pP]3\\'" 'php-ts-mode 'php-mode 'php))
    (require 'sub-php-mode))

  (my-remap-ts-mode 'shell-script-mode 'bash-ts-mode 'bash)
  (my-remap-ts-mode 'sh-mode 'bash-ts-mode 'bash)
  (my-auto-mode-ts "/make\\.conf\\'" 'bash-ts-mode 'sh-mode 'bash)

  (my-remap-ts-mode 'css-mode 'css-ts-mode 'css)

  (my-remap-ts-mode 'js2-mode 'js-ts-mode 'javascript)
  (my-remap-ts-mode 'js-mode 'js-ts-mode 'javascript)
  (my-auto-mode-ts "\\.[jJ][sS]\\'" 'js-ts-mode 'js-mode 'javascript)
  (my-auto-mode-ts "/\\.ipynb\\'" 'json-ts-mode 'js-json-mode 'json)

  (if (my-treesit-language-available-p 'lua)
      (progn
        (my-auto-mode-ts "\\.[lL][uU][aA]\\'" 'lua-ts-mode nil 'lua))
    (require 'sub-lua-mode))

  (my-auto-mode-ts "/[dD][oO][cC][kK][eE][rR]\\'" 'dockerfile-ts-mode nil 'dockerfile)
  (my-auto-mode-ts "/[Cc][Oo][Nn][Tt][Aa][Ii][Nn][Rr][fF][iI][lL][eE]\\'" 'dockerfile-ts-mode nil 'dockerfile)
  (my-auto-mode-ts "/[dD][oO][cC][kK][eE][rR][fF][iI][lL][eE]\\'" 'dockerfile-ts-mode nil 'dockerfile)

  (if (my-treesit-language-available-p 'html)
      (progn
        (my-remap-ts-mode 'html-mode 'html-ts-mode 'html)
        (my-remap-ts-mode 'mhtml-mode 'mhtml-ts-mode 'html)
        (my-auto-mode-ts "\\.[hH][tT][mM][lL]\\'" 'mhtml-ts-mode 'mhtml-mode 'html)
        (my-auto-mode-ts "\\.[Pp][hH][tT][mM][lL]\\'" 'mhtml-ts-mode 'mhtml-mode 'html))
    (use-package sgml-mode
      :ensure nil
      :commands (sgml-mode sgml-electric-tag-pair-mode sgml-name-8bit-mode)
      :hook
      (html-mode . sgml-electric-tag-pair-mode)
      (mhtml-mode . sgml-electric-tag-pair-mode)
      (html-mode . sgml-name-8bit-mode)
      (mhtml-mode . sgml-name-8bit-mode)))

  (my-remap-ts-mode 'python-mode 'python-ts-mode 'python)

  (when (and (> emacs-major-version 30)
             (my-treesit-language-available-p 'markdown))
    (add-hook 'markdown-ts-mode-hook 'outline-minor-mode)
    (add-hook 'markdown-ts-mode-hook #'my-setup-markdown-mode)))

(add-hook 'after-init-hook #'my-load-treesit-fallbacks)

;;; Provide

(provide 'mod-filetype)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; env-allow-syntax-checker-package-lint: nil
;; End:

;;; mod-filetype.el ends here

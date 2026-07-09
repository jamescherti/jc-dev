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

;;; Filetype defaults

(setq sgml-basic-offset 2)  ;; HTML
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq javascript-indent-level 2)
(setq html-indent-offset 2)
(setq sgml-basic-offset 2)
(setq lua-indent-level 2)
(setq lua-ts-indent-offset 2)
(setq yaml-indent-offset 2)

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

;; Bad idea. It loads too many modes.
;; (setq initial-major-mode 'txt-file-mode)
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
  ;; :mode
  ;; ("/vim\\(rc\\)?\\'" . vimrc-mode)
  ;; ("\\.vim\\(rc\\)?\\'" . vimrc-mode)
  ;; ("\\.vimrc.local?\\'" . vimrc-mode)
  ;; ("\\.lvimrc?\\'" . vimrc-mode)
  ;; ("/\\.vim\\(rc\\)?\\'" . vimrc-mode)
  )

(add-hook 'vimrc-mode-hook #'(lambda ()
                               (setq-local indent-tabs-mode nil)
                               (if (fboundp 'my-set-tab-width)
                                   (my-set-tab-width 2)
                                 (error "Undefined: my-set-tab-width"))))

;; ("/\\.vimrc.local?\\'" . vimrc-mode)
;; March .vimrc, .vimrc.local, .vim-after, .vim-before...
;; ("/\\.l?vim[^/]*\\'" . vimrc-mode)
;; ("/\\.lvimrc?\\'" . vimrc-mode)
(add-to-list 'auto-mode-alist '("/\\.l?vim\\(rc\\)?\\([^/]*\\)?\\'" . vimrc-mode))

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

;; (defun my-treesit-require-and-override (feature)
;;   "Require FEATURE and override `treesit-language-source-alist' with its entries."
;;   (let ((new-entries nil))
;;     (let ((treesit-language-source-alist nil))
;;       (require feature)
;;       (setq new-entries treesit-language-source-alist))
;;     (dolist (entry new-entries)
;;       (setq treesit-language-source-alist
;;             (assq-delete-all (car entry) treesit-language-source-alist))
;;       (push entry treesit-language-source-alist))))

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
  (mod-filetype-install 'python 'python)
  (mod-filetype-install 'sh-script 'bash)
  (mod-filetype-install 'json-ts-mode 'json)
  (mod-filetype-install 'lua-ts-mode 'lua)
  ;; (mod-filetype-install 'c-ts-mode '(c cpp))
  (mod-filetype-install 'dockerfile-ts-mode 'dockerfile)
  ;; (mod-filetype-install 'go-ts-mode 'go)
  (mod-filetype-install 'js 'javascript)
  ;; (mod-filetype-install 'java-ts-mode 'java)
  (mod-filetype-install 'php-ts-mode 'php 'php-ts-mode-install-parsers)
  (mod-filetype-install 'markdown-mode '(markdown markdown-inline)
                        'markdown-ts-mode-install-parsers)
  (mod-filetype-install 'mhtml-ts-mode 'html
                        'mhtml-ts-mode-install-parsers)

  (mod-filetype-install 'python-ts-mode 'python)
  (mod-filetype-install 'yaml-ts-mode 'yaml)
  (mod-filetype-install 'bash-ts-mode 'bash)
  (mod-filetype-install 'lua-ts-mode 'lua)
  (mod-filetype-install 'json-ts-mode 'json)
  (mod-filetype-install 'c-ts-mode 'c)
  (mod-filetype-install 'c-ts-mode 'cpp)
  (mod-filetype-install 'dockerfile-ts-mode 'dockerfile)

  ;; (mod-filetype-install 'go-ts-mode 'go)
  ;; (mod-filetype-install 'java-ts-mode 'java)

  ;; TODO
  ;; (mod-filetype-install 'java-ts-mode 'javascript)

  (mod-filetype-install nil '(javascript))

  ;; (treesit-install-language-grammar 'python)
  ;; (treesit-install-language-grammar 'bash)
  ;; (treesit-install-language-grammar 'yaml)
  ;; (treesit-install-language-grammar 'json)
  ;; (treesit-install-language-grammar 'lua)
  ;; (treesit-install-language-grammar 'c)
  ;; (treesit-install-language-grammar 'cpp)
  ;; (treesit-install-language-grammar 'dockerfile)
  ;; (treesit-install-language-grammar 'go)
  ;; (treesit-install-language-grammar 'java)
  ;; (treesit-install-language-grammar 'javascript)
  ;;
  ;; (if (fboundp 'mhtml-ts-mode-install-parsers)
  ;;     (mhtml-ts-mode-install-parsers)
  ;;   (treesit-install-language-grammar 'html))
  ;;
  ;; (if (fboundp 'markdown-ts-mode-install-parsers)
  ;;     (markdown-ts-mode-install-parsers)
  ;;   (treesit-install-language-grammar 'markdown)
  ;;   (treesit-install-language-grammar 'markdown-inline))
  ;;
  ;; (if (fboundp 'php-ts-mode-install-parsers)
  ;;     (php-ts-mode-install-parsers)
  ;;   (treesit-install-language-grammar 'php))

  )

;;; Misc languages

(when (my-treesit-language-available-p 'c)
  (push '(c-mode . c-ts-mode) major-mode-remap-alist))

(when (my-treesit-language-available-p 'cpp)
  (push '(c++-mode . c++-ts-mode) major-mode-remap-alist))

(when (my-treesit-language-available-p 'json)
  (push '(js-json-mode . json-ts-mode) major-mode-remap-alist))

(when (my-treesit-language-available-p 'java)
  (push '(java-mode . java-ts-mode) major-mode-remap-alist))

;; (when (my-treesit-language-available-p 'go)
;;   (add-to-list 'auto-mode-alist '("\.[gG][oO]\\'" . go-ts-mode)))

;;; Yaml and Ansible

(defvar treesit-yaml-available (my-treesit-language-available-p 'yaml))

;; Must be evaluated before Org is loaded
(with-eval-after-load 'org
  (if (and (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'yaml))
      (push (cons "yaml" 'yaml-ts) org-src-lang-modes)
    (push (cons "yaml" 'yaml) org-src-lang-modes)))

(defun my-setup-yaml-mode ()
  "Config Yaml mode."
  ;; TODO put it back
  ;; (setq-local indent-line-function 'smartindent-indent-relative-to-visible)
  t
  )

(with-suppressed-warnings ((free-vars flymake-yamllint-arguments)
                           (free-vars yaml-ts-mode-yamllint-options))
  (setq flymake-yamllint-arguments
        (list "-c" (expand-file-name "~/.yamllint_global.yml")))
  (setq yaml-ts-mode-yamllint-options
        (copy-sequence flymake-yamllint-arguments)))

(if treesit-yaml-available
    (progn
      ;; (with-eval-after-load 'mod-cleanup
      ;;   (push 'flymake-yamllint mod-cleanup-packages-list)
      ;;   (push 'yaml-mode mod-cleanup-packages-list))

      (define-derived-mode ansible-mode yaml-ts-mode "Ansible"
        "Major mode for editing Ansible files.")

      ;; Remove the auto-mode-alist entry (Useful to prevent yaml-ts-mode from
      ;; activating on ansible-mode)
      (with-eval-after-load 'yaml-ts-mode
        (setq auto-mode-alist
              (rassq-delete-all 'yaml-ts-mode auto-mode-alist))

        (push '(yaml-mode . yaml-ts-mode) major-mode-remap-alist))
      (add-hook 'yaml-ts-mode-hook #'my-setup-yaml-mode))
  ;; non tree sitter
  (require 'sub-flymake-yamllint)

  (when (fboundp 'yaml-mode)
    (define-derived-mode ansible-mode yaml-mode "Ansible"
      "Major mode for editing Ansible files.")))

;;; Ansible `ansible-mode'

(if treesit-yaml-available
    ;; Non tree-sitter
    (define-derived-mode ansible-mode yaml-ts-mode "Ansible"
      "Major mode for editing Ansible files.")
  ;; Non tree-sitter
  (when (fboundp 'yaml-mode)
    (define-derived-mode ansible-mode yaml-mode "Ansible"
      "Major mode for editing Ansible files.")))

;;; Ansible: Setup

(defun my-setup-ansible-mode ()
  "Setup `ansible-mode'."
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
  ;; (modify-syntax-entry ?$ ".")

  ;; Ensures that (.), (,) and (!) are treated as part of symbols or words
  ;; within YAML documents. In YAML, these characters may be used as part
  ;; of keys in quoted strings.
  ;;
  ;; (.) is for symbols such as: ansible.builtin.command
  (modify-syntax-entry ?. "_")
  (modify-syntax-entry ?, "_")
  (modify-syntax-entry ?! "_"))

(add-hook 'ansible-mode-hook #'my-setup-ansible-mode)

;;; Ansible: Auto detect

(defvar my-ansible-file-regexp (rx "/"
                                   (group (or "tasks"
                                              "handlers"
                                              "vars"
                                              "defaults"
                                              "ansible"
                                              "playbooks"))
                                   "/" (+ (not (any "/\\")))
                                   "." (regexp "[yY][aA]?[mM][lL]")
                                   string-end))

;; When auto-mode-alist is bypassed, use a hook function
(defun ansible-detect-and-enable-mode ()
  "Enable `ansible-mode' for YAML files in Ansible-related directories."
  ;; This works better than auto-mode-alist
  (when (and (not (derived-mode-p 'ansible-mode))
             (buffer-file-name (buffer-base-buffer))
             (string-match my-ansible-file-regexp buffer-file-name)
             (fboundp 'ansible-mode))
    (ansible-mode)))

(add-to-list 'auto-mode-alist (cons my-ansible-file-regexp 'ansible-mode))
(add-hook 'yaml-mode-hook #'ansible-detect-and-enable-mode)
(add-hook 'yaml-ts-mode-hook #'ansible-detect-and-enable-mode)


;;; Ansible: ansible-doc

(lightemacs-use-package ansible-doc
  :commands ansible-doc
  :init
  (add-to-list 'display-buffer-alist '("\\*ansible-doc"
                                       (display-buffer-same-window))))


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

;;; PHP

(if (my-treesit-language-available-p 'php)
    (progn
      (push '(php-mode . php-ts-mode) major-mode-remap-alist)
      (add-to-list 'auto-mode-alist '("\\.[pP][hH][pP]\\'" . php-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.[pP][hH][pP]3\\'" . php-ts-mode)))
  (require 'sub-php-mode))

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

(if (my-treesit-language-available-p 'bash)
    (progn
      (push '(shell-script-mode . bash-ts-mode) major-mode-remap-alist)
      (push '(sh-mode . bash-ts-mode) major-mode-remap-alist))
  ;; use-package sh-mode
  ;; :ensure nil
  ;; :commands shell-script-mode
  ;; :mode (("\\.sh\\'" . shell-script-mode)
  ;;        ("\\.bash\\'" . shell-script-mode)
  ;;        ("\\.pbs\\'" . shell-script-mode))
  ;; :custom
  (with-eval-after-load 'sh-script
    (when (fboundp 'sh-indent-supported)
      (sh-indent-supported (append sh-indent-supported '((bash . sh)))))))

;;; Javascript

(if (my-treesit-language-available-p 'javascript)
    (progn
      (push '(js2-mode . js-ts-mode) major-mode-remap-alist)
      (push '(js-mode . js-ts-mode) major-mode-remap-alist)
      (add-to-list 'auto-mode-alist '("\.[jJ][sS]\\'" . js-ts-mode)))
  (progn
    (add-to-list 'auto-mode-alist '("\.[jJ][sS]\\'" . js-mode))

    ;; Not required
    ;; (use-package js2-mode
    ;;   :commands js2-mode
    ;;   ;; :mode
    ;;   ;; ("\\.js\\'" . js2-mode)
    ;;   )
    ))


;;; Lua

(if (my-treesit-language-available-p 'lua)
    (add-to-list 'auto-mode-alist '("\\.[lL][uU][aA]\\'" . lua-ts-mode))
  (require 'sub-lua-mode))

;;; Dockerfile

(if (my-treesit-language-available-p 'dockerfile)
    (progn
      (add-to-list 'auto-mode-alist
                   '("/[dD][oO][cC][kK][eE][rR]\\'"
                     . dockerfile-ts-mode))
      (add-to-list 'auto-mode-alist
                   '("/[Cc][Oo][Nn][Tt][Aa][Ii][Nn][Rr][fF][iI][lL][eE]\\'"
                     . dockerfile-ts-mode))
      (add-to-list 'auto-mode-alist
                   '("/[dD][oO][cC][kK][eE][rR][fF][iI][lL][eE]\\'"
                     . dockerfile-ts-mode)))
  ;;(use-package dockerfile-mode
  ;;  :defer t
  ;;  :commands dockerfile-mode
  ;;  :init
  ;;  ;; For some reason, this path is not automatically added to load-path
  ;;  (add-to-list 'load-path (expand-file-name "dockerfile-mode"
  ;;                                            emacs-packages-dir))
  ;;  (add-to-list 'auto-mode-alist
  ;;               (cons (concat "[/\\]"
  ;;                             "\\(?:Containerfile\\|Dockerfile\\)"
  ;;                             "\\(?:\\.[^/\\]*\\)?\\'")
  ;;                     'dockerfile-mode)))
  t)

;;; HTML

(if (my-treesit-language-available-p 'html)
    (progn
      (push '(html-mode . html-ts-mode) major-mode-remap-alist)
      (add-to-list 'auto-mode-alist '("\\.[hH][tT][mM][lL]\\'" . html-ts-mode)))
  ;; (use-package web-mode
  ;;   :commands web-mode
  ;;   :mode "\\.html?\\'"
  ;;   :mode "\\.css\\'"
  ;;   :mode "\\.phtml\\'"
  ;;   :mode "\\.tpl\\.php\\'"
  ;;   :mode "\\.[agj]sp\\'"
  ;;   :mode "\\.as[cp]x\\'"
  ;;   :mode "\\.erb\\'"
  ;;   :mode "\\.mustache\\'"
  ;;   :mode "\\.djhtml\\'"
  ;;   :mode "\\.php3\\'"
  ;;   :mode "\\.php\\'"
  ;;   :custom
  ;;   (web-mode-enable-auto-pairing t)
  ;;   ;; Code folding
  ;;   (web-mode-enable-current-element-highlight t)
  ;;   ;; (web-mode-enable-current-column-highlight t)
  ;;   ;; (web-mode-enable-css-colorization t)
  ;;   ;; (web-mode-enable-block-face t)
  ;;   ;; (web-mode-enable-part-face t)
  ;;   ;; (web-mode-enable-comment-interpolation t)
  ;;   ;; (web-mode-enable-heredoc-fontification t)
  ;;   (web-mode-markup-indent-offset 2)
  ;;   (web-mode-css-indent-offset 2)
  ;;   (web-mode-code-indent-offset 2))

  (use-package sgml-mode
    :ensure nil
    :commands (sgml-mode
               sgml-electric-tag-pair-mode
               sgml-name-8bit-mode)
    :hook
    (html-mode . sgml-electric-tag-pair-mode)
    (mhtml-mode . sgml-electric-tag-pair-mode)
    (html-mode . sgml-name-8bit-mode)
    (mhtml-mode . sgml-name-8bit-mode)))

;;; Python: major-mode remap alist

(when (my-treesit-language-available-p 'python)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist))

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
;;   :commands jinja2-mode
;;   :mode ("\\.j2\\'" . jinja2-mode))

;;; ultisnips-mode

(lightemacs-use-package ultisnips-mode
  :commands ultisnips-mode
  :mode ("\\.snippets\\'" . ultisnips-mode))

;;; Jenkinsfile
;; (lightemacs-use-package jenkinsfile-mode
;;   :commands jenkinsfile-mode
;;   :mode
;;   (("/Jenkinsfile[^/]*\\'" . jenkinsfile-mode)
;;    ("/Jenkinsfile\\'" . jenkinsfile-mode))
;;   :init
;;   ;; (add-to-list 'auto-mode-alist '("/Jenkinsfile.*\\'" . jenkinsfile-mode))
;;   ;; (add-to-list 'auto-mode-alist '("Jenkinsfile[^/]*\\'" . jenkinsfile-mode))
;;   ;; (add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . jenkinsfile-mode))
;;   )

;;; BASIC
;; (lightemacs-use-package basic-mode
;;   :commands (cp437-dos
;;              basic-qb45-mode)
;;   :init
;;   ;; (setq default-buffer-file-coding-system 'cp437-dos)
;;
;;   ;; Djgpp and rhide
;;   (add-to-list 'file-coding-system-alist '("\\.C\\'" . cp437-dos))
;;   (add-to-list 'file-coding-system-alist '("\\.H\\'" . cp437-dos))
;;
;;   (add-to-list 'file-coding-system-alist '("\\.[bB][aA][sS]\\'" . cp437-dos))
;;
;;   ;; (autoload 'basic-generic-mode "basic-mode" "Major mode for editing BASIC
;;   ;; code." t)
;;   (add-to-list 'auto-mode-alist '("\\.[bB][aA][sS]\\'" . basic-qb45-mode)))

;;; auto-mode-alist

;; TODO minimal-emacs readme?
(add-to-list 'auto-mode-alist '("/LICENSE\\'" . text-mode))
(add-to-list 'auto-mode-alist '("rc\\'" . conf-mode) 'append)

;; This regular expression matches the full file path for any .conf file
;; residing within either /etc/fonts/ or .config/fontconfig/ and maps them
;; directly to xml-mode.
(add-to-list 'auto-mode-alist '("/etc/fonts/.*\\.conf\\'" . xml-mode))
(add-to-list 'auto-mode-alist
             (cons (concat
                    (regexp-quote (expand-file-name "~/.config/fontconfig/"))
                    ".*\\.conf\\'")
                   'xml-mode))

(nconc auto-mode-alist
       '(;; conf-mode
         ("\\.profile\\'" . conf-mode)  ; firejail profiles
         ("^/etc/[^/]+" . conf-unix-mode)

         ("/known_hosts\\'" . conf-space-mode)

         ("/COMMIT_EDITMSG\\'" . diff-mode)
         ("\\.[Oo][Rr][Gg]\\.[aA][sS][cC]\\'" . org-mode)

         ;; Gentoo
         ("/make\\.conf\\'" . sh-mode)

         ;; Gentoo (/etc/portage files)
         ("package\\.\\(?:license\\|mask\\|use\\|accept_keywords\\)/.+\\'" . conf-unix-mode)
         ("package\\.\\(?:env\\|unmask\\)\\'" . conf-unix-mode)

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

;;; Provide

(provide 'mod-filetype)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-filetype.el ends here

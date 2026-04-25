;;; mod-misc.el --- mod-misc -*- lexical-binding: t -*-

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
(eval-and-compile
  (require 'lightemacs-use-package))
(require 'seq)
(require 'my-defun)

;;; testing

(setq global-auto-revert-non-file-buffers nil)

;;; Delete unused packages

(defun my-delete-unused-packages ()
  "Delete unused packages."
  (when (eq lightemacs-package-manager 'builtin-package)
    (require 'package)
    (when (fboundp 'package-delete)
      (dolist (item '(olivetti
                      posframe
                      rainbow-mode
                      with-editor
                      lsp-mode
                      git-timemachine
                      focus
                      spinner
                      shut-up
                      llama
                      lv
                      jenkinsfile-mode
                      basic-mode
                      highlight-numbers
                      ;; TODO remove on mac
                      exec-path-from-shell
                      groovy-mode
                      php-mode
                      erefactor
                      parent-mode
                      popup
                      xclip
                      tempel
                      tempel-collection
                      stillness-mode
                      golden-ratio
                      tabgo
                      ws-butler
                      quickrun
                      xterm-color
                      ztree
                      vundo
                      flyspell-lazy
                      vterm
                      eat
                      magit
                      magit-section
                      ace-window))
        (let ((desc (cadr (assq item package-alist))))
          (if (not desc)
              (when init-file-debug
                (message "Package %s not found in alist (already deleted?)" item))
            (condition-case err
                (progn
                  (package-delete desc)
                  (message "Successfully deleted: %s" item))
              (error
               ;; This captures the actual error message from Emacs
               (message "Failed to delete %s: %s: %s"
                        item
                        (error-message-string err)
                        desc)))))))))

;; scroll-margin: Setting this to 0 ensures that the cursor can sit on the
;; absolute top or bottom line of the window. If this is set to a positive
;; integer (like 3 or 5), Emacs will force the screen to scroll before you reach
;; the edge.
;; (setq scroll-margin 0)

;; If you hate the "0.5 character" margin you mentioned earlier, be careful.
;; Using window-resize-pixelwise can actually create that half-character look at
;; the bottom of your windows more often, because the window height is no longer
;; forced to be a multiple of your line height.
;; (setq window-resize-pixelwise t)

;; removed
;; (setq tramp-completion-reread-directory-timeout 50)
;; (setq remote-file-name-inhibit-cache 50)

;; Removed it from m.e
;; (setq next-screen-context-lines 0): Setting this to 0 can make it
;; disorienting to track your position when paging down. The default value of 2
;; provides better visual continuity when reading large files.
;; TODO check again

;;
;; Number of lines of continuity when scrolling by screenfuls.
;; (setq next-screen-context-lines 0)

;; removed from minimal-emacs.d
;; (setq savehist-save-minibuffer-history t)
;; (setq window-resize-pixelwise nil)
;; (setq x-stretch-cursor nil)
;; (setq recentf-exclude nil)

;; Disable visual indicators in the fringe for buffer boundaries and empty lines
;; (setq-default indicate-buffer-boundaries nil)
;; (setq-default indicate-empty-lines nil)

;; Number of lines of margin at the top and bottom of a window.
;; (setq scroll-margin 0)

;;; gc sentinel

(lightemacs-use-package gcsentinel
  :ensure nil
  :commands gcsentinel-mode
  :init
  (add-hook 'lightemacs-emacs-startup-hook #'gcsentinel-mode 200)
  (setq gcsentinel-low-cons-threshold minimal-emacs-gc-cons-threshold))

;;; Target hooks

(setq lightemacs-buffer-terminator-target-hooks '())

(setq lightemacs-aggressive-indent-target-hooks '(emacs-lisp-mode-hook))

(setq lightemacs-stripspace-target-hooks '(prog-mode-hook))

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

;;; straight

;; (defvar my-straight-default-profile (expand-file-name
;;                                      "~/.emacs-data/etc/straight-profile.el")
;;   "The default straight profile.")
;; (setq straight-profiles
;;       `((nil . ,my-straight-default-profile)))

;;; Packages: use-package

;; (defun my-package-pin (package repository)
;;   (setq package-pinned-packages
;;         (assq-delete-all package package-pinned-packages))
;;   (add-to-list 'package-pinned-packages (list (cons package repository))))

(defun my-update-package-pinned-packages (pinned-packages)
  "Update `package-pinned-packages\=' with the entries in PINNED-PACKAGES.
This replaces existing entries that match the provided packages and appends
any new ones."
  (when (eq lightemacs-package-manager 'use-package)
    (setq package-pinned-packages (append pinned-packages
                                          (seq-remove
                                           (lambda (pkg)
                                             (assq (car pkg) pinned-packages))
                                           package-pinned-packages)))))

(setq my-package-pinned-packages
      '((buffer-terminator             . "melpa")
        (dir-config                    . "melpa")
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

        (git-gutter                    . "melpa")

        (visual-fill-column            . "melpa")

        ;; lightemacs?
        (undo-fu                       . "melpa")
        (undo-fu-session               . "melpa")

        ;; To fix the window-start bug
        (apheleia                      . "melpa-stable")

        ;; 3 months ago
        ;; (gptel                         . "melpa")
        ))

(defvar my-shared-user-emacs-directory (expand-file-name "~/.emacs-data/var"))


;;; config

(defun my-evil-config ()
  "Setup evil."
  ;; Make `v$` exclude the final newline
  (setq evil-v$-excludes-newline t)

  ;; Prevent Evil state from being echoed, preserving Eldoc display in the
  ;; minibuffer (If set to t, Eldoc output in the minibuffer will be overridden)
  (setq evil-echo-state nil)

  ;; Enable automatic horizontal split below
  (setq evil-split-window-below t)

  ;; Enable automatic vertical split to the right
  (setq evil-vsplit-window-right t)

  ;; Enable fine-grained undo behavior
  (setq evil-want-fine-undo t)

  ;; Required by evil-collection

  ;; Do not move cursor back when exiting insert state
  (setq evil-move-cursor-back nil)

  ;; Only complete in the current buffer
  (setq evil-complete-all-buffers nil)

  (setq evil-command-window-height 8)
  (setq evil-display-shell-error-in-message nil)

  ;; Controls whether evil-collection defines Vim-unimpaired-style keybindings
  ;; (setq evil-collection-want-unimpaired-p nil)
  (setq evil-collection-calendar-want-org-bindings t)

  (setq tooltip-hide-delay 20) ;; seconds
  (setq tooltip-delay 0.4)
  (setq tooltip-short-delay 0.08)

  ;; TODO is this good?
  (setq mouse-wheel-progressive-speed nil) ; disable acceleration of scrolling
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . hscroll) ((meta))
          ((control meta) . global-text-scale)
          ((control) . text-scale)))

  (setq inhibit-mouse-button-numbers '(1 2 3))
  (setq pixel-scroll-precision-use-momentum nil))

(defun my-set-tab-width (width)
  "Set the tab width.
WIDTH is the tab width."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width width)
  (setq-local standard-indent width)
  ;; (setq-local evil-shift-width width)
  )

(defun my-setup-filetype ()
  "Setup filetype."
  (add-to-list 'auto-mode-alist '("\\.[Oo][Rr][Gg]\\.[aA][sS][cC]\\'" . org-mode))
  (defun my-org-mode-setup ()
    "When active, indent text according to outline structure."
    ;; (auto-fill-mode -1)

    ;; In org buffers we set `nobreak-char-display' to nil locally so that the
    ;; Unicode no-break space (U+00A0) is rendered just like a regular ASCII
    ;; space. This suppresses the distinct glyph or face Emacs normally applies
    ;; to NBSP, keeping the buffer free of distracting blue highlights while
    ;; preserving the character's internal no-break semantics.
    ;;
    ;; Here is an example of what is highlighted: $5 billion-valued.
    ;; When `nobreak-char-display' is non-nil, the non-breaking space after `5`
    ;; and the hyphen after n are rendered as highlighted glyphs.
    (setq-local nobreak-char-display nil)

    ;; TODO: bug. When jumping to org file from org agenda todo list,
    ;; org-indent-mode is not enabled by default.
    (when (fboundp 'org-indent-mode)
      (org-indent-mode 1))

    ;; (display-line-numbers-mode -1)

    (when (derived-mode-p 'org-mode)
      ;; It makes o not auto indent after a bullet list like * or -
      (setq-local evil-auto-indent nil)

      ;; (setq-local indent-line-function nil)
      ;; (setq-local search-invisible nil)

      ;; Fixes a bug of jumping in org mode when scrolling many lines in my
      ;; file
      ;; TODO: bug?
      ;; (setq-local scroll-margin 1)

      ;; (toggle-truncate-lines 0)

      ;; (custom-set-faces `(org-block ((t (:height 0.7)))))
      ;; (custom-set-faces `(org-block-begin-line ((t (:height 0.6)))))
      ;; (custom-set-faces `(org-block-end-line ((t (:height 0.6 :extend t)))))
      ))

  (when (fboundp 'my-org-mode-setup)
    (add-hook 'org-mode-hook #'my-org-mode-setup))

  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-clock-report-include-clocking-task t)

  (setq org-fold-show-context-detail
        '(;; 'local' reveals the current heading but keeps children folded.
          ;; Useful to focus strictly on the agenda item without visual clutter.
          ;; (agenda . local)

          ;; This fixes:
          ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-08/msg01128.html
          ;; TODO patch org?
          ;;
          ;; 'canonical' reveals the current headline, its direct ancestors, and
          ;; its immediate children. This is ideal for searching. It gives you
          ;; enough structural context to know exactly where you are in the
          ;; document hierarchy without unfolding the entire tree.
          (isearch . canonical)

          ;; when exposing a bookmark location 'canonical' is highly useful for
          ;; bookmarks that point to project roots or major category headers,
          ;; allowing you to see the immediate contents upon jumping.
          (bookmark-jump . canonical)

          ;; when using the command org-occur (C-c / /)
          ;; 'canonical' is useful here because it shows the immediate children
          ;; of the matched headings, providing a broader overview of the
          ;; matched section in your sparse tree rather than just an isolated
          ;; line.
          (occur-tree . canonical)

          ;; When using the command org-goto (C-c C-j)
          ;; 'canonical' is useful here if you frequently jump to parent
          ;; headings and immediately need to see their sub-headings to navigate
          ;; further.
          ;; (org-goto . canonical)

          ;; when constructing a sparse tree based on tags matches 'canonical'
          ;; is useful if your tags are applied to high-level categories and you
          ;; want the sparse tree to automatically reveal the specific items
          ;; underneath them.
          ;; (tags-tree . canonical)

          ;; when exposing search matches associated with a link 'canonical' is
          ;; useful if your internal links frequently point to index or parent
          ;; nodes and you want to see the associated subcategories immediately
          ;; upon arrival.
          ;; (link-search . canonical)

          ;; when exposing the jump goal of a mark 'canonical' helps re-orient
          ;; you by showing the immediate children of the location you just
          ;; popped back to via the mark ring.
          (mark-goto . canonical)

          ;; The fallback for any context not explicitly defined above.
          ;; 'ancestors' keeps the buffer as tidy as possible by only unfolding
          ;; the direct path from the top level down to your target, leaving all
          ;; other sibling and child trees completely folded.
          (default . canonical)))

  (setq sgml-basic-offset 2)  ;; HTML
  (setq css-indent-offset 2)
  (setq javascript-indent-level 2)
  (setq html-indent-offset 2)
  (setq sgml-basic-offset 2)
  (setq lua-indent-level 3)
  (setq yaml-indent-offset 2)

  ;; python
  (defun setup-python-mode ()
    "Setup `python-mode'."
    (display-fill-column-indicator-mode)
    (my-set-tab-width 4)
    (setq-local fill-column 79))

  (when (fboundp 'setup-python-mode)
    (add-hook 'python-mode-hook #'setup-python-mode)
    (add-hook 'python-ts-mode-hook #'setup-python-mode))

  ;; sh
  (setq sh-basic-offset 2)
  (defun setup-sh-mode ()
    "Setup `sh-mode'."
    (display-fill-column-indicator-mode)
    (unless (string-suffix-p ".ebuild" (buffer-file-name (buffer-base-buffer)))
      (my-set-tab-width sh-basic-offset)
      (setq-local fill-column 80)))
  (when (fboundp 'setup-sh-mode)
    (add-hook 'sh-mode-hook #'setup-sh-mode)
    (add-hook 'bash-ts-mode-hook #'setup-sh-mode)))

;;; user post init

(defun lightemacs-user-post-init ()
  "This function is executed right before loading modules."
  ;; pre early init

  ;; ■ Warning (treesit): Cannot activate tree-sitter, because language grammar
  ;; for yaml is unavailable (not-found): ...
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-types '(treesit))
    (add-to-list 'warning-suppress-log-types '(treesit)))

  (my-setup-filetype)

  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "•")

  ;; I find the default prefix for smerge-mode C-c^ cumbersome so I have changed
  ;; it to C-cv
  (setq smerge-command-prefix "\C-xc")
  (setq smerge-diff-buffer-name "*smerge-diff*")
  (setq smerge-refine-shadow-cursor nil)

  ;; TODO: lightemacs?
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-extra-load-path
                 (expand-file-name "tree-sitter" lightemacs-var-directory)))

  (unless IS-MAC
    ;; Mac Port
    (add-to-list 'treesit-extra-load-path "/opt/local/lib"))

  (unless noninteractive
    (with-eval-after-load 'evil
      (with-eval-after-load 'evil-collection
        (require 'my-config-evil))))

  (unless noninteractive
    ;; (global-set-key (kbd "M-RET") 'toggle-term-tmux)
    ;; (global-set-key (kbd "M-<enter>") 'toggle-term-tmux)
    ;; (global-set-key (kbd "M-<return>") 'toggle-term-tmux)

    (global-set-key (kbd "M-o") 'my-previous-interesting-buffer)
    (global-set-key (kbd "M-i") 'my-next-interesting-buffer)
    ;; (global-set-key (kbd "M-=") 'global-text-scale-adjust)
    (global-set-key (kbd "C--") 'text-scale-decrease)
    (global-set-key (kbd "C-+") 'text-scale-increase)
    (global-set-key (kbd "C-S-k") 'my-tab-bar-move-tab-backward)
    (global-set-key (kbd "C-S-j") 'my-tab-bar-move-tab)
    (global-set-key (kbd "C-k") 'my-tab-previous)
    (global-set-key (kbd "C-j") 'my-tab-next))

  (setq lightemacs-dired-filter-setup-hook '(dired-filter-by-omit
                                             dired-filter-by-git-ignored
                                             dired-filter-by-dot-files))

  ;; Prevent yasnippet from highlighting inserted fields, you need to modify the
  ;; display face that it uses for overlays. This is done by changing the
  ;; attributes of yas-field-highlight-face.
  (defun my-clear-yasnippet-field-highlight (&rest _args)
    "Clear yasnippet field highlight face to keep original syntax highlighting."
    (when (facep 'yas-field-highlight-face)
      (set-face-attribute 'yas-field-highlight-face nil
                          :inherit 'unspecified
                          :background 'unspecified
                          :foreground 'unspecified
                          :box 'unspecified
                          :underline 'unspecified)))
  ;; Apply the fix whenever a theme is loaded
  (with-no-warnings
    (advice-add 'load-theme :after #'my-clear-yasnippet-field-highlight))

  ;; Ensure it also applies when yasnippet is first loaded
  (with-no-warnings
    (add-hook 'yas-minor-mode-hook #'my-clear-yasnippet-field-highlight))

  (setq yas-snippet-dirs '())
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "yasnippet/snippets" "~/.emacs-data/etc"))
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "yasnippet/snippets-auto" "~/.emacs-data/etc"))

  (with-eval-after-load 'yasnippet
    ;; (add-hook-text-editing-modes 'yas-minor-mode-on)
    (unless noninteractive
      (define-key yas-minor-mode-map (kbd "C-f") 'yas-expand))

    (setq yas-prompt-functions '(yas-no-prompt))  ; Do not ask the user

    ;; (add-to-list 'yas-snippet-dirs
    ;;              (expand-file-name "yasnippet/snippets" emacs-var-dir))
    ;; (add-hook-text-editing-modes 'yas-minor-mode-on)

    ;; (define-key yas-keymap (kbd "RET") (yas-filtered-definition
    ;;                                     'yas-next-field-or-maybe-expand))

    )

  (setq hs-hide-comments-when-hiding-all nil)
  (setq hs-isearch-open t)  ;; Open both comments and code

  ;; Code folding
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'conf-mode-hook 'outline-minor-mode)
  ;; (add-hook 'grep-mode-hook 'outline-minor-mode)
  (add-hook 'markdown-mode-hook 'outline-minor-mode)

  ;; Note: html-mode usually relies on specialized packages like sgml-mode or web-mode folding

  (progn
    ;; Systems and General Purpose
    (add-hook 'c-mode-hook #'hs-minor-mode)
    (add-hook 'c++-mode-hook #'hs-minor-mode)
    (add-hook 'java-mode-hook #'hs-minor-mode)
    (add-hook 'rust-mode-hook #'hs-minor-mode)
    (add-hook 'go-mode-hook #'hs-minor-mode)
    (add-hook 'ruby-mode-hook #'hs-minor-mode)

    ;; Web and Frontend
    (add-hook 'js-mode-hook #'hs-minor-mode)
    (add-hook 'typescript-mode-hook #'hs-minor-mode)
    (add-hook 'css-mode-hook #'hs-minor-mode)

    ;; Scripting, Data, and Infrastructure
    (add-hook 'sh-mode-hook #'hs-minor-mode) ; for bash/shell scripts
    (add-hook 'json-mode-hook #'hs-minor-mode)
    (add-hook 'lua-mode-hook #'hs-minor-mode))

  ;; (add-hook 'web-mode-hook 'hs-minor-mode)

  ;; (add-hook 'sh-mode-hook 'outline-indent-minor-mode)
  (progn
    (add-hook 'yaml-mode-hook 'outline-indent-minor-mode)
    (add-hook 'python-mode-hook 'outline-indent-minor-mode)

    ;; TODO right choice?
    ;; (add-hook 'bash-ts-mode-hook 'outline-indent-minor-mode)
    (add-hook 'yaml-ts-mode-hook 'outline-indent-minor-mode)
    (add-hook 'python-ts-mode-hook 'outline-indent-minor-mode))

  (progn
    (add-hook 'markdown-ts-mode-hook 'outline-minor-mode))

  ;; (add-hook 'js-ts-mode-hook 'hs-minor-mode)

  ;; TODO lightemacs treesit-fold?
  (progn
    ;; Systems and General Purpose
    (add-hook 'c-ts-mode-hook 'treesit-fold-mode)
    (add-hook 'c++-ts-mode-hook 'treesit-fold-mode)
    (add-hook 'java-ts-mode-hook 'treesit-fold-mode)
    (add-hook 'rust-ts-mode-hook 'treesit-fold-mode)
    (add-hook 'go-ts-mode-hook 'treesit-fold-mode)
    (add-hook 'ruby-ts-mode-hook 'treesit-fold-mode)

    ;; Web and Frontend
    (add-hook 'js-ts-mode-hook 'treesit-fold-mode)
    (add-hook 'typescript-ts-mode-hook 'treesit-fold-mode)
    (add-hook 'tsx-ts-mode-hook 'treesit-fold-mode)
    (add-hook 'css-ts-mode-hook 'treesit-fold-mode)
    (add-hook 'html-ts-mode-hook 'treesit-fold-mode)

    ;; Scripting and Infrastructure
    (add-hook 'bash-ts-mode-hook 'treesit-fold-mode)
    (add-hook 'cmake-ts-mode-hook 'treesit-fold-mode)
    (add-hook 'dockerfile-ts-mode-hook 'treesit-fold-mode)

    ;; Data and Configuration
    (add-hook 'json-ts-mode-hook 'treesit-fold-mode)
    (add-hook 'toml-ts-mode-hook 'treesit-fold-mode)

    ;; Third-party
    ;; (add-hook 'kotlin-ts-mode-hook #'treesit-fold-mode)
    ;; (add-hook 'swift-ts-mode-hook #'treesit-fold-mode)
    ;; (add-hook 'elixir-ts-mode-hook #'treesit-fold-mode)
    ;; (add-hook 'zig-ts-mode-hook #'treesit-fold-mode))
    )

  ;; (add-hook 'lisp-interaction-mode-hook #'outline-minor-mode)

  ;; This fixes the skipping when scrolling long org documents
  ;; NOTE: MANAGED BY MINIMAL-EMACS
  (setq scroll-conservatively most-positive-fixnum)

  ;; TODO put them back
  ;;(setq eldoc-idle-delay 0.5)
  ;;(setq eldoc-echo-area-display-truncation-message t)
  ;; (setq eldoc-echo-area-prefer-doc-buffer nil)
  (setq eldoc-echo-area-use-multiline-p nil)  ;; Prevent some errors from showing

  (with-eval-after-load 'recentf
    (setq recentf-exclude
          (append recentf-exclude
                  (list
                   "^/\\(?:su\\|sudo\\)?:"
                   "^~/\\.emacs"
                   "^~/\\.src"
                   "^~/src/forks/"
                   "^~/\\.[a-z]*-?emacs"
                   "^/opt/local/"
                   "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$" "\\.bz$"
                   "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zpaq$" "\\.lz$" "\\.lrz$"
                   "\\.lzo$" "\\.lzma$" "\\.shar$" "\\.kgb$" "\\.zip$" "\\.Z$"
                   "\\.7z$" "\\.rar$"

                   ;; TODO lighemacs?
                   "COMMIT_EDITMSG\\'"

                   "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"

                   ;; TODO add lightemacs dir

                   "-autoloads\\.el$"
                   "autoload\\.el$"))))

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-mixed-fonts nil)
  (setq modus-themes-prompts '(bold intense))

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
  (setq initial-major-mode 'txt-file-mode)
  (push (cons "\\.[Tt][Xx][Tt]\\'" 'txt-file-mode) auto-mode-alist)
  (push (cons "\\.[Tt][Xx][Tt]\\.[aA][sS][cC]\\'" 'txt-file-mode) auto-mode-alist)

  (nconc auto-mode-alist
         '(;; conf-mode
           ("\\.profile\\'" . conf-mode)  ; firejail profiles
           ("^/etc/[^/]+" . conf-unix-mode)

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

  ;; This regular expression matches the full file path for any .conf file
  ;; residing within either /etc/fonts/ or .config/fontconfig/ and maps them
  ;; directly to xml-mode.
  (add-to-list 'auto-mode-alist '("/etc/fonts/.*\\.conf\\'" . xml-mode))
  (add-to-list 'auto-mode-alist
               (cons (concat
                      (regexp-quote (expand-file-name "~/.config/fontconfig/"))
                      ".*\\.conf\\'")
                     'xml-mode))

  (defun my-setup-conf-mode ()
    "Setup `conf-mode'."
    (setq-local evil-auto-indent nil)
    (setq-local indent-line-function #'ignore))
  (with-no-warnings
    (add-hook 'conf-mode-hook #'my-setup-conf-mode))

  (setq markdown-toc-mode-map nil)
  (setq markdown-toc-header-toc-title "## Table of Contents")
  (add-to-list 'auto-mode-alist '("\\.md\\.asc\\'" . markdown-mode))

  ;; The function that is called by default is `vc-shrink-buffer-window',
  ;; which calls `shrink-window-if-larger-than-buffer' when BUFFER is visible.
  ;; This function shrinks height of WINDOW if its buffer doesn’t need so many
  ;; lines. More precisely, shrink WINDOW vertically to be as small as possible,
  ;; while still showing the full contents of its buffer. WINDOW must be a live
  ;; window and defaults to the selected one.
  (setq vc-diff-finish-functions nil)
  (setq vc-handled-backends '(Git))
  (setq vc-git-diff-switches '("--histogram"  ; Faster algorithm
                               "--textconv"
                               "--stat"

                               ;; "--ignore-cr-at-eol"

                               ;; Ignore changes in amount of white space.
                               ;; For example these would be considered the same:
                               ;; -foo    bar
                               ;; +foo bar
                               ;; "--ignore-space-change"

                               ;; Ignore all white space.
                               ;; "--ignore-all-space"
                               "-w"

                               ;; Ignore changes whose lines are all blank.
                               ;; "--ignore-blank-lines"
                               ))
  ;; Allow completing Git revisions from all refs, not only branches.
  (setq vc-git-revision-complete-only-branches nil)

  ;; Keep related changes together when generating changelogs.
  ;;
  ;; When you run `add-change-log-entry' or generate a changelog,
  ;; Emacs groups changes that belong to the same logical change together,
  ;; rather than scattering them across separate entries.
  ;;
  ;; This results in cleaner, more coherent changelog entries,
  ;; especially useful when editing multiple related files or making
  ;; several small fixes that belong to a single change.
  (setq add-log-keep-changes-together t)

  ;; Hide "up-to-date" messages in vc-dir buffers when reverting, reducing noise
  ;; (available since Emacs 31).
  (setq vc-dir-hide-up-to-date-on-revert t)

  ;; Ignore large, commonly untracked directories (like node_modules) in VC
  ;; operations to improve performance.
  (with-eval-after-load 'tramp
    (setq vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                       vc-ignore-dir-regexp
                                       tramp-file-name-regexp
                                       "[/\\\\]node_modules")))

  (setq kirigami-preserve-visual-position t)
  (setq buffer-terminator-verbose 'inhibit-message)

  ;; Hide markers like * / _ = ~; cleaner view but markers are not visible for
  ;; editing emphasis.
  ;; TODO add again
  (setq org-hide-emphasis-markers t)

  ;; No extra indentation for source blocks. It keeps code aligned with text.
  (with-no-warnings
    ;; Obsolete
    (setq org-edit-src-content-indentation 0))
  (setq org-src-content-indentation 0)

  ;; Fast todo selection without popup; efficient for experts but hides guidance
  ;; for beginners.
  (setq org-use-fast-todo-selection 'expert)

  ;; Source block settings
  (setq org-directory "~/src/wip/notes")
  (setq org-edit-src-persistent-message nil)
  (setq org-export-backends '(html texinfo md))

  ;; Lists
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))

  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (shell . t)
                                   (python . t)))

  (setq org-tag-alist '((:startgroup)
                        ;; Status
                        ("next" . ?n)
                        ("wip" . ?w)
                        ("soon" . ?o)
                        ("future" . ?f)
                        ("maybe" . ?e)
                        (:endgroup)

                        (:startgroup)
                        ;; Contexts
                        ("@home" . ?h)
                        ("@work" . ?r)
                        ("@outside" . ?u)
                        (:endgroup)

                        (:startgroup)
                        ;; Priorities
                        ("high" . ?i)
                        ("medium" . ?m)
                        ("low" . ?l)
                        (:endgroup)

                        (:startgroup)
                        ("quick" . ?q)
                        ("mediumtime" . ?t)
                        ("long" . ?g)
                        (:endgroup)))


  (setq org-src-lang-modes '(("python" . python)
                             ("sh" . sh)
                             ("bash" . sh)
                             ("elisp" . emacs-lisp)))
  ;; Tag colors
  (setq org-tag-faces
        '(("@home" . (:foreground "green" :weight bold))
          ("@work" . (:foreground "green" :weight bold))
          ("@outside" . (:foreground "green" :weight bold))
          ("@computer" . (:foreground "green" :weight bold))
          ("@phone" . (:foreground "green" :weight bold))

          ("next" . (:foreground "cyan"  :weight bold))
          ("wip" . (:foreground "cyan"  :weight bold))
          ("soon" . (:foreground "cyan"  :weight bold))
          ("future" . (:foreground "cyan"  :weight bold))
          ("maybe" . (:foreground "cyan"  :weight bold))

          ("high" . (:foreground "orange"    :weight bold))
          ("medium" . (:foreground "orange"    :weight bold))
          ("low" . (:foreground "orange"    :weight bold))

          ("quick"        . (:foreground "red"        :weight bold))
          ("medium-time"        . (:foreground "red"        :weight bold))
          ("long"        . (:foreground "red"        :weight bold))

          ;; ("meeting"   . (:foreground "yellow1"       :weight bold))
          ;; ("CRITICAL"  . (:foreground "red1"          :weight bold))
          ))

  ;; Set tag column to 0 (tags appear immediately after heading); simplifies
  ;; layout but may make long headings with tags harder to read.
  (setq org-hide-block-startup t)

  (setq project-switch-commands #'project-dired)
  (setq project-vc-extra-root-markers '(".projectile"
                                        ".dir-locals.el"
                                        "requirements.txt"
                                        "autogen.sh"
                                        ".project"))


  (setq savehist-autosave-interval 650)
  (setq tab-bar-history-limit 15)

  (setq outline-blank-line t)

  (setq vim-tab-bar-update-group-name-function #'(lambda(name)
                                                   (concat " [" name "] ")))
  (setq vim-tab-bar-show-groups nil)

  (setq bufferfile-use-vc t)
  ;; (setq bufferfile-delete-switch-to 'previous-buffer)
  (setq bufferfile-delete-switch-to 'parent-directory)

  (setq markdown-gfm-use-electric-backquote nil)
  (setq markdown-heading-scaling t)
  (unless noninteractive
    (with-eval-after-load 'markdown-mode
      (define-key markdown-mode-map (kbd "TAB") #'ignore)))

  (setq grep-use-null-device nil
        grep-use-null-filename-separator nil
        grep-use-headings nil)
  (with-eval-after-load 'le-core-cli-tools
    (setq grep-command
          (concat (if lightemacs--ripgrep-executable
                      lightemacs--ripgrep-executable
                    "rg")
                  ;; Lightemacs
                  " --hidden -g !.git -g !.svn -g !.hg"

                  ;; Default
                  " --null --line-buffered --color=never --max-columns=1000"
                  " --path-separator / --smart-case --no-heading"
                  " --with-filename --line-number --search-zip")))

  (with-eval-after-load 'savehist
    (setq savehist-autosave-interval 650)

    (defvar my-savehist-additional-variables-added nil)

    (unless my-savehist-additional-variables-added
      (setq savehist-additional-variables
            (append
             savehist-additional-variables
             '(;; Record of all interactive commands entered
               command-history

               ;; Preserve jump history (used for C-o / C-i)
               ;; evil-jumps-history

               ;; Custom
               lightemacs-theme-package
               lightemacs-theme-name)))
      (setq my-savehist-additional-variables-added t))

    ;; Moved it here because it does not work from :hook
    ;; TODO replace this with easysession or with custom le-theme
    (add-hook 'savehist-mode-hook
              #'(lambda()
                  (when (fboundp 'lightemacs-load-default-theme)
                    (lightemacs-load-default-theme)))))

  ;; (setq easysession-save-pretty-print t)

  ;; (setq easysession-switch-to-save-session nil)
  ;; (setq easysession-mode-line-misc-info t)
  ;; (setq easysession-setup-load-session nil)

  (setq easysession-switch-to-exclude-current t)
  (setq easysession-save-interval (* 14 60))
  (add-hook 'easysession-before-reset-hook
            #'(lambda()
                ;; Save all with no questions
                (my-save-all-buffers)))
  ;; (defun my-easysession-only-main-saved ()
  ;;   "Only save the main session."
  ;;   (when (and (fboundp 'easysession-get-session-name)
  ;;              (string= "main" (funcall 'easysession-get-session-name)))
  ;;     t))
  ;; (setq easysession-save-mode-predicate 'my-easysession-only-main-saved)
  (add-hook 'easysession-new-session-hook 'easysession-reset)

  (setq flymake-start-on-flymake-mode (when (> (num-processors) 8) t))
  ;; (setq flymake-no-changes-timeout (when (> (num-processors) 8) 0.8))
  (setq flymake-no-changes-timeout 0.8)
  (setq flymake-start-on-save-buffer t)  ;; Do not enable or it will enable it on save
  (setq flymake-suppress-zero-counters t)

  (with-no-warnings
    (add-hook 'org-mode-hook 'hl-line-mode)
    (add-hook 'grep-mode-hook 'hl-line-mode))
  (unless noninteractive
    (with-eval-after-load 'icomplete
      (define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-force-complete-and-exit)))

  ;; Modify all of them
  ;; ORIGINAL:               "[-–!|#%;>*·•‣⁃◦ 	]*"
  ;; This prevents fill from adding - to every new line
  (setq adaptive-fill-regexp "[  !|#%;>*·•‣⁃◦   ]*")

  ;; Display the current line and column numbers in the mode line
  ;; Non-nil if searches and matches should ignore case.
  ;; nil means case is significant.
  (setq-default case-fold-search nil)

  (setq persist-text-scale-handle-file-renames t)

  (setq vterm-timer-delay 0.001)  ;; Only works when added after :config
  (setq vterm-max-scrollback 1)

  (setq eat-enable-yank-to-terminal t
        eat-enable-directory-tracking t
        eat-enable-shell-command-history t
        eat-enable-shell-prompt-annotation t
        eat-term-scrollback-size nil)
  (add-hook 'eat-mode-hook
            #'(lambda ()
                (my-disable-fringe-truncation-arrow)
                (display-line-numbers-mode -1)
                (setq-local show-paren-mode nil)
                (setq-local line-number-mode nil)
                (setq-local column-number-mode nil)))
  (with-eval-after-load 'eat
    (with-eval-after-load 'evil-collection
      (defun evil-collection-enable-eat-toggle-send-escape ()
        (unless (bound-and-true-p evil-collection-eat-send-escape-to-eat-p)
          ;; Hide "Sending ESC to eat."
          (let ((inhibit-message t))
            (when (fboundp 'evil-collection-eat-toggle-send-escape)
              (funcall 'evil-collection-eat-toggle-send-escape)))))
      (add-hook 'eat-mode-hook 'evil-collection-enable-eat-toggle-send-escape)))

  (setq show-paren-mode nil)

  (setq icomplete-separator "\n")
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-prospects-height 10)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-with-completion-tables t)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-tidy-shadowed-file-names t)

  (add-hook 'emacs-lisp-mode-hook
            #'(lambda()
                (setq-local dabbrev-case-fold-search t)
                (setq-local case-fold-search t)))

  ;; Control whether dabbrev searches should ignore case.
  ;; Any other non-nil version means case is not significant.
  ;; nil means case is significant.
  (setq dabbrev-case-fold-search nil)

  ;; Whether dabbrev applies the abbreviations’s case pattern to the expansion.
  ;; A value of nil means preserve the expansion’s case pattern.
  (setq dabbrev-case-replace nil)

  (setq dabbrev-check-all-buffers nil)

  ;; It configures dabbrev (dynamic abbreviation expansion) to search only in the
  ;; current buffer when expanding abbreviations, instead of searching in other
  ;; buffers as well.
  ;; (setq dabbrev-check-other-buffers t)  ;; Default t

  ;; (dabbrev-abbrev-char-regexp "\\\\sw\\\\|\\\\s_")
  ;; (dabbrev-ignored-buffer-names '("*Messages*" "*Ibuffer*"))
  ;; (dabbrev-limit 1000)
  (setq dabbrev-case-distinction nil)

  (setq epg-gpg-program "gpg2")
  ;; (setq epa-pinentry-mode 'loopback)  ;; Obsolete

  (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)\\(~\\|\\.~[0-9]+~\\)?\\'")

  ;; Manually update the handler alist to recognize the new extension immediately
  (setq file-name-handler-alist
        (cons (cons epa-file-name-regexp #'epa-file-handler)
              file-name-handler-alist))

  (setq large-file-warning-threshold (* 100 1024 1024))  ; 100 Mb

  ;; Add ( and ) to modes such as yaml-ts-mode
  (defvar original-electric-pair-pairs '((40 . 41)   ;; ( and ) for yaml-ts-mode
                                         (123 . 125) ;; { and } for elisp
                                         ;; (91 . 93) ;; [ and ]
                                         ;; Default:
                                         (34 . 34) ;; Double quote
                                         (8216 . 8217)
                                         (8220 . 8221)))

  ;; comments
  (defvar original-electric-pair-text-pairs '((40 . 41) ;; ( and ) for yaml-ts-mode
                                              (123 . 125) ;; { and } for elisp
                                              (96 . 96) ;; ` and ` for elisp comments (instead of `')
                                              ;; (91 . 93) ;; [ and ]
                                              ;; Default:
                                              (34 . 34) ;; Double quote
                                              (8216 . 8217)
                                              (8220 . 8221)))

  (setq electric-pair-text-pairs original-electric-pair-text-pairs)
  (setq electric-pair-pairs original-electric-pair-pairs)

  (setq which-key-idle-delay 1.5)

  ;; It defines the "en_US" spell-check dictionary locally, telling Emacs to use
  ;; UTF-8 encoding, match words using alphabetic characters, allow apostrophes
  ;; inside words, treat non-alphabetic characters as word boundaries, and pass
  ;; -d en_US to the underlying spell-check program.
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

  ;; Non-nil means suppress messages in ispell-word.
  (setq ispell-quietly t)
  (setq flyspell-issue-welcome-flag nil)
  (setq flyspell-issue-message-flag nil)

  ;; If non-nil, add correction to abbreviation table.
  (setq flyspell-abbrev-p nil)
  ;; (setq flyspell-use-global-abbrev-table-p t)

  (with-eval-after-load 'flyspell
    ;; Remove strings from Flyspell
    (setq flyspell-prog-text-faces (delq 'font-lock-string-face
                                         flyspell-prog-text-faces))

    ;; Remove doc from Flyspell
    (setq flyspell-prog-text-faces (delq 'font-lock-doc-face
                                         flyspell-prog-text-faces)))

  (setq ispell-program-name "aspell")
  (setq ispell-local-dictionary "en_US")

  ;; Configures Aspell's suggestion mode to "ultra", which provides more
  ;; aggressive and detailed suggestions for misspelled words. The language
  ;; is set to "en_US" for US English, which can be replaced with your desired
  ;; language code (e.g., "en_GB" for British English, "de_DE" for German).
  (setq ispell-extra-args '("--sug-mode=ultra"
                            "--lang=en_US"))

  (setq ispell-dictionary "en_US")

  (with-eval-after-load 'which-key
    (when (bound-and-true-p which-key-buffer-name)
      (add-to-list 'winner-boring-buffers which-key-buffer-name)))

  (with-eval-after-load 'le-aggressive-indent
    (add-hook 'lua-mode-hook 'aggressive-indent-mode))

  (my-update-package-pinned-packages my-package-pinned-packages)

  ;; Abbrev
  (add-hook 'markdown-mode-hook #'abbrev-mode)
  (add-hook 'markdown-ts-mode-hook #'abbrev-mode)
  (add-hook 'org-mode-hook #'abbrev-mode)
  (define-abbrev-table 'global-abbrev-table
    '(("ot" "to")
      ("i" "I")))

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
          (?f . evil-surround-function)))

  (my-evil-config)

  (setq user-full-name "user"
        user-mail-address "user@domain.ext")

  (with-eval-after-load 'ediff
    (defun my/ediff-kill-control-buffer ()
      "Kill the Ediff control buffer and temporary buffers upon quitting.
    Safely handles multiple concurrent Ediff sessions and numbered buffers."
      ;; Keep the fallback logic for the local control buffer
      (when (and (boundp 'ediff-control-buffer)
                 (buffer-live-p ediff-control-buffer))
        (kill-buffer ediff-control-buffer))

      ;; Check if there are any other active Ediff sessions
      (let ((active-ediffs (seq-filter
                            (lambda (buf)
                              (with-current-buffer buf
                                (eq major-mode 'ediff-control-mode)))
                            (buffer-list))))
        ;; If this is the last one (or zero left), kill the shared buffers
        (when (<= (length active-ediffs) 1)
          (dolist (buf (buffer-list))
            (let ((name (buffer-name buf)))
              ;; We are using `string-prefix-p' because Ediff sometimes creates
              ;; variants of these names such as *ediff-diffs<2>*.
              (when (or (string-prefix-p "*ediff-errors" name)
                        (string-prefix-p "*ediff-diff" name)
                        (string-prefix-p "*ediff-fine-diff" name)
                        (string-prefix-p "*Ediff Registry" name))
                (kill-buffer buf)))))))

    (add-hook 'ediff-startup-hook 'ediff-next-difference)
    (add-hook 'ediff-quit-hook #'(lambda()
                                   (when (fboundp 'my/ediff-kill-control-buffer)
                                     (my/ediff-kill-control-buffer))

                                   (with-eval-after-load 'winner
                                     (when (and (bound-and-true-p winner-mode)
                                                (fboundp 'winner-undo))
                                       (winner-undo))))))
  (setq ediff-keep-variants t)  ; Do not kill ediff buffers
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-confirm-copy t)

  ;; This seems to change ediff
  (setq diff-default-read-only t)

  ;; This mimics the Magit's diff format by making the hunk header less cryptic,
  ;; and on GUI frames also displays insertion and deletion indicators on the
  ;; left fringe (if it's available).
  ;;
  ;; This is better for patches.
  (setq diff-font-lock-prettify t)

  ;; Use reliable file-based syntax highlighting when available and hunk-based
  ;; syntax highlighting otherwise as a fallback.
  (setq diff-font-lock-syntax 'hunk-also)

  ;; (setq diff-advance-after-apply-hunk t)

  ;; This sometimes interacts poorly with the undo mechanism
  ;; (setq diff-update-on-the-fly t)

  ;; Set this to nil if you want to do it on demand, with my `agitate' package
  ;; (setq diff-refine nil)

  (add-hook 'diff-mode-hook #'outline-minor-mode)

  (setq vertico-count 13)
  (with-eval-after-load 'consult
    (dolist (regexp '("^\*helpful"
                      "^\*sdcv"
                      "^\*EGLOT"
                      "^\*Help"
                      "^\*scratch\*"
                      "^\*tmux\*"
                      "^\*Warnings\*"
                      "^todo.org$"
                      "^\*Native-compile-Log\*"
                      "^\*Async-native-compile-log\*"
                      "^tmp-"
                      "^\*Compile-Log\*"
                      "^\*ansible-doc"))
      (push regexp consult-buffer-filter)))

  (dolist (err '("\\`rx ['']\\*\\*[''] range error"
                 search-failed

                 ;; Wrong syntax (PDF)
                 invalid-read-syntax

                 "This function supports only emacs-lisp-mode"

                 "Cannot find a suitable checker"

                 ;; "Attempt to delete the sole visible or iconified frame"
                 ;; (push "Attempt to delete the sole visible or iconified frame" debug-ignored-errors)

                 "Selecting deleted buffer"

                 "Already at top level of the outline"

                 "This buffer cannot use 'imenu-default-create-index-function'"

                 ;; Debugger entered--Lisp error: (permission-denied "Setting current directory"
                 ;; "Permission denied" "/dir/")
                 permission-denied

                 ;; Debugger entered--Lisp error: (invalid-regexp "Unmatched [ or [^")
                 ;;  evil-ex-search-find-next-pattern(("[\"" t t) forward)
                 ;;  evil-ex-find-next(("[\"" t t) forward t)
                 ;;  evil-ex-search-full-pattern("[\"" nil forward)
                 ;;  evil-ex-start-search(forward nil)
                 ;;  evil-ex-search-forward(nil)
                 ;;  funcall-interactively(evil-ex-search-forward nil)
                 ;;  command-execute(evil-ex-search-forward)
                 invalid-regexp

                 ;; Debugger entered--Lisp error: (error "Accessing an empty ring")
                 ;;  error("Accessing an empty ring")
                 ;;  ring-ref((0 0 . [nil nil nil nil nil nil nil nil nil nil]) 0)
                 ;;  evil-repeat(nil nil)
                 ;;  funcall-interactively(evil-repeat nil nil)
                 ;;  command-execute(evil-repeat)
                 "Accessing an empty ring"

                 ;; goto last change
                 ;; ----------------
                 ;; Debugger entered--Lisp error: (error "Negative arg: Cannot reverse as the first operation")
                 ;; error("Negative arg: Cannot reverse as the first operation")
                 ;; goto-last-change(-)
                 ;; goto-last-change-reverse(nil)
                 ;; evil-goto-last-change-reverse(nil)
                 ;; funcall-interactively(evil-goto-last-change-reverse nil)
                 ;; command-execute(evil-goto-last-change-reverse)
                 "Negative arg: Cannot reverse as the first operation"

                 ;; goto-chg
                 "Buffer has not been changed"

                 ;; Paredit
                 "Mismatched parenthesis depth"
                 "Mismatched character quotation"
                 "Mismatched comment state:"
                 "Mismatched string state:"

                 ;; Outline next/previous heading
                 ;; (outline-back-to-heading) and (show-children)
                 outline-before-first-heading
                 "No previous same-level heading"
                 "No following same-level heading"

                 ;; Debugger entered--Lisp error: (error "Last directory")
                 ;;   error("%s directory" "Last")
                 ;;   dired-next-subdir(1)
                 ;;   funcall-interactively(dired-next-subdir 1)
                 ;;   command-execute(dired-next-subdir)
                 "Last directory"

                 ;; easysession
                 treesit-query-error

                 "Bad diff region number"))
    (push err debug-ignored-errors))

  (setq consult-preview-excluded-files '("\\`/[^/|:]+:" "\\.asc\\'"
                                         "\\`/[^/|:]+:" "\\.gpg\\'"))
  (add-hook 'embark-collect-mode-hook
            #'(lambda()
                ;; TODO: What sets this to t?
                (setq make-window-start-visible nil)
                (my-disable-fringe-truncation-arrow)))

  (add-hook 'embark-collect-mode-hook
            (lambda ()
              "Disable auto-hscroll in embark-collect buffers."
              (setq-local auto-hscroll-mode nil)))

  (with-eval-after-load 'consult
    (setq consult-fd-args
          (concat (if lightemacs--fdfind-executable
                      lightemacs--fdfind-executable
                    "fd")
                  ;; This config
                  " --type f"

                  ;; Lightemacs
                  " --hidden --exclude .git --absolute-path"
                  (if (memq system-type '(cygwin windows-nt ms-dos))
                      " --path-separator=/"
                    "")

                  ;; Default
                  " --full-path --color=never")))

  (add-hook 'text-mode-hook #'(lambda () (setq-local indent-tabs-mode nil)))

  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
    (add-hook hook #'(lambda ()
                       (display-fill-column-indicator-mode)
                       (if (fboundp 'my-set-tab-width)
                           (my-set-tab-width 2)
                         (error "Undefined: my-set-tab-width")))))

  (setq read-process-output-max (* 32 1024 1024))

  (unless (display-graphic-p)
    (xterm-mouse-mode 1))


  (unless noninteractive
    (windmove-default-keybindings)
    ;; Conflict with XFCE C-m-h
    (global-unset-key (kbd "C-M-h"))
    (global-unset-key (kbd "C-M-l"))
    (global-set-key [next] #'ignore)
    (global-set-key [prior] #'ignore)
    (global-set-key (kbd "M-SPC") #'ignore)  ; Disable cycle spacing
    (global-set-key (kbd "C-SPC") #'ignore)  ; Disable C-SPC mark
    (global-set-key (kbd "<C-prior>") 'my-tab-previous)
    (global-set-key (kbd "<C-next>") 'my-tab-next)
    ;; (global-set-key (kbd "C-?") 'help-command)
    )

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
  ;; (setq yank-excluded-properties t)

  ;; Strip 'fontified' and other text properties when pasting so jit-lock
  ;; rescans the text, allowing org-indent-mode to apply the correct visual
  ;; indentation.
  ;; (setq yank-excluded-properties
  ;;       '(category field follow-link fontified font-lock-face help-echo
  ;;                  intangible invisible keymap local-map mouse-face read-only
  ;;                  yank-handler))

  ;; ;; Define custom handler functions to process the 'font-lock-face' and
  ;; ;; 'category' text properties when pasting text.
  ;; (setq yank-handled-properties
  ;;       '((font-lock-face . yank-handle-font-lock-face-property)
  ;;         (category . yank-handle-category-property)))

  ;; Shows all options when running apropos. For more info,
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
  (with-eval-after-load 'compile-angel
    (when (fboundp 'compile-angel-exclude-file)
      (compile-angel-exclude-file
       (expand-file-name "prescient-save.el"
                         my-shared-user-emacs-directory))))


  (setq backup-directory-alist
        `(("." . ,(expand-file-name "backup" my-shared-user-emacs-directory))))
  (setq tramp-backup-directory-alist backup-directory-alist)

  ;;-------------------------------------> BLOCK CHANGE AUTO SAVE PATH
  (setq auto-save-list-file-prefix
        (expand-file-name "autosave/" my-shared-user-emacs-directory))
  (setq tramp-auto-save-directory
        (expand-file-name "tramp-autosave/" my-shared-user-emacs-directory))
  (setq auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
           ,(file-name-concat auto-save-list-file-prefix "tramp-\\2-") sha1)
          ("\\`/\\([^/]+/\\)*\\([^/]+\\)\\'"
           ,(file-name-concat auto-save-list-file-prefix "\\2-") sha1)))

  (when auto-save-default
    (let ((auto-save-dir (file-name-directory auto-save-list-file-prefix)))
      (unless (file-exists-p auto-save-dir)
        (with-file-modes #o700
          (make-directory auto-save-dir t)))))
  ;;-------------------------------------> BLOCK CHANGE AUTO SAVE PATH

  (setq tramp-auto-save-directory
        (expand-file-name "tramp-autosave/" my-shared-user-emacs-directory))

  (setq save-place-file (expand-file-name "saveplace" my-shared-user-emacs-directory))

  (setq abbrev-file-name (expand-file-name "abbrev_defs" my-shared-user-emacs-directory))

  (setq easysession-debug t)
  (setq easysession-refresh-tab-bar t)
  (setq buffer-terminator-track-tab-bar-buffers t)

  (setq easysession-directory
        (expand-file-name "easysession" my-shared-user-emacs-directory))

  (setq my-project-list-file-auto
        (expand-file-name "projects-auto"
                          my-shared-user-emacs-directory))

  (setq recentf-save-file
        (expand-file-name "recentf" my-shared-user-emacs-directory)))

;;; Useful functions

(defun my-dir-config--buffer-cwd ()
  "Return the directory associated with the current buffer.
Returns:
- The directory path if the buffer is in `dired-mode', or
- The directory of the file if the buffer is visiting a file, or
- nil if neither condition is met."
  (let ((file-name (buffer-file-name (buffer-base-buffer))))
    (cond ((derived-mode-p 'dired-mode)
           default-directory)

          (file-name
           (file-name-directory file-name)))))

(defun buffer-cwd ()
  "Return the directory of the current buffer."
  (interactive)
  (or (my-dir-config--buffer-cwd) default-directory))

;;; Startup time

(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs loaded in %.2f seconds (Init only: %.2fs) with %d garbage collections."
           (time-to-seconds (time-since before-init-time))
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(add-hook 'lightemacs-emacs-startup-hook #'display-startup-time 200)

;;; Ignored errors

;; Org + vertico preview error: Debugger entered--Lisp error: (error "rx ‘**’
;; range error")
;;; Other settings

;; Control ^ = Control
;; Command = Fn/Globe
;; Fn/Globe = Command
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)

;; Ignore local variables that are declared in files
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Local-Variables.html
;; Enable loading variables from .dir-locals.el
(setq enable-local-variables :safe)  ;; Also loads variables that are in comments
;; (setq enable-local-variables nil)
(setq enable-local-eval nil)

;;; ibuffer

(setq ibuffer-filter-group-name-face '(:inherit (success bold)))
(setq
 ;; ibuffer-hidden-filter-groups nil

 ;; The number of hours before a buffer is considered "old".
 ibuffer-old-time 5

 ;; If non-nil, then forward and backwards movement commands cycle.
 ibuffer-movement-cycle nil

 ibuffer-show-empty-filter-groups nil

 ;; If non-nil, don’t ask for confirmation of "dangerous" operations.
 ibuffer-expert t

 ;; If non-nil, summarize Ibuffer columns.
 ibuffer-display-summary nil

 ;; If non-nil, display Ibuffer in another window by default.
 ;; ibuffer-use-other-window nil

 ibuffer-use-header-line nil
 ;; ibuffer-default-shrink-to-minimum-size t

 ;; Prevent long file names from wrapping when using olivetti
 ibuffer-truncate-lines t)


(with-eval-after-load 'ibuffer

  (progn
    ;; Add size
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))

    (setq ibuffer-formats
          '((mark modified read-only " "
                  (name 55 55 :left :nil) " "
                  (size-h 9 -1 :right) " "
                  (mode 16 16 :left :elide) " "
                  filename-and-process))))

  (add-hook 'ibuffer-hook
            (lambda ()
              ;; For some reason, ibuffer-truncate-lines doesn't work (TODO)
              (my-disable-fringe-truncation-arrow)
              (toggle-truncate-lines 1)))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (require 'ibuf-ext)
              (when (fboundp 'ibuffer-switch-to-saved-filter-groups)
                (ibuffer-switch-to-saved-filter-groups "default"))))

  (setq ibuffer-saved-filter-groups
        (quote (("default"

                 ("Programming" (and (derived-mode . prog-mode)
                                     (not (starred-name))))

                 ("Text" (and (derived-mode . text-mode)
                              (not (starred-name))))

                 ("Org" (or (name . "^\\*Calendar\\*$")
                            (name . "^\\*Org Agenda")
                            (name . "^ \\*Agenda")
                            (mode . org-mode)
                            (name . "^diary$")
                            (mode . muse-mode)
                            (mode . org-mode)
                            (mode . org-agenda-mode)))

                 ("Emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")
                           (name . "^\\*Warnings\\*$")
                           (name . "^\\*Compile-Log\\*$")
                           (name . "^\\*Async-native-compile-log\\*$")
                           (name . "^\\*dashboard\\*$")
                           (name . "^\\*compilation\\*$")
                           (name . "^\\*Backtrace\\*$")
                           (name . "^\\*Packages\\*$")
                           (name . "^\\*Customize\\*$")
                           (name . "^\\*\\(Echo\\|Minibuf\\)")))

                 ("Help" (or (name . "^\\*Help\\*$")
                             (name . "^\\*Apropos\\*$")
                             (name . "^\\*info\\*$")
                             (mode . Man-mode)
                             (mode . woman-mode)))

                 ("Repl" (or (mode . gnuplot-comint-mode)
                             (mode . inferior-emacs-lisp-mode)
                             (mode . inferior-python-mode)))

                 ("VC" (or (mode . diff-mode)
                           (derived-mode . log-view-mode)))

                 ("Starred" (starred-name))

                 ("Term" (or (mode . term-mode)
                             (mode . shell-mode)
                             (mode . vterm-mode)
                             (mode . compilation-mode)
                             (mode . eshell-mode)))

                 ("Dired" (or (mode . dired-mode)
                              (mode . sr-mode)))

                 ("Other" (name . ".*"))

                 ;; (add-hook 'ibuffer-mode-hook
                 ;;           (lambda ()
                 ;;             (ibuffer-switch-to-saved-filter-groups "custom")
                 ;;             (setq ibuffer-hidden-filter-groups nil)))

                 ;; ("Conf" (or (mode . yaml-mode)
                 ;;             (mode . yaml-ts-mode)
                 ;;             (mode . conf-mode)))

                 ;; ("Coq" (or
                 ;;         (mode . coq-mode)
                 ;;         (name . "\\<coq\\>")
                 ;;         (name . "_CoqProject")))
                 ;; ("code" (or (mode . emacs-lisp-mode)
                 ;;             (mode . yaml-mode)
                 ;;             (mode . yaml-ts-mode)
                 ;;             (mode . cperl-mode)
                 ;;             (mode . c-mode)
                 ;;             (mode . java-mode)
                 ;;             (mode . idl-mode)
                 ;;             (mode . web-mode)
                 ;;             (mode . lisp-mode)
                 ;;             (mode . js2-mode)
                 ;;             (mode . c++-mode)
                 ;;             (mode . lua-mode)
                 ;;             (mode . cmake-mode)
                 ;;             (mode . ruby-mode)
                 ;;             (mode . css-mode)
                 ;;             (mode . objc-mode)
                 ;;             (mode . sql-mode)
                 ;;             (mode . python-mode)
                 ;;             (mode . php-mode)
                 ;;             (mode . sh-mode)
                 ;;             (mode . json-mode)
                 ;;             (mode . scala-mode)
                 ;;             (mode . go-mode)
                 ;;             (mode . erlang-mode)))

                 ;; ("Unsaved" (modified))
                 ;; ("erc" (mode . erc-mode))
                 ;; ("Browser" (or (mode . eww-mode)
                 ;;                (mode . xwidget-webkit-mode)))
                 ;; ("Mail" (or (mode . mail-mode)
                 ;;             (mode . message-mode)
                 ;;             (derived-mode . gnus-mode)))
                 ;; ("Dict" (or (mode . fanyi-mode)
                 ;;             (mode . dictionary-mode)))
                 ;; ("Magit" (or (mode . magit-repolist-mode)
                 ;;              (mode . magit-submodule-list-mode)
                 ;;              (mode . git-rebase-mode)
                 ;;              (derived-mode . magit-section-mode)
                 ;;              (mode . vc-annotate-mode)))
                 ;; ("IRC" (or (mode . rcirc-mode)
                 ;;            (mode . erc-mode)))

                 ;; ("gnus" (or (mode . message-mode)
                 ;;             (mode . bbdb-mode)
                 ;;             (mode . mail-mode)
                 ;;             (mode . gnus-group-mode)
                 ;;             (mode . gnus-summary-mode)
                 ;;             (mode . gnus-article-mode)
                 ;;             (name . "^\\.bbdb$")
                 ;;             (name . "^\\.newsrc-dribble")))
                 )))))

;;; Setup scratch

(defvar-local my-scratch-setup-done nil)

(defun my-setup-scratch-buffer ()
  "Setup the scratch buffer."
  (let ((buffer (get-buffer "*scratch*")))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless my-scratch-setup-done
          (setq my-scratch-setup-done t)
          (setq fill-column 60))))))

(advice-add 'scratch-buffer :after #'my-setup-scratch-buffer)

;;; Display line numbers

(defun my-setup-display-line-numbers-mode ()
  "Setup `display-line-numbers-mode'."
  (cond
   ((or (derived-mode-p 'markdown-mode)
        (derived-mode-p 'org-mode))
    ;; (setq-local display-line-numbers-type 'relative)
    (display-line-numbers-mode 1))

   (t
    ;; (setq-local display-line-numbers-type 'visual)
    (display-line-numbers-mode 1))))

;; TODO replace add hook with add-hook-text-editing-modes
;; (add-hook-text-editing-modes 'my-setup-display-line-numbers-mode)
(add-hook 'prog-mode-hook #'my-setup-display-line-numbers-mode)
(add-hook 'text-mode-hook #'my-setup-display-line-numbers-mode)
(add-hook 'conf-mode-hook #'my-setup-display-line-numbers-mode)
(add-hook 'ibuffer-mode-hook #'my-setup-display-line-numbers-mode)

(add-hook 'helpful-mode-hook #'my-setup-display-line-numbers-mode)
(add-hook 'dired-mode-hook #'my-setup-display-line-numbers-mode)
(add-hook 'org-agenda-mode-hook #'my-setup-display-line-numbers-mode)

;; TODO
;; (setq lightemacs-display-line-numbers-mode-target-hooks nil)

;; Use absolute numbers; 'relative and 'visual are significantly slower
;; t=absolute
;; (setq-default display-line-numbers-type t)
;; (setq-default display-line-numbers-type 'visual)
(setq-default display-line-numbers-type 'visual)
(setq display-line-numbers-grow-only t)  ; t is slow. Use nil.
(setq display-line-numbers-current-absolute nil)  ;; t=line num / nil=0

;;; Sync dictionary

;; Silence: Truncate long lines disabled
;; TODO: Should we execute it after inserting anything in the speller
(defun run-sync-spell-dict-if-exists ()
  "Run sync-spell-dict command if it exists."
  (when (executable-find "sync-spell-dict")
    (shell-command "sync-spell-dict >/dev/null 2>&1 & disown")))

(add-hook 'kill-emacs-hook 'run-sync-spell-dict-if-exists)

;;; apheleia

(with-eval-after-load 'apheleia
  (setq apheleia-mode-alist nil)
  (setq apheleia-formatters nil)

  ;; Elisp
  (setf (alist-get 'lisp-indent apheleia-formatters)
        'apheleia-indent-lisp-buffer)
  (setf (alist-get 'emacs-lisp-mode apheleia-mode-alist) 'lisp-indent)

  ;; Bash shell scripts
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "--binary-next-line"
          "-filename" filepath
          (when (and apheleia-formatters-respect-indent-level
                     (boundp 'sh-basic-offset))
            (list "-i" (number-to-string sh-basic-offset)))
          "-"))

  ;; Python
  (setf (alist-get 'autopep8 apheleia-formatters)
        '("autopep8"
          "--max-line-length=79"
          "--aggressive"
          ;; "--aggressive"
          "-"))
  (setf (alist-get 'isort apheleia-formatters) '("isort" "--stdout" "-"))

  (setf (alist-get 'bash-ts-mode apheleia-mode-alist) '(shfmt))
  (setf (alist-get 'sh-mode apheleia-mode-alist) '(shfmt))

  (setf (alist-get 'python-mode apheleia-mode-alist) '())
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '())
  (setf (alist-get 'python-mode apheleia-mode-alist) '(isort autopep8))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(isort autopep8))
  )

;;; Elisp

;; (defun better-jump--setup-imenu-elisp ()
;;   "Setup imenu."
;;   (setq-local imenu-generic-expression
;;               (cl-remove-if (lambda (item)
;;                               (or (string= (car item) "Packages")
;;                                   (string= (car item) "Variables")
;;                                   (string= (car item) "Types")))
;;                             imenu-generic-expression)))

(defun better-jump--setup-imenu-elisp ()
  "Setup imenu."
  (setq-local imenu-generic-expression
              (seq-remove (lambda (item)
                            (member (car item) '("Packages"
                                                 "Variables"
                                                 "Types")))
                          imenu-generic-expression)))

(add-hook 'emacs-lisp-mode-hook #'better-jump--setup-imenu-elisp)

;;; Flymake

(defun my-limit-package-lint-flymake-setup-a (orig-fn &rest args)
  "Limit package lint flymake setup.
ORIG-FN and ARGS is the functions and its arguments."
  ;;(setq-local package-lint-main-file
  ;;            (file-name-nondirectory
  ;;             (buffer-file-name (buffer-base-buffer))))
  (when (my-code-checker-allowed-p)
    (let* ((filename (buffer-file-name (buffer-base-buffer)))
           (basename (if filename (file-name-nondirectory filename) "")))
      (when (and filename
                 (not (string-match-p "/lisp/local/" filename))
                 (not (string= basename ".dir-locals.el"))
                 (not (string= basename ".dir-config.el"))
                 (not (string= basename ".dir-settings.el"))
                 (not (string= basename "init.el"))
                 (not (string= basename "early-init.el"))
                 (not (string-prefix-p "le-" basename)))
        (apply orig-fn args)))))

(with-eval-after-load 'le-package-lint-flymake
  (advice-add 'package-lint-flymake-setup :around
              #'my-limit-package-lint-flymake-setup-a))

;; ignore pckage lint: The word "emacs" is redundant in Emacs package names.

(defun my-package-lint-ignore (orig-fun desc)
  "Bypass the \"emacs\" name check for files in a specific directory.
ORIG-FUN is the advised function.  DESC is the package description struct."
  (let ((target-dir (expand-file-name "~/src/emacs/lightemacs"))
        (file-name (buffer-file-name (buffer-base-buffer))))
    (when file-name
      (if (and (buffer-file-name (buffer-base-buffer))
               (buffer-file-name (buffer-base-buffer))
               (file-in-directory-p file-name target-dir))
          ;; Condition met: return nil to skip the original function
          nil
        ;; Condition not met: execute the original function
        (funcall orig-fun desc)))))

(with-eval-after-load 'package-lint
  ;; Apply the :around advice to the specific package-lint function

  (advice-add 'package-lint--check-package-summary :around
              #'my-package-lint-ignore)

  (advice-add 'package-lint--check-no-emacs-in-package-name :around
              #'my-package-lint-ignore))



;;; dired

(defun my-dired-get-file-open-command (file-path)
  "Return FILE-PATH corresponding command from `dired-guess-shell-alist-user'."
  (let* ((case-fold-search nil)
         (result (seq-find (lambda (pattern)
                             (string-match-p (car pattern) file-path))
                           dired-guess-shell-alist-user)))
    (when result
      (car (cdr result)))))

(defun my-dired-open-with-external-command ()
  "Open the current file in `dired' using an external command based on file type."
  (interactive nil dired-mode)
  (if (and (fboundp 'dired-get-file-for-visit)
           (fboundp 'dired--find-possibly-alternative-file))
      (let* ((file (dired-get-file-for-visit)))
        (let ((shell-cmd (my-dired-get-file-open-command file)))
          (if shell-cmd
              (progn
                ;; (message "[RUN] %s %s" shell-cmd file)
                (if (fboundp 'quick-fasd-add-path)
                    (quick-fasd-add-path file)
                  (message "Warning: Undefined: `quick-fasd-add-path'"))
                (call-process shell-cmd nil nil nil file))
            (dired--find-possibly-alternative-file file))))
    (error "Undefined: dired-get-file-for-visit or dired--find-possibly-alternative-file")))

(with-eval-after-load 'dired

  ;; --------------------------------------------------------------------------
  ;; Functions
  ;; --------------------------------------------------------------------------
  (defun my-dired-home ()
    "Dired home."
    (interactive)
    (dired "~/"))

  ;; --------------------------------------------------------------------------
  ;; Abbreviate dired header
  ;; https://emacs.stackexchange.com/questions/33799/is-there-any-way-to-abbreviate-dired-header
  ;;
  ;; I modified it to make it only modify the first line
  ;;
  ;; NOTE: does not work. it sometimes changes where the directory is
  ;; --------------------------------------------------------------------------
  ;; TODO: Contribution to Emacs?
  ;; (defvar dired-abbreviate-header t)
  ;;
  ;; (defun my-dired-readin-abbreviate-header (&rest _)
  ;;   "Abbreviate home directory path to '~' in the first line of the buffer."
  ;;   (when dired-abbreviate-header
  ;;     (save-excursion
  ;;       (goto-char (point-min))
  ;;       (let ((inhibit-read-only t)
  ;;             (case-fold-search nil)
  ;;             (home (expand-file-name "~"))
  ;;             (line-end (line-end-position)))
  ;;         (while (search-forward home line-end t)
  ;;           (replace-match "~" t t))))))
  ;;
  ;; (advice-add 'dired-readin :after 'my-dired-readin-abbreviate-header)

  ;; --------------------------------------------------------------------------
  ;; Using xdg-open/open/start for certain filetypes
  ;; --------------------------------------------------------------------------
  (defvar my-dired-xdg-open-cmd nil)

  (defun my-dired-xdg-open ()
    "Make Dired open the file under the cursor."
    (interactive)
    ;; Removed: (dired-get-filename nil t)
    (if (fboundp 'dired-get-file-for-visit)
        (let* ((file (dired-get-file-for-visit)))
          (when my-dired-xdg-open-cmd
            (call-process my-dired-xdg-open-cmd nil nil nil file)))
      (error "Undefined: dired-get-file-for-visit")))

  (when-let* ((cmd (cond (IS-MAC "open")
                         (IS-LINUX "xdg-open")
                         (IS-WINDOWS "start"))))
    (when cmd
      (setq my-dired-xdg-open-cmd cmd)
      (setq dired-guess-shell-alist-user
            `(("\\.\\(?:docx\\|pdf\\|odt\\|odg\\|ods\\|djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpe?g\\|webp\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|m4a\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ;; ("\\.csv\\'" ,cmd)
              ;; ("\\.html?\\'" ,cmd)
              ;; ("\\.md\\'" ,cmd)
              ))))

  ;; Advise `dired-find-file' to use `my-dired-open-with-external-command'
  ;; instead
  (advice-add 'dired-find-file :override #'my-dired-open-with-external-command))

;;; Last resort hjkl

(defun my-forward-char-same-line ()
  "Forward char can go paste the ellipsis.
This function prevents forward char from going to another line, which is
generally one of the lines that are folded."
  (interactive)
  (ignore-errors
    (if (or (derived-mode-p 'fundamental-mode)
            (region-active-p))
        (right-char)
      (when (= (line-number-at-pos)
               (save-excursion (right-char)
                               (line-number-at-pos)))
        (right-char)))))

(defun my-backward-char-same-line ()
  "Backward to the same line."
  (interactive)
  (ignore-errors
    (if (or (derived-mode-p 'fundamental-mode)
            (region-active-p))
        (left-char)
      (if (= (current-column) 0)
          ;; Prevent the cursor from changing the line
          (when (= (line-number-at-pos)
                   (save-excursion (left-char)
                                   (line-number-at-pos)))
            (left-char))
        ;; We can go to the previous line because the cursor could be next to an
        ;; Ellipsis
        (left-char)))))

(with-eval-after-load 'eldoc
  (eldoc-add-command 'my-backward-char-same-line)
  (eldoc-add-command 'my-forward-char-same-line))

;; Last resort hjkl
(unless noninteractive
  (when (fboundp 'my-backward-char-same-line)
    (global-set-key (kbd "M-h") #'my-backward-char-same-line))
  (when (fboundp 'my-forward-char-same-line)
    (global-set-key (kbd "M-l") #'my-forward-char-same-line)))

;;; Persist text scale

(defun my-window-redisplay ()
  "Redisplay window."
  (when (bound-and-true-p text-scale-mode-amount)
    (let ((amount text-scale-mode-amount))
      (when (or (not (bound-and-true-p
                      my-window-redisplay-last-text-scale-amount))
                (/= amount my-window-redisplay-last-text-scale-amount))
        (setq-local my-window-redisplay-last-text-scale-amount amount)
        (let ((window (selected-window)))
          (cond
           ((<= emacs-major-version 27)
            (run-hook-with-args 'window-size-change-functions window))
           ((> emacs-major-version 27)
            (run-hook-with-args 'window-state-change-functions window)))))))

  (run-hooks 'window-configuration-change-hook))

(defun my-persist-text-scale-adjust ()
  "Ensure the window is updated.
I have identified an issue that affects Emacs packages such as eat (terminal)
and visual-fill-column. Functions like `text-scale-increase`,
`text-scale-decrease`, and `text-scale-set` do not trigger hooks like
`window-configuration-change-hook`. As a result, the eat package does not
immediately update the window when the text scale is changed, and
visual-fill-column does not update the margin right away (it updates only after
the window is resized). This function fixes these issues."
  (when (or (derived-mode-p 'eat-mode)
            (bound-and-true-p visual-fill-column-mode))
    (my-window-redisplay)))

(with-eval-after-load 'persist-text-scale
  (defun my-persist-text-scale-function ()
    ;; TODO: Add to the official?
    ;;
    ;; Corfu context menu adjusts the text size based on the size of the
    ;; window from which the text completion is triggered. It should be
    ;; ignored.
    (let ((buffer-name (buffer-name)))
      (cond
       ((string= buffer-name " *transient*")
        :ignore)

       ;; TODO: add to the official one
       ((string-prefix-p "*Embark Export:" buffer-name)
        "c:embark-export")

       ((string-prefix-p "*sdcv:" buffer-name)
        "c:sdcv"))))

  (defvar my-window-redisplay-last-text-scale-amount nil)

  ;; Force windows update
  (progn
    (add-hook 'text-scale-mode-hook #'my-persist-text-scale-adjust))

  (setq persist-text-scale-buffer-category-function 'my-persist-text-scale-function))

;;; ediff: Synchronize text scale when ediff starts

(defun my-ediff-sync-text-scale ()
  "Synchronize the text scale zoom of all ediff buffers to match Buffer A."
  (let ((zoom-level (with-current-buffer ediff-buffer-A
                      (if (bound-and-true-p text-scale-mode)
                          text-scale-mode-amount
                        0))))
    (when (buffer-live-p ediff-buffer-B)
      (with-current-buffer ediff-buffer-B
        (text-scale-set zoom-level)))
    (when (and (boundp 'ediff-buffer-C)
               (buffer-live-p ediff-buffer-C))
      (with-current-buffer ediff-buffer-C
        (text-scale-set zoom-level)))))

(add-hook 'ediff-startup-hook #'my-ediff-sync-text-scale)

;;; ediff: Synchronize text scale when the user changes it

(defun pkg-diff--ediff-control-panel-window ()
  "Return the window displaying the *Ediff Control Panel* buffer, if any."
  (seq-find (lambda (win)
              ;; The Ediff control panel buffer name may have suffixes like
              ;; "<2>", e.g., "*Ediff Control Panel<2>*", if multiple sessions
              ;; are active.
              (string-prefix-p "*Ediff Control Panel"
                               (buffer-name (window-buffer win))))
            (window-list)))

(defun pkg-diff--ediff-buffers ()
  "Return list of live ediff buffers A, B, and C, if bound."
  (when-let* ((window (pkg-diff--ediff-control-panel-window)))
    (with-selected-window window
      (delq nil
            (list (and (bound-and-true-p ediff-buffer-A) ediff-buffer-A)
                  (and (bound-and-true-p ediff-buffer-B) ediff-buffer-B)
                  (and (bound-and-true-p ediff-buffer-C) ediff-buffer-C))))))

(defun pkg-diff--ediff-auto-text-scale (&rest _)
  "Synchronize text scale across all Ediff buffers based on Ediff buffer A."
  (when (and (boundp 'text-scale-mode-hook)
             (bound-and-true-p text-scale-mode-amount))
    (let ((window (pkg-diff--ediff-control-panel-window)))
      (if (not window)
          (with-no-warnings
            (remove-hook 'text-scale-mode-hook #'pkg-diff--ediff-auto-text-scale t))
        (let ((original-buf (current-buffer))
              (original-buf-text-scale-amount text-scale-mode-amount))
          (with-selected-window window
            (when (and (bound-and-true-p ediff-buffer-A)
                       (buffer-live-p ediff-buffer-A))
              (with-current-buffer ediff-buffer-A
                (dolist (buf (pkg-diff--ediff-buffers))
                  (when (and (buffer-live-p buf)
                             (not (eq buf original-buf)))
                    (with-current-buffer buf
                      (with-no-warnings
                        (let ((text-scale-mode-hook
                               (delq 'pkg-diff--ediff-auto-text-scale
                                     text-scale-mode-hook)))
                          (text-scale-set original-buf-text-scale-amount))))))))))))))

(defun pkg-diff--setup-ediff-auto-text-scale ()
  "Add a buffer-local hook to keep text scale synchronized during Ediff sessions.
This installs `pkg-diff--ediff-auto-text-scale` on `text-scale-mode-hook` in
each Ediff buffer when the session starts, and cleans up automatically when the
session ends."
  (with-no-warnings
    (add-hook 'text-scale-mode-hook #'pkg-diff--ediff-auto-text-scale 99 t)))

(add-hook 'ediff-prepare-buffer-hook #'pkg-diff--setup-ediff-auto-text-scale)

;;; tree sitter

(setq treesit-language-source-alist
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
         ;; For split parsers like Markdown, the extra two fields are required:
         ;; 1. "split_parser" indicates that this language uses a parser split
         ;;    into multiple components.
         ;; 2. The directory path (e.g., "tree-sitter-markdown/src") points to
         ;;    the location of the parser source within the repository. Without
         ;;    these, treesit would not be able to find and compile the parser
         ;;    correctly.
         ;;
         ;; A split parser is a Tree-sitter parser that is divided into multiple
         ;; smaller parsers instead of being a single file or module. Each
         ;; smaller parser handles a part of the language, such as different
         ;; syntaxes or embedded languages, and together they form the complete
         ;; parser. This approach makes it easier to manage complex languages,
         ;; like Markdown, which can contain code blocks, inline formatting, and
         ;; other embedded languages. In Emacs, specifying "split_parser" and
         ;; the source directory tells treesit how to find and build all the
         ;; pieces correctly.
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
        (glsl "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (julia "https://github.com/tree-sitter/tree-sitter-julia")
        (make "https://github.com/tree-sitter-grammars/tree-sitter-make")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (scala "https://github.com/tree-sitter/tree-sitter-scala")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
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
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")))

(defun my-treesit-update-language-grammar ()
  "Update language grammar."
  (interactive)
  (treesit-install-language-grammar 'markdown)
  (treesit-install-language-grammar 'markdown-inline)
  (treesit-install-language-grammar 'python)
  (treesit-install-language-grammar 'bash)
  (treesit-install-language-grammar 'yaml)
  (treesit-install-language-grammar 'json)
  (treesit-install-language-grammar 'html)
  (treesit-install-language-grammar 'lua)
  (treesit-install-language-grammar 'c)
  (treesit-install-language-grammar 'cpp)
  (treesit-install-language-grammar 'dockerfile)
  (treesit-install-language-grammar 'go)
  (treesit-install-language-grammar 'java)
  (treesit-install-language-grammar 'javascript)
  (treesit-install-language-grammar 'php)
  ;; (treesit-install-language-grammar 'toml)
  ;; (treesit-install-language-grammar 'make)
  )

(defun my-setup-yaml-mode ()
  "Config Yaml mode."
  ;; TODO put it back
  ;; (setq-local indent-line-function 'smartindent-indent-relative-to-visible)
  t
  )


;; Highlight $variables
;; This one works, but it also highlights the variable that are in comments
(defvar sh-script-extra-font-lock-keywords
  '(("\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\|[[:digit:]]+\\)"
     (2 font-lock-variable-name-face prepend))))

(defun sh-script-extra-font-lock-activate ()
  "Activate additional font-locking for variables in double-quoted strings."
  (font-lock-add-keywords nil sh-script-extra-font-lock-keywords))

(add-hook 'sh-mode-hook #'sh-script-extra-font-lock-activate)
(add-hook 'bash-ts-mode-hook #'sh-script-extra-font-lock-activate)

;; When auto-mode-alist is bypassed, use a hook function
(defun ansible-detect-and-enable-mode ()
  "Enable `ansible-mode' for YAML files in Ansible-related directories."
  ;; This works better than auto-mode-alist
  (when (and (not (derived-mode-p 'ansible-mode))
             buffer-file-name
             (string-match
              my-ansible-file-regexp
              buffer-file-name)
             (fboundp 'ansible-mode))
    (funcall 'ansible-mode)))

(defun my-config-tree-sitter ()
  "Config Tree Sitter."
  (when (my-treesit-language-available-p 'c)
    (push '(c-mode . c-ts-mode) major-mode-remap-alist))

  (when (my-treesit-language-available-p 'cpp)
    (push '(c++-mode . c++-ts-mode) major-mode-remap-alist))

  (when (my-treesit-language-available-p 'go)
    (add-to-list 'auto-mode-alist '("\.[gG][oO]\\'" . go-ts-mode)))

  (when (my-treesit-language-available-p 'java)
    (push '(java-mode . java-ts-mode) major-mode-remap-alist))

  (when (my-treesit-language-available-p 'json)
    (push '(js-json-mode . json-ts-mode) major-mode-remap-alist))

  (if (my-treesit-language-available-p 'yaml)
      (progn
        (define-derived-mode ansible-mode yaml-ts-mode "Ansible"
          "Major mode for editing Ansible files.")

        (defun my-setup-ansible-mode ()
          ;; (set-syntax-table (copy-syntax-table))

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
        (when (fboundp 'my-setup-ansible-mode)
          (add-hook 'ansible-mode-hook #'my-setup-ansible-mode))

        ;; Remove the auto-mode-alist entry (Useful to prevent yaml-ts-mode from
        ;; activating on ansible-mode)
        (with-eval-after-load 'yaml-ts-mode
          (setq auto-mode-alist
                (rassq-delete-all 'yaml-ts-mode auto-mode-alist))

          (push '(yaml-mode . yaml-ts-mode) major-mode-remap-alist))

        ;; Treesitter
        (add-hook 'yaml-ts-mode-hook #'my-setup-yaml-mode))
    (when (fboundp 'yaml-mode)
      (define-derived-mode ansible-mode yaml-mode "Ansible"
        "Major mode for editing Ansible files.")))

  (defvar my-ansible-file-regexp
    (rx "/"
        (group (or "tasks"
                   "handlers"
                   "vars"
                   "defaults"
                   "ansible"
                   "playbooks"))
        "/" (+ (not (any "/\\")))
        "." (regexp "[yY][aA]?[mM][lL]")
        string-end))

  (add-to-list 'auto-mode-alist (cons my-ansible-file-regexp 'ansible-mode))

  (add-hook 'yaml-mode-hook #'ansible-detect-and-enable-mode)
  (add-hook 'yaml-ts-mode-hook #'ansible-detect-and-enable-mode)

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


  (when (my-treesit-language-available-p 'markdown)
    (add-to-list 'auto-mode-alist '("\\.[lL][uU][aA]\\'" . lua-ts-mode)))

  (when (my-treesit-language-available-p 'dockerfile)
    (add-to-list 'auto-mode-alist '("/[dD][oO][cC][kK][eE][rR]\\'"
                                    . dockerfile-ts-mode))
    (add-to-list 'auto-mode-alist '("/[dD][oO][cC][kK][eE][rR][fF][iI][lL][eE]\\'"
                                    . dockerfile-ts-mode)))
  )

(add-hook 'lightemacs-after-init-hook #'my-config-tree-sitter)

;;; ansible

(add-to-list 'display-buffer-alist '("\\*ansible-doc"
                                     (display-buffer-same-window)))

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

(defun ansible-doc-local-setup-buffer ()
  "Setup `ansible-doc'."
  (setq-local evil-lookup-func 'ansible-doc-symbol))

(add-hook 'ansible-mode-hook 'ansible-doc-local-setup-buffer)

;;; Highlight codetags

;; TODO: do not do anything
;; FIXME: Fix this
;; BUG: there is a bug here
;; XXX: Good job
;; NOTE: This is important
;; HACK! Bad

(defvar highlight-codetags-keywords
  '(("\\<\\(TODO\\|FIXME\\|BUG\\|XXX\\)\\>" 1 font-lock-warning-face prepend)
    ("\\<\\(NOTE\\|HACK\\|DONE\\)\\>" 1 font-lock-doc-face prepend)))

(define-minor-mode highlight-codetags-local-mode
  "Highlight codetags like TODO, FIXME..."
  :global nil
  (if highlight-codetags-local-mode
      (font-lock-add-keywords nil highlight-codetags-keywords)
    (font-lock-remove-keywords nil highlight-codetags-keywords))

  (when (bound-and-true-p font-lock-mode)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

;;; Disable arrow in the fringe

(defun my-minibuffer-mode-setup ()
  "Setup minibuffer mode."
  ;; Remove the arrow when the line is wrapped
  (my-disable-fringe-truncation-arrow)
  (setq-local truncate-lines t))

(when (fboundp 'my-minibuffer-mode-setup)
  (add-hook 'minibuffer-setup-hook #'my-minibuffer-mode-setup))

(add-hook-text-editing-modes #'my-disable-fringe-truncation-arrow)
(with-eval-after-load 'consult
  (add-hook 'consult-preview-allowed-hooks #'my-disable-fringe-truncation-arrow))

(add-hook-text-editing-modes #'highlight-codetags-local-mode)
(with-eval-after-load 'consult
  (add-hook 'consult-preview-allowed-hooks #'highlight-codetags-local-mode))

;; Enable smerge
(defun my-enable-smerge-maybe ()
  "Enable `smerge'."
  (when (and buffer-file-name
             (vc-backend buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode +1)))))

(add-hook 'find-file-hook #'my-enable-smerge-maybe)

;; Switch to `fundamental-mode' when `smerge-mode' is activated.
(defun my-smerge-to-fundamental-mode ()
  "Switch to `fundamental-mode' when `smerge-mode' is activated."
  (when (and smerge-mode (not (eq major-mode 'fundamental-mode)))
    (fundamental-mode)
    (smerge-mode 1)))
(add-hook 'smerge-mode-hook #'my-smerge-to-fundamental-mode)

;;; Prune cache

;; TODO lightemacs?
;; TODO minimal-emacs.d?

(defvar my-native-compile-prune nil
  "Prune.")

(defvar my-native-compile--prune-cache-done nil
  "Flag to ensure native compile cache is pruned only once per session.")

(defun my-native-compile-prune-cache ()
  "Prune the native compile cache safely.
This function checks for native compilation support and ensures the operation
only runs once per session to avoid redundant I/O."
  (interactive)
  ;; Check if we have already run this session
  (unless my-native-compile--prune-cache-done
    ;; Check if native compilation is actually available
    (when (and (fboundp 'native-comp-available-p)
               (native-comp-available-p)
               (fboundp 'native-compile-prune-cache))
      ;; Set flag immediately to prevent re-entry
      (setq my-native-compile--prune-cache-done t)
      (message "[native-comp] Native compilation cache pruned")
      (with-demoted-errors "Error pruning native cache: %S"
        (native-compile-prune-cache)))))

;; Only register the timer if native compilation is enabled
(when (and my-native-compile-prune
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Run once after 5 minutes of idle time
  (add-hook 'kill-emacs-hook 'my-native-compile-prune-cache)

  ;; 5 minutes: This is the standard definition of "Away From Keyboard." If you
  ;; haven't touched Emacs for 5 minutes, you have likely stepped away (coffee,
  ;; meeting, etc.). This is the safest time to burn CPU cycles or disk I/O
  ;; without affecting user experience.
  (add-hook 'lightemacs-after-init-hook
            #'(lambda()
                (run-with-idle-timer (* 5 60) nil #'my-native-compile-prune-cache))))

;;; Prune native comp tmp files

(defun my-clean-native-comp-temp-files ()
  "Delete temporary native compilation files from the temporary directory.
This targets files matching `emacs-async-comp-*' and
`emacs-int-comp-*' (including trampoline files) ending in .el."
  (interactive)
  (let* ((target-dir temporary-file-directory)
         ;; Regex matches both emacs-async-comp- and emacs-int-comp-
         ;;
         ;; Async Compilation Files (emacs-async-comp-): These files are
         ;; generated during the standard asynchronous native compilation of
         ;; Emacs Lisp libraries.
         ;;
         ;; Internal/Trampoline Files (emacs-int-comp-): These
         ;; files—specifically those matching your example
         ;; emacs-int-comp-subr--trampoline—are generated to handle function
         ;; advice on C primitives. The emacs-int-comp- files are specific to
         ;; native compilation. They are not generated by other standard Emacs
         ;; processes. Origin: Emacs primitives (functions written in C, like
         ;; write-region or car) cannot be natively advised (modified) directly.
         ;; When you use advice-add on a C primitive, Emacs must generate a
         ;; "trampoline" function. This small bridge function calls the original
         ;; C code while allowing Lisp-level advice to intervene.
         (regexp "^emacs-\\(?:async\\|int\\)-comp-.*\\.el$")
         ;; (regexp "^emacs-async-comp-.*\\.el$")
         (files (directory-files target-dir t regexp)))
    (dolist (file files)
      ;; Check existence to avoid race conditions
      ;; Safety Check: Explicitly verify the filename starts with "emacs"
      ;; Uses file-name-nondirectory to ignore the path components.
      (if (string-prefix-p "emacs-" (file-name-nondirectory file))
          (progn
            (delete-file file)
            ;; (message "Deleted native-comp temp file: %s" file)
            )
        ;; (message "Safety check failed (skipping): %s" file)
        ))))

;; Run this function automatically when asynchronous native compilation finishes
(add-hook 'native-comp-async-all-done-hook #'my-clean-native-comp-temp-files)
;; (add-hook 'kill-emacs-hook #'my-clean-native-comp-temp-files)

;;; vc

(defun my-log-view-diff-stay (orig-fun &rest args)
  "Advice to keep focus in the log buffer when showing diffs.
ORIG-FUN - the original function being advised
ARGS - the arguments passed to the original function"
  (save-selected-window
    (apply orig-fun args)))

(with-eval-after-load 'log-view
  (advice-add 'log-view-diff :around #'my-log-view-diff-stay))

;;; Modeline
(add-hook 'lightemacs-after-init-hook #'display-time-mode)
(setq display-time-mail-function #'ignore)
(setq display-time-mail-string "")
(setq display-time-mail-directory nil)
(setq display-time-use-mail-icon nil)
(setq display-time-mail-face nil)
(setq display-time-format " %Y-%m-%d  %I:%M %p")

(defun mode-line-right ()
  "Render the `mode-line-right-format'."
  (let* ((mode-line-right-format '(mode-line-front-space
                                   mode-line-misc-info
                                   mode-line-end-spaces))
         (formatted-line (format-mode-line mode-line-right-format)))
    (list (propertize
           " "
           'display
           `(space :align-to (+ 2
                                (- right
                                   (+ ,(string-width formatted-line) right-fringe
                                      right-margin)))))
          formatted-line)))

(defun my-gc-cons-threshold-mode-line ()
  "Return a short string with the current `gc-cons-threshold`."
  (format " GC:%s" (cond
                    ((= gc-cons-threshold most-positive-fixnum) "∞")
                    (t (format "%sM" (/ gc-cons-threshold 1000000))))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-modified
                "  |  "
                mode-line-buffer-identification
                "  |  "
                (vc-mode vc-mode)
                (:eval
                 (if (fboundp 'my-project-name)
                     (let ((project-name (my-project-name)))
                       (if project-name
                           (format "  |  Project:%s" project-name)
                         (format "  |  Dir:%s"
                                 (abbreviate-file-name (buffer-cwd)))))
                   "")
                 )
                "  |  "
                mode-line-position
                "  |  "
                (:eval (my-gc-cons-threshold-mode-line))
                ;; mode-line-modes
                ;; Slow eval
                (:eval (mode-line-right))))

;;; auto insert if new file

;; This is called by main.el
(defun my/autoinsert-yas-expand()
  "Replace text with Yasnippet template."
  (when (fboundp 'yas-expand-snippet)
    (condition-case nil
        (progn
          (funcall 'yas-expand-snippet (buffer-string) (point-min) (point-max))
          (when (and (not (bobp))
                     (fboundp 'evil-insert-state)
                     (not (zerop (buffer-size))))
            (evil-insert-state)))
      (error
       nil))))

(defun my-auto-insert-if-new-file ()
  "Auto-insert template only if the file is newly created and does not exist."
  (when-let* ((file-name (buffer-file-name (buffer-base-buffer))))
    (when (and (not (file-exists-p file-name))
               (= (buffer-size) 0))
      ;; Execute the default auto-insert function or custom logic here. For
      ;; simplicity, we invoke `auto-insert` directly.
      (condition-case nil
          (progn
            (when (bound-and-true-p yas-minor-mode) (auto-insert)))
        ;; Ignore errors
        (error
         nil)))))

(defun config-template-system ()
  "Configure the template system."
  ;; Add the custom function to `find-file-hook`
  (add-hook 'find-file-hook 'my-auto-insert-if-new-file)

  ;; :config
  (let ((template-elisp-file (expand-file-name "main.el"
                                               auto-insert-directory)))
    (let ((inhibit-message t))
      (load template-elisp-file :no-error :no-message))))

(setq auto-insert 'other)
(setq auto-insert-query nil)
(setq auto-insert-alist nil)  ;; Will be changed by template-elisp-file
(setq auto-insert-directory (expand-file-name "file-templates-auto/"
                                              "~/.emacs-data/etc")) ;;; Or use custom, *NOTE* Trailing slash important
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion

(add-hook 'lightemacs-after-init-hook 'config-template-system)

;;; hideshow

(with-eval-after-load 'hideshow
  ;; TODO lightemacs?
  ;; Fringe
  (define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])
  (defcustom hs-fringe-face 'hs-fringe-face
    "*Specify face used to highlight the fringe on hidden regions."
    :type 'face
    :group 'hideshow)
  (defface hs-fringe-face
    '((t (:foreground "#888" :box (:line-width 2 :color "grey75"
                                               :style released-button))))
    "Face used to highlight the fringe on folded regions"
    :group 'hideshow)
  (defcustom hs-face 'hs-face
    "*Specify the face to to use for the hidden region indicator"
    :type 'face
    :group 'hideshow)
  (defface hs-face
    '((t (:foreground "grey" :background unspecified :box nil)))
    "Face to hightlight the ... area of hidden regions"
    :group 'hideshow)

  (defun display-code-line-counts (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* (;;(marker-string "*fringe-dummy*")
             ;; (marker-length (length marker-string))
             (display-string (format "(%d)..."
                                     (count-lines (overlay-start ov)
                                                  (overlay-end ov)))))
        (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
        ;; (put-text-property 0 marker-length 'display
        ;;                    (list 'left-fringe 'hs-marker 'hs-fringe-face)
        ;;                    marker-string)
        ;; (overlay-put ov 'before-string marker-string)
        (put-text-property 0 (length display-string) 'face 'hs-face
                           display-string)
        (overlay-put ov 'display display-string))))
  (setq hs-set-up-overlay 'display-code-line-counts))

;;===========================================================================
;; Geometry
;;===========================================================================
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

;;; server

;; (custom-set-variables '(package-selected-packages nil))

(setq server-client-instructions nil)

;;; quiet

;; In addition to the shut-up package, this module provides the
;; `lightemacs-shut-up-advice-add' function, which prevents a function from
;; displaying messages.

;; readme: In addition to the *shut-up* package, this module provides the
;; `lightemacs-shut-up-advice-add` function, which attaches advice to a given
;; function to suppress all of its output.

(defun lightemacs--shut-up-funcall (fn &rest args)
  "Call FN with ARGS while suppressing all output.
This function evaluates FN with the given ARGS while redirecting output that
would normally be sent to `standard-output' and suppressing messages produced by
`message'. It also overrides `write-region' and `load' with custom
implementations that prevent unintended output."
  ;; I have an issue with shut-up TODO especially with straight
  ;; (shut-up
  ;;   (apply fn args))
  (let ((inhibit-message t))
    (apply fn args)
    ;; (cl-letf
    ;;     ;; Override `standard-output' (for `print'), `message',
    ;;     ;; `write-region', `load'.
    ;;     ((standard-output #'ignore)
    ;;      ((symbol-function 'message) 'ignore)
    ;;      ;; ((symbol-function 'write-region) 'shut-up-write-region)
    ;;      ;; ((symbol-function 'write-region) 'shut-up-write-region)
    ;;      ;; ((symbol-function 'load) 'shut-up-load)
    ;;      )
    ;;   (apply fn args))
    ))

;;;###autoload
(defun lightemacs-shut-up-advice-add (fn)
  "Advise the FN function so that all its output is suppressed.
This attaches an around-advice to FN using `lightemacs--shut-up-funcall',
ensuring that when FN is invoked, it produces no messages, does not write to
`standard-output', and does not display output from `write-region' or `load'."
  (advice-add fn :around #'lightemacs--shut-up-funcall))

;;;###autoload
(defun lightemacs-shut-up-advice-remove (fn)
  "Remove the silence advice from the FN function.
This detaches the around-advice previously installed by
`lightemacs-shut-up-advice-add', restoring FN to its original behavior where
messages and output are no longer suppressed."
  (advice-remove fn #'lightemacs--shut-up-funcall))

(with-eval-after-load 'undo-fu-session
  (lightemacs-shut-up-advice-add 'undo-fu-session--recover-safe))

(with-eval-after-load 'evil
  (lightemacs-shut-up-advice-add 'evil-redo)
  (lightemacs-shut-up-advice-add 'evil-undo))

(with-eval-after-load 'sh-script
  (when (fboundp 'sh-set-shell)
    (lightemacs-shut-up-advice-add 'sh-set-shell)))

;; TODO silence it
;; (lightemacs-shut-up-advice-add 'toggle-truncate-lines)

;; (with-eval-after-load 'flyspell
;;   (lightemacs-shut-up-advice-add 'flyspell-prog-mode)
;;   (lightemacs-shut-up-advice-add 'flyspell-mode))

;; (with-eval-after-load 'recentf
;;   (lightemacs-shut-up-advice-add 'recentf-save-list)
;;   (lightemacs-shut-up-advice-add 'recentf-cleanup)
;;   (lightemacs-shut-up-advice-add 'recentf-mode))

;;; recenter after jump


(defvar lightemacs-maybe-recenter-after-jump t
  "Non-nil enables recentering the window when the point jumps out of view.
Recentering only occurs when `scroll-conservatively' is >= 101. The recenter
position can be customized using `lightemacs-maybe-recenter-after-jump-value'.")

(defvar lightemacs-maybe-recenter-after-jump-value 12
  "The line position for recentering the window when the point jumps out of view.
Only used when `lightemacs-maybe-recenter-after-jump' is non-nil and
`scroll-conservatively' is >= 101. A numeric value indicates the number of lines
from the top of the window; nil recenters in the middle.")

(require 'lightemacs)  ;; lightemacs-recenter-maybe

(defun lightemacs-default-settings--recenter-maybe ()
  "Recenter conditionally when `scroll-conservatively' is set to 101 or higher.
This ensures that conservative scrolling is preserved while maintaining point
visibility when navigation commands are executed."
  (when (and lightemacs-maybe-recenter-after-jump
             (>= scroll-conservatively 101))
    (lightemacs-recenter-maybe lightemacs-maybe-recenter-after-jump-value)))

(defun lightemacs-default-settings--recenter-maybe-adjust-arg ()
  "Recenter conditionally when `scroll-conservatively' is set to 101 or higher.
This ensures that conservative scrolling is preserved while maintaining point
visibility when navigation commands are executed."
  (when (and lightemacs-maybe-recenter-after-jump
             (>= scroll-conservatively 101))
    (lightemacs-recenter-maybe lightemacs-maybe-recenter-after-jump-value t)))

(defun lightemacs-default-settings--advice-recenter-maybe-adjust-arg (fn &rest args)
  "FN is the advised function. ARGS are the function arguments."
  (unwind-protect
      (apply fn args)
    (lightemacs-default-settings--recenter-maybe-adjust-arg)))

;; TODO use post-command-hook?

(defun lightemacs-default-settings--advice-recenter-maybe (fn &rest args)
  "FN is the advised function. ARGS are the function arguments."
  (unwind-protect
      (apply fn args)
    (lightemacs-default-settings--recenter-maybe)))

;; TODO use a loop to add to hooks and advice functions
(with-eval-after-load 'flymake
  (advice-add 'flymake-goto-next-error :around
              #'lightemacs-default-settings--advice-recenter-maybe)
  (advice-add 'flymake-goto-prev-error :around
              #'lightemacs-default-settings--advice-recenter-maybe))

(with-eval-after-load 'evil
  (advice-add 'evil-goto-mark :around
              #'lightemacs-default-settings--advice-recenter-maybe-adjust-arg)

  (advice-add 'evil-ex-search-previous :around
              #'lightemacs-default-settings--advice-recenter-maybe-adjust-arg)
  (advice-add 'evil-ex-search-next :around
              #'lightemacs-default-settings--advice-recenter-maybe-adjust-arg))

;; When the user presses C-o
(unless noninteractive
  (add-hook 'evil-jumps-post-jump-hook
            #'lightemacs-default-settings--recenter-maybe-adjust-arg 70))

(with-eval-after-load 'simple
  (add-hook 'next-error-hook
            #'lightemacs-default-settings--recenter-maybe-adjust-arg))

(with-eval-after-load 'xref
  (let ((xref-pulse-originally-present (memq 'xref-pulse-momentarily
                                             xref-after-jump-hook)))
    (remove-hook 'xref-after-jump-hook 'recenter)
    (remove-hook 'xref-after-jump-hook 'xref-pulse-momentarily)

    (add-hook 'xref-after-return-hook
              #'lightemacs-default-settings--recenter-maybe 70)

    (add-hook 'xref-after-jump-hook
              'lightemacs-default-settings--recenter-maybe 70)
    (when xref-pulse-originally-present
      (add-hook 'xref-after-jump-hook 'xref-pulse-momentarily 71))))

;;; Target hooks

;;(setq-local package-lint-main-file
;;            (file-name-nondirectory
;;             (buffer-file-name (buffer-base-buffer))))

(defun my-prevent-execution-only-when-code-checker-allowed (orig-fun &rest args)
  "Execute ORIG-FUN with ARGS only if it is allowed.
This function is intended for use as :around advice."
  (when (and (fboundp 'my-code-checker-allowed-p)
             (my-code-checker-allowed-p))
    (apply orig-fun args)))

(with-eval-after-load 'le-aggressive-indent
  (advice-add 'aggressive-indent-mode :around
              #'my-prevent-execution-only-when-code-checker-allowed))

(with-eval-after-load 'le-stripspace
  (advice-add 'stripspace-local-mode :around
              #'my-prevent-execution-only-when-code-checker-allowed))

(with-eval-after-load 'le-apheleia
  (advice-add 'apheleia-mode :around
              #'my-prevent-execution-only-when-code-checker-allowed))

(with-eval-after-load 'le-flymake
  (advice-add 'flymake-mode :around
              #'my-prevent-execution-only-when-code-checker-allowed))

;;; Lazy loader

(lightemacs-use-package lazy-loader
  :ensure nil
  :commands lazy-loader-mode
  :hook
  (lightemacs-after-init . lazy-loader-mode)
  :custom
  (lazy-loader-verbose nil)
  (lazy-loader-modules '(org term))
  (lazy-loader-files (delq nil
                           (list (when (bound-and-true-p file-path-todo)
                                   file-path-todo))))
  ;; (lazy-loader-buffers
  ;;  '(("*tmux*" .
  ;;     (lambda ()
  ;;       (let ((buf (get-buffer-create "*tmux*")))
  ;;         (with-current-buffer buf
  ;;           (vterm-mode)
  ;;
  ;;           (vterm-send-string "tmux-session -l emacs")
  ;;           (vterm-send-string "\n")
  ;;           (vterm-send-return))
  ;;         buf)))))
  )

;;; Themes config

(when (fboundp 'lightemacs-theme-create-loader)

  ;; (lightemacs-theme-create-loader "nano-light" 'nano-theme)
  ;; (lightemacs-theme-create-loader "nano-dark" 'nano-theme)

  (lightemacs-theme-create-loader "modus-operandi" 'modus-themes)
  (lightemacs-theme-create-loader "modus-operandi-tinted" 'modus-themes)
  (lightemacs-theme-create-loader "modus-operandi-tritanopia" 'modus-themes)
  (lightemacs-theme-create-loader "modus-operandi-deuteranopia" 'modus-themes)
  (lightemacs-theme-create-loader "modus-vivendi" 'modus-themes)
  (lightemacs-theme-create-loader "modus-vivendi-tinted" 'modus-themes)
  (lightemacs-theme-create-loader "modus-vivendi-tritanopia" 'modus-themes)
  (lightemacs-theme-create-loader "tango-dark" 'modus-themes)
  (lightemacs-theme-create-loader "tango" 'modus-themes)
  (lightemacs-theme-create-loader "tsdh-light" 'modus-themes)

  (lightemacs-theme-create-loader "tomorrow-night-deepblue"
                                  'tomorrow-night-deepblue-theme)

  ;;(use-package tomorrow-night-deepblue-theme
  ;;  :config
  ;;  (lightemacs-theme-create-loader "tomorrow-night-deepblue"))

  ;; Not as good as doom themes
  ;; (lightemacs-theme-create-loader "gruvbox-light-soft" 'gruvbox)
  ;; (lightemacs-theme-create-loader "gruvbox-light-medium" 'gruvbox)
  ;; (lightemacs-theme-create-loader "gruvbox-light-hard" 'gruvbox)

  ;; (with-eval-after-load 'doom-themes
  ;;   (doom-themes-org-config))
  (lightemacs-theme-create-loader "doom-gruvbox-light" 'doom-themes)
  (lightemacs-theme-create-loader "doom-one" 'doom-themes)
  (lightemacs-theme-create-loader "doom-1337" 'doom-themes)
  (lightemacs-theme-create-loader "doom-gruvbox" 'doom-themes)
  (lightemacs-theme-create-loader "doom-solarized-light" 'doom-themes)
  (lightemacs-theme-create-loader "doom-tomorrow-night" 'doom-themes)
  (lightemacs-theme-create-loader "doom-tomorrow-day" 'doom-themes)
  (lightemacs-theme-create-loader "doom-snazzy" 'doom-themes)
  (lightemacs-theme-create-loader "doom-ir-black" 'doom-themes)
  (lightemacs-theme-create-loader "doom-ayu-dark" 'doom-themes)
  (lightemacs-theme-create-loader "doom-acario-light" 'doom-themes)

  ;;(require 'ef-themes)
  ;; Dark
  ;;(lightemacs-theme-create-loader "ef-dark" "ef-themes")
  (lightemacs-theme-create-loader "ef-melissa-dark" 'ef-themes)
  (lightemacs-theme-create-loader "ef-symbiosis" 'ef-themes)
  ;; ;; Yellow
  (lightemacs-theme-create-loader "ef-melissa-light" 'ef-themes)
  (lightemacs-theme-create-loader "ef-duo-light" 'ef-themes)
  ;; ;; Blue
  (lightemacs-theme-create-loader "ef-frost" 'ef-themes)
  (lightemacs-theme-create-loader "ef-light" 'ef-themes)
  (lightemacs-theme-create-loader "ef-maris-light" 'ef-themes)
  ;; ;; Orange
  (lightemacs-theme-create-loader "ef-day" 'ef-themes)
  ;; ;; Green
  (lightemacs-theme-create-loader "ef-spring" 'ef-themes)
  (lightemacs-theme-create-loader "ef-elea-light" 'ef-themes)
  (lightemacs-theme-create-loader "ef-cyprus" 'ef-themes))

;;; flyspell

(defun my-setup-flyspell-text-mode ()
  "Setup `flyspell-mode' for `text-mode-hook'."
  ;; unless (or (derived-mode-p 'yaml-mode)
  ;;            (derived-mode-p 'yaml-ts-mode)
  ;;            (derived-mode-p 'ansible-mode))
  (if (or (derived-mode-p 'yaml-mode)
          (derived-mode-p 'yaml-ts-mode)
          (derived-mode-p 'ansible-mode))
      ;; Yaml/Ansible
      (flyspell-prog-mode)
    ;; Other (e.g., Markdown)
    (let ((file-name (buffer-file-name (buffer-base-buffer))))
      (when (and file-name
                 (string-suffix-p "/readme.md"
                                  (downcase file-name))
                 (string-suffix-p "/changelog.md"
                                  (downcase file-name)))
        ;; (run-with-idle-timer 1 nil #'flyspell-mode 1)
        (flyspell-mode 1)))))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'my-setup-flyspell-text-mode)

;;; Golden-ratio

;; package cl is deprecated
;; (lightemacs-use-package golden-ratio
;;   :commands (golden-ratio
;;              golden-ratio-mode
;;              golden-ratio-toggle-widescreen
;;              golden-ratio-adjust)
;;   ;; :hook
;;   ;; (add-hook 'lightemacs-after-init-hook #'golden-ratio-mode)
;;   )


;;; Focus

;; (lightemacs-use-package focus
;;   :commands (focus-mode
;;              focus-change-thing
;;              focus-pin
;;              focus-unpin
;;              focus-next-thing
;;              focus-prev-thing
;;              focus-read-only-mode
;;              focus-turn-off-read-only-mode))

;;; flymake ansible lint

(lightemacs-use-package flymake-ansible-lint
  :commands flymake-ansible-lint-setup
  :preface
  (defun my-setup-flymake-ansible-lint ()
    (when (my-code-checker-allowed-p)
      (flymake-ansible-lint-setup)))

  (defun my-setup-flymake-ansible-lint-project-dir ()
    "Configure `flymake-ansible-lint' to use the project or VC root."
    (setq-local flymake-ansible-lint-args
                (append flymake-ansible-lint-args
                        (let* ((project (project-current nil))
                               (project-root (when project
                                               (project-root project)))
                               (vc-root (unless project-root
                                          (vc-root-dir))))
                          (cond
                           (project-root
                            (list "--project-dir"
                                  (expand-file-name project-root)))

                           (vc-root
                            (list "--project-dir"
                                  (expand-file-name project-root)))

                           (t
                            nil))))))

  :init
  (add-hook 'ansible-mode-hook #'my-setup-flymake-ansible-lint-project-dir)

  (add-hook 'ansible-mode-hook #'my-setup-flymake-ansible-lint)

  (unless noninteractive
    (with-eval-after-load 'flymake
      (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
      (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)))

  (setq flymake-ansible-lint-args
        '("--offline"
          "--skip=yamllint"
          "--skip-list"
          "run-once[play],no-free-form,trailing-whitespace,yaml[line-length]")))

;; Why does it start with elisp? TODO

;;; flymake bashate
(lightemacs-use-package flymake-bashate
  :commands flymake-bashate-setup
  :config
  (setq flymake-bashate-max-line-length 80)
  ;; To make bashate ignore specific Bashate rules, such as E003 (ensure all
  ;; indents are a multiple of 4 spaces) and E006 (check for lines longer than
  ;; 79 columns), set the following variable:
  ;; (flymake-bashate-ignore "E003,E006")
  ;;
  ;; E001: trailing whitespace
  (setq flymake-bashate-ignore "E003,E001")

  :init
  (defun my-setup-flymake-bashate ()
    (when (my-code-checker-allowed-p)
      (flymake-bashate-setup)))

  (add-hook 'bash-ts-mode-hook 'my-setup-flymake-bashate)
  (add-hook 'sh-mode-hook 'my-setup-flymake-bashate))

;;; flymake yamllint

(lightemacs-use-package flymake-yamllint
  :after flymake
  :commands flymake-yamllint-setup

  :preface
  (defun my-flymake-yamllint-setup ()
    "Setup Flymake yamllint."
    (when (my-code-checker-allowed-p)
      (flymake-yamllint-setup)))

  :init
  (setq flymake-yamllint-arguments
        (list "-c" (expand-file-name "~/.yamllint_global.yml")))
  (add-hook 'yaml-mode-hook #'my-flymake-yamllint-setup)
  (add-hook 'yaml-ts-mode-hook #'my-flymake-yamllint-setup))

;;; Disabled packages

;; (lightemacs-use-package ws-butler
;;   :commands ws-butler-mode)

;; (lightemacs-use-package quickrun
;;   :commands (quickrun
;;              quickrun-replace-region
;;              quickrun-eval-print
;;              quickrun-shell
;;              quickrun-with-arg
;;              quickrun-select
;;              quickrun-region
;;              quickrun-compile-only-select
;;              quickrun-compile-only))

;; (lightemacs-use-package ztree
;;   :commands ztree-diff)

;;; auto revert

;; (setq ;; revert-without-query (list ".")  ; Do not prompt
;;  auto-revert-stop-on-user-input nil)

;;; battery angel

;; (require 'battery-angel)
;;
;; (defvar battery-angel--manage-compile-angel nil)
;;
;; (defun setup-battery-angel-on-ac ()
;;   "This is called on AC."
;;   ;; (when battery-angel-verbose
;;   ;;   (message "Applying AC parameters"))
;;
;;   (setq auto-revert-interval 3)
;;
;;   ;; Flymake
;;   (setq flymake-start-on-flymake-mode
;;         (when (> (num-processors) 8)
;;           t))
;;
;;   (setq flymake-no-changes-timeout
;;         (when (> (num-processors) 8)
;;           0.8))
;;
;;   (when (and battery-angel--manage-compile-angel
;;              (fboundp 'compile-angel-on-load-mode)
;;              (not compile-angel-on-load-mode))
;;     (compile-angel-on-load-mode 1))
;;
;;   ;; Fast Consult
;;   (setq consult-async-input-debounce 0.02
;;         consult-async-input-throttle 0.05
;;         consult-async-refresh-delay 0.02))
;;
;; (defun setup-battery-angel-on-bat ()
;;   "This is called on BAT."
;;   ;; (when battery-angel-verbose
;;   ;;   (message "Applying BAT parameters"))
;;
;;   (setq auto-revert-interval 10)
;;
;;   ;; Flymake
;;   (setq flymake-start-on-flymake-mode nil)
;;   (setq flymake-no-changes-timeout nil)
;;
;;   (when (and battery-angel--manage-compile-angel
;;              (fboundp 'compile-angel-on-load-mode)
;;              compile-angel-on-load-mode)
;;     ;; (setq battery-angel--manage-compile-angel t)
;;     (compile-angel-on-load-mode -1))
;;
;;   ;; Default consult parameters
;;   ;; (setq consult-async-input-debounce 0.1
;;   ;;       consult-async-input-throttle 0.2
;;   ;;       consult-async-refresh-delay 0.1)
;;
;;   (setq consult-async-input-debounce 0.2
;;         consult-async-input-throttle 0.5
;;         consult-async-refresh-delay 0.2))
;;
;; (setq battery-angel-verbose nil)
;; (add-hook 'lightemacs-emacs-startup-hook #'battery-angel-mode 90)
;; (add-hook 'battery-angel-on-ac-hook #'setup-battery-angel-on-ac)
;; (add-hook 'battery-angel-on-bat-hook #'setup-battery-angel-on-bat)

;;; buffer guardian

(lightemacs-use-package buffer-guardian
  :ensure nil
  :commands buffer-guardian-mode

  :hook
  (lightemacs-emacs-startup . buffer-guardian-mode)

  :init
  (setq buffer-guardian-verbose nil)
  (setq buffer-guardian-save-all-buffers-interval (* 60 30))
  (setq buffer-guardian-save-all-buffers-idle 30))

;; Simpler alternative to bg
(progn
  (setq remote-file-name-inhibit-auto-save-visited t)
  (setq auto-save-visited-interval 30)
  ;; (auto-save-visited-mode 1)

  ;; Make (save-some-buffers 1) only save buffers when they exist in the disk

  ;; Focus
  ;; (defun my-save-on-focus-change ()
  ;;   "Save all buffers when Emacs loses focus."
  ;;   (when (not (frame-focus-state))
  ;;     (my-save-all-buffers)))
  ;; (add-function :after after-focus-change-function #'my-save-on-focus-change)

  ;; Save some buffers
  (setq save-some-buffers-default-predicate
        (lambda ()
          (and (buffer-file-name (buffer-base-buffer))
               (file-exists-p buffer-file-name)))))

;;; Rainbow

;; (lightemacs-use-package rainbow-mode
;;   :commands rainbow-mode
;;   :no-require t)

;;; easysession scratch

(defvar lightemacs-easysession-save-scratch t
  "Make EasySession also save and restore the scratch buffer.")

(when lightemacs-easysession-save-scratch
  (require 'easysession-scratch)
  (when (fboundp 'easysession-scratch-mode)
    (easysession-scratch-mode 1)))

;;; flymake elisp done

(defvar lightemacs-flymake--setup-elisp-done nil
  "Non-nil once `elisp-flymake-byte-compile-load-path' has been extended.")

(defun lightemacs-flymake-initialize-elisp-path ()
  "Extend `elisp-flymake-byte-compile-load-path' with the current `load-path'.
This function ensures that the Flymake subprocess inherits the session's library
environment for accurate linting."
  (with-eval-after-load 'elisp-mode
    (unless lightemacs-flymake--setup-elisp-done
      (setq elisp-flymake-byte-compile-load-path
            (append elisp-flymake-byte-compile-load-path load-path))
      (setq lightemacs-flymake--setup-elisp-done t))))

(add-hook 'lightemacs-emacs-startup-hook
          #'lightemacs-flymake-initialize-elisp-path 99)

;;; git-timemachine

;; (lightemacs-use-package git-timemachine
;;   :commands git-timemachine
;;   :bind (:map vc-prefix-map
;;               ("t" . git-timemachine)))

;;; stillness-mode

;; (lightemacs-use-package stillness-mode
;;   :commands stillness-mode
;;   :hook (lightemacs-after-init . stillness-mode))

;;; popper

;; shell-pop-default-directory "~/src"
;; shell-pop-shell-type (cond
;;                       ((eq system-type 'gnu/linux)
;;                        '("vterm" "*vterm*" #'vterm))
;;                       (IS-WINDOWS '("eshell" "*eshell*" #'eshell))
;;                       (t '("terminal" "*terminal*"
;;                            (lambda () (term shell-pop-term-shell)))))

;; (lightemacs-use-package popper
;;   :commands (popper-mode
;;              popper-echo-mode
;;              popper-cycle
;;              popper-toggle
;;              popper-toggle-type
;;              popper-cycle-backwards
;;              popper-kill-latest-popup)
;;
;;   :hook
;;   ((after-init . popper-mode)
;;    (after-init . popper-echo-mode))
;;
;;   :bind (("C-;"   . popper-toggle)
;;          ;; ("M-;"   . popper-cycle)  ;; conflict with comment-dwim
;;          ("C-M-;" . popper-toggle-type))
;;
;;   :custom
;;   (popper-reference-buffers
;;    '("\\*vterm\\*"
;;      vterm-mode
;;      ;; help-mode
;;      ;; compilation-mode
;;      ))
;;   (popper-window-height 80)
;;
;;   ;; (setq popper-window-height 20)
;;   (popper-display-control t)
;;
;;   :config
;;
;;   ;; group by project.el project root, with fall back to default-directory
;;   ;; (setq popper-group-function #'popper-group-by-directory)
;;
;;   ;; Match eshell, shell, term and/or vterm buffers
;;   ;; (setq popper-reference-buffers
;;   ;;       '("\\*Messages\\*"
;;   ;;         "Output\\*$"
;;   ;;         "\\*Async Shell Command\\*"
;;   ;;         help-mode
;;   ;;         compilation-mode))
;;
;;   ;; (setq popper-mode-line "")
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*$"
;;           "Output\\*$" "\\*Pp Eval Output\\*$"
;;           "^\\*eldoc.*\\*$"
;;           "\\*Compile-Log\\*$"
;;           "\\*Completions\\*$"
;;           "\\*Warnings\\*$"
;;           "\\*Async Shell Command\\*$"
;;           "\\*Apropos\\*$"
;;           "\\*Backtrace\\*$"
;;           "\\*Calendar\\*$"
;;           "\\*Fd\\*$" "\\*Find\\*$" "\\*Finder\\*$"
;;           "\\*Kill Ring\\*$"
;;           "\\*Embark \\(Collect\\|Live\\):.*\\*$"
;;
;;           bookmark-bmenu-mode
;;           comint-mode
;;           compilation-mode
;;           help-mode helpful-mode
;;           tabulated-list-mode
;;           Buffer-menu-mode
;;
;;           flymake-diagnostics-buffer-mode
;;           flycheck-error-list-mode flycheck-verify-mode
;;
;;           gnus-article-mode devdocs-mode
;;           grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
;;           youdao-dictionary-mode osx-dictionary-mode fanyi-mode
;;           "^\\*gt-result\\*$" "^\\*gt-log\\*$"
;;
;;           "^\\*Process List\\*$" process-menu-mode
;;           list-environment-mode cargo-process-mode
;;
;;           "^\\*.*eat.*\\*.*$"
;;           "^\\*.*eshell.*\\*.*$"
;;           "^\\*.*shell.*\\*.*$"
;;           "^\\*.*terminal.*\\*.*$"
;;           "^\\*.*vterm[inal]*.*\\*.*$"
;;
;;           "\\*DAP Templates\\*$" dap-server-log-mode
;;           "\\*ELP Profiling Restuls\\*" profiler-report-mode
;;           "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
;;           "\\*[Wo]*Man.*\\*$"
;;           "\\*ert\\*$" overseer-buffer-mode
;;           "\\*gud-debug\\*$"
;;           "\\*lsp-help\\*$" "\\*lsp session\\*$"
;;           "\\*quickrun\\*$"
;;           "\\*tldr\\*$"
;;           "\\*vc-.*\\**"
;;           "\\*diff-hl\\**"
;;           "^\\*macro expansion\\**"
;;
;;           "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
;;           "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
;;           "\\*docker-.+\\*"
;;           "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
;;           "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
;;           rustic-cargo-outdated-mode rustic-cargo-run-mode
;;           rustic-cargo-test-mode))
;;   )

;;; shell-pop

;; shell-pop-default-directory "~/src"
;; shell-pop-shell-type (cond
;;                       ((eq system-type 'gnu/linux)
;;                        '("vterm" "*vterm*" #'vterm))
;;                       (IS-WINDOWS '("eshell" "*eshell*" #'eshell))
;;                       (t '("terminal" "*terminal*"
;;                            (lambda () (term shell-pop-term-shell)))))

(lightemacs-use-package shell-pop
  :vc (:url "https://github.com/jamescherti/shell-pop-el"
            :rev :newest)
  :commands shell-pop
  ;; :bind (("<f2>" . shell-pop))
  :custom
  (shell-pop-shell-type '("vterm"
                          "*vterm*"
                          (lambda ()
                            (let ((tmux-buffer (vterm)))
                              (with-current-buffer tmux-buffer
                                (vterm-send-string "exec tmux-session emacs")
                                (vterm-send-string "\n")
                                (vterm-send-return))))))
  :init
  (setq shell-pop-window-position "bottom")
  ;; (setq shell-pop-window-position "full")
  (setq shell-pop-full-span nil)
  (setq shell-pop-autocd-to-working-dir nil)
  (setq shell-pop-term-shell "/bin/bash")
  (setq shell-pop-window-size 80)
  ;; (setq shell-pop-shell-type '("ansi-term"
  ;;                              "*ansi-term*"
  ;;                              (lambda ()
  ;;                                (ansi-term shell-pop-term-shell))))

  ;; This protects you from the pop-out bug
  (setq shell-pop-restore-window-configuration nil)

  :preface
  (defvar my-shell-pop-saved-window-config nil
    "Stores the absolute window state prior to shell-pop-up.")

  (defun my-shell-pop-save-layout (&rest _args)
    "Capture the exact layout before shell-pop opens."
    (setq my-shell-pop-saved-window-config (current-window-configuration)))

  (defun my-shell-pop-restore-layout (orig-fun &rest args)
    "Restore the layout without relying on linear winner history.
  Only restores if the original working window is still alive."
    (if (and my-shell-pop-saved-window-config
             shell-pop-last-window
             shell-pop-last-buffer
             (window-live-p shell-pop-last-window)
             (eq (window-buffer shell-pop-last-window) shell-pop-last-buffer))
        (progn
          (run-hooks 'shell-pop-out-hook)
          (bury-buffer)
          (set-window-configuration my-shell-pop-saved-window-config)
          (setq my-shell-pop-saved-window-config nil))
      ;; Clean up the saved config and fall back to native behavior
      ;; if the original window was destroyed.
      (setq my-shell-pop-saved-window-config nil)
      (apply orig-fun args)))

  (advice-add 'shell-pop-up :before #'my-shell-pop-save-layout)
  (advice-add 'shell-pop-out :around #'my-shell-pop-restore-layout))

;; shell-pop: Change the default directory
(defun my-around-shell-pop (fn &rest args)
  "FN is the advised function. ARGS are the function arguments."
  (with-temp-buffer
    (insert (expand-file-name default-directory))
    (let ((coding-system-for-write 'utf-8-emacs)
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil))
      (write-region (point-min) (point-max) "~/.bash_lastdir" nil 'silent)))
  (apply fn args))
(with-eval-after-load 'shell-pop
  (advice-add 'shell-pop :around #'my-around-shell-pop))

;; Ensure switching to insert mode
(defun my-shell-pop-to-insert-state ()
  "Ensure the terminal is in char-mode and Evil is in insert state."
  ;; If using term/ansi-term, this lets keys pass to the shell
  ;; (when (and (fboundp 'term-char-mode)
  ;;            (derived-mode-p 'term-mode))
  ;;   (term-char-mode))

  (my-save-all-buffers)

  ;; Force Evil into insert state
  (when (fboundp 'evil-insert-state)
    (evil-insert-state))

  ;; Fix issue that causes the cursor to move to the top-left of the screen
  (when (and (derived-mode-p 'vterm-mode)
             (fboundp 'vterm-reset-cursor-point))
    (vterm-reset-cursor-point)))

(add-hook 'shell-pop-in-after-hook #'my-shell-pop-to-insert-state)

;;; vterm-toggle

;; (lightemacs-use-package vterm-toggle
;;   :commands vterm-toggle
;;   :bind (("<f2>" . vterm-toggle))
;;   :init
;;   (add-to-list 'display-buffer-alist
;;                '("^\\*vterm"
;;                  (display-buffer-reuse-window display-buffer-at-bottom)
;;                  (window-height . 80)))
;;   ;; :config
;;   ;; ;; vterm-toggle: Change the default directory
;;   ;; (defun my-around-vterm-toggle (fn &rest args)
;;   ;;   "FN is the advised function. ARGS are the function arguments."
;;   ;;   (with-temp-buffer
;;   ;;     (insert (expand-file-name default-directory))
;;   ;;     (let ((coding-system-for-write 'utf-8-emacs)
;;   ;;           (write-region-annotate-functions nil)
;;   ;;           (write-region-post-annotation-function nil))
;;   ;;       (write-region (point-min) (point-max) "~/.bash_lastdir" nil 'silent)))
;;   ;;   (apply fn args))
;;   ;; (advice-add 'vterm-toggle :around #'my-around-vterm-toggle)
;;   ;;
;;   ;; ;; Inject tmux session on creation
;;   ;; (defun my-vterm-toggle-tmux-setup (orig-fun &rest args)
;;   ;;   "Inject tmux command when a new vterm buffer is created."
;;   ;;   (let* ((buffer-name (or (car args) vterm-buffer-name))
;;   ;;          (buffer-exists (get-buffer buffer-name))
;;   ;;          (buf (apply orig-fun args)))
;;   ;;     (unless buffer-exists
;;   ;;       (with-current-buffer buf
;;   ;;         (vterm-send-string "exec tmux-session emacs")
;;   ;;         (vterm-send-string "\n")
;;   ;;         (vterm-send-return)))
;;   ;;     buf))
;;   ;; (advice-add 'vterm-toggle--new :around #'my-vterm-toggle-tmux-setup)
;;   ;;
;;   ;; ;; Ensure switching to insert mode
;;   ;; (defun my-vterm-toggle-to-insert-state ()
;;   ;;   "Ensure the terminal is in char-mode and Evil is in insert state."
;;   ;;   ;; If using term/ansi-term, this lets keys pass to the shell
;;   ;;   ;; (when (and (fboundp 'term-char-mode)
;;   ;;   ;;            (derived-mode-p 'term-mode))
;;   ;;   ;;   (term-char-mode))
;;   ;;
;;   ;;   (my-save-all-buffers)
;;   ;;
;;   ;;   ;; Force Evil into insert state
;;   ;;   (when (fboundp 'evil-insert-state)
;;   ;;     (evil-insert-state))
;;   ;;
;;   ;;   ;; Fix issue that causes the cursor to move to the top-left of the screen
;;   ;;   (when (and (derived-mode-p 'vterm-mode)
;;   ;;              (fboundp 'vterm-reset-cursor-point))
;;   ;;     (vterm-reset-cursor-point)))
;;   ;;
;;   ;; (add-hook 'vterm-toggle-show-hook #'my-vterm-toggle-to-insert-state)
;;   )

;;; Vimrc mode

(lightemacs-use-package vimrc-mode
  :commands vimrc-mode
  :mode
  ("/\\.vim\\(rc\\)?\\'" . vimrc-mode))

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

;;; exec file form shell

;; TODO enable on mac
;; (lightemacs-use-package exec-path-from-shell
;;   :if (and (or (display-graphic-p) (daemonp))
;;            (eq system-type 'darwin)) ; macOS only
;;   :demand t
;;   :functions exec-path-from-shell-initialize
;;   :init
;;   (setq exec-path-from-shell-variables
;;         '("PATH" "MANPATH"
;;           "TMPDIR"
;;           "SSH_AUTH_SOCK" "SSH_AGENT_PID"
;;           "GPG_AGENT_INFO"
;;           ;; "FZF_DEFAULT_COMMAND" "FZF_DEFAULT_OPTS" ; fzf
;;           ;; "VIRTUAL_ENV" ; Python
;;           ;; "GOPATH" "GOROOT" "GOBIN" ; Go
;;           ;; "CARGO_HOME" "RUSTUP_HOME" ; Rust
;;           ;; "NVM_DIR" "NODE_PATH" ; Node/JS
;;           "LANG" "LC_CTYPE"))
;;   :config
;;   ;; Initialize
;;   (exec-path-from-shell-initialize))

;;; html

(if (my-treesit-language-available-p 'html)
    (progn
      (push '(html-mode . html-ts-mode) major-mode-remap-alist)
      (add-to-list 'auto-mode-alist '("\\.[hH][tT][mM][lL]\\'" . html-ts-mode)))
  (progn
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

    ;; Add the hook for both html-mode and mhtml-mode
    (add-hook 'html-mode-hook 'sgml-electric-tag-pair-mode)
    (add-hook 'mhtml-mode-hook 'sgml-electric-tag-pair-mode)))

;;; jinja2-mode and csv-mode

(lightemacs-use-package jinja2-mode
  :commands jinja2-mode
  :mode ("\\.j2\\'" . jinja2-mode))

;;; org

;; Must be evaluated before Org is loaded
(with-eval-after-load 'org
  (if (and (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'yaml))
      (push (cons "yaml" 'yaml-ts) org-src-lang-modes)
    (push (cons "yaml" 'yaml) org-src-lang-modes)))

(defun my-org-agenda-switch-to-todos ()
  "Open the Org Agenda directly showing all TODO items."
  (interactive)
  (let ((buffer (get-buffer "*Org Agenda*")))
    (if buffer
        (switch-to-buffer buffer)
      (org-agenda nil "t"))))

(defun my-org-move-todo-before-first-done ()
  "Move the current TODO heading down, right before the first DONE heading.
If there are no DONE headings, it will be moved below all TODO headings
at the same level."
  (interactive)
  (if (and (fboundp 'org-at-heading-p)
           (fboundp 'org-back-to-heading)
           (fboundp 'org-get-todo-state)
           (fboundp 'org-forward-heading-same-level)
           (fboundp 'org-move-subtree-down))
      (progn
        (unless (org-at-heading-p)
          (org-back-to-heading t))

        ;; Only execute if the current heading is an active TODO state
        (when (member (org-get-todo-state) org-not-done-keywords)
          (let ((moving t)
                (last-point -1))
            (while moving
              (if (= (point) last-point)
                  (setq moving nil) ;; Break loop if movement failed silently
                (setq last-point (point))
                (let ((is-next-done
                       (save-excursion
                         (if (org-forward-heading-same-level 1)
                             (member (org-get-todo-state) org-done-keywords)
                           'no-next))))
                  (if (or (eq is-next-done 'no-next) is-next-done)
                      (setq moving nil)
                    (org-move-subtree-down 1))))))))
    (error "Org functions are not defined")))

(defun my-org-todo-and-toggle ()
  "Toggle the current Org mode item's TODO/DONE."
  (interactive)
  (when (and (fboundp 'org-todo)
             (fboundp 'org-hide-entry)
             (fboundp 'org-get-todo-state))
    (let ((column (current-column)))
      (save-excursion
        (beginning-of-visual-line)
        (let ((current-state (org-get-todo-state)))
          (if (or (not current-state) (string= current-state "DONE"))
              (org-todo "TODO")
            (my-org-move-todo-before-first-done)
            (org-todo "DONE")))
        (org-hide-entry))

      (move-to-column column))))

;; TODO lightemacs?
(defun my-org-capture-switch-insert ()
  "Switch to insert mode on org capture."
  (when (and (bound-and-true-p evil-local-mode)
             (fboundp 'evil-insert-state))
    (funcall 'evil-insert-state)))
(add-hook 'org-capture-mode-hook #'my-org-capture-switch-insert)
(with-eval-after-load 'org
  ;; The function inserts a new heading at the current cursor position, and
  ;; prepends it with "TODO " if activated while on a "TODO" task, thus creating
  ;; a new to-do item. In addition to that, for those utilizing evil-mode the
  ;; function transitions the user into insert mode right after the "TODO "
  ;; insertion.
  (defun my-org-insert-heading-respect-content-and-prepend-todo ()
    "Insert a new org heading respecting content and prepend it with TODO.
  Additionally, ensure entry into insert state when evil-mode is active."
    (interactive)
    (when (and (fboundp 'org-entry-is-todo-p)
               (fboundp 'org-entry-is-done-p)
               (fboundp 'org-insert-heading-respect-content))
      (let ((entry-is-todo (org-entry-is-todo-p))
            (entry-is-done (org-entry-is-done-p)))
        (when (and (bound-and-true-p evil-local-mode)
                   (fboundp 'evil-insert-state))
          (funcall 'evil-insert-state))
        (org-insert-heading-respect-content)
        (when (or entry-is-todo entry-is-done)
          (just-one-space)
          (insert "TODO")
          (just-one-space)))))

  (unless noninteractive
    (define-key org-mode-map (kbd "C-<return>")
                'my-org-insert-heading-respect-content-and-prepend-todo)

    (define-key org-mode-map (kbd "C-c C-e") 'org-babel-execute-maybe)
    (define-key org-mode-map (kbd "C-c C-c") 'org-edit-src-code)
    (define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit)
    (define-key org-mode-map (kbd "C-c C-d") 'my-org-todo-and-toggle)

    (define-key org-mode-map (kbd "M-h") nil))

  ;; (defun org-todo-and-close-fold ()
  ;;   "Mark the current Org mode item as TODO and close its subtree."
  ;;   (interactive)
  ;;   (org-todo 'done)
  ;;   (org-hide-entry))

  (defun org-toggle-emphasis ()
    "Toggle hiding/showing of org emphasize markers."
    (interactive)
    (if org-hide-emphasis-markers
        (set-variable 'org-hide-emphasis-markers nil)
      (set-variable 'org-hide-emphasis-markers t)))

  ;; (custom-set-faces
  ;;  ;; Face used for todo keywords that indicate DONE items.
  ;;  '(org-done ((t (:strike-through t))))
  ;;  ;; Face used to indicate that a headline is DONE. This face is only used if
  ;;  ;; ‘org-fontify-done-headline’ is set. If applies to the part of the headline
  ;;  ;; after the DONE keyword.
  ;;  '(org-headline-done ((t (:strike-through t)))))

  ;; (set-face-attribute 'org-done nil :strike-through t)
  ;; (set-face-attribute 'org-headline-done nil :strike-through t)

  (face-spec-set 'org-done
                 '((t (:strike-through t))))

  (face-spec-set 'org-headline-done
                 '((t (:strike-through t)))))

(defun my-org-capture-move-cursor-end-line ()
  "Move cursor to end line."
  (when (eq major-mode 'org-mode)
    (end-of-line)))

(when (fboundp 'my-org-capture-move-cursor-end-line)
  (add-hook 'org-capture-before-finalize-hook
            #'my-org-capture-move-cursor-end-line))

(with-eval-after-load 'org-agenda
  (unless noninteractive
    (define-key org-agenda-keymap (kbd "<tab>") #'ignore))
  (defun my-org-agenda-goto-in-same-window ()
    "`org-agenda-goto` that opens the target buffer in the current window."
    (interactive)
    (require 'cl-lib)
    (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
      (when (fboundp 'org-agenda-goto)
        (funcall 'org-agenda-goto))))

  (setq org-agenda-file-regexp (replace-regexp-in-string
                                "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                org-agenda-file-regexp)))


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

;;; ansible-doc

(lightemacs-use-package ansible-doc
  :commands ansible-doc)

;;; Lua

(unless (my-treesit-language-available-p 'markdown)
  (lightemacs-use-package lua-mode
    :commands lua-mode
    :mode
    ("\\.lua\\'" . lua-mode)
    ;; :init
    ;; (add-hook 'lua-mode-hook
    ;;           #'(lambda ()
    ;;               (my-set-tab-width 3)))
    ))

;;; org-ibullets

(lightemacs-use-package org-ibullets
  :vc (:url "https://github.com/jamescherti/org-ibullets.el"
            :rev :newest)
  :ensure nil
  :after org
  :commands org-ibullets-mode
  :hook (org-mode . org-ibullets-mode)
  ;; :custom
  ;; (org-ibullets-bullet-list '("●" "◉" "○" "♦" "▶" "♣" "♠"))
  )

;;; olivetti

;; perfect-margin: Specifically built to play nicely with tools that live on the
;; edges of your windows, such as line numbers, minimap, and treemacs. It
;; dynamically calculates the margins to keep the text centered without
;; displacing these side-pane elements.
;;
;; olivetti: Sometimes struggles with side-pane elements. For example, if you
;; enable line numbers, Olivetti might push them into the middle of the screen
;; right next to the text block, which can look jarring.
;; (lightemacs-use-package olivetti
;;   ;; :if (display-graphic-p)
;;   :commands olivetti-mode
;;   :init
;;   (setq olivetti-body-width 110)
;;   (setq olivetti-minimum-body-width 60)
;;
;;   ;; Removes the default `visual-line-mode'
;;   (setq olivetti-mode-on-hook nil)
;;
;;   :preface
;;   (defun my-setup-olivetti-mode ()
;;     "Setup `olivetti-mode'."
;;     (when (derived-mode-p 'ibuffer-mode)
;;       (setq-local olivetti-body-width 150))
;;
;;     ;; This ensures that olivetti works well with session managers such as
;;     ;; easysession.
;;     (if (bound-and-true-p easysession-load-in-progress)
;;         (run-with-idle-timer
;;          0 nil
;;          #'(lambda()
;;              (unless (bound-and-true-p olivetti-mode)
;;                (olivetti-mode 1))))
;;       (olivetti-mode 1)))
;;
;;   :init
;;   (with-eval-after-load 'consult
;;     (add-hook 'consult-preview-allowed-hooks #'my-setup-olivetti-mode))
;;   (add-hook 'find-file-hook #'my-setup-olivetti-mode)
;;   (add-hook 'dired-mode-hook #'my-setup-olivetti-mode)
;;   (add-hook 'ibuffer-mode-hook #'my-setup-olivetti-mode)
;;   ;; (add-hook 'text-mode-hook #'my-setup-olivetti-mode)
;;   ;; (add-hook 'prog-mode-hook #'my-setup-olivetti-mode)
;;   )

;;; Perfect margin

;; Buggy
;; (use-package perfect-margin
;;   :ensure t
;;   :init
;;   (setq perfect-margin-visible-width 100)
;;   ;; auto-center everything --i.e., do not ignore any kind of windows
;;   (setq perfect-margin-ignore-filters '(window-minibuffer-p))
;;   (setq perfect-margin-ignore-regexps '(
;;                                         "^minibuf"
;;                                         ;; "^[[:space:]]*\\*"
;;                                         ))
;;   :hook
;;   (lightemacs-after-init . perfect-margin-mode))

;;; visual-fill-column

;; (lightemacs-use-package visual-fill-column
;;   :commands visual-fill-column-for-vline
;;   ;; :hook
;;   ;; (
;;   ;;  ;; (visual-line-mode .  visual-fill-column-for-vline)
;;   ;;  ;; (prog-mode . visual-line-mode)
;;   ;;  ;; (text-mode . visual-line-mode)
;;   ;;
;;   ;;  (prog-mode . visual-fill-column-mode)
;;   ;;  (text-mode . visual-fill-column-mode)
;;   ;;  ((markdown-mode org-mode) . (lambda()
;;   ;;                                (setq fill-column 120))))
;;
;;   :custom
;;   ;; Global settings
;;   (visual-fill-column-center-text nil)
;;   (visual-fill-column-enable-sensible-window-split t)
;;   ;; :config
;;   ;; TODO does this replace my config?
;;   ;; THIS DOES NOT WORK
;;   ;; Fix for text scaling (C-x C-+ / C-x C--)
;;   ;; (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
;;   )

;;; Trust framework files

(defcustom lightemacs-trust-framework-files nil
  "If non-nil, append the Lightemacs core directory to `trusted-content'.
This prevents Flymake warnings when viewing framework source files in Emacs
30+."
  :type 'boolean
  :group 'lightemacs)

(when (and lightemacs-trust-framework-files
           (boundp 'trusted-content)
           (listp trusted-content))
  (let ((dir (file-name-as-directory lightemacs-core-directory)))
    (add-to-list 'trusted-content dir)))

;;; track eol (TODO light emacs)

(setq evil-track-eol nil)

;; When navigating vertically with visual line movement commands such as
;; previous-visual-line or next-visual-line (or evil-previous-visual-line and
;; evil-next-visual-line when using Evil), the cursor may enter invisible text
;; if folded regions are present.
;;
;; This behavior occurs under the following conditions:
;; - End-of-line tracking is enabled and the cursor originates from a longer
;;   line. During vertical movement, the cursor attempts to preserve its
;;   logical column position, which can correspond to a location inside hidden
;;   content on the target line.
;; - line-move-ignore-invisible is set to nil, causing Emacs to include hidden
;;   or folded text during vertical movement rather than skipping it.
;;
;; The following ensures that vertical navigation never lands in invisible
;; text within folded regions, add the following to your configuration:
(setq track-eol nil)

;; (setq evil-track-eol track-eol)
(setq line-move-ignore-invisible t)

;;; macrostep

;; Buggy
;; (lightemacs-use-package macrostep
;;   :bind (:map emacs-lisp-mode-map
;;               ("C-c e" . macrostep-expand)
;;               :map lisp-interaction-mode-map
;;               ("C-c e" . macrostep-expand)))

(defun my-macroexpand ()
  "Expand the macro at point one level at a time.
If the result is still a macro, subsequent calls will expand the next level.
The result is displayed in a pretty-printed temporary buffer."
  (interactive)
  (let* ((sexp (sexp-at-point))
         (expansion (macroexpand-1 sexp)))
    (if (equal sexp expansion)
        (message "No further expansion possible.")
      (with-current-buffer (get-buffer-create "*Macro Expansion*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (emacs-lisp-mode)
          (insert (pp-to-string expansion))
          (indent-region (point-min) (point-max))
          (goto-char (point-min)))
        (read-only-mode 1)
        (display-buffer (current-buffer))))))

;;; so long

;; (setq so-long-threshold 10000)
(add-hook 'lightemacs-after-init-hook #'global-so-long-mode)

;;; auto save

;; TODO find a better way
;; (defvar-local auto-recover-prompted nil
;;   "Flag to prevent infinite recovery loops.")
;;
;; (defun auto-recover-prompt-on-visit ()
;;   "Prompt to recover the auto-save file if it is newer."
;;   (remove-hook 'find-file-hook #'auto-recover-prompt-on-visit)
;;   (let ((auto-save-file (make-auto-save-file-name)))
;;     ;; Check if we have already prompted and ensure we are not currently
;;     ;; reverting
;;     ;; let ((find-file-hook (seq-remove
;;     ;;                       ;; This prevents an infinite loop
;;     ;;                       (lambda(hook)
;;     ;;                         (when (eq hook 'auto-recover-prompt-on-visit)
;;     ;;                           t))
;;     ;;                       find-file-hook)))
;;     (when (and buffer-file-name
;;                (not (buffer-base-buffer))
;;                (not auto-recover-prompted)
;;                (not revert-buffer-in-progress-p)
;;                (file-exists-p auto-save-file)
;;                (file-newer-than-file-p auto-save-file buffer-file-name))
;;       (ignore find-file-hook)
;;       ;; Set the flag to true immediately so it cannot fire again for this
;;       ;; buffer
;;       (setq auto-recover-prompted t)
;;       (run-with-timer
;;        0 nil
;;        (lambda (buf)
;;          (when (buffer-live-p buf)
;;            (with-current-buffer buf
;;              (when (y-or-n-p
;;                     (format
;;                      "An auto-save file is newer than '%s'. Start recovery? "
;;                      buffer-file-name))
;;                (call-interactively #'recover-this-file)))))
;;        (current-buffer)))))
;;
;; (add-hook 'find-file-hook #'auto-recover-prompt-on-visit 90)

;;; Other modules

(unless noninteractive
  ;; Optional
  (require 'mod-misc2 nil t)

  ;; (require 'mod-toggle-term)
  ;; TODO put it back

  ;; (when (< emacs-major-version 31)
  ;;   (require 'mod-kirigami))
  (require 'mod-kirigami)

  (require 'mod-project)
  (require 'mod-buffer-terminator)
  (require 'buffer-guardian)
  (require 'mod-eglot)
  (require 'smartindent)
  ;; (require 'battery-angel)
  (require 'point-manager))

;;; term kill

;; Automatically close the buffer when the terminal session ends
;; TODO lightemacs
(defun my-term-close-on-exit (process _event)
  "Close the buffer when PROCESS finishes with EVENT."
  (when (memq (process-status process) '(exit signal))
    (kill-buffer (process-buffer process))))
(defun my-term-exec-hook ()
  "Attach the sentinel to the terminal process."
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (set-process-sentinel proc #'my-term-close-on-exit))))
(add-hook 'term-exec-hook #'my-term-exec-hook)

;;; term preferences

;; ============================================================================
;; Terminal Color and Emulation Configuration
;; ============================================================================
;; This is the optimal configuration for ansi-term because it separates
;; color output capabilities from structural terminal rendering rules.
;;
;; 1. (setenv "COLORTERM" "truecolor")
;;    Modern command-line tools check this environment variable to determine
;;    color support. By explicitly setting it to "truecolor", you force external
;;    programs to output raw 24-bit RGB ANSI escape sequences, entirely bypassing
;;    the color limitations of the active terminfo profile. Emacs can natively
;;    parse and render these 24-bit codes.
;;
;; 2. (setq term-term-name "eterm-color")
;;    While COLORTERM handles the colors, the TERM variable dictates how
;;    applications draw UI elements, move the cursor, and render boxes. The
;;    ansi-term engine is hardcoded to understand the "eterm-color" control
;;    sequences. If you change this to "xterm-256color", applications like
;;    pinentry-curses will send xterm-specific Alternate Character Set (ACS)
;;    sequences. Because ansi-term cannot interpret xterm ACS sequences, it
;;    outputs garbage characters.
;;
;; Conclusion: This combination allows modern tools to push full 24-bit colors
;; while forcing curses applications to use the specific layout sequences that
;; the Emacs terminal engine was built to understand.
;;
;; ADVICE:
;; -------
;; The `ncurses-term' (Debian) package is useful for this purpose.
;; On Arch Linux, the equivalent package is simply ncurses.
;;
;; It provides:
;;
;; /usr/share/terminfo/x/xterm-direct
;; /usr/share/terminfo/x/xterm-direct256
;; /usr/share/terminfo/x/xterm+direct
;;
;; This xterm-direct file is exactly what Emacs 27.1 and newer versions rely on.
;; When you launch Emacs with TERM=xterm-direct, Emacs queries that specific
;; file, detects the RGB capability flag, and natively activates 24-bit direct
;; color mode without needing the custom xterm-emacs profile.
;;
;; Additionally, if you check the "e" directory in your list, you will find:
;;
;; /usr/share/terminfo/e/eterm-color
;;
;; This is the exact profile that Emacs's ansi-term requires to render its
;; layout and curses applications properly. While Debian usually includes
;; eterm-color in the base installation, having the full ncurses-term package
;; guarantees it is available and up to date.
;;
;; Therefore, installing ncurses-term provides the official system-level
;; definitions needed for true color. However, my previous recommendation
;; remains the same: simply exporting COLORTERM=truecolor in your shell profile
;; is a much better approach than changing your TERM variable to xterm-direct.
;; Changing your TERM to xterm-direct globally will often break color rendering
;; in other terminal multiplexers or applications that strictly expect standard
;; 256-color definitions.
;;
;; Emacs GUI
;; ---------
;; When you open M-x ansi-term inside GUI Emacs, you are starting an isolated
;; terminal emulator environment. The shell running inside that buffer faces the
;; exact same limitations as before: it relies on the TERM variable to
;; understand what the emulator can handle.
;;
;; Because of this, the Elisp configuration we established earlier is still
;; highly useful and applicable.
;;
;; (setq term-term-name "eterm-color"): This remains necessary. The ansi-term
;; engine inside GUI Emacs still needs subprocesses to send layout and
;; box-drawing sequences that it understands. If you change this, you will still
;; get the garbage characters in curses applications.
;; This guarantees that programs continue to send the specific structural layout
;; and cursor control sequences that the ansi-term engine understands. This
;; prevents display corruption in curses-based tools.
;;
;; (setenv "COLORTERM" "truecolor"): This remains the perfect solution for
;; colors. Modern CLI tools running inside your GUI ansi-term will see this
;; environment variable and output 24-bit color ANSI sequences. Because you are
;; using the GUI, Emacs will natively parse and render those colors beautifully
;; without any terminal constraints.
;; This forces modern command-line tools to emit raw 24-bit RGB ANSI escape
;; sequences. Emacs version 28 and newer natively parses and renders these
;; sequences inside terminal buffers.
;; ============================================================================
(setenv "COLORTERM" "truecolor")
(setq term-term-name "eterm-color")

(setq explicit-shell-file-name "bash")
(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")

;; TODO adapt this one

;; Commands like ls --color=auto output raw ANSI color escape sequences (such as
;; \033[31m for red text). The function ansi-color-for-comint-mode-on intercepts
;; these raw codes and translates them into native Emacs text properties so they
;; display as colored text. If you do not enable this, your buffer will be
;; filled with unreadable bracketed codes instead of colors.
;;
;; Required for M-x shell: The standard shell-mode is based on comint-mode
;; (Command Interpreter). It is essentially a basic text buffer that sends and
;; receives text. It does not know how to parse terminal colors by default, so
;; it relies entirely on this hook to display them properly.
;;
;; Unnecessary for M-x ansi-term: Since ansi-term is a dedicated terminal
;; emulator, its internal engine handles ANSI color parsing automatically. The
;; hook will not affect `ansi-term' buffers.
;;
;; If you occasionally use M-x shell alongside ansi-term, keeping this line in
;; your configuration is recommended to ensure your basic shell buffers remain
;; readable.
(add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

;; (setq term-buffer-maximum-size 10000)

;;; straight

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

(when (fboundp 'straight-freeze-versions)
  (advice-add 'straight-freeze-versions :around #'my-copy-straight-profile-advice))

;;; Lazily load buffers (TODO easysession)

(defvar-local my-lazy-load-buffer--filename nil)
(defvar-local my-lazy-load-buffer--window-start nil)
(defvar-local my-lazy-load-buffer--point nil)

(defun my-lazy-generate-file-restore-buffer (buffer-name
                                             filename
                                             point
                                             window-start)
  "Create BUFFER-NAME with a button to load FILENAME when activated.

This function creates a buffer named BUFFER-NAME that contains a button labeled
[Restore]. When the user activates this button, the buffer is replaced with the
contents of FILENAME using `find-file-noselect', and the window's point and
start position are restored to the values given by POINT and WINDOW-START.

If BUFFER-NAME already exists, an error is raised to prevent overwriting.

This is useful for deferred loading of file buffers, allowing the user to
explicitly trigger file loading only when desired."
  (interactive)
  (when (get-buffer buffer-name)
    (error "The buffer `%s' is not supposed to exist" filename))
  (let ((new-buffer (get-buffer-create buffer-name)))
    (with-current-buffer new-buffer
      (setq my-lazy-load-buffer--filename new-buffer)
      (setq my-lazy-load-buffer--window-start window-start)
      (setq my-lazy-load-buffer--point point)
      (insert (format-message "This window displayed `%s'.\n" filename))
      (when filename
        (insert-button
         "[Restore]" 'action
         (lambda (_button)
           (let ((window (selected-window))
                 (temporary-buffer (current-buffer)))
             (set-window-buffer window (find-file-noselect filename))
             (unless (eq (window-buffer) temporary-buffer)
               (kill-buffer temporary-buffer))
             (set-window-start window my-lazy-load-buffer--window-start t)
             (set-window-point window my-lazy-load-buffer--point))))
        (insert "\n"))
      (goto-char (point-min))
      (special-mode))))

;;; vertico postframe

;; Too slow
;; (unless IS-MAC
;;   (use-package vertico-posframe
;;     :after vertico
;;     :commands vertico-posframe-mode
;;     :custom
;;     (vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-right-corner)
;;     :init
;;     (add-hook 'vertico-mode-hook #'vertico-posframe-mode)
;;
;;     (setq vertico-posframe-parameters
;;           '((left-fringe . 8)
;;             (right-fringe . 8)))
;;
;;     (setq vertico-posframe-height 11
;;           vertico-posframe-width 100)
;;
;;     ;; vertico-posframe-parameters '((left-fringe . 20)
;;     ;;                               (right-fringe . 20))
;;     ;; vertico-posframe-border-width 2
;;     ;; vertico-posframe-min-height 2
;;     ;; (vertico-posframe-mode 1)
;;     ))

;;; tab-bar: only display file visiting buffers

(defun custom-tab-valid-buffer-p (buf)
  "Return non-nil if BUF is visiting a file or is a `dired' buffer."
  (or (buffer-file-name (or (buffer-base-buffer buf) buf))
      (with-current-buffer buf
        (derived-mode-p 'dired-mode))))

(defun custom-tab-bar-tab-name ()
  "Return the name of the current or visible file-visiting buffer.
This prevents non-file buffers, such as popup shells or help windows, from
taking over the tab name. It keeps the tab-bar focused on the actual files you
are editing by falling back to another visible file buffer."
  (let ((current-buf (window-buffer (minibuffer-selected-window))))
    (cond
     ((or (one-window-p)
          (custom-tab-valid-buffer-p current-buf))
      (buffer-name current-buf))

     (t
      (catch 'found
        (dolist (win (window-list))
          (let ((buf (window-buffer win)))
            (when (custom-tab-valid-buffer-p buf)
              (throw 'found (buffer-name buf)))))
        ;; Fallback if no visible valid buffer is found
        (buffer-name current-buf))))))

(setq tab-bar-tab-name-function #'custom-tab-bar-tab-name)

;;; fold things when opening them

;; TODO kirigami close fold except this one
(defun my-kirigami-auto-open ()
  "Close all folds."
  (unless (bound-and-true-p easysession-load-in-progress)
    (save-excursion
      (ignore-errors
        (require 'kirigami nil t)
        (when (fboundp 'kirigami-open-fold)
          (kirigami-open-fold))))))

(defun my-kirigami-auto-close-all ()
  "Close all folds."
  (unless (bound-and-true-p easysession-load-in-progress)
    (save-excursion
      (ignore-errors
        (require 'kirigami nil t)
        (when (fboundp 'kirigami-close-folds)
          (kirigami-close-folds))))))

(add-hook 'outline-minor-mode-hook #'my-kirigami-auto-close-all 90)
(add-hook 'save-place-after-find-file-hook #'my-kirigami-auto-open 90)

;;; git gutter

(lightemacs-use-package git-gutter
  :commands (git-gutter-mode)

  :init
  (setq git-gutter:added-sign "+"
        git-gutter:deleted-sign "-"
        git-gutter:ask-p nil
        git-gutter:diff-option "-w"
        git-gutter:handled-backends '(git)
        git-gutter:disabled-modes '(image-mode fundamental-mode)
        git-gutter:hide-gutter t
        git-gutter:modified-sign "="
        ;; git-gutter:visual-line t        ; Better for wrapped lines
        git-gutter:update-interval 0
        git-gutter:verbosity 0)

  :config
  (global-set-key (kbd "C-x v n") 'git-gutter:next-hunk)
  (global-set-key (kbd "C-x v p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x v c") 'git-gutter:clear-gutter)
  (global-set-key (kbd "C-x v p") 'git-gutter:popup-hunk)
  (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk))

;; (lightemacs-use-package git-gutter-fringe
;;   :after git-gutter)

;;; Provide

(provide 'mod-misc)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-misc.el ends here

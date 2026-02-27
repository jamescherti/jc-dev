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

;; put it back. problem with vterm TODO

;;; Require

(require 'my-defun)
(require 'lightemacs-use-package)

;;; Lazy loader

(lightemacs-use-package lazy-loader
  :ensure nil
  :commands lazy-loader-mode
  :hook
  (lightemacs-after-init . lazy-loader-mode)
  :custom
  (lazy-loader-verbose nil)
  (lazy-loader-modules '(org vterm))
  (lazy-loader-files (delq nil
                           (list (when (bound-and-true-p file-path-todo)
                                   file-path-todo))))
  (lazy-loader-buffers
   '(("*tmux*" .
      (lambda ()
        (let ((buf (get-buffer-create "*tmux*")))
          (with-current-buffer buf
            (vterm-mode)

            (vterm-send-string "tmux-session -l emacs")
            (vterm-send-string "\n")
            (vterm-send-return))
          buf))))))

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

  (lightemacs-theme-create-loader "tomorrow-night-deepblue" 'tomorrow-night-deepblue-theme)

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
      ;; (flyspell-prog-mode)
      t
    ;; Other (e.g., Markdown)
    (let ((file-name (buffer-file-name (buffer-base-buffer))))
      (when (and file-name
                 (string-suffix-p "/readme.md"
                                  (downcase file-name))
                 (string-suffix-p "/changelog.md"
                                  (downcase file-name)))
        ;; (run-with-idle-timer 1 nil #'flyspell-mode 1)
        (flyspell-mode 1)))))

;; (add-hook 'markdown-mode-hook #'flyspell-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'my-setup-flyspell-text-mode)

;;; Golden-ratio

(lightemacs-use-package golden-ratio
  :commands (golden-ratio
             golden-ratio-mode
             golden-ratio-toggle-widescreen
             golden-ratio-adjust)
  ;; :hook
  ;; (add-hook 'after-init-hook #'golden-ratio-mode)
  )


;;; Focus

(lightemacs-use-package focus
  :commands (focus-mode
             focus-change-thing
             focus-pin
             focus-unpin
             focus-next-thing
             focus-prev-thing
             focus-read-only-mode
             focus-turn-off-read-only-mode))

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

  (with-eval-after-load 'flymake
    (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
    (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

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

;;; battery angel

(require 'battery-angel)

(defvar battery-angel--manage-compile-angel nil)

(defun setup-battery-angel-on-ac ()
  "This is called on AC."
  ;; (when battery-angel-verbose
  ;;   (message "Applying AC parameters"))

  (setq auto-revert-interval 3)

  ;; Flymake
  (setq flymake-start-on-flymake-mode
        (when (> (num-processors) 8)
          t))

  (setq flymake-no-changes-timeout
        (when (> (num-processors) 8)
          0.8))

  (when (and battery-angel--manage-compile-angel
             (fboundp 'compile-angel-on-load-mode)
             (not compile-angel-on-load-mode))
    (compile-angel-on-load-mode 1))

  ;; Fast Consult
  (setq consult-async-input-debounce 0.02
        consult-async-input-throttle 0.05
        consult-async-refresh-delay 0.02))

(defun setup-battery-angel-on-bat ()
  "This is called on BAT."
  ;; (when battery-angel-verbose
  ;;   (message "Applying BAT parameters"))

  (setq auto-revert-interval 10)

  ;; Flymake
  (setq flymake-start-on-flymake-mode nil)
  (setq flymake-no-changes-timeout nil)

  (when (and battery-angel--manage-compile-angel
             (fboundp 'compile-angel-on-load-mode)
             compile-angel-on-load-mode)
    ;; (setq battery-angel--manage-compile-angel t)
    (compile-angel-on-load-mode -1))

  ;; Default consult parameters
  ;; (setq consult-async-input-debounce 0.1
  ;;       consult-async-input-throttle 0.2
  ;;       consult-async-refresh-delay 0.1)

  (setq consult-async-input-debounce 0.2
        consult-async-input-throttle 0.5
        consult-async-refresh-delay 0.2))

(setq battery-angel-verbose nil)
(add-hook 'emacs-startup-hook #'battery-angel-mode 90)
(add-hook 'battery-angel-on-ac-hook #'setup-battery-angel-on-ac)
(add-hook 'battery-angel-on-bat-hook #'setup-battery-angel-on-bat)

;;; buffer guardian

(lightemacs-use-package buffer-guardian
  :ensure nil
  :commands buffer-guardian-mode

  :hook
  (emacs-startup . buffer-guardian-mode)

  :init
  (setq buffer-guardian-verbose nil)

  (setq buffer-guardian-save-on-focus-change t)
  (setq buffer-guardian-save-on-minibuffer t)
  (setq buffer-guardian-save-all-buffers-interval (* 60 5))
  (setq buffer-guardian-save-all-buffers-idle 25)

  (setq buffer-guardian-unattended-built-in-save-some-buffers t)

  ;; super-save-hook-triggers use add hook
  ;; TODO: mouse-leave-buffer-hook
  ;; (setq buffer-guardian-trigger-hooks '(mouse-leave-buffer-hook))

  ;; TODO  Change this to a function `buffer-guardian-advice-add'
  (setq buffer-guardian-functions-auto-save-current-buffer
        '(windmove-up
          windmove-down
          windmove-left

          windmove-right

          tab-previous
          tab-next
          tab-close
          tab-new

          my-tab-previous
          my-tab-next

          next-buffer
          previous-buffer

          save-buffers-kill-emacs

          save-buffers-kill-terminal

          switch-to-buffer
          pop-to-buffer
          other-window
          delete-window

          other-frame
          delete-frame
          make-frame

          kill-this-buffer))

  ;; Auto save
  ;; ---------
  ;; Auto-save safeguards against crashes or data loss. The
  ;;  `recover-file' or `recover-session' functions can be used to restore
  ;;  auto-saved data.
  ;;
  ;; Emacs periodically saves all files that you are visiting; this is
  ;; called auto-saving. Auto-saving prevents you from losing more than a
  ;; limited amount of work if the system crashes. By default, auto-saves
  ;; happen every 300 keystrokes, or after around 30 seconds of idle time.
  ;; See Auto-Saving: Protection Against Disasters in The GNU Emacs
  ;; Manual, for information on auto-save for users. Here we describe the
  ;; functions used to implement auto-saving and the variables that
  ;; control them.
  (setq auto-save-interval 0)  ; Disabled
  (setq auto-save-timeout 0)  ; Disabled

  ;; Test without this to see if auto-save-visited is affected by it
  (setq auto-save-no-message (not buffer-guardian-verbose))

  ;; Disable auto-saving
  (setq auto-save-default nil)

  ;; Auto save visited (Disabled by default)
  ;; ---------------------------------------
  ;; When auto-save-visited-mode is enabled, Emacs will auto-save
  ;; file-visiting buffers after a certain amount of of idle time.
  ;;
  ;; Predicate function for `auto-save-visited-mode'.
  ;; If non-nil, the value should be a function with no arguments; it
  ;; will be called once in each file-visiting buffer when it's time to
  ;; auto-save. A buffer will be saved only if the predicate function
  ;; returns a non-nil value.
  (setq remote-file-name-inhibit-auto-save-visited t)
  (setq auto-save-visited-interval 10)
  ;; (setq auto-save-visited-predicate #'buffer-guardian-predicate)
  ;; (auto-save-visited-mode 1)
  )

;;; Rainbow

(lightemacs-use-package rainbow-mode
  :commands rainbow-mode
  :no-require t)

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

(add-hook 'emacs-startup-hook #'lightemacs-flymake-initialize-elisp-path 99)

;;; flyspell-lazy

(lightemacs-use-package flyspell-lazy
  :commands flyspell-lazy-mode
  :hook ((lightemacs-after-init . (lambda()
                                    (save-window-excursion
                                      (save-excursion
                                        (flyspell-lazy-mode))))))
  :init
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3))

;;; git-timemachine

(lightemacs-use-package git-timemachine
  :commands git-timemachine
  :bind (:map vc-prefix-map
              ("t" . git-timemachine)))

;;; stillness-mode

(lightemacs-use-package stillness-mode
  :commands stillness-mode
  :hook (lightemacs-after-init . stillness-mode))

;;; shell-pop

(lightemacs-use-package shell-pop
  :commands shell-pop
  :bind (("<f2>" . shell-pop))
  :init
  (setq shell-pop-window-size 30
        shell-pop-default-directory "~/src"
        shell-pop-shell-type (quote ("ansi-term" "*ansi-term*"
                                     (lambda nil
                                       (ansi-term shell-pop-term-shell))))
        ;; shell-pop-shell-type (cond
        ;;                       ((eq system-type 'gnu/linux)
        ;;                        '("vterm" "*vterm*" #'vterm))
        ;;                       (IS-WINDOWS '("eshell" "*eshell*" #'eshell))
        ;;                       (t '("terminal" "*terminal*"
        ;;                            (lambda () (term shell-pop-term-shell)))))
        ))

;;; Vimrc mode

(lightemacs-use-package vimrc-mode
  :commands vimrc-mode
  :mode
  ("\\.vim\\(rc\\)?\\'" . vimrc-mode)
  ("\\.vimrc.local?\\'" . vimrc-mode)
  ("\\.lvimrc?\\'" . vimrc-mode)
  :init
  (add-hook 'vimrc-mode-hook #'(lambda ()
                                 (setq-local indent-tabs-mode nil)
                                 (if (fboundp 'my-set-tab-width)
                                     (my-set-tab-width 2)
                                   (error "Undefined: my-set-tab-width")))))

;;; exec file form shell

(lightemacs-use-package exec-path-from-shell
  :if (and (or (display-graphic-p) (daemonp))
           (eq system-type 'darwin)) ; macOS only
  :demand t
  :functions exec-path-from-shell-initialize
  :init
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH"
          "TMPDIR"
          "SSH_AUTH_SOCK" "SSH_AGENT_PID"
          "GPG_AGENT_INFO"
          ;; "FZF_DEFAULT_COMMAND" "FZF_DEFAULT_OPTS" ; fzf
          ;; "VIRTUAL_ENV" ; Python
          ;; "GOPATH" "GOROOT" "GOBIN" ; Go
          ;; "CARGO_HOME" "RUSTUP_HOME" ; Rust
          ;; "NVM_DIR" "NODE_PATH" ; Node/JS
          "LANG" "LC_CTYPE"))
  :config
  ;; Initialize
  (exec-path-from-shell-initialize))

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

    (lightemacs-use-package sgml-mode
      :ensure nil
      :commands (sgml-mode
                 sgml-electric-tag-pair-mode
                 sgml-name-8bit-mode)
      :hook
      (html-mode . sgml-electric-tag-pair-mode)
      (mhtml-mode . sgml-electric-tag-pair-mode)
      (html-mode . sgml-name-8bit-mode)
      (mhtml-mode . sgml-name-8bit-mode))))

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
          (let ((moving t))
            (while moving
              (let ((is-next-done
                     (save-excursion
                       (if (org-forward-heading-same-level 1)
                           (member (org-get-todo-state) org-done-keywords)
                         'no-next))))
                (if (or (eq is-next-done 'no-next) is-next-done)
                    (setq moving nil)
                  (org-move-subtree-down 1)))))))
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

  (define-key org-mode-map (kbd "C-<return>")
              'my-org-insert-heading-respect-content-and-prepend-todo)

  (define-key org-mode-map (kbd "C-c C-e") 'org-babel-execute-maybe)
  (define-key org-mode-map (kbd "C-c C-c") 'org-edit-src-code)
  (define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit)
  (define-key org-mode-map (kbd "C-c C-d") 'my-org-todo-and-toggle)

  (define-key org-mode-map (kbd "M-h") nil)

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
  (define-key org-agenda-keymap (kbd "<tab>") #'ignore)
  (defun my-org-agenda-goto-in-same-window ()
    "`org-agenda-goto` that opens the target buffer in the current window."
    (interactive)
    (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
      (when (fboundp 'org-agenda-goto)
        (funcall 'org-agenda-goto))))

  (setq org-agenda-file-regexp (replace-regexp-in-string
                                "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                org-agenda-file-regexp)))


;;; Jenkinsfile
(lightemacs-use-package jenkinsfile-mode
  :commands jenkinsfile-mode
  :mode
  (("/Jenkinsfile[^/]*\\'" . jenkinsfile-mode)
   ("/Jenkinsfile\\'" . jenkinsfile-mode))
  :init
  ;; (add-to-list 'auto-mode-alist '("/Jenkinsfile.*\\'" . jenkinsfile-mode))
  ;; (add-to-list 'auto-mode-alist '("Jenkinsfile[^/]*\\'" . jenkinsfile-mode))
  ;; (add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . jenkinsfile-mode))
  )

;;; BASIC
(lightemacs-use-package basic-mode
  :commands (cp437-dos
             basic-qb45-mode)
  :init
  ;; (setq default-buffer-file-coding-system 'cp437-dos)

  ;; Djgpp and rhide
  (add-to-list 'file-coding-system-alist '("\\.C\\'" . cp437-dos))
  (add-to-list 'file-coding-system-alist '("\\.H\\'" . cp437-dos))

  (add-to-list 'file-coding-system-alist '("\\.[bB][aA][sS]\\'" . cp437-dos))

  ;; (autoload 'basic-generic-mode "basic-mode" "Major mode for editing BASIC code." t)
  (add-to-list 'auto-mode-alist '("\\.[bB][aA][sS]\\'" . basic-qb45-mode)))

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

;;; Provide

(provide 'mod-misc)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-misc.el ends here

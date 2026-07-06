;;; mod-kirigami.el --- Better Folds -*- lexical-binding: t -*-

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
;; This package provides enhanced folding and unfolding capabilities in Emacs.
;; It offers a more intuitive commands to manipulate code and text folds.

;;; Code:

;;; Require

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

;;; Code folding settings

(setq lightemacs-outline-indent-minor-target-hooks '(yaml-mode-hook
                                                     yaml-ts-mode-hook
                                                     python-mode-hook
                                                     python-ts-mode-hook
                                                     haskell-mode-hook
                                                     ;; My preference
                                                     sh-mode-hook
                                                     bash-ts-mode-hook
                                                     php-mode-hook
                                                     php-ts-mode-hook
                                                     txt-file-mode-hook))

(setq lightemacs-outline-minor-target-hooks '(emacs-lisp-mode-hook
                                              lisp-mode-hook
                                              conf-mode-hook
                                              markdown-mode-hook
                                              ;; TODO?
                                              ;; markdown-ts-mode-hook
                                              diff-mode-hook))

(setq lightemacs-treesit-fold-target-hooks '(c-ts-mode-hook
                                             c++-ts-mode-hook
                                             java-ts-mode-hook
                                             rust-ts-mode-hook
                                             go-ts-mode-hook
                                             ruby-ts-mode-hook
                                             php-ts-mode-hook
                                             csharp-ts-mode-hook
                                             go-mod-ts-mode-hook
                                             lua-ts-mode-hook
                                             js-ts-mode-hook
                                             typescript-ts-mode-hook
                                             tsx-ts-mode-hook
                                             css-ts-mode-hook
                                             html-ts-mode-hook
                                             heex-ts-mode-hook
                                             xml-ts-mode-hook
                                             ;; bash-ts-mode-hook
                                             cmake-ts-mode-hook
                                             dockerfile-ts-mode-hook
                                             awk-ts-mode-hook
                                             vimscript-ts-mode-hook
                                             nix-ts-mode-hook
                                             json-ts-mode-hook
                                             toml-ts-mode-hook
                                             makefile-ts-mode-hook
                                             verilog-ts-mode-hook
                                             vhdl-ts-mode-hook
                                             hlsl-ts-mode-hook
                                             latex-ts-mode-hook
                                             beancount-ts-mode-hook
                                             markdown-ts-mode-hook
                                             mermaid-ts-mode-hook
                                             gdscript-ts-mode-hook
                                             clojure-ts-mode-hook
                                             caml-ts-mode-hook
                                             ocaml-ts-mode-hook
                                             erlang-ts-mode-hook
                                             elixir-ts-mode-hook
                                             scala-ts-mode-hook
                                             dart-ts-mode-hook
                                             haskell-ts-mode-hook
                                             julia-ts-mode-hook
                                             kotlin-ts-mode-hook
                                             gleam-ts-mode-hook
                                             noir-ts-mode-hook
                                             kotlin-ts-mode-hook
                                             swift-ts-mode-hook
                                             elixir-ts-mode-hook
                                             zig-ts-mode-hook))

(setq lightemacs-hs-minor-target-hooks '(;; Systems and General Purpose
                                         c-mode-hook
                                         c++-mode-hook
                                         java-mode-hook
                                         rust-mode-hook
                                         go-mode-hook
                                         ruby-mode-hook
                                         perl-mode-hook

                                         ;; Web and frontend
                                         js-mode-hook
                                         typescript-mode-hook
                                         css-mode-hook

                                         ;; Scripting, Data, and Infrastructure
                                         json-mode-hook
                                         lua-mode-hook
                                         nxml-mode-hook
                                         html-mode-hook))

;;; Install Kirigami

(setq kirigami-menu-bar-label "Folds")
(setq kirigami-context-menu-label "Contextual folds")
(setq kirigami-show-menu-bar t)
(setq kirigami-show-context-menu t)

;; Important for kirigami jump to load
;; Copied/pasted from le-kirigami
(lightemacs-use-package kirigami
  :commands (kirigami-open-fold
             kirigami-open-fold-rec
             kirigami-close-fold
             kirigami-toggle-fold
             kirigami-open-folds
             kirigami-close-folds-except-current
             kirigami-close-folds
             kirigami-global-mode)
  :hook (lightemacs-after-init . kirigami-global-mode)
  :init
  (lightemacs-module-bind kirigami
    (global-set-key (kbd "C-c z o") 'kirigami-open-fold)
    (global-set-key (kbd "C-c z c") 'kirigami-close-fold)
    (global-set-key (kbd "C-c z m") 'kirigami-close-folds)
    (global-set-key (kbd "C-c z r") 'kirigami-open-folds)
    (global-set-key (kbd "C-c z O") 'kirigami-open-fold-rec)
    (global-set-key (kbd "C-c z TAB") 'kirigami-toggle-fold))

  :config
  (require 'kirigami-jump)
  (when (fboundp 'kirigami-jump-mode)
    (kirigami-jump-mode 1))

  (with-eval-after-load 'evil
    (require 'kirigami-evil)
    (when (fboundp 'kirigami-evil-mode)
      (kirigami-evil-mode 1))))


;;; Useful function: my-reveal-kirigami-fold-after-undo-advice

(defun my-reveal-kirigami-fold (&rest _args)
  "Open folds using `kirigami-open-fold' after an undo operation."
  (interactive)
  (ignore-errors
    (kirigami-open-fold)))

(defun my-reveal-kirigami-fold-after-undo-advice (&rest _args)
  "Open folds using `kirigami-open-fold' after an undo operation."
  (interactive)
  (when (invisible-p (point))
    (my-reveal-kirigami-fold)))

;;; Press SPC

(with-eval-after-load 'evil
  (when (bound-and-true-p evil-normal-state-map)
    (define-key evil-normal-state-map (kbd "SPC") 'my-reveal-kirigami-fold)))

;;; DISABLED: Adjust window to ensure heading is visible

;; TODO Find a way to add this fix to kirigami??
;; Adjust the window to ensure the current heading remains visible. This fixes
;; a bug where not the whole heading is displayed, with makes outline only
;; display an allipsis at window-start
;; (defun kirigami--outline-run-on-window-change (&optional object)
;;   "Ensure outline headings remain visible when windows display new buffers.
;; OBJECT can be a window or a frame."
;;   (when (fboundp 'kirigami--outline-ensure-window-start-heading-visible)
;;     (dolist (window (cond
;;                      ((windowp object)
;;                       (list object))
;;
;;                      ((framep object)
;;                       (window-list object))
;;
;;                      ((not object)
;;                       (list (selected-window)))))
;;       (kirigami--outline-ensure-window-start-heading-visible window))))

;;; Outline: Ensure window-start is visible

;; Good implementation but risky when immediately is t
(defvar kirigami--outline-fix-window-start-immediately nil)

;; TODO store windows is a list and run-with-timer should act on them
(defun mod-kirigami--outline-ensure-visible (object)
  "Ensure outline headings remain visible when windows display new buffers.
OBJECT can be nil (current window), a window, or a frame."
  (when (fboundp 'kirigami--outline-ensure-window-start-heading-visible)
    (dolist (window (cond
                     ((windowp object)
                      (list object))

                     ((framep object)
                      (window-list object))

                     ((not object)
                      (list (selected-window)))

                     ((listp object)
                      object)

                     (t
                      (error
                       "OBJECT needs to be a window, a frame, nil, or a list of windows"))))
      (when (window-live-p window)
        (with-selected-window window
          (when (or (derived-mode-p 'outline-mode)
                    (bound-and-true-p outline-minor-mode))
            (if kirigami--outline-fix-window-start-immediately
                (kirigami--outline-ensure-window-start-heading-visible)
              ;; SAFETY: Defer the adjustment to the next event loop cycle.
              ;; This prevents 'set-window-start' from clashing with the
              ;; active redisplay.
              (run-with-timer
               0 nil
               #'kirigami--outline-ensure-window-start-heading-visible))))))))

(add-hook 'window-buffer-change-functions
          #'mod-kirigami--outline-ensure-visible)

;;; kirigami settings

;; (setq pulse-delay 0.04)

;; (defun test/kirigami-pre-hook (_action)
;;   "Test pre-hook that logs the ACTION.
;; Must return non-nil so `run-hook-with-args-until-failure` does not stop."
;;   ;; (message "[2] PRE-HOOK: Validating state for %S" action)
;;   t
;;   )
;;
;; (defun test/kirigami-post-hook (_action)
;;   "Test post-hook that logs the completion of ACTION."
;;   ;; (message "[3] POST-HOOK: Finalized operation %S" action)
;;   t
;;   )
;;
;; ;; Registration
;; (add-hook 'kirigami-pre-action-predicates #'test/kirigami-pre-hook)
;; (add-hook 'kirigami-post-action-functions #'test/kirigami-post-hook)

;; -----------------------------------------------
;; PULSE
;; -----------------------------------------------
;; (defun mod-kirigami-pulse (_action)
;;   "Visually emphasize the current line after a Kirigami folding operation.
;; ACTION identifies the completed folding operation and is ignored by
;; this function. The function produces a brief, transient highlight on
;; the line at point, providing visual feedback that the folding change
;; has finished."
;;   (pulse-momentary-highlight-one-line)
;;   )
;;
;; (add-hook 'kirigami-post-action-functions #'mod-kirigami-pulse)

;;; Undo

;; Built-in
(dolist (cmd '(undo
               undo-only
               undo-redo))
  (when (fboundp cmd)
    (advice-add cmd :after #'my-reveal-kirigami-fold-after-undo-advice)))

;; (with-eval-after-load 'undo-fu
;;   (dolist (cmd '(undo-fu-only-undo
;;                  undo-fu-only-redo))
;;     (when (fboundp cmd)
;;       (advice-add cmd :after #'my-reveal-kirigami-fold-after-undo-advice))))

;; (with-eval-after-load 'undo-tree
;;   (dolist (cmd '(undo-tree-undo
;;                  undo-tree-redo))
;;     (when (fboundp cmd)
;;       (advice-add cmd :after #'my-reveal-kirigami-fold-after-undo-advice))))

;; (with-eval-after-load 'vundo
;;   (dolist (cmd '(vundo-backward
;;                  vundo-forward))
;;     (when (fboundp cmd)
;;       (advice-add cmd :after #'my-reveal-kirigami-fold-after-undo-advice))))

;; (defun reveal-kirigami-fold-after-undo ()
;;   "Open folds using `kirigami-open-fold' after an undo operation."
;;   (message "THIS COMMAND: %s" this-command)
;;   (when (memq this-command '(undo
;;                              undo-only
;;                              undo-redo
;;                              undo-fu-only-undo
;;                              undo-fu-only-redo
;;                              ;; undo-tree-undo
;;                              ;; undo-tree-redo
;;                              ;; vundo-backward
;;                              ;; vundo-forward
;;                              ))
;;     (when (and (invisible-p (point))
;;                (fboundp 'kirigami-open-fold))
;;       (ignore-errors
;;         (kirigami-open-fold)))))
;;
;; (add-hook 'post-command-hook #'reveal-kirigami-fold-after-undo)

;;; DISABLED: pulse

;; Useless when loading because window-start does not change
;; (defun kirigami--outline-enable-immediate-fix ()
;;   "Enable the immediate window start fix for the current buffer."
;;   ;; Add locally so it only runs on relevant buffers
;;   (when (or (derived-mode-p 'outline-mode)
;;             (bound-and-true-p outline-minor-mode))
;;     (add-hook 'window-scroll-functions
;;               #'(lambda(window _display-start &rest _args)
;;                   (let ((kirigami--outline-fix-window-start-immediately t))
;;                     (mod-kirigami--outline-ensure-visible window)))
;;               nil  ; depth
;;               t))) ; local
;; (add-hook 'outline-minor-mode-hook #'kirigami--outline-enable-immediate-fix)
;; (add-hook 'outline-mode-hook #'kirigami--outline-enable-immediate-fix)

;; (with-eval-after-load 'evil
;;   (require 'kirigami-evil)
;;   (kirigami-evil-mode))

;;; DISABLED: outline

;; (defun my-setup-outline-minor-mode-kirigami ()
;;   "Close all folds."
;;   (let ((buffer-name (buffer-name)))
;;     ;; Buffers such as *vc-diff*
;;     (unless (string-prefix-p "*vc-" buffer-name)
;;       (ignore-errors
;;         (unwind-protect
;;             (kirigami-close-folds)
;;           (kirigami-open-fold))))))
;;
;; (add-hook 'outline-minor-mode-hook #'my-setup-outline-minor-mode-kirigami)

;;; comment/uncomment

(defcustom kirigami-unfold-before-comment t
  "When non-nil, automatically unfold before commenting.
This advises standard Emacs comment commands to unfold the text at point
or within the active region before applying the comment."
  :type 'boolean
  :group 'kirigami)

(defun kirigami--unfold-for-comment-advice (&rest _args)
  "Advice function to unfold text before commenting operations."
  (when kirigami-unfold-before-comment
    (save-excursion
      (if (use-region-p)
          (goto-char (region-beginning))
        (beginning-of-line))
      (ignore-errors
        (kirigami-open-fold-rec)))))

(advice-add 'comment-dwim :before #'kirigami--unfold-for-comment-advice)
(advice-add 'comment-region :before #'kirigami--unfold-for-comment-advice)
(advice-add 'comment-or-uncomment-region :before #'kirigami--unfold-for-comment-advice)
(advice-add 'uncomment-region :before #'kirigami--unfold-for-comment-advice)

;;; Kirigami: Fold things when opening them

;; TODO kirigami close fold except this one
(defun my-kirigami-auto-open ()
  "Close all folds."
  (unless (bound-and-true-p easysession-load-in-progress)
    (save-excursion
      (ignore-errors
        (require 'kirigami nil t)
        (when (fboundp 'kirigami-open-fold)
          (kirigami-open-fold))))))

(defcustom my-kirigami-auto-close-min-lines 100
  "Minimum number of lines required to trigger auto-closing of folds.
If nil, this static line check is disabled."
  :type '(choice (integer :tag "Minimum line count")
                 (const :tag "Disable static line check" nil))
  :group 'kirigami)

(defcustom my-kirigami-auto-close-fit-window 1
  "If non-nil, auto-close folds if the buffer lines exceed the window height."
  :type 'boolean
  :group 'kirigami)

(defun my-kirigami-auto-close-all ()
  "Close all folds."
  (unless (bound-and-true-p easysession-load-in-progress)
    (save-excursion
      (ignore-errors
        (when (and (let ((total-lines (line-number-at-pos (point-max))))
                     (or (and (integerp my-kirigami-auto-close-min-lines)
                              (> total-lines my-kirigami-auto-close-min-lines))
                         (and my-kirigami-auto-close-fit-window
                              (> total-lines (window-body-height)))))
                   (require 'kirigami nil t)
                   (fboundp 'kirigami-close-folds)
                   (let ((name (buffer-name)))
                     (and (not (string-prefix-p "*" name))
                          (not (string-prefix-p " " name))
                          (not (string-suffix-p "*" name))))
                   (not (bound-and-true-p edit-indirect--overlay))
                   (not (and (fboundp 'org-src-edit-buffer-p)
                             (org-src-edit-buffer-p))))
          (kirigami-close-folds))))))

(add-hook 'outline-minor-mode-hook #'my-kirigami-auto-close-all 99)
(add-hook 'save-place-after-find-file-hook #'my-kirigami-auto-open 99)

;;; Provide

(provide 'mod-kirigami)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-kirigami.el ends here

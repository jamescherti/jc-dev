;;; my-config-evil.el --- Config evil -*- lexical-binding: t -*-

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

(require 'evil)
(require 'evil-collection)
(require 'my-defun)
(require 'lightemacs)  ; lightemacs-save-window-start
(eval-and-compile
  (require 'lightemacs-use-package))  ; lightemacs-save-window-start
(require 'mod-project)

;;; Better evil

(defun evilbuffer-switch-to-scratch ()
  "Switch to the *scratch* buffer."
  (interactive)
  (scratch-buffer))

(defun evilbuffer-toggle-truncate-line ()
  "Toggle truncate line."
  (interactive)
  (if truncate-lines
      ;; When the text is wrapped
      (progn
        ;; (visual-line-mode 1)
        ;; (setq-local my-was-visual-line-fill-column-mode
        ;;             (and (boundp 'my-was-visual-line-fill-column-mode)
        ;;                  my-was-visual-line-fill-column-mode))
        ;; (when my-was-visual-line-fill-column-mode
        ;;   (visual-line-fill-column-mode 1)
        ;;   (setq-local my-was-visual-line-fill-column-mode nil))
        (let ((inhibit-message t))
          ;; Wrap
          (toggle-truncate-lines 0)))
    ;; When the text is truncated
    ;; (visual-line-mode 1)
    ;; (when (and (boundp 'visual-line-fill-column-mode)
    ;;            visual-line-fill-column-mode)
    ;;   (visual-line-fill-column-mode -1)
    ;;   (setq-local my-was-visual-line-fill-column-mode t))

    (let ((inhibit-message t))
      ;; Truncate
      (toggle-truncate-lines 1))))

(defun evilbuffer-erase ()
  "Delete the entire contents of the current buffer, even if it is read-only."
  (interactive)
  (let ((buffer-name (buffer-name))
        (inhibit-read-only t))
    (when (yes-or-no-p "Are you sure you want to erase the buffer?")
      (erase-buffer))

    (cond
     ((string-prefix-p "*Ollama" buffer-name)
      (cond
       ((derived-mode-p 'org-mode)
        (insert "* "))

       ((or (derived-mode-p 'markdown-ts-mode)
            (derived-mode-p 'markdown-mode))
        (insert "# ")))

      (evil-insert-state)))))

(defun my-clear-highlights ()
  "Clear highlight and related state in the buffer."
  ;; Clear lazy highlights
  (lazy-highlight-cleanup)

  ;; Clear Evil mode highlights
  (evil-ex-nohighlight)

  ;; Update font lock to clear new highlights after functions are evaluated
  (when (and (bound-and-true-p font-lock-mode)
             (fboundp 'font-lock-update))
    (font-lock-update)))

(defun evilbuffer-clear-highlights ()
  "Clear."
  (interactive)

  (save-window-excursion
    (save-excursion
      (cond
       ((string= (buffer-name) "*Messages*")
        (when (y-or-n-p "Delete the content of the *Messages* buffer?")
          (let ((inhibit-read-only t))
            (erase-buffer))))

       ;; For some reason,
       (t
        (unless (derived-mode-p 'embark-collect-mode)
          ;; Do not clear highlights during `embark-collect-mode' to prevent
          ;; disruptive color changes.
          (my-clear-highlights))

        ;; If the current character lacks a font-lock face, ensure the entire
        ;; buffer is fontified. This addresses an Org mode issue where point is
        ;; inside a source block, but the #+BEGIN_SRC line is above the window
        ;; start and thus not yet fontified.
        ;; (when (and (bound-and-true-p font-lock-mode)
        ;;            (not (get-text-property (point) 'face)))
        ;;   (font-lock-ensure))
        )))))

(defun my-evil-disable-remove-spaces ()
  "Disable automatic removal of trailing spaces in `evil-mode'."
  (setq-local evil-maybe-remove-spaces nil))

(defun my-goto-end-of-buffer (&rest _)
  "Go to the end of the buffer."
  (interactive)
  (let ((column (current-column)))
    (unwind-protect
        (progn
          (goto-char (point-max)))
      ;; Small customization to never be on eobp
      ;; NOTE: this is handeled by a post-command-hook
      ;; (when (eobp)
      ;;   ;; Move the cursor before `eobp' (last empty line).
      ;;   (backward-char 1))

      ;; (beginning-of-visual-line)
      ;; (move-to-column column)
      ;; (recenter -1)

      (lightemacs-recenter-maybe -1 t))))

;; TODO: Article? Bug report?
(defun my-uncomment-and-join-region ()
  "Join the selected lines."
  (interactive)
  (let* ((beg (if (use-region-p)
                  (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
                (line-beginning-position)))
         (end (if (use-region-p)
                  (let ((rend (region-end)))
                    (if (eq (char-before rend) ?\n)
                        (1- rend)  ; Exclude the final newline
                      rend))
                (save-excursion
                  (goto-char beg)
                  (forward-line 1)
                  (line-end-position))))
         ;; (contents (buffer-substring-no-properties beg end))
         ;; (is-comment nil)
         (original-point nil)
         (beg-line nil)
         (end-line nil))
    (save-excursion
      (when (and beg end)
        (when (> beg end)
          (let ((old-beg beg))
            (setq beg end)
            (setq end old-beg))))

      (setq original-point (if (use-region-p)
                               beg
                             (point)))

      (setq beg-line (save-excursion
                       (goto-char beg)
                       (line-number-at-pos)))
      (setq end-line (save-excursion
                       (goto-char end)
                       (line-number-at-pos)))

      ;; (unless (string= (buffer-substring-no-properties beg end)
      ;;                  contents)
      ;;   (setq is-comment t))

      (deactivate-mark)

      (when (/= beg-line end-line)
        (goto-char beg)
        (save-excursion
          (forward-line 1)
          (uncomment-region (line-beginning-position) end)))

      (save-excursion
        (let ((count (- end-line beg-line)))
          (goto-char beg)
          (dotimes (i count)
            (when (= (1+ i) count)
              (beginning-of-line))
            (join-line 1)))))

    (goto-char original-point)))

;; TODO lightemacs?
(defun my-bash-stdops-sre ()
  "Call sre."
  (interactive)
  (let ((project-dir (expand-file-name (my-project-root-dir))))
    (unless project-dir
      (user-error "Unable to find the project path: %s" project-dir))
    (when (fboundp 'bash-stdops-project-sre)
      (bash-stdops-project-sre nil nil project-dir))))

;; eval buffer
(defun evileval-buffer ()
  "Evaluate the current buffer and display a message."
  (interactive)
  (unless (derived-mode-p 'emacs-lisp-mode)
    (user-error "This function supports only emacs-lisp-mode"))
  (eval-buffer)
  ;; (when (featurep package-name)
  ;;   (unload-feature package-name t))
  ;; (load (buffer-file-name (buffer-base-buffer)))
  (message "Buffer evaluated!"))

(defun evileval-region ()
  "Evaluate the current region and display a message."
  (interactive)
  (unless (or (derived-mode-p 'markdown-mode)
              (derived-mode-p 'org-mode)
              (derived-mode-p 'emacs-lisp-mode))
    (error "This function supports only emacs-lisp-mode"))
  (if (use-region-p)
      (progn
        (eval-region (region-beginning) (region-end))
        (message "Region evaluated!"))
    (message "No region selected!")))

(evil-define-key 'normal 'global (kbd "<leader>er") 'evileval-region)
(evil-define-key 'normal 'global (kbd "<leader>eb") 'evileval-buffer)

;; Goto end buffer
(define-key evil-normal-state-map "G" 'my-goto-end-of-buffer)

;; Enhanced versions of `evil-shift-right' and `evil-shift-left' for visual
;; mode. These functions shift the selected region to the right or left, then
;; temporarily exit normal mode to ensure the visual selection is restored
;; correctly. This provides a smoother experience when indenting multiple
;; lines in Evil visual mode, preserving the selection and allowing repeated
;; shifts without losing the highlighted region.
(defun lightemacs-evil-shift-right ()
  "Shift the selected region to the right, preserving the selection."
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end 1 nil)
  (evil-normal-state)
  (evil-visual-restore))

(defun lightemacs-evil-shift-left ()
  "Shift the selected region to the left, preserving the selection."
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end 1 nil)
  (evil-normal-state)
  (evil-visual-restore))
(evil-define-key 'visual 'global
  (kbd ">") 'lightemacs-evil-shift-right
  (kbd "<") 'lightemacs-evil-shift-left)

;; The `evil-search-next` and `evil-search-previous` functions can sometimes
;; leave the buffer window scrolled horizontally. This advice adds an around
;; advice to these functions that resets the horizontal scroll position
;; (`set-window-hscroll`) to 0 when navigating using search, so the user is
;; presented with the correct starting point for their next search.

(defun my-advice-search-next (orig-fun &rest args)
  "Advice for search-next to reset horizontal scroll position.
ORIG-FUN is the function and ARGS the arguments."
  (when (/= 0 (window-hscroll))
    (set-window-hscroll nil 0))
  (apply orig-fun args))

(defun my-advice-search-previous (orig-fun &rest args)
  "Advice for search-previous to reset horizontal scroll position.
ORIG-FUN is the function and ARGS the arguments."
  (when (/= 0 (window-hscroll))
    (set-window-hscroll nil 0))
  (apply orig-fun args))

(with-no-warnings
  (advice-add 'search-next :around #'my-advice-search-next)
  (advice-add 'search-previous :around #'my-advice-search-previous))

;; Join the current line and the next one while preserving the cursor position.
(defun evilcursor-join-normal ()
  "Join the current line and the next one while preserving the cursor position."
  (interactive)
  (save-excursion
    (evil-join (line-beginning-position) (line-end-position))))
(evil-define-key 'normal 'global (kbd "J") 'evilcursor-join-normal)
(evil-define-key 'visual 'global (kbd "J") 'evil-join)

;; M-[ and M-]: Previous and next section
(defun my-evil-forward-section-end ()
  "Move to the next section."
  (interactive)
  (execute-kbd-macro (read-kbd-macro "]]")))
(defun my-evil-backward-section-end ()
  "Move to the previous section."
  (interactive)
  (execute-kbd-macro (read-kbd-macro "[[")))
(evil-define-key 'normal 'global (kbd "<leader>j") 'my-uncomment-and-join-region)
(evil-define-key 'visual 'global (kbd "<leader>j") 'my-uncomment-and-join-region)
(when (display-graphic-p)
  ;; Only on display-graphic-p because the M-[ issue occurs because modern
  ;; terminals use "Escape sequences" beginning with the ESC [ characters (the
  ;; byte-level equivalent of M-[) to communicate complex input like mouse
  ;; movements, pixel-perfect clicks, and bracketed pastes. When you bind a
  ;; custom command to M-[ in your Emacs configuration, you effectively
  ;; "highjack" the prefix of these incoming messages; Emacs consumes the first
  ;; two characters to trigger your function, leaving the remaining coordinate
  ;; or paste data orphaned and uninterpreted. Consequently, the terminal’s
  ;; attempt to say "the mouse clicked at these coordinates" is chopped up, and
  ;; the tail end of that technical string—the "gibberish" you see—is dumped
  ;; directly into your buffer as literal text. Would you like me to provide a
  ;; snippet to identify exactly which command is currently "stealing" that M-[
  ;; prefix?
  (evil-define-key '(insert motion) 'global (kbd "M-[")
    'my-evil-backward-section-end))
(evil-define-key '(insert motion) 'global (kbd "M-]")
  'my-evil-forward-section-end)

;; Do not fail when the kill-ring is empty
;; To prevent the p command in Evil mode from failing when the paste ring
;; (akin to the clipboard in Vim) is empty, you can redefine the paste
;; function to check if the ring is empty before attempting to paste. Here’s
;; how you can do it using Emacs Lisp:
(defun ignore-empty-ring-errors (orig-func &rest args)
  "Ignore errors related to the empty ring when calling ORIG-FUNC with ARGS."
  (condition-case nil
      (apply orig-func args)
    (error (message "Nothing to paste!") nil)))
(with-no-warnings
  (advice-add 'evil-paste-after :around #'ignore-empty-ring-errors)
  (advice-add 'evil-paste-before :around #'ignore-empty-ring-errors))

(evil-define-key '(normal insert visual) 'global (kbd "C-s")
  #'my-save-buffer)

(defun evilclipboard-select-pasted ()
  "Visually select last pasted text."
  (interactive)
  (evil-goto-mark ?\[)
  (evil-visual-char)
  (evil-goto-mark ?\]))
(with-no-warnings
  (define-key evil-normal-state-map (kbd "gp") 'evilclipboard-select-pasted)
  (define-key evil-normal-state-map (kbd "<leader>gp") 'evilclipboard-select-pasted))

(setopt evil-want-Y-yank-to-eol t)

(evil-set-leader 'normal (kbd ","))
(evil-set-leader 'visual (kbd ","))
(with-eval-after-load 'ibuffer
  (evil-define-key 'normal ibuffer-mode-map (kbd ",") nil)

  ;; Conflict between evil and `ibuffer-mode-map'
  ;; For some reason, this is not enough TODO
  ;; (define-key ibuffer-mode-map (kbd ",") nil)
  )
(evil-define-key 'normal 'global (kbd "<leader>q") #'ibuffer)

(with-eval-after-load 'eldoc
  (eldoc-add-command-completions "evilcursor-"))

(evil-define-key 'normal 'global (kbd "<leader>sre") 'my-bash-stdops-sre)

(evil-define-key 'normal 'global (kbd "<leader>ev") 'tab-bar-switch-to-tab)
(evil-define-key '(visual normal insert) 'global (kbd "M-p") 'project-switch-project)

(evil-define-key 'insert 'global (kbd "M-H") 'evil-backward-word-begin)
(evil-define-key 'insert 'global (kbd "M-L") 'evil-forward-word-begin)

(with-eval-after-load 'icomplete
  (evil-define-key 'normal icomplete-fido-mode-map (kbd "j") 'icomplete-forward-completions)
  (evil-define-key 'normal icomplete-fido-mode-map (kbd "k") 'icomplete-backward-completions)
  (evil-define-key 'normal icomplete-fido-mode-map (kbd "<down>") 'icomplete-forward-completions)
  (evil-define-key 'normal icomplete-fido-mode-map (kbd "<up>") 'icomplete-backward-completions)
  (evil-define-key 'insert icomplete-fido-mode-map (kbd "M-j") 'icomplete-forward-completions)
  (evil-define-key 'insert icomplete-fido-mode-map (kbd "M-k") 'icomplete-backward-completions))

(with-eval-after-load 'eat
  ;; Causes problems
  ;; (evil-define-key 'insert eat-mode-map (kbd "C-c")
  ;;   #'eat-self-input)
  (evil-define-key 'insert eat-mode-map (kbd "M-<left>") 'eat-self-input)
  (evil-define-key 'insert eat-mode-map (kbd "M-<right>") 'eat-self-input)
  (evil-define-key 'insert eat-mode-map (kbd "M-j") 'eat-self-input)
  (evil-define-key 'insert eat-mode-map (kbd "M-k") 'eat-self-input)
  (evil-define-key 'insert eat-mode-map (kbd "M-H") 'eat-self-input)
  (evil-define-key 'insert eat-mode-map (kbd "M-L") 'eat-self-input))

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "~") 'my-dired-home))

(evil-define-key 'normal 'global (kbd "<leader>im") 'inhibit-mouse-mode)
(evil-define-key 'normal 'global (kbd "<leader>ib") #'ibuffer)
  ;;; Automatic removal of spaces
(add-hook 'evil-insert-state-entry-hook #'my-evil-disable-remove-spaces)

(define-key evil-normal-state-map (kbd "C-l") 'evilbuffer-clear-highlights)
(define-key evil-insert-state-map (kbd "C-l") 'evilbuffer-clear-highlights)
(define-key evil-visual-state-map (kbd "C-l") 'evilbuffer-clear-highlights)
(with-eval-after-load 'messages-buffer-mode
  (define-key messages-buffer-mode-map (kbd "C-l") 'evilbuffer-clear-highlights))

(define-key evil-normal-state-map (kbd "<leader>wr") 'evilbuffer-toggle-truncate-line)
(define-key evil-normal-state-map (kbd "<leader>eB") 'evilbuffer-erase)

;; TODO patch evil: this should restore point with :restore-point t
(with-no-warnings
  (evil-define-operator my-evil-fill-and-move-operator (beg end)
    "Fill text and move point to the end of the filled region BEG and END.
This enhancement prevents the cursor from moving."
    :move-point nil
    :type line
    :restore-point t
    (save-excursion
      (evil-fill-and-move beg end))))
(define-key evil-normal-state-map "gq" 'my-evil-fill-and-move-operator)

;; It seems to only work when declared as default
(defun my-setup-evil-mode ()
  "Removed: `git-rebase-mode' erc-mode circe-server-mode circe-chat-mode."
  ;; circe-query-mode sauron-mode
  (dolist (mode '(vterm-mode
                  eat-mode
                  ;;custom-mode
                  ;; eshell-mode
                  ;; term-mode
                  ))
    (add-to-list 'evil-emacs-state-modes mode)))
(add-hook 'evil-mode-hook 'my-setup-evil-mode)

(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'my-save-buffers-kill-emacs))

(define-key evil-normal-state-map (kbd "C-q") 'my-save-buffers-kill-emacs)

;; Make goto mark use ' to restore the column
(define-key evil-motion-state-map "`" 'evil-goto-mark-line)
(define-key evil-motion-state-map "'" 'evil-goto-mark)

;; Useful to insert a quote instead of two quotes when packages such as electric quote are activated
(define-key evil-insert-state-map (kbd "C-\"") (lambda () (interactive) (insert "\"")))

(global-set-key (kbd "M-c") nil)  ;; Change the word to uppercase in Emacs
(global-set-key (kbd "M-v") nil)  ;; scroll the buffer up by one screenful
(global-set-key (kbd "M-z") nil)  ;; Zap to char (delete text from the current cursor position up to a specific character)

(define-key evil-insert-state-map (kbd "C-a") nil)
(define-key evil-insert-state-map (kbd "A-DEL") 'evil-delete-backward-word)
(define-key evil-insert-state-map (kbd "C-<Backspace>") 'evil-delete-backward-word)
;; (define-key evil-normal-state-map (kbd "gdp") 'delete-pair)  ;; NOTE: broken
(define-key evil-normal-state-map (kbd "<leader>p") 'delete-pair)
(define-key evil-normal-state-map (kbd "<leader>k") 'describe-key)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-b") 'move-beginning-of-line)


(define-key evil-visual-state-map (kbd "u") 'ignore) ;; Disable lower/upper case region
(define-key evil-visual-state-map (kbd "U") 'ignore) ;; Disable lower/upper case region
(define-key evil-visual-state-map (kbd "C-c") 'evil-yank)

(define-key evil-normal-state-map (kbd "<leader>gs") 'global-text-scale-adjust)
(define-key evil-normal-state-map (kbd "<leader>cf") 'my-temporary-file)
(define-key evil-normal-state-map (kbd "<leader>ce") 'my-temporary-diff)
(define-key evil-normal-state-map (kbd "<leader>t")  'my-tab-split)
(define-key evil-normal-state-map (kbd "<leader>T")  'tab-bar-change-tab-group)
(define-key evil-normal-state-map (kbd "<leader>em") 'toggle-menu-bar-mode-from-frame)

(defun my-go-home ()
  "Go home."
  (interactive)
  (if (fboundp 'my-wip)
      (my-wip)
    (find-file "~/")))
(define-key evil-normal-state-map (kbd "<leader>ww") 'my-go-home)
(define-key evil-normal-state-map (kbd "<leader>W")  'my-go-home)

(define-key evil-normal-state-map (kbd "gs") 'evilbuffer-switch-to-scratch)

;; (when (fboundp 'my-dabbrev-completion-backwards)
;;   (setq evil-complete-next-func #'my-dabbrev-completion-backwards))
;;
;; (when (fboundp 'my-dabbrev-completion-forward)
;;   (setq evil-complete-previous-func #'my-dabbrev-completion-forward))
;; TODO use cape-dabbrev
;; (define-key evil-insert-state-map (kbd "C-p") 'my-dabbrev-completion-backwards)
;; (define-key evil-insert-state-map (kbd "C-n") 'my-dabbrev-completion-forward)

(defun my-dabbrev-completion-forward-all-buffers (arg)
  "Advice around `dabbrev-completion', with the same ARG arguments."
  (let ((dabbrev-check-all-buffers t))
    (dabbrev-completion arg)))

(with-eval-after-load 'vertico
  ;; TODO move somewhere else like mod-vertico-evil
  (evil-define-key '(insert normal) 'global (kbd "S-<return>") 'embark-dwim)
  (evil-define-key 'insert vertico-map (kbd "C-f") 'vertico-scroll-up)
  (evil-define-key 'normal vertico-map (kbd "C-f") 'vertico-scroll-up)
  (evil-define-key 'insert vertico-map (kbd "C-b") 'vertico-scroll-down)
  (evil-define-key 'normal vertico-map (kbd "C-b") 'vertico-scroll-down)
  (evil-define-key 'normal vertico-map (kbd "gg") 'vertico-first)
  (evil-define-key 'normal vertico-map (kbd "G") 'vertico-last)
  (evil-define-key 'normal vertico-map (kbd "<escape>") nil)
  (evil-define-key 'insert vertico-map (kbd "<tab>") 'vertico-insert)

  (evil-define-key 'insert vertico-map (kbd "M-k") 'vertico-previous)
  (evil-define-key 'insert vertico-map (kbd "M-j") 'vertico-next)

  ;; Useful for vertico-buffer
  (evil-define-key 'normal vertico-map (kbd "M-k") 'vertico-previous)
  (evil-define-key 'normal vertico-map (kbd "M-j") 'vertico-next)

  ;; I do not like it
  ;; (define-key minibuffer-local-map (kbd "<up>") 'previous-history-element)
  ;; (define-key minibuffer-local-map (kbd "<down>") 'next-history-element)
  (define-key minibuffer-local-map (kbd "C-<up>") 'previous-history-element)
  (define-key minibuffer-local-map (kbd "C-<down>") 'next-history-element)

  ;; Open in a NEW TAB
  (keymap-set vertico-map "C-t"
              #'(lambda()
                  ;; Open embark-dwim in a new tab
                  (when (fboundp 'tab-new-func-buffer-from-other-window)
                    (tab-new-func-buffer-from-other-window 'embark-dwim))))

  ;; TODO part of this is sub-better-evil.el

  (add-hook
   'embark-after-export-hook
   #'(lambda()
       ;; wgrep
       (if (or t (< emacs-major-version 31))
           (when (fboundp 'wgrep-change-to-wgrep-mode)
             (wgrep-change-to-wgrep-mode)
             (when (fboundp 'wgrep-finish-edit)
               ;; TODO: save and restore the cursor: wgrep-finish-edit
               (evil-define-key 'normal 'local (kbd "C-s")
                 #'(lambda()
                     (interactive)
                     (lightemacs-save-window-start
                       (lightemacs-save-window-hscroll
                         (save-mark-and-excursion
                           (wgrep-finish-edit)
                           (wgrep-change-to-wgrep-mode))))))))

         ;; Emacs >= 31
         (when (fboundp 'grep-change-to-grep-edit-mode)
           (grep-change-to-grep-edit-mode)
           (when (fboundp 'grep-edit-save-changes)
             ;; TODO: save and restore the cursor: wgrep-finish-edit
             ;; (evil-define-key 'normal 'local (kbd "C-s")
             ;;   #'(lambda()
             ;;       (interactive)
             ;;       (grep-edit-save-changes)))
             )))))

  (evil-define-key 'normal 'global (kbd "<leader>ee") 'embark-dwim)
  (evil-define-key 'normal 'global (kbd "<leader>ew") 'embark-act))

(with-eval-after-load 'vterm
  (evil-define-key 'insert vterm-mode-map (kbd "M-H") 'my-vterm--send-Alt-Shift-H)
  (evil-define-key 'insert vterm-mode-map (kbd "M-L") 'my-vterm--send-Alt-Shift-L)

  ;; Useful for nano
  ;;(define-key vterm-mode-map (kbd "C-c") 'vterm--self-insert)
  ;;(define-key vterm-mode-map (kbd "C-g") 'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-x") 'vterm--self-insert)

  (define-key vterm-mode-map (kbd "C-c C-c") 'vterm--self-insert)

  (define-key vterm-mode-map (kbd "M-j") 'vterm--self-insert)
  (define-key vterm-mode-map (kbd "M-k") 'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "M-j") 'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "M-k") 'vterm--self-insert)

  (define-key vterm-mode-map (kbd "M-H") 'my-vterm--send-Alt-Shift-H)
  (define-key vterm-mode-map (kbd "M-L") 'my-vterm--send-Alt-Shift-L))

;;; evil jump

(defun eviljump-goto-definition-try-imenu-first (imenu-only)
  "Improved `evil-goto-definition` to open folds correctly in outline mode.
When IMENU-ONLY is nil it only uses imenu."
  (require 'xref)
  (when (fboundp 'xref-push-marker-stack)
    (let ((previous-point (point))
          (previous-point-marker (point-marker)))
      ;; Try imenu first
      (when (member 'evil-goto-definition-imenu evil-goto-definition-functions)
        (let ((evil-goto-definition-functions '(evil-goto-definition-imenu)))
          (evil-goto-definition))

        (when (not (eq previous-point (point)))
          ;; Add it to the stack because it does not do it by default
          (xref-push-marker-stack previous-point-marker)
          (setq imenu-only t)))

      ;; Try the others
      (unless imenu-only
        (let ((evil-goto-definition-functions
               (cl-remove 'evil-goto-definition-imenu
                          evil-goto-definition-functions)))
          (evil-goto-definition))))))

(defun eviljump-goto-definition (&optional force-all)
  "Find definition and scroll line to top.
When FORCE-ALL is non-nil, use all functions."
  (interactive)
  (lightemacs-recenter-if-out-of-view
    (cond
     ((and (fboundp 'eglot-managed-p)
           (eglot-managed-p))
      (xref-find-definitions (thing-at-point 'symbol t)))

     ((and (boundp 'lsp-mode)
           lsp-mode
           (fboundp 'lsp-find-definition))
      (lsp-find-definition))

     ((and (not force-all)
           (derived-mode-p 'emacs-lisp-mode))
      ;; Do not jump to emacs.d. Only use imenu.
      (eviljump-goto-definition-try-imenu-first t))

     (t
      (eviljump-goto-definition-try-imenu-first nil)))))

(defun eviljump-goto-definition-force ()
  "Go to definition."
  (interactive)
  (eviljump-goto-definition t))

(evil-define-key 'normal 'global (kbd "<leader>d") 'eviljump-goto-definition)
(evil-define-key 'normal 'global (kbd "<leader>D") 'eviljump-goto-definition-force)
;; Causes bugs
;; (evil-define-key 'normal 'global (kbd "gd") 'eviljump-goto-definition)
;; (evil-define-key 'normal 'global (kbd "gD") 'eviljump-goto-definition-force)


;;; Paste with current indentation

(defun string-get-indentation (str)
  "Get the indentation of STR (leading spaces and tabs)."
  (replace-regexp-in-string "^\\([ \t]*\\).*$" "\\1" str))

(defun evil-clipboard--string-unindent (input-str)
  "Unindent INPUT-STR by removing the minimal common indentation from all lines."
  (let ((lines (split-string input-str "\n"))
        (min-indent nil))

    ;; Find the minimum indentation
    (dolist (line lines)
      (let* ((indent (string-get-indentation line))
             (length-indent (length indent)))
        (when (and
               ;; The line must not be empty
               (not (string= (string-trim-left line) ""))
               ;; And the indent should be lower than the one that has been found
               ;; previously
               (or (not min-indent)
                   (< length-indent min-indent)))
          (setq min-indent length-indent))))

    (unless min-indent
      (setq min-indent 0))

    ;; Remove the minimum indentation from all lines
    (let ((result ""))
      (dolist (line lines)
        (setq result
              (concat result
                      (if (string= result "") "" "\n")
                      ;; (substring line min-indent)
                      (if (>= (length line) min-indent)
                          (substring line min-indent)
                        line)
                      )))
      result)))

(defun evilclipboard-paste-with-current-indentation ()
  "Paste text from the clipboard with the current line's indentation."
  (interactive)
  (let* ((original-register-contents (evil-get-register ?a t))
         (new-indentation (make-string (current-column) ?\s))
         (kill-ring-content (ignore-errors (current-kill 0)))
         (text (if kill-ring-content
                   (evil-clipboard--string-unindent (string-trim-right
                                                     (substring-no-properties kill-ring-content)
                                                     "\n"))
                 ""))
         (text-to-paste (string-trim-left
                         (replace-regexp-in-string "^" new-indentation text))))
    (evil-set-register ?a text-to-paste)
    (if (use-region-p)
        (evil-visual-paste 1 ?a)
      (progn
        (evil-paste-before 1 ?a)
        (when evil-move-cursor-back
          (forward-char 1))
        (when original-register-contents
          (evil-set-register ?a original-register-contents))))))

(defun evilclipboard-paste-with-current-indentation-restore-point ()
  "Paste text from the clipboard with the current line's indentation.
This function also restores window start and point when pasting multiple lines."
  (interactive)
  (if (minibufferp)
      (let ((evil-move-cursor-back nil))
        (evil-paste-before 1))
    (lightemacs-save-window-hscroll
      (lightemacs-save-window-start
        (save-mark-and-excursion
          (evilclipboard-paste-with-current-indentation))))))

;; (define-key evil-insert-state-map (kbd "C-a p") 'evilclipboard-paste-with-current-indentation-restore-point)
;; (define-key evil-insert-state-map (kbd "C-a C-p") 'evilclipboard-paste-with-current-indentation-restore-point)

(evil-define-key 'insert 'global (kbd "C-v") 'evilclipboard-paste-with-current-indentation-restore-point)

;;; Copy with without indentation

(defun evilclipboard-evil-yank-region-unindented ()
  "Copy the region, un-indented by the length of its minimum indent.
If numeric prefix argument PAD is supplied, indent the resulting
text by that amount."
  (interactive)
  (when (use-region-p)
    (evil-yank (region-beginning) (region-end))
    (dolist (register '(?\" ?*))
      (let ((original-contents (evil-get-register register t)))
        (when original-contents
          (evil-set-register
           register (evil-clipboard--string-unindent
                     (substring-no-properties original-contents))))))))

(evil-define-key 'visual 'global (kbd "C") 'evilclipboard-evil-yank-region-unindented)

;;; evilwindow: split and select

(defun evilwindow-split-and-select-new-window (split-direction)
  "Split the window in the specified direction then switch to the new window.
SPLIT-DIRECTION is the direction (v or h).
By using this function, Emacs and Evil can mimic the behavior of Vim when the
user presses Ctrl-w v and Ctrl-w s. It prohibits Emacs from altering the cursor
position when the user presses Ctrl-v or Ctrl-s to create a new split and
guarantees that the new window is selected, as in Vim."
  ;; Save current buffer's cursor position and view
  (let ((buf (current-buffer))
        (pos (point))
        (view (window-start)))
    ;; Split the window
    (if (equal split-direction "v")
        (evil-window-vsplit)
      (evil-window-split))
    ;; Restore cursor and view of previous window
    (with-current-buffer buf
      (goto-char pos)
      (set-window-start nil view t))
    ;; Go back to the previous window
    ;; (other-window 1)
    ))

(defun evilwindow-split-select-below ()
  "Split the window horizontally then switch to the new window."
  (interactive)
  (evilwindow-split-and-select-new-window "h"))

(defun evilwindow-split-select-right ()
  "Split the window vertically then switch to the new window."
  (interactive)
  (evilwindow-split-and-select-new-window "v"))

(defun evilwindow-edit-fold-and-open-first-fold (file-path)
  "Edit, fold, and open the first fold of FILE-PATH."
  (when (and (fboundp 'kirigami-close-folds)
             (fboundp 'kirigami-open-fold))
    (let ((file-buffer (find-buffer-visiting file-path)))
      (if file-buffer (switch-to-buffer file-buffer)
        (find-file file-path)
        (kirigami-close-folds)
        (kirigami-open-fold)
        (evil-goto-line 1)))))

(define-key evil-normal-state-map (kbd "C-w v") 'evilwindow-split-select-right)
(define-key evil-normal-state-map (kbd "C-w s") 'evilwindow-split-select-below)

;;; evil: browse-url

(defun my-evil-browse-url-copy-to-clipboard (url &optional _args)
  "Copy the URL to the clipboard instead of opening it."
  (dolist (register '(?\" ?*))
    (evil-set-register register url))
  (message "URL copied to clipboard: %s" url))

;; Override the browse-url function
(setq browse-url-browser-function 'my-evil-browse-url-copy-to-clipboard)

;;; evil consult

(defun my-consult-fd-project ()
  "Run `consult-fd` in the root directory of the current project."
  (interactive)
  (require 'consult)
  (let* ((project (project-current nil "."))
         (project-root (when (and project
                                  (fboundp 'project-root))
                         (project-root project)))
         (consult-fd-args
          (when (boundp 'consult-fd-args)
            (concat consult-fd-args
                    " --threads "
                    (number-to-string (num-processors))))))
    (if (fboundp 'consult-fd)
        (consult-fd project-root)
      (error "Undefined: consult-fd"))))

(defun my-consult-imenu ()
  "Call `consult-imenu'."
  (interactive)
  (transient-mark-mode -1)
  ;; (when (fboundp 'evil-normal-state)
  ;;   (evil-normal-state))
  (if (fboundp 'consult-imenu)
      (consult-imenu)
    (error "Undefined: consult-imenu")))

(define-key evil-normal-state-map (kbd "<leader>ff") 'my-consult-imenu)
(define-key evil-normal-state-map (kbd "<leader>m") 'consult-recent-file)
(define-key evil-normal-state-map (kbd "<leader>b") 'consult-recent-file)
;; (define-key evil-normal-state-map (kbd "<leadrr>B") 'switch-to-buffer)

(defun my-consult-buffer ()
  "My consult buffer."
  (interactive)
  (when (fboundp 'consult-buffer)
    (let ((orig-buffer-list (symbol-function 'buffer-list)))
      (cl-letf (((symbol-function 'buffer-list)
                 (lambda (&optional frame)
                   (seq-filter (lambda (buf)
                                 (not (with-current-buffer buf
                                        (derived-mode-p 'dired-mode))))
                               (funcall orig-buffer-list frame)))))
        (consult-buffer)))))

(define-key evil-normal-state-map (kbd "<leader>B") 'my-consult-buffer)
(define-key evil-normal-state-map (kbd "M-/") 'consult-line)

(define-key evil-normal-state-map (kbd "C-p") 'my-consult-fd-project)

;; (define-key evil-normal-state-map (kbd "C-p") 'consult-fd)

(defun my-consult-grep-dir (&optional dir)
  "Execute ripgrep in the current directory, using the selection if available.
DIR is the directory."
  (interactive)
  (require 'consult)
  (progn
    (let ((selection (when (use-region-p)
                       (buffer-substring-no-properties (region-beginning)
                                                       (region-end))))
          (consult-ripgrep-args (when consult-ripgrep-args
                                  (concat consult-ripgrep-args
                                          " --threads "
                                          (number-to-string (num-processors))))))
      (when selection
        (save-excursion
          (deactivate-mark)))

      (my-save-all-buffers)
      (when (fboundp 'consult-ripgrep)
        (consult-ripgrep (or dir (buffer-cwd)) selection)))))

(defun my-consult-grep-project ()
  "Run `consult-fd` in the root directory of the current project."
  (interactive)
  (require 'consult)
  (let* ((project (project-current nil "."))
         (project-root (when (and project (fboundp 'project-root))
                         (project-root project))))
    (my-consult-grep-dir (or project-root (buffer-cwd)))))

;; (evil-define-key 'normal 'global (kbd "gR") 'my-consult-grep-project)
;; (evil-define-key 'normal 'global (kbd "gr") 'my-consult-grep-dir)
(evil-define-key 'normal 'global (kbd "<leader>gR") 'my-consult-grep-dir)
(evil-define-key 'normal 'global (kbd "<leader>gr") 'my-consult-grep-project)

;;; evil org

;; TODO put this back?
;; (when (fboundp 'indentnav-backward-to-empty-line)
;;   (evil-define-key 'normal 'local (kbd "{") 'indentnav-backward-to-empty-line))
;; (when (fboundp 'indentnav-forward-to-empty-line)
;;   (evil-define-key 'normal 'local (kbd "}") 'indentnav-forward-to-empty-line))

;; Disable org cycle
;; TODO put this back
;; (evil-define-key 'normal 'local (kbd "<tab>") 'ignore)

(defun my-evil-delete-to-heading-star ()
  "Delete everything before the cursor except the first '*' and space.

In Org mode with Evil insert state, this function deletes everything before the
cursor except the first Org heading star (`*`), the space following it, and any
word after the space that contains at least two uppercase characters."
  (interactive)
  (let ((cur-point (point)))
    (save-excursion
      (if (re-search-backward
           "^\\(*+\\)\\( +\\)\\([A-Z]\\{2,\\}\\)?\\( +\\)?"
           (line-beginning-position) t)
          (let ((delete-start (match-end 0)))
            (delete-region delete-start cur-point))
        (evil-delete-back-to-indentation)))))

(with-eval-after-load 'org
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-set-initial-state 'org-agenda-mode 'normal)

  (evil-define-key 'normal 'global (kbd "<leader>oa") 'org-agenda)
  (evil-define-key 'normal 'global (kbd "<leader>oc") 'org-capture)
  (evil-define-key 'normal 'global (kbd "<leader>os") 'org-schedule)
  (evil-define-key 'normal 'global (kbd "<leader>Z") 'my-org-agenda-switch-to-todos)

  (evil-define-key 'motion org-agenda-keymap (kbd "/") 'org-agenda-filter)
  (evil-define-key '(normal motion) org-agenda-keymap (kbd "RET")
    'my-org-agenda-goto-in-same-window)
  (evil-define-key '(normal) org-agenda-keymap (kbd "<leader>cd") 'org-agenda-todo)
  (evil-define-key '(normal) org-agenda-keymap (kbd "<tab>") 'org-agenda-set-tags)
  (evil-define-key '(normal motion) org-agenda-keymap (kbd "<leader>oo") 'org-agenda-set-tags)
  ;; (evil-define-key '(normal motion) org-agenda-keymap (kbd "<tab>") 'org-agenda-goto)
  (evil-define-key '(normal motion) org-agenda-keymap (kbd "C-l")
    #'(lambda()
        (interactive)
        (when (fboundp 'org-agenda-filter-remove-all)
          (org-agenda-filter-remove-all))

        (evilbuffer-clear-highlights)))

  (evil-define-key 'insert org-mode-map (kbd "C-u") 'my-evil-delete-to-heading-star)

  ;; Equivalent to C-c C-q
  (evil-define-key 'normal org-mode-map (kbd "<leader>oo") 'org-set-tags-command)
  (evil-define-key 'normal org-mode-map (kbd "<leader>xx") 'org-babel-execute-maybe)
  (evil-define-key 'normal org-mode-map (kbd "<leader>cd") 'my-org-todo-and-toggle)
  ;; (evil-define-key 'normal org-mode-map (kbd "<leader>xx") 'org-edit-src-code)
  ;; (evil-define-key 'normal org-src-mode-map (kbd "<leader>xx") 'org-edit-src-exit)
  )

;;; cape: complete before point

;;------------------------------------------------------------------------------
;; CAPE COMPLETE BEFORE POINT
;;------------------------------------------------------------------------------
;; TODO lightemacs?

;; This has been changed
(defun my-cape--dabbrev-bounds ()
  "Return bounds of abbreviation using only text before point.

This variant restricts the abbreviation bounds to the symbol
fragment preceding point.  It identifies the start of the current
word (a sequence of characters matching `dabbrev-abbrev-char-regexp`)
and uses point as the end boundary.  This ensures that dabbrev
completions are based solely on the fragment already typed, not
on text following the cursor."
  (unless (boundp 'dabbrev-abbrev-char-regexp)
    (require 'dabbrev))
  (let ((re (or dabbrev-abbrev-char-regexp "\\sw\\|\\s_"))
        (limit (minibuffer-prompt-end)))
    (when (or (looking-at re)
              (and (> (point) limit)
                   (save-excursion (forward-char -1) (looking-at re))))
      (cons (save-excursion
              (while (and (> (point) limit)
                          (save-excursion (forward-char -1) (looking-at re)))
                (forward-char -1))
              (when dabbrev-abbrev-skip-leading-regexp
                (while (looking-at dabbrev-abbrev-skip-leading-regexp)
                  (forward-char 1)))
              (point))
            ;; This is the part that I modified
            (point)

            ;; TODO contrib to cape?
            ;;
            ;; This is the part that I removed
            ;;
            ;; The following code scans forward from point over all characters
            ;; matching re (typically word or symbol characters) to determine
            ;; the end of the current abbreviation, returning it as a point
            ;; value while leaving the cursor in place due to save-excursion;
            ;; in the original cape-dabbrev, this ensures that the completion
            ;; can replace the entire symbol under the cursor, including any
            ;; text after point, whereas fixing END to (point) restricts
            ;; completions to only the fragment typed before the cursor.
            ;;
            ;; It is not useless. it is useful because it allows cape-dabbrev
            ;; (and dabbrev-like completions) to identify the full extent of the
            ;; symbol at point, so that when a completion is chosen, it can
            ;; replace the entire word under the cursor rather than just the
            ;; part typed so far; this behavior is important in typical Emacs
            ;; completion, where completing a symbol mid-word should overwrite
            ;; the rest of the word, but if the goal is to restrict completions
            ;; to only the text before the cursor, scanning forward becomes
            ;; unnecessary and can be replaced by simply using (point) as the
            ;; end.
            ;;
            ;; (save-excursion
            ;;   (while (looking-at re)
            ;;     (forward-char 1))
            ;;   (point))
            ))))

(with-eval-after-load 'cape
  (advice-add 'cape--dabbrev-bounds :override #'my-cape--dabbrev-bounds))

;; (defun my-cape-dabbrev-backwards ()
;;   "Cape-dabbrev backwards."
;;   (interactive)
;;   (let ((dabbrev-backward-only t))
;;     (when (fboundp 'cape-dabbrev)
;;       (cape-dabbrev t))))
;;
;; (evil-define-key 'insert 'global (kbd "C-p") 'my-cape-dabbrev-backwards)

(defun my-cape-dabbrev ()
  "Cape dabbrev."
  (interactive)
  (if (minibufferp)
      (let (;; (dabbrev-check-other-buffers t)
            ;; (dabbrev-case-replace t)
            ;; (dabbrev-case-fold-search t)
            ;; (dabbrev-case-distinction nil)
            (dabbrev-check-all-buffers t))
        (call-interactively 'cape-dabbrev))
    (call-interactively 'cape-dabbrev)))

(evil-define-key 'insert 'global (kbd "C-p") 'my-cape-dabbrev)
(evil-define-key 'insert 'global (kbd "C-n") 'my-cape-dabbrev)

;;; cape: evil

(defun my-minibuffer-setup-dabbrev-evil ()
  "Bind `C-p' and `C-n' to dabbrev completion in minibuffer using evil."
  (evil-define-key 'insert 'local
    (kbd "C-p") 'cape-dabbrev
    (kbd "C-n") 'cape-dabbrev)

  ;; (when (and (boundp 'completion-at-point-functions)
  ;;            (listp completion-at-point-functions))
  ;;   (add-hook 'completion-at-point-functions #'cape-dabbrev nil t))
  )

;; TODO lightemacs?
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-dabbrev-evil)

(with-eval-after-load 'cape
  (evil-define-key 'insert 'global (kbd "C-x C-f") 'cape-file))

(with-eval-after-load 'cape
  ;; Emulate Vim's C-x C-f

  ;; Use symbol occurence to sort candidates
  ;;
  ;; To achieve this, you must rely on the natural search order of dabbrev
  ;; (which scans from the cursor outwards) and prevent corfu from re-sorting
  ;; the candidates. By default, corfu sorts candidates by length or
  ;; alphabetically, which overrides the proximity-based order returned by the
  ;; backend.
  ;;
  ;; You can use cape-capf-properties to strictly disable sorting for
  ;; cape-dabbrev while keeping the default sorting for other backends.
  (if (fboundp 'cape-capf-properties)
      (add-to-list 'completion-at-point-functions
                   (cape-capf-properties 'cape-dabbrev :sort nil))
    (error "Undeclared: cape-capf-properties")))

;;; evil corfu

;; TODO lightemacs?
(evil-define-key 'insert 'global (kbd "C-x C-f") 'cape-file)

(with-eval-after-load 'corfu
  (with-eval-after-load "evil"
    (evil-define-key 'insert 'global (kbd "C-SPC") 'completion-at-point)

    (unless (display-graphic-p)
      (define-key key-translation-map (kbd "C-@") (kbd "C-SPC")))

    ;; Setup auto complete
    ;; (defun pkg-corfu-setup-auto-complete ()
    ;;   "Setup Corfu auto complete."
    ;;   ;; (corfu-mode)
    ;;   (evil-define-key 'insert 'local (kbd "C-SPC")
    ;;     (if (my-code-checker-allowed-p) #'completion-at-point #'ignore)))
    ;; (add-hook-text-editing-modes #'pkg-corfu-setup-auto-complete)
    ;; (dolist (hook '(inferior-python-mode-hook
    ;;                 lisp-interaction-mode-hook))
    ;;   (add-hook hook #'pkg-corfu-setup-auto-complete))
    ;; (define-key minibuffer-mode-map (kbd "C-SPC") 'completion-at-point)

    ))

;;; evil inferior mode (Python)

(defun evilinferior-run-python ()
  "Run Python."
  (interactive)
  (run-python)
  (let* ((python-buffer-name "*Python*")
         (python-buffer (get-buffer python-buffer-name)))
    (when python-buffer
      (switch-to-buffer python-buffer)
      (with-current-buffer python-buffer
        (evil-insert 1)))))

(defun evilinferior-mode-clear ()
  "Equivalent to clear the screen in a terminal."
  (interactive)
  ;; (evil-insert-state)
  (recenter 0))

(defun evilinferior-setup ()
  "Set up keybinding for recentering in inferior modes."
  ;; (evil-define-key 'normal 'local (kbd "M-k") 'comint-previous-input)
  ;; (evil-define-key 'normal 'local (kbd "M-j") 'comint-next-input)
  (evil-define-key 'normal 'local (kbd "C-l") 'evilinferior-mode-clear)
  (evil-define-key 'insert 'local (kbd "C-l") 'evilinferior-mode-clear))

;; Python
(add-hook 'inferior-python-mode-hook 'evilinferior-setup)
(add-hook 'ielm-mode-hook 'evilinferior-setup)

;; M-DEL and C-<backspace> in Emacs modify the kill ring. The following
;; makes both key mappings use the Evil version that does not modify the
;; kill ring.
(global-set-key (kbd "C-<backspace>") #'evil-delete-backward-word)
(global-set-key (kbd "M-DEL") #'evil-delete-backward-word)

(evil-define-key 'normal 'global (kbd "<leader>ep") 'evilinferior-run-python)
(evil-define-key 'normal 'global (kbd "<leader>el") 'ielm)

;;; evil intercept (TODO replace with global?)

(defvar my-intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(define-minor-mode my-intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t
  :group 'my-intercept-mode)

(dolist (state '(normal visual insert))
  (evil-make-intercept-map
   (evil-get-auxiliary-keymap my-intercept-mode-map state t t)
   state))

(evil-define-key '(normal insert visual) my-intercept-mode-map
  ;; (kbd "M-RET") 'toggle-term-tmux
  ;; (kbd "M-<enter>") 'toggle-term-tmux
  ;; (kbd "M-<return>") 'toggle-term-tmux
  (kbd "M-x") 'execute-extended-command

  (kbd "M-RET") 'shell-pop
  (kbd "M-<enter>") 'shell-pop
  (kbd "M-<return>") 'shell-pop
  (kbd "M-o") 'my-previous-interesting-buffer
  (kbd "M-i") 'my-next-interesting-buffer
  ;; (kbd "M-=") 'global-text-scale-adjust

  ;; handled by le-default-keybindings.el
  ;; (kbd "C--") 'text-scale-decrease
  ;; (kbd "C-+") 'text-scale-increase

  (kbd "C-S-k") 'my-tab-bar-move-tab-backward
  (kbd "C-S-j") 'my-tab-bar-move-tab
  (kbd "C-k") 'my-tab-previous
  (kbd "C-j") 'my-tab-next)

(my-intercept-mode 1)

;; Doesn't work
;; (require 'le-default-keybindings)  ; `lightemacs-keymap-override-map'
;; (unless noninteractive
;;   (dolist (binding '(("C--" . text-scale-decrease)
;;                      ("C-+" . text-scale-increase)
;;                      ("M-RET" . toggle-term-tmux)
;;                      ("M-<enter>" . toggle-term-tmux)
;;                      ("M-<return>" . toggle-term-tmux)
;;                      ("M-o" . my-previous-interesting-buffer)
;;                      ("M-i" . my-next-interesting-buffer)
;;                      ("C-S-k" . my-tab-bar-move-tab-backward)
;;                      ("C-S-j" . my-tab-bar-move-tab)
;;                      ("C-k" . my-tab-previous)
;;                      ("C-j" . my-tab-next)))
;;     (let ((key (car binding))
;;           (cmd (cdr binding)))
;;       (evil-define-key* 'normal lightemacs-keymap-override-map (kbd key) cmd))))

;;; Move region

(defvar move-region-skip-invisible t)

(defun move-region (n)
  "Move the current region up or down by N lines."
  ;; (interactive "r\np")
  (interactive)
  (when (use-region-p)
    ;; (region-to-linewise-region)
    (let ((start (region-beginning))
          (end (region-end)))
      ;; Exit visual state explicitly before performing the operation. If we
      ;; do not do this, Evil will automatically restore the original point
      ;; and mark after the command finishes, overriding any cursor movement
      ;; or region updates made within the function.
      (when (fboundp 'evil-exit-visual-state)
        (evil-exit-visual-state))

      (let ((line-text (delete-and-extract-region start end)))
        (if move-region-skip-invisible
            (forward-visible-line n)
          (forward-line n))
        (let ((start (point))
              ;; (length-text (length line-text))
              )
          (insert line-text)
          ;; (backward-char)

          (setq end (point))
          (set-mark end)

          (goto-char start)

          ;; Ensure that the region remains active after the command finishes.
          ;; In Emacs, the variable `deactivate-mark` controls whether the
          ;; region is deactivated automatically at the end of a command. By
          ;; default, it is set to `t` by many interactive commands, which
          ;; causes the region to be cleared after execution. Setting
          ;; `deactivate-mark` to `nil` prevents this automatic deactivation.
          ;; This is necessary even when `activate-mark` is called explicitly,
          ;; because without setting `deactivate-mark` to `nil`, the region may
          ;; appear briefly and then vanish immediately after the function
          ;; returns, giving the false impression that the selection failed.
          (setq deactivate-mark nil)
          (activate-mark))))))

(defun move-region-up (&rest _)
  "Move the current line up by N lines."
  (interactive)
  (move-region -1))

(defun move-region-down (&rest _)
  "Move the current line down by N lines."
  (interactive)
  (move-region 1))

(define-key evil-visual-state-map (kbd "M-j") 'move-region-down)
(define-key evil-visual-state-map (kbd "M-k") 'move-region-up)

;;; check parens no jump

;; TODO: Part of smooth cursor?
;;-----------------------------
(defun my-check-parens-no-jump (&optional no-error)
  "Check for unbalanced parentheses in the current buffer.
This function scans the entire buffer for balanced parentheses. If an imbalance
is found, it either triggers a user error or simply shows a message, depending
on the optional argument NO-ERROR.
If NO-ERROR is non-nil, it only displays a message about the unmatched
parenthesis or quote, including the line number, column, and file name, but does
not raise an error.
If NO-ERROR is nil or omitted, a user error is raised with the same details, and
the function returns nil.
If the parentheses are balanced, the function returns t."
  (interactive)
  (condition-case data
      (progn
        (scan-sexps (point-min) (point-max))
        ;; Return t
        t)
    (scan-error
     (let* ((char (nth 2 data))
            (msg
             (format "Unmatched bracket or quote in line %s, column %s in %s"
                     (line-number-at-pos char)
                     (let ((column (save-excursion (goto-char char)
                                                   (current-column))))
                       (when (integerp column)
                         (1+ column)))
                     (let ((file-name (buffer-file-name (buffer-base-buffer))))
                       (when file-name
                         (abbreviate-file-name file-name))))))

       (if no-error
           (message msg)
         (user-error msg))
       ;; Return nil
       nil))))

;; Use va( instead
(defun my-elisp-mode-select-sexp ()
  "Select the sexp at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (if bounds
        (progn
          (goto-char (car bounds))
          (push-mark (point) t t)
          (goto-char (cdr bounds))
          (activate-mark))
      (message "No sexp at point."))))

;; apheleia does this
(with-eval-after-load 'lisp
  (advice-add 'check-parens :override #'my-check-parens-no-jump))

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (with-eval-after-load 'evil
                (evil-define-key 'normal 'global (kbd "gV")
                  #'my-elisp-mode-select-sexp)

                (add-hook 'evil-insert-state-exit-hook
                          #'(lambda() (when (and
                                             (fboundp 'evil-insert-state-p)
                                             (evil-insert-state-p))
                                        (my-check-parens-no-jump t)))
                          nil t))
              (add-hook 'after-save-hook #'my-check-parens-no-jump -99 t)))

;;; markdown mode

(with-eval-after-load 'markdown-mode
  ;; (evil-define-key 'normal markdown-mode-map (kbd "TAB") #'ignore)
  ;; (evil-define-key 'normal markdown-mode-map (kbd "<tab>") #'ignore)
  (evil-define-key 'normal markdown-mode-map (kbd "C-c C-c") 'markdown-edit-code-block))

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

  ;; visual-line-mode is slow?
  ;; (visual-line-mode 1)

  (let ((inhibit-message t))
    (toggle-truncate-lines 0))

  ;; (setq-local evil-auto-indent nil)

  ;; (evil-define-key 'normal 'local (kbd "TAB") 'ignore)
  ;; (evil-define-key 'normal 'local (kbd "<tab>") 'ignore)

  ;; DONE already added to markdown-mode (TODO not released yet)
  ;; Make / a punctuation (for, for example, strings like group/package) I
  ;; sometimes have `var' in Yaml files
  ;; (set-syntax-table (copy-syntax-table))
  (modify-syntax-entry ?' ".")
  (modify-syntax-entry ?* ".") ; Things like *word* (italic)
  (modify-syntax-entry ?> ".")
  (modify-syntax-entry ?< ".")
  ;; (modify-syntax-entry ?- ".")  ;; Annoying for editing elisp README.md
  (modify-syntax-entry ?_ ".")

  ;; RET can sometimes check and uncheck boxes. This is not something I want.
  ;; NOTE: THIS IS WRONG. DO NOT ACTIVATE.
  ;; (evil-define-key 'normal 'local (kbd "RET") 'ignore)

  ;; TODO reenable?
  ;; (when (fboundp 'indentnav-backward-to-empty-line)
  ;;   (evil-define-key 'normal 'local (kbd "{") 'indentnav-backward-to-empty-line))
  ;; (when (fboundp 'indentnav-forward-to-empty-line)
  ;;   (evil-define-key 'normal 'local (kbd "}") 'indentnav-forward-to-empty-line))

  ;; (when (fboundp 'indentnav-backward-to-empty-line)
  ;;   (evil-define-key 'normal 'local (kbd "{") 'evil-backward-paragraph))
  ;;
  ;; (when (fboundp 'indentnav-forward-to-empty-line)
  ;;   (evil-define-key 'normal 'local (kbd "}") 'evil-forward-paragraph))

  )

(add-hook 'markdown-mode-hook #'my-setup-markdown-mode)
(add-hook 'markdown-ts-mode-hook #'my-setup-markdown-mode)
(add-hook 'gfm-mode-hook #'my-setup-markdown-mode)

;;; Code that replaces evil visualstar

(defun le-evil--search-regexp (regex direction count)
  "Search for REGEX.
Inspired from `evil-ex-start-word-search'.
The search matches the COUNT occurrence of the word. The DIRECTION argument
should be either `forward' or `backward', determining the search direction."
  (setq evil-ex-search-count count
        evil-ex-search-direction direction
        evil-ex-search-pattern
        (let (evil-ex-search-vim-style-regexp)
          (evil-ex-make-search-pattern regex))
        evil-ex-search-offset nil
        evil-ex-last-was-search t)
  ;; Update search history unless this pattern equals the previous pattern
  (unless (equal regex (car evil-ex-search-history))
    (push regex evil-ex-search-history))
  (evil-push-search-history regex (eq direction 'forward))
  (evil-ex-delete-hl 'evil-ex-search)
  (evil-ex-search-next count))

(defun evilbuffer-search-symbol (&optional direction)
  "Search for the symbol at point using Evil search.
The DIRECTION argument can be either \='forward' or \='backward, determining the
search direction (default: \='forward)."
  (interactive)
  (let* ((visual-p (evil-visual-state-p))
         (count 1)
         (direction (or direction 'forward))
         (bounds (unless visual-p
                   (bounds-of-thing-at-point 'symbol)))
         ;; Fetch boundaries before evil-exit-visual-state deactivates the region
         (bnd-start (if visual-p
                        (region-beginning)
                      (car bounds)))
         (bnd-end (if visual-p
                      (region-end)
                    (cdr bounds)))
         (text (if visual-p
                   (let ((selection (buffer-substring-no-properties
                                     bnd-start
                                     bnd-end)))
                     (evil-exit-visual-state)
                     selection)
                 (thing-at-point 'symbol t)))
         (current-pattern (and evil-ex-search-pattern
                               (car evil-ex-search-pattern)))
         (regex (when text
                  (if visual-p
                      (regexp-quote text)
                    (concat "\\_<" (regexp-quote text) "\\_>")))))
    (when regex
      (if (and current-pattern
               (string= regex current-pattern))
          ;; Instead of calling `le-evil--search-regexp' for the same keyword,
          ;; just highlight the current keyword
          (condition-case err
              (progn
                ;; Both `save-window-excursion' and `save-excursion' are used
                ;; here to ensure that performing the search does not move the
                ;; cursor or change the visible portion of the buffer
                ;; (`window-start'). This allows Evil's search functions to
                ;; update highlights and internal search state while leaving the
                ;; user's point and window view completely unchanged.
                (save-window-excursion
                  (save-excursion
                    ;; Use evil-ex-search-next instead of the interactive evil-ex-search
                    (evil-ex-search-next count))))
            (search-failed
             nil))
        ;; Both `save-window-excursion' and `save-excursion' are used here to
        ;; ensure that performing the search does not move the cursor or change
        ;; the visible portion of the buffer (`window-start'). This allows
        ;; Evil's search functions to update highlights and internal search
        ;; state while leaving the user's point and window view completely
        ;; unchanged.
        (save-window-excursion
          (save-excursion
            ;; Search from the beginning or from the end
            (if (eq direction 'forward)
                (goto-char (point-min))
              (goto-char (point-max)))

            (le-evil--search-regexp regex
                                    direction
                                    count))))

      ;; When `le-evil--search-regexp' executes, Evil updates
      ;; `evil-ex-search-match-beg' and `evil-ex-search-match-end' to the
      ;; distant location it just found. You have to manually overwrite those
      ;; internal variables with your original coordinates so the highlight
      ;; applies to the text currently under your point.
      (when (and bnd-start bnd-end)
        (setq evil-ex-search-match-beg bnd-start
              evil-ex-search-match-end bnd-end)
        (when (boundp 'evil-ex-search-pattern)
          (evil-ex-search-activate-highlight evil-ex-search-pattern))))))

(defun le-evil--search-symbol-backwards ()
  "Search for the symbol at point using Evil search.
The DIRECTION argument can be either `forward' or `backward', determining the
search direction (default: \='forward)."
  (interactive)
  (evilbuffer-search-symbol 'backward))

;; Key mappings
(define-key evil-normal-state-map (kbd "*") 'evilbuffer-search-symbol)
(define-key evil-visual-state-map (kbd "*") 'evilbuffer-search-symbol)
(define-key evil-normal-state-map (kbd "#") 'le-evil--search-symbol-backwards)
(define-key evil-visual-state-map (kbd "#") 'le-evil--search-symbol-backwards)
(define-key evil-visual-state-map (kbd "?") 'le-evil--search-symbol-backwards)

;;; vterm settings

;; TODO lightemacs?
(defun my-setup-vterm ()
  "Better evil integration with `vterm'."
  ;; https://www.reddit.com/r/emacs/comments/xyo2fo/orgmode_vterm_tmux/
  ;; With the first line, you can use a binding like `M-SPC ESC` (`M-SPC`
  ;; being the default alt-leader key) to switch the vterm buffer to evil
  ;; normal state. Most of the time I don't use it, and simply interact with
  ;; the vterm the same exact way I do with any other terminal.
  (with-eval-after-load 'evil-collection
    (when (and (not (bound-and-true-p evil-collection-vterm-send-escape-to-vterm-p))
               (fboundp 'evil-collection-vterm-toggle-send-escape))
      (let ((inhibit-message t))
        ;; TODO add shut-up back?
        (evil-collection-vterm-toggle-send-escape))))

  (setq-local line-number-mode nil)
  (setq-local column-number-mode nil)
  (setq-local cursor-type 'bar)
  (setq mode-line-format nil)

  (when-let* ((proc (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag proc nil)))

(add-to-list 'evil-emacs-state-modes 'vterm-mode)

(add-hook 'vterm-mode-hook 'my-setup-vterm)

(setq vterm-clear-scrollback-when-clearing t)

(setq vterm-max-scrollback 100)
;; (setq vterm-set-bold-hightbright t)
;; (setq vterm-disable-bold t)
;; (setq vterm-copy-exclude-prompt t)
;; "C-x" "C-c" "C-g"
(setq vterm-keymap-exceptions '("M-RET" "C-x" "C-c" "M-x" "M-o" "C-y" "M-y"))
(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")

(defun my-vterm--send-Alt-Shift-H ()
  "Send Alt Shift H to vterm."
  (interactive)
  (when (fboundp 'vterm-send-key)
    (vterm-send-key (kbd "H") t t)))

(defun my-vterm--send-Alt-Shift-L ()
  "Send Alt Shift L to vterm."
  (interactive)
  (when (fboundp 'vterm-send-key)
    (vterm-send-key (kbd "L") t t)))

;;; mode line

(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))
(setq mode-line-percent-position nil)


;;; flymake fixes

;; TODO: Should this fail silently? (Patch Emacs)
(defun my-flymake-proc-legacy-safe-advice (orig-fun &rest args)
  "Call `flymake-proc-legacy-flymake' safely, ignoring missing init function.

ORIG-FUN is the original `flymake-proc-legacy-flymake` function.
ARGS are the arguments passed to ORIG-FUN.

If the error message contains \"find a suitable init function\", it is
ignored and logged as a warning. All other errors are re-raised."
  (condition-case err
      (apply orig-fun args)
    ((error)
     (let ((error-message (error-message-string err)))
       (if (string-match-p "find a suitable init function"
                           error-message)
           (let ((inhibit-message t))
             (message "[WARNING] Flymake: %s: %s"
                      buffer-file-name
                      error-message))
         (signal (car err) (cdr err)))))))

(with-eval-after-load 'flymake-proc
  (advice-add 'flymake-proc-legacy-flymake :around
              #'my-flymake-proc-legacy-safe-advice))

;;; markdown toc

;;; Markdown

(defun my-markdown-toc-gen-if-present ()
  "Gen table of contents if present."
  (when (and (fboundp 'markdown-toc--toc-already-present-p)
             (fboundp 'markdown-toc-generate-toc)
             (funcall 'markdown-toc--toc-already-present-p))
    ;; atomic-change-group in Emacs creates a temporary “transaction” for buffer
    ;; modifications: all changes made inside the group are treated as a single,
    ;; atomic operation for undo purposes. This means that if the group
    ;; completes successfully, all modifications are merged into one undo step,
    ;; so pressing undo will revert everything in the group at once instead of
    ;; step by step. If an error occurs inside the group, the changes are
    ;; automatically rolled back, leaving the buffer unchanged. It also ensures
    ;; that point and mark positions are preserved unless explicitly modified,
    ;; which is why it’s useful for functions like markdown-toc-generate-toc
    ;; where you want the buffer updated without losing your cursor location.
    (atomic-change-group
      (funcall 'markdown-toc-generate-toc))))

(defun my-setup-markdown-toc ()
  "Setup the markdown-toc package."
  (add-hook 'before-save-hook #'my-markdown-toc-gen-if-present 99 t))

(when (fboundp 'my-setup-markdown-toc)
  (add-hook 'gfm-mode-hook #'my-setup-markdown-toc)
  (add-hook 'markdown-ts-mode-hook #'my-setup-markdown-toc)
  (add-hook 'markdown-mode-hook #'my-setup-markdown-toc))


;;; Silence C-f

(evil-define-command evilcursor-scroll-page-down (count)
  "Scroll the window COUNT pages downwards.
This prevents `evil-scroll-page-down' from displaying: End of buffer."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (ignore-errors (evil-scroll-page-down count)))

(evil-define-key 'motion 'global (kbd "C-f") #'evilcursor-scroll-page-down)

;;; Backward char

(evil-define-motion evilcursor-backward-char (count &optional crosslines noerror)
  "Move cursor to the left by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the beginning
of the line or the buffer; just return nil."
  :type exclusive
  (interactive "<c>" (list evil-cross-lines
                           (evil-kbd-macro-suppress-motion-error)))
  (ignore-errors (evil-backward-char count crosslines noerror)))

(evil-define-motion evilcursor-forward-char (count &optional crosslines noerror)
  "Move cursor to the right by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the end
of the line or the buffer; just return nil."
  :type exclusive
  (interactive "<c>" (list evil-cross-lines
                           (evil-kbd-macro-suppress-motion-error)))
  (ignore-errors (evil-forward-char count crosslines noerror)))

(evil-define-key 'motion 'global (kbd "l") #'evilcursor-forward-char)
(evil-define-key 'motion 'global (kbd "h") #'evilcursor-backward-char)

;;; Smart previous/next line

(defun evilcursor--get-category-at-point ()
  "Get the category at point."
  (save-excursion
    (beginning-of-line)
    (when-let* ((prop (get-text-property
                       (point)
                       'category)))
      (symbol-name prop))))

;; TODO should this be part of Emacs ? PATCH
(defun evilcursor--after-vertical-movement ()
  "Run this after a vertical movement."
  (when (invisible-p (line-beginning-position))
    (vertical-motion 0)
    ;; TODO Which is faster: end-of-line or goto-char?
    ;; (goto-char (line-end-position))
    (end-of-line)))

(evil-define-motion evilcursor-next-visual-line (count)
  "Move the cursor COUNT screen lines down."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count 1)))
  (evilcursor--after-vertical-movement))

(evil-define-motion evilcursor-previous-visual-line (count)
  "Move the cursor COUNT screen lines up."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (- (or count 1))))
  (evilcursor--after-vertical-movement))

(defun evilcursor-forward-line (n)
  "Move N lines forward (backward if N is negative).
More accurate than `evil-next-line' and `evil-previous-line' when lines are not
truncated."
  (interactive)
  (unless n
    (setq n 1))
  (cond
   ((minibufferp)
    ;; ignore-errors fixes issues with icomplete
    (if (> (or n 1) 0)
        (ignore-errors
          (next-line-or-history-element))
      (ignore-errors
        (previous-line-or-history-element))))

   ;; Not Minibuffer
   (t
    (let* ((count (abs n))
           (forwardp (> (or n 1) 0))
           (line-number-type (when (bound-and-true-p display-line-numbers-type)
                               display-line-numbers-type))

           ;; (evil-respect-visual-line-mode nil)

           ;; TODO enable?
           ;; (evil-track-eol nil)
           ;; (track-eol nil)
           ;; (line-move-ignore-invisible t)

           (func-change-line (if forwardp
                                 #'evil-next-line
                               #'evil-previous-line))
           (func-change-line-visual (if forwardp
                                        #'evilcursor-next-visual-line
                                      #'evilcursor-previous-visual-line)))
      (cond
       ;; TODO patch embark
       ((eq major-mode 'embark-collect-mode)
        (let ((previous-cat (evilcursor--get-category-at-point))
              (start-point (point))
              (current-cat nil)
              (next-cat nil))
          (funcall func-change-line count)
          (setq current-cat (evilcursor--get-category-at-point))

          (unless (= start-point (point))
            (setq next-cat (save-excursion
                             (funcall func-change-line count)
                             (evilcursor--get-category-at-point)))

            (when (and previous-cat
                       current-cat
                       next-cat
                       (string= current-cat "embark-collect-group-button")
                       (not (string= previous-cat "embark-collect-group-button"))
                       (not (string= next-cat "embark-collect-group-button")))
              (funcall func-change-line count)))))

       ;; ((and (or (eq major-mode 'org-mode)
       ;;           (derived-mode-p 'org-mode))
       ;;       (bound-and-true-p org-indent-mode))
       ;;
       ;;  ;; (condition-case nil
       ;;  ;;     ;; I added org-mode here because the indentation after a
       ;;  ;;     ;; header is not taken into consideration with a simple
       ;;  ;;     ;; next/previous line
       ;;  ;;     (progn
       ;;  ;;       (funcall func-change-line-visual count)
       ;;  ;;       t)
       ;;  ;;   (error nil))
       ;;
       ;;  ;; (when (condition-case nil
       ;;  ;;           ;; I added org-mode here because the indentation after a
       ;;  ;;           ;; header is not taken into consideration with a simple
       ;;  ;;           ;; next/previous line
       ;;  ;;           (progn
       ;;  ;;             (funcall func-change-line-visual count)
       ;;  ;;             t)
       ;;  ;;         (error nil))
       ;;  ;;   ;; When the cursor is placed inside the content of a folded heading,
       ;;  ;;   ;; it should automatically move to the end of the heading instead of
       ;;  ;;   ;; remaining within the hidden content, as this can be inconvenient
       ;;  ;;   ;; when the heading is expanded.
       ;;  ;;   (when (and (fboundp 'org-on-heading-p)
       ;;  ;;              (org-on-heading-p t)
       ;;  ;;              (save-excursion
       ;;  ;;                (condition-case nil
       ;;  ;;                    (progn
       ;;  ;;                      (outline-back-to-heading)
       ;;  ;;                      (end-of-line)
       ;;  ;;                      (outline-invisible-p (point)))
       ;;  ;;                  (error nil))))
       ;;  ;;     (outline-back-to-heading)
       ;;  ;;     (end-of-line)))
       ;;  )

       ((eq line-number-type 'visual)
        (funcall func-change-line-visual count)

        ;; (if (or truncate-lines
        ;;         (= count 1))
        ;;     ;; This speeds-up scrolling because it does not take into
        ;;     ;; consideration visual things
        ;;     (progn
        ;;       ;; (let ((line-move-visual t))
        ;;       ;;   (if (> n 0)
        ;;       ;;       (progn
        ;;       ;;         (call-interactively 'next-line count)
        ;;       ;;         (when (get-char-property (point) 'invisible)
        ;;       ;;           (let ((prev-visible (previous-single-char-property-change (point) 'invisible nil (point-min))))
        ;;       ;;             (when prev-visible
        ;;       ;;               (goto-char prev-visible))))
        ;;       ;;         )
        ;;       ;;     (call-interactively 'previous-line count)))
        ;;
        ;;       ;; Fixes kirigami/outline/org...
        ;;       (funcall func-change-line-visual count)
        ;;
        ;;
        ;;       ;; (funcall func-change-line count)
        ;;       )
        ;;   (funcall func-change-line-visual count))
        )

       ((eq line-number-type 'relative)
        (funcall func-change-line count)

        ;; TODO patch?
        ;; Long lines do not wrap; they disappear off the right edge of the
        ;; window. Because there is no line wrapping, one logical line
        ;; corresponds exactly to one vertical visual line on the screen. Using
        ;; func-change-line (logical movement) here is faster and avoids the
        ;; overhead of calculating visual screen lines.
        ;; (if (or truncate-lines (truncated-partial-width-window-p))
        ;;     (funcall func-change-line count)
        ;;   ;; When truncate-lines is nil: Long lines wrap to the next visual
        ;;   ;; line. To navigate these wrapped lines intuitively, you must use
        ;;   ;; func-change-line-visual. If you used logical movement here,
        ;;   ;; pressing down once could jump the cursor past several lines of
        ;;   ;; wrapped text.
        ;;   (funcall func-change-line-visual count))

        ;;
        ;; (funcall (if (> count 1)
        ;;              func-change-line
        ;;            func-change-line-visual)
        ;;          count)
        ;; (let (;; Do not ignore invisible when moving more than one line because
        ;;       ;; the line numbers displayed by `display-line-numbers-mode' when
        ;;       ;; display-line-numbers-type is relative doesn't ignore invisible
        ;;       ;; lines.
        ;;       (line-move-visual nil)
        ;;       ;; (line-move-ignore-invisible (when (< count 2) t))
        ;;       )
        ;;   (if truncate-lines
        ;;       (funcall func-change-line count)
        ;;     (funcall (if (> count 1)
        ;;                  func-change-line
        ;;                func-change-line-visual)
        ;;              count)))
        )

       ;; Absolute
       ;; ((eq line-number-type t)
       ;;  (let ((line-move-visual nil))
       ;;    ;; Force logical line movement to match absolute line numbers.
       ;;    ;; Emacs natively handles narrowing bounds, so no widen/save-restriction is needed.
       ;;    (funcall func-change-line count)))
       ((eq line-number-type t)
        ;; TODO doesn't work when count > 1
        (funcall func-change-line count)

        ;; (let ((start-line (line-number-at-pos))
        ;;       (line-move-ignore-invisible t)
        ;;       (line-move-visual nil)
        ;;       (evil-respect-visual-line-mode nil)
        ;;       (track-eol nil)
        ;;       (evil-track-eol nil))
        ;;   ;; TODO Patch to Emacs / evil?
        ;;   (if (buffer-narrowed-p)
        ;;       (goto-char
        ;;        (save-restriction
        ;;          (widen)
        ;;          (goto-char (point-min))
        ;;          (funcall func-change-line count)
        ;;          ;; (forward-line n)
        ;;          ;; The point is now at the absolute target line. When this
        ;;          ;; block ends, save-restriction restores the narrowing. If
        ;;          ;; the point is outside the restored narrowing, Emacs will
        ;;          ;; automatically handle the display or you can manually clamp
        ;;          ;; it.
        ;;          (point)))
        ;;     ;; (line-number-at-pos nil t) returns the absolute line number,
        ;;     ;; accounting for the narrowing offset automatically.
        ;;     ;; TODO fix this, does not support narrowing
        ;;     ;; (funcall func-change-line (if (> n 0)
        ;;     ;;                               ;; Previous
        ;;     ;;                               (1+ (- count start-line))
        ;;     ;;                             ;; Next
        ;;     ;;                             (- count start-line)))
        ;;     )
        ;;   )
        )

       (t
        (message
         "Unsupported evilcursor-smart-next-line/evilcursor-smart-previous-line.")
        ;; (funcall func-change-line count)
        ))))))

(evil-define-motion evilcursor-smart-next-line (count)
  :type line
  (unless count
    (setq count 1))
  (let ((inhibit-message t))
    (evilcursor-forward-line count)))

(evil-define-motion evilcursor-smart-previous-line (count)
  :type line
  (unless count
    (setq count 1))
  (let ((inhibit-message t))
    (evilcursor-forward-line (* count -1))))

(evil-define-key 'insert 'global (kbd "M-k") #'evilcursor-smart-previous-line)
(evil-define-key 'insert 'global (kbd "M-j") #'evilcursor-smart-next-line)

(evil-define-key 'normal 'global (kbd "k") #'evilcursor-smart-previous-line)
(evil-define-key 'normal 'global (kbd "j") #'evilcursor-smart-next-line)
(evil-define-key 'motion 'global (kbd "k") nil)
(evil-define-key 'motion 'global (kbd "j") nil)

;;; O: Evil Open Above

(evil-define-command evilcursor-open-above (count)
  "Insert a new line above point, using current line's indentation.
The insertion will be repeated COUNT times.
This command temporarily disables `electric-indent-mode` to prevent automatic
re-indentation after inserting the copied indentation."
  :suppress-operator t
  (interactive "p")
  (let ((indentation (buffer-substring-no-properties
                      (line-beginning-position)
                      (progn (back-to-indentation) (point))))
        (electric-indent-mode nil))
    (evil-narrow-to-field
      (evil-open-above count)
      (delete-horizontal-space t)
      (insert indentation))))

(define-key evil-normal-state-map "O" 'evilcursor-open-above)

;;; Evil search forward without jumping

(evil-define-motion my-evil-ex-search-forward (count)
  "Start a forward search without jumping to the next item."
  :jump t
  :type exclusive
  :repeat evil-repeat-ex-search
  (save-excursion (evil-ex-search-forward count)))

(evil-define-key 'normal 'global (kbd "C-/") #'my-evil-ex-search-forward)

;;; Evil search key mappings for cursor

(defun evilcursor-previous-history-element-and-move-end-of-line ()
  "Previous history element and move to the end of the line."
  (interactive)
  (previous-history-element 1)
  (move-end-of-line 1))

(defun evilcursor-next-history-element-and-move-end-of-line ()
  "Next history element and move to the end of the line."
  (interactive)
  (next-history-element 1)
  (move-end-of-line 1))

(define-key evil-eval-map [prior] 'evilcursor-previous-history-element-and-move-end-of-line)
(define-key evil-ex-completion-map [prior] 'evilcursor-previous-history-element-and-move-end-of-line)
(define-key evil-ex-search-keymap "\C-p" 'evilcursor-previous-history-element-and-move-end-of-line)
(define-key evil-ex-search-keymap (kbd "M-k") 'evilcursor-previous-history-element-and-move-end-of-line)

(define-key evil-eval-map [next] 'evilcursor-next-history-element-and-move-end-of-line)
(define-key evil-ex-completion-map [next] 'evilcursor-next-history-element-and-move-end-of-line)
(define-key evil-ex-search-keymap "\C-n" 'evilcursor-next-history-element-and-move-end-of-line)
(define-key evil-ex-search-keymap (kbd "M-j") 'evilcursor-next-history-element-and-move-end-of-line)

(define-key evil-eval-map (kbd "M-k") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-k") 'previous-complete-history-element)
(define-key evil-eval-map (kbd "M-j") 'next-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-j") 'next-complete-history-element)

;;; Evil search next and previous

(evil-define-motion evilcursor-find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR."
  :type inclusive
  (interactive "<c><C>")
  (let ((evil-respect-visual-line-mode nil))
    (evil-find-char count char)))

(evil-define-motion evilcursor-find-char-backward (count char)
  "Move to the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (let ((evil-respect-visual-line-mode nil))
    (evil-find-char-backward count char)))

(evil-define-motion evilcursor-find-char-to (count char)
  "Move before the next COUNT'th occurrence of CHAR."
  :type inclusive
  (interactive "<c><C>")
  (let ((evil-respect-visual-line-mode nil))
    (evil-find-char-to count char)))

(evil-define-motion evilcursor-find-char-to-backward (count char)
  "Move before the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (let ((evil-respect-visual-line-mode nil))
    (evil-find-char-to (- (or count 1)) char)))

(define-key evil-motion-state-map "t" 'evilcursor-find-char-to)
(define-key evil-motion-state-map "T" 'evilcursor-find-char-to-backward)
(define-key evil-motion-state-map "f" 'evilcursor-find-char)
(define-key evil-motion-state-map "F" 'evilcursor-find-char-backward)

;;; n/N (and M-n and M-N): Search and recenter

(evil-define-motion evilcursor-ex-search-next-recenter (count)
  "Go to the next occurrence."
  :type exclusive
  (ignore-errors (evil-ex-search-next count))
  (recenter nil))

(evil-define-motion evilcursor-ex-search-previous-recenter (count)
  "Go the the previous occurrence."
  :type exclusive
  (ignore-errors (evil-ex-search-previous count))
  (recenter nil))

(evil-define-motion evilcursor-ex-search-next (count)
  "Go to the next occurrence."
  :type exclusive
  (ignore-errors (evil-ex-search-next count)))

(evil-define-motion evilcursor-ex-search-previous (count)
  "Go the the previous occurrence."
  :type exclusive
  (ignore-errors (evil-ex-search-previous count)))

(evil-define-key 'motion 'global (kbd "n") #'evilcursor-ex-search-next)
(evil-define-key 'motion 'global (kbd "N") #'evilcursor-ex-search-previous)

(evil-define-key 'motion 'global (kbd "M-N") #'evilcursor-ex-search-previous-recenter)
(evil-define-key 'motion 'global (kbd "M-n") #'evilcursor-ex-search-next-recenter)

;;; Global keys

(evil-define-motion my-evil-end-of-line (count)
  "Move the cursor to the end of the current line.
Never go beyond EOL.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (let ((evil-move-beyond-eol t))
    (evil-end-of-line count)
    ;; This prevents `evil-end-of-line' from setting `most-positive-fixnum' to
    ;; the temporary column
    (setq-local temporary-goal-column (current-column))))

(with-eval-after-load 'evil
  (define-key evil-motion-state-map "$" 'my-evil-end-of-line)
  (define-key evil-motion-state-map [end] 'my-evil-end-of-line))

;;; evil outline

(with-eval-after-load "outline-indent"
  (evil-define-key 'normal outline-mode-map (kbd "]]") nil)
  (evil-define-key 'normal outline-mode-map (kbd "[[") nil))

(defun my-evil-define-key-outline-indent-minor-mode ()
  "Set `M-h' and `M-l' to decrease/increase the indentation level of blocks."
  (evil-define-key 'normal 'local (kbd "M-h") 'outline-indent-shift-left)
  (evil-define-key 'normal 'local (kbd "M-l") 'outline-indent-shift-right)

  (unless (derived-mode-p 'prog-mode)
    ;; In prog-mode, [[, ]], gj, and gk provide navigation to the previous
    ;; and next function, so there is no need to override them.
    (evil-define-key 'normal 'local (kbd "]]") 'outline-indent-forward-same-level)
    (evil-define-key 'normal 'local (kbd "[[") 'outline-indent-backward-same-level)
    (evil-define-key 'normal 'local (kbd "gj") 'outline-indent-forward-same-level)
    (evil-define-key 'normal 'local (kbd "gk") 'outline-indent-backward-same-level))

  (evil-define-key 'normal 'local (kbd "gV") 'outline-indent-select)

  ;; Set C-<return> to insert a new line with the same indentation
  ;; level/depth as the current line just before the next heading
  (evil-define-key '(normal insert) 'local (kbd "C-<return>")
    (defun my-evil-outline-indent-insert-heading ()
      (interactive)
      (when (fboundp 'outline-indent-insert-heading)
        (outline-indent-insert-heading)
        (evil-insert-state)))))

(add-hook 'outline-indent-minor-mode-hook
          #'my-evil-define-key-outline-indent-minor-mode)

;;; Toggle comment

(evil-define-operator le-evil-toggle-comment-visual (beg end)
  "Toggle comment from BEG to END."
  (interactive "<r>")
  (unless (derived-mode-p 'org-mode)
    (comment-or-uncomment-region beg end)))

(defun le-evil-toggle-comment-line ()
  "Toggle comment in the current line."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-end-position))))

(define-key evil-normal-state-map (kbd "gc") #'le-evil-toggle-comment-line)
(define-key evil-visual-state-map (kbd "gc") #'le-evil-toggle-comment-visual)

;;; Packages

(lightemacs-use-package tabgo
  :commands tabgo
  :bind (("M-z" . tabgo)
         ("M-t" . tabgo)))

(evil-define-key 'normal 'global (kbd "gt") #'tabgo)

;; TODO: Contribute to evil-match it to add tree-sitter
;; (lightemacs-use-package evil-matchit
;;   :commands evil-matchit-mode
;;
;;   :init
;;   ;; Forces evil-matchit to prioritize the native `evil-jump-item' logic (which
;;   ;; handles standard delimiters like braces and parenthesis) before attempting
;;   ;; to match keywords or tags defined in matchit plugins.
;;   (setq evilmi-always-simple-jump t)
;;
;;   (dolist (hook '(js-mode-hook
;;                   json-mode-hook
;;                   js2-mode-hook
;;                   js3-mode-hook
;;                   javascript-mode-hook
;;                   js-ts-mode-hook
;;                   rjsx-mode-hook
;;                   js2-jsx-mode-hook
;;                   react-mode-hook
;;                   typescript-mode-hook
;;                   typescript-tsx-mode-hook
;;                   typescript-ts-mode-hook
;;                   tsx-ts-mode-hook
;;
;;                   cmake-mode-hook
;;                   cmake-ts-mode-hook
;;
;;                   css-ts-mode-hook
;;                   css-mode-hook
;;                   less-mode-hook
;;                   scss-mode-hook
;;
;;                   diff-mode-hook
;;
;;                   java-mode-hook
;;                   perl-mode-hook
;;                   cperl-mode-hook
;;                   go-mode-hook
;;
;;                   web-mode-hook
;;                   html-mode-hook
;;                   nxml-mode-hook
;;                   nxhtml-mode-hook
;;                   sgml-mode-hook
;;                   php-mode-hook
;;                   php-ts-mode-hook
;;                   message-mode-hook
;;                   mhtml-mode-hook
;;
;;                   org-mode-hook
;;
;;                   lua-mode-hook
;;                   lua-ts-mode-hook
;;
;;                   python-mode-hook
;;                   python-ts-mode-hook
;;
;;                   yaml-mode-hook
;;                   yaml-ts-mode-hook
;;
;;                   c-ts-mode-hook
;;                   c++-ts-mode-hook
;;                   c-mode-hook
;;                   c++-mode-hook
;;
;;                   sh-mode-hook
;;                   bash-ts-mode-hook))
;;     (add-hook hook #'evil-matchit-mode))
;;
;;   :config
;;   (add-to-list 'debug-ignored-errors "Unbalanced parentheses")
;;
;;   (when (fboundp 'evilmi-load-plugin-rules)
;;     (evilmi-load-plugin-rules '(cmake-ts-mode) '(cmake))
;;     (evilmi-load-plugin-rules '(c-ts-mode c++-ts-mode) '(c simple))
;;
;;     ;; TODO unbalanced parenthesis that happen in .bashrc (jc-dotfiles)
;;     ;; # evilmi BUG here: place the cursor on { and press %
;;     ;; _jc_better_cd() {
;;     (evilmi-load-plugin-rules '(bash-ts-mode) '(simple sh))
;;
;;     ;; (evilmi-load-plugin-rules '(cmake-ts-mode) '(simple cmake))
;;     ;; (evilmi-load-plugin-rules '(c-ts-mode c++-ts-mode) '(simple c))
;;
;;     (evilmi-load-plugin-rules '(css-ts-mode) '(simple))
;;     (evilmi-load-plugin-rules '(php-ts-mode) '(simple template html))
;;     (evilmi-load-plugin-rules '(lua-ts-mode) '(simple script))
;;     (evilmi-load-plugin-rules '(python-ts-mode) '(simple python))
;;     (evilmi-load-plugin-rules '(yaml-ts-mode) '(simple yaml))
;;
;;     ;; Loads the 'simple' plugin before the 'sh' plugin. The 'simple' plugin
;;     ;; handles basic text matching for brackets and quotes. Placing it first
;;     ;; ensures that [ ] pairs are matched immediately, preventing the 'sh'
;;     ;; plugin from incorrectly interpreting them as part of the if/fi control
;;     ;; flow structure.
;;     ;; (evilmi-load-plugin-rules '(bash-ts-mode) '(simple sh))
;;
;;     )
;;
;;   ;; TODO unbalanced parenthesis that happen in .bashrc (jc-dotfiles)
;;   ;; # evilmi BUG here: place the cursor on { and press %
;;   ;; (defun my/evilmi-jump-items-around (orig-fn &rest args)
;;   ;;   "Advice to use `evil-jump-item' for ()[]{} and `evilmi-jump-items' otherwise."
;;   ;;   (let ((char (char-after)))
;;   ;;     (if (and char (member char '(?\( ?\) ?\[ ?\] ?\{ ?\})))
;;   ;;         (evil-jump-item)
;;   ;;       (apply orig-fn args))))
;;   ;; (advice-add 'evilmi-jump-items :around #'my/evilmi-jump-items-around)
;;
;;   (with-eval-after-load 'evil
;;     (require 'evil-matchit-evil-setup)))

;;; Package: quick-sdcv

(defun my-setup-quick-sdcv ()
  "Setup quick-sdcv."
  ;; (goto-address-mode 1)
  (my-disable-fringe-truncation-arrow)

  (let ((inhibit-message t))
    (toggle-truncate-lines 0))

  ;; Only show the first one
  ;; (outline-minor-mode 1)
  ;; (goto-char (point-min))
  ;; (outline-hide-sublevels 1)
  ;; (outline-hide-body)
  ;; (outline-show-entry)
  )

(add-hook 'quick-sdcv-mode-hook 'my-setup-quick-sdcv)
(add-hook 'quick-sdcv-mode-hook 'goto-address-mode)

(lightemacs-use-package quick-sdcv
  :commands (quick-sdcv-search-at-point
             quick-sdcv-search-input)

  :init
  (setq quick-sdcv-ellipsis " ")
  (setq quick-sdcv-unique-buffers t)
  ;; (quick-sdcv-only-data-dir nil)
  ;; (quick-sdcv-exact-search t)

  (add-to-list 'display-buffer-alist '("\\*sdcv"
                                       (display-buffer-same-window)))

  ;; Dictionary lookup
  ;; (add-hook 'quick-sdcv-mode-hook 'my-evil-quick-sdcv-search-at-point)
  ;; (add-hook 'markdown-mode-hook 'my-evil-quick-sdcv-search-at-point)
  ;; (add-hook 'org-mode-hook 'my-evil-quick-sdcv-search-at-point)
  ;; (add-hook 'txt-file-mode-hook 'my-evil-quick-sdcv-search-at-point)
  (dolist (mode-hook '(markdown-mode-hook
                       org-mode-hook
                       txt-file-mode-hook))
    (add-hook mode-hook
              (lambda ()
                (setq-local evil-lookup-func 'quick-sdcv-search-at-point))))

  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "<leader>ed") 'quick-sdcv-search-input)))

;;; quick-fasd

;; TODO Lightemacs
(lightemacs-use-package quick-fasd
  :commands (quick-fasd-mode
             quick-fasd-find-path
             quick-fasd-add-path)

  :bind (("C-x C-d" . quick-fasd-find-path)
         :map minibuffer-local-completion-map
         ("C-x C-d" . quick-fasd-find-path))

  :custom
  (quick-fasd-auto-add-on-buffer-change t)
  (quick-fasd-enable-initial-prompt nil)
  (quick-fasd-command-args '("-d"))

  :config
  (quick-fasd-mode))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<leader>fd")
    #'quick-fasd-find-path))

;;; ultisnips-mode

(lightemacs-use-package ultisnips-mode)

;;; Use-package pathaction

(defun pathaction-install ()
  "Install."
  (interactive)
  (when (fboundp 'pathaction-run)
    (pathaction-run "install")))

(defun pathaction-main ()
  "Execute main the task."
  (interactive)
  (when (fboundp 'pathaction-run)
    (pathaction-run "main")))

(defun my-save-some-buffers ()
  "Prevent `save-some-buffers' from prompting by passing 1 to it."
  ;; (save-some-buffers 1)
  (my-save-all-buffers))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "<leader>ei") #'pathaction-install)
  (define-key evil-normal-state-map (kbd "<leader>xx") #'pathaction-main))

(with-eval-after-load 'pathaction
  (remove-hook 'pathaction-before-run-hook 'save-some-buffers)
  (remove-hook 'pathaction-before-run-hook 'pathaction--save-buffer)
  (add-hook 'pathaction-before-run-hook #'my-save-some-buffers))

(add-hook 'ultisnips-mode-hook #'hs-minor-mode)

;;; use-package bufferwizard

(lightemacs-use-package bufferwizard
  :ensure nil
  :vc (:url "https://github.com/jamescherti/bufferwizard.el"
            :rev :newest)
  ;; :straight (bufferwizard
  ;;            :type git
  ;;            :host github
  ;;            :repo "jamescherti/bufferwizard.el")
  :commands (bufferwizard-clone-and-switch-to-indirect-buffer
             bufferwizard-unhighlight
             bufferwizard-toggle-highlight-at-point
             bufferwizard-switch-to-base-buffer
             bufferwizard-replace-symbol-at-point)

  :init
  ;; Indirect buffer
  (evil-define-key 'normal 'global (kbd "<leader>ec") #'bufferwizard-clone-and-switch-to-indirect-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>eC") #'bufferwizard-switch-to-base-buffer)

  ;; Rename
  (evil-define-key 'normal 'global (kbd "<leader>R") #'bufferwizard-replace-symbol-at-point)

  ;; Highlight
  ;; (evil-define-key 'normal 'global (kbd "C-h") #'bufferwizard-toggle-highlight-at-point)
  (evil-define-key 'normal 'global (kbd "<leader>eh") #'bufferwizard-toggle-highlight-at-point)
  (evil-define-key 'normal 'global (kbd "<leader>eH") #'bufferwizard-unhighlight))

(defun pkg-bufferwizard-smart-rename ()
  "Smartly decide how to rename the symbol at point."
  (interactive)
  (cond
   ;; Eglot or LSP-Mode
   ((and (not (region-active-p))
         (or (bound-and-true-p eglot--managed-mode)
             (bound-and-true-p lsp-managed-mode)))
    (let* ((from-string (thing-at-point 'symbol))
           (to-string (read-string (format "Replace '%s' with: " from-string)
                                   from-string nil from-string)))
      (cond
       ((and (bound-and-true-p lsp-managed-mode)
             (fboundp 'lsp-rename))
        (lsp-rename to-string))
       ((and (bound-and-true-p eglot--managed-mode)
             (fboundp 'eglot-rename))
        (eglot-rename to-string)))))

   ;; Replace string
   (t
    (ignore-errors
      (when (fboundp 'bufferwizard-replace-symbol-at-point)
        (bufferwizard-replace-symbol-at-point))))))

(evil-define-key 'normal 'global (kbd "<leader>r") #'pkg-bufferwizard-smart-rename)

;;; better vc

(defun mod-better-vc-diff ()
  "Save and call `vc-diff' silently."
  (interactive)
  (let ((inhibit-message t))
    (my-save-buffer)
    (vc-diff)))

(defun mod-better-vc-git-toplevel ()
  "Opens the directory returned by `git rev-parse --show-toplevel`."
  (interactive)
  (let ((git-toplevel (vc-root-dir)))
    (cond
     ((and git-toplevel (file-directory-p git-toplevel))
      (find-file git-toplevel)
      (user-error "Git top-level directory not found"))

     (t
      (user-error "Not a Git repository")))))

(with-eval-after-load 'evil
  ;; (evil-define-key 'normal 'global (kbd "<leader>vl") #'better-vc-version-diff-main)
  ;; (evil-define-key 'normal 'global (kbd "<leader>vd") #'vc-print-log)
  (define-key evil-normal-state-map (kbd "<leader>gt") #'mod-better-vc-git-toplevel)
  (define-key evil-normal-state-map (kbd "<leader>vt") #'mod-better-vc-git-toplevel)
  (define-key evil-normal-state-map (kbd "<leader>vd") #'mod-better-vc-diff)
  (define-key evil-normal-state-map (kbd "<leader>vb") #'vc-print-branch-log))

;;; Scroll the window down

(evil-define-command my-evil-scroll-line-down (count)
  "Scroll the window COUNT lines downward.
The key distinction from the main option is that this variant preserves the
column layout, except when a point falls on the first visible line."
  :repeat nil
  :keep-visual t
  (interactive "p")
  ;; TODO pull request evil mode
  (scroll-up count))

(with-no-warnings
  (define-key evil-normal-state-map (kbd "C-e") #'my-evil-scroll-line-down))

;;; M-e/M-y: Scroll line up and down

(evil-define-command evilcursor-scroll-line-up (count)
  "Scroll the window COUNT lines upwards and move the cursor COUNT downwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (let (scroll-preserve-screen-position)
    (evil-scroll-line-down count)
    (evil-next-line count)))

(evil-define-command evilcursor-scroll-line-down (count)
  "Scroll the window COUNT lines downwards and move the cursor COUNT upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (let (scroll-preserve-screen-position)
    (evil-scroll-line-up count)
    (evil-previous-line count)))

(define-key evil-normal-state-map (kbd "M-e") 'evilcursor-scroll-line-up)
(define-key evil-normal-state-map (kbd "M-y") 'evilcursor-scroll-line-down)

;;; bufferfile

(evil-define-key 'normal 'global (kbd "<leader>ur") 'bufferfile-rename)
(evil-define-key 'normal 'global (kbd "<leader>ud") 'bufferfile-delete)

;;; Better grep

(lightemacs-use-package mod-better-grep
  :ensure nil
  :commands mod-better-grep
  ;; :init
  ;; (with-eval-after-load 'evil
  ;;   (evil-define-key 'normal 'global (kbd "<leader>gR") #'better-grep)
  ;;   (evil-define-key 'normal 'global (kbd "<leader>gr") #'better-grep)
  ;;   (add-hook 'on-first-input-hook #'fido-vertical-mode))
  )

;;; lightemacs-dired-filter-toggle

(with-eval-after-load 'dired
  (require 'le-dired-filter)
  (evil-define-key 'normal dired-mode-map (kbd "I") 'lightemacs-dired-filter-toggle))

;;; flyspell

(defun my-flyspell-region (beg end)
  "Flyspell the region from BEG to END."
  (interactive "r")
  (flyspell-region beg end)
  (message "Flyspell region done"))

(defun my-flyspell-buffer ()
  "Flyspell the whole buffer.
In `prog-mode', this configures flyspell to check only comments and strings."
  (interactive)
  (save-excursion
    (when (derived-mode-p 'prog-mode)
      (setq flyspell-generic-check-word-predicate
            'flyspell-generic-progmode-verify))
    (flyspell-buffer))
  (message "Flyspell buffer done"))

(defun my-flyspell-clear ()
  "Clear all Flyspell overlays from the current buffer."
  (interactive)
  (when (fboundp 'flyspell-delete-all-overlays)
    (flyspell-delete-all-overlays)))

(evil-define-key 'visual 'global (kbd "<leader>fs") #'my-flyspell-region)
(evil-define-key 'normal 'global (kbd "<leader>fb") #'my-flyspell-buffer)
(evil-define-key 'normal 'global (kbd "<leader>fc") #'my-flyspell-clear)

;;; Do not display Search Failed in evil-ex
(defun my-around-evil-ex-search-update-pattern-no-echo (fn &rest args)
  "FN is the advised function. ARGS are the function arguments."
  (cl-letf (((symbol-function #'evil-ex-echo)
             (lambda (&rest _args) nil)))
    (apply fn args)))

(with-eval-after-load 'evil-search
  (when (fboundp 'my-around-evil-ex-search-update-pattern-no-echo)
    (advice-add 'evil-ex-search-update-pattern
                :around #'my-around-evil-ex-search-update-pattern-no-echo)))

;;; term

;; (with-eval-after-load 'term
;;   ;; Unbind Escape in Insert state so it passes directly to the shell process
;;   ;; (evil-collection-define-key 'insert 'term-raw-map
;;   ;;   (kbd "<escape>") nil
;;   ;;   (kbd "ESC") nil)
;;
;;   ;; Bind M-j and M-k to Evil window navigation in the terminal
;;   (evil-define-key '(insert emacs) term-raw-map
;;     (kbd "C-c C-c") 'term-send-raw
;;     (kbd "M-j") 'term-send-raw-meta
;;     (kbd "M-k") 'term-send-raw-meta
;;     (kbd "S-C-v") 'term-paste))

;; allow moving around the buffer in emacs >= 26.1 in evil's normal mode
;; (setq term-char-mode-point-at-process-mark nil)

;; Change the default escape prefix to C-x
;; This allows C-c to be sent directly to the underlying shell
;; (add-hook 'term-mode-hook (lambda ()
;;                             (when (fboundp 'term-set-escape-char)
;;                               (term-set-escape-char ?\C-x))))

;; Restore Ctrl-c Ctrl-c to close programs
;; (with-eval-after-load 'term
;;   (evil-set-initial-state 'term-mode 'emacs))


;; TODO
;; (evil-define-key 'insert term-raw-map (kbd "C-s")
;;   '(lambda() (interactive)
;;      (term-send-raw)))



;; (with-eval-after-load 'term
;;   ;; Keep the native Emacs binding for term-char-mode
;;   (define-key term-raw-map (kbd "M-h") #'term-send-raw-meta)
;;
;;   ;; Tell Evil to map the key correctly when term-mode is active
;;   (evil-define-key 'insert term-raw-map (kbd "M-h") #'term-send-raw-meta)
;;   ;; (evil-define-key 'insert term-mode-map (kbd "M-l") #'term-send-raw-meta)
;;   )


;; (with-eval-after-load 'term
;;   ;; Bind directly to term-raw-map instead of using evil-define-key.
;;   ;; This ensures they work natively when the terminal is in char mode.
;;   ;; (define-key term-raw-map (kbd "C-c C-c") 'term-send-raw)
;;   ;; (define-key term-raw-map (kbd "M-j") 'term-send-raw-meta)
;;   ;; (define-key term-raw-map (kbd "M-k") 'term-send-raw-meta)
;;   ;; (define-key term-raw-map (kbd "M-h") 'term-send-raw-meta) ; Added M-h
;;   ;; (define-key term-raw-map (kbd "M-l") 'term-send-raw-meta) ; Added M-l for completeness
;;   ;;
;;   ;; (evil-define-key 'insert term-raw-map (kbd "M-h") 'term-send-raw-meta)
;;   ;; (evil-define-key 'motion term-raw-map (kbd "M-h") 'term-send-raw-meta)
;;   ;; (define-key term-raw-map (kbd "S-C-v") 'term-paste)
;;   )

;; (defun my-force-ansi-term-escape ()
;;   "Force Escape to be sent to the terminal process in Evil insert state."
;;   )
;;
;; ;; The 't' at the end ensures this hook runs last, overriding other packages
;; (add-hook 'term-mode-hook 'my-force-ansi-term-escape t)

;; -------------------------------------------------------------

;; (define-key term-raw-map (kbd "C-y") 'term-send-raw)
;; (define-key term-raw-map (kbd "C-p") 'term-send-raw)
;; (define-key term-raw-map (kbd "C-n") 'term-send-raw)
;; (define-key term-raw-map (kbd "C-r") 'term-send-raw)
;; (define-key term-raw-map (kbd "M-d") (lambda () (interactive) (term-send-raw-string "\ed")))
;; (define-key term-raw-map (kbd "<C-backspace>") (lambda () (interactive) (term-send-raw-string "\e\C-?")))
;; (define-key term-raw-map (kbd "M-p") (lambda () (interactive) (term-send-raw-string "\ep")))
;; (define-key term-raw-map (kbd "M-n") (lambda () (interactive) (term-send-raw-string "\en")))
;; (define-key term-raw-map (kbd "C-S-y") 'term-paste)
;; (define-key term-raw-map (kbd "C-]") nil)

;; (defun my-ansi-term-send-colon ()
;;   "Send a literal colon directly to the ansi-term process."
;;   (interactive)
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (when proc (process-send-string proc ":"))))
;;
;; (defun my-ansi-term-send-meta-shift-h ()
;;   "Send Meta+Shift+h directly to the ansi-term process."
;;   (interactive)
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (when proc (process-send-string proc "\eH"))))
;;
;; (defun my-ansi-term-send-meta-shift-l ()
;;   "Send Meta+Shift+l directly to the ansi-term process."
;;   (interactive)
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (when proc (process-send-string proc "\eL"))))
;;
;; (with-eval-after-load 'term
;;   (with-eval-after-load 'evil
;;     ;; Apply bindings to the base term-mode-map
;;     (evil-define-key '(normal motion) term-mode-map
;;       (kbd ":") 'my-ansi-term-send-colon
;;       ;; Binding both uppercase and explicit Shift variations ensures
;;       ;; it catches the input regardless of how Emacs translates the key
;;       (kbd "M-S-h") 'my-ansi-term-send-meta-shift-h
;;       (kbd "M-H") 'my-ansi-term-send-meta-shift-h
;;       (kbd "M-S-l") 'my-ansi-term-send-meta-shift-l
;;       (kbd "M-L") 'my-ansi-term-send-meta-shift-l)
;;
;;     ;; Ensure bindings survive if evil-collection overrides term maps
;;     (with-eval-after-load 'evil-collection-term
;;       (evil-define-key '(normal motion) term-mode-map
;;         (kbd ":") 'my-ansi-term-send-colon
;;         (kbd "M-S-h") 'my-ansi-term-send-meta-shift-h
;;         (kbd "M-H") 'my-ansi-term-send-meta-shift-h
;;         (kbd "M-S-l") 'my-ansi-term-send-meta-shift-l
;;         (kbd "M-L") 'my-ansi-term-send-meta-shift-l))))

;; (with-eval-after-load 'term
;;   (defun my-evil-term-send-colon ()
;;     "Send a literal colon to the terminal process."
;;     (interactive)
;;     (when (fboundp 'term-send-raw-string)
;;       (term-send-raw-string ":")))
;;
;;   (defun my-evil-term-send-meta-shift-h ()
;;     "Send Meta+Shift+h to the terminal process."
;;     (interactive)
;;     (when (fboundp 'term-send-raw-string)
;;       (term-send-raw-string "\eH")))
;;
;;   (defun my-evil-term-send-meta-shift-l ()
;;     "Send Meta+Shift+l to the terminal process."
;;     (interactive)
;;     (when (fboundp 'term-send-raw-string)
;;       (term-send-raw-string "\eL")))
;;
;;   ;; Apply bindings to term-mode-map for normal and motion states
;;   (evil-define-key '(insert emacs) term-mode-map
;;     (kbd ":") 'my-evil-term-send-colon
;;     (kbd "M-S-h") 'my-evil-term-send-meta-shift-h
;;     (kbd "M-S-l") 'my-evil-term-send-meta-shift-l))

;; ;; Revert to the native Emacs terminal identifier
;; (setq term-term-name "eterm-color")

;; (with-eval-after-load 'term
;;   (let ((map term-raw-map)
;;         (exceptions '(( "M-RET" . org-meta-return)
;;                       ( "M-x"   . execute-extended-command)
;;                       ( "M-o"   . other-window)
;;                       ( "C-y"   . term-paste)
;;                       ( "M-y"   . yank-pop))))
;;
;;     ;; 1. Handle specific command exceptions
;;     (dolist (exception exceptions)
;;       (define-key map (kbd (car exception)) (cdr exception)))
;;
;;     ;; 2. Handle prefixes (C-x, C-c)
;;     ;; Setting these to nil allows them to "fall through" to your global bindings
;;     (define-key map (kbd "C-x") nil)
;;
;;     ;; NOTE: C-c is the default 'term-escape-char'.
;;     ;; If you want C-c to work as a normal Emacs prefix,
;;     ;; you must move the escape char to something else first.
;;     (setq term-escape-char [?\C-q]) ; Moves escape to C-q
;;     (define-key map (kbd "C-c") nil)))

;; (setq evil-collection-term-sync-state-and-mode-p t)
;; (with-eval-after-load 'evil-collection
;;   (evil-collection-init '(term)))

;; (with-eval-after-load 'term
;;   ;; Change the escape character to C-q (standard is C-c)
;;   (setq term-escape-char [?\C-q])
;;
;;   ;; Now that C-c is free, explicitly tell term-mode to send it to the shell.
;;   ;; This allows you to interrupt processes (SIGINT) with a single C-c.
;;   (define-key term-raw-map (kbd "C-c") #'term-send-raw))

;; Tell external apps you are using a standard 256-color terminal
;; NOTE: doesn't work
;; (setq term-term-name "xterm-256color")

;; Enable the translation of 256-color codes in term-mode buffers
;; (This requires the eterm-256color package to be installed)
;; (use-package eterm-256color
;;   :hook (term-mode . eterm-256color-mode))
;; ;; Use eterm-256color to handle complex output and build the correct terminfo
;; (use-package eterm-256color
;;   :ensure t
;;   :hook (term-mode . eterm-256color-mode))

;; Pass function keys directly to the underlying shell in term-mode
;; (setq term-bind-function-keys t)

;; Instruct Emacs to inform child processes that truecolor is supported
;; (setenv "COLORTERM" "truecolor")

;; Use a standard TERM string so command-line applications output colors correctly
;; (setenv "TERM" "xterm-256color")
;; (setq term-term-name "xterm-256color")

;; Activate the newly upgraded native ANSI color filter for M-x shell
;; (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

;; xterm color
;; (lightemacs-use-package xterm-color
;;   ;; :custom
;;   ;; (compilation-environment '("TERM=xterm-256color"))
;;   )


;; (defun my-ansi-term-send-m-h ()
;;   "Send M-h directly to the ansi-term process."
;;   (interactive)
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (when proc (process-send-string proc "\eh"))))
;;
;; (with-eval-after-load 'term
;;   ;; Bind for standard Emacs state
;;   (define-key term-raw-map (kbd "M-h") #'my-ansi-term-send-m-h))
;;
;; (with-eval-after-load 'evil
;;   ;; Bind specifically for Evil's insert state in the terminal
;;   (evil-define-key 'insert term-raw-map (kbd "M-h") #'my-ansi-term-send-m-h))

(with-eval-after-load 'term
  ;; (define-key term-raw-map (kbd "M-x") nil)  ; unbind M-x

  (define-key term-raw-map (kbd "C-s") 'term-send-raw)
  (evil-define-key 'insert term-raw-map (kbd "C-s") nil)
  ) ;; unbind isearch
;;
;; (with-eval-after-load 'term
;;   ;; 1. Define the keys in the native term-raw-map
;;   (define-key term-raw-map [?\M-h] #'term-send-raw-meta)
;;   (define-key term-raw-map [?\M-j] #'term-send-raw-meta)
;;   (define-key term-raw-map [?\M-k] #'term-send-raw-meta)
;;   (define-key term-raw-map [?\M-l] #'term-send-raw-meta))
;;
;; (with-eval-after-load 'evil
;;   ;; 2. Explicitly define the keys in Evil's insert state map for term-raw-map.
;;   ;; We use vector notation [?\M-h] to prevent the "ESC prefix" error.
;;   (evil-define-key 'insert term-raw-map
;;     [?\M-h] #'term-send-raw-meta
;;     [?\M-j] #'term-send-raw-meta
;;     [?\M-k] #'term-send-raw-meta
;;     [?\M-l] #'term-send-raw-meta))
;;
;; (with-eval-after-load 'term
;;   ;; 1. Define the keys in the native term-raw-map
;;   (define-key term-mode-map [?\M-h] #'term-send-raw-meta)
;;   (define-key term-mode-map [?\M-j] #'term-send-raw-meta)
;;   (define-key term-mode-map [?\M-k] #'term-send-raw-meta)
;;   (define-key term-mode-map [?\M-l] #'term-send-raw-meta))
;;
;; (with-eval-after-load 'evil
;;   ;; 2. Explicitly define the keys in Evil's insert state map for term-raw-map.
;;   ;; We use vector notation [?\M-h] to prevent the "ESC prefix" error.
;;   (evil-define-key 'insert term-mode-map
;;     [?\M-h] #'term-send-raw-meta
;;     [?\M-j] #'term-send-raw-meta
;;     [?\M-k] #'term-send-raw-meta
;;     [?\M-l] #'term-send-raw-meta))

;; 2. Define your meta function
;; (defun term-send-raw-meta ()
;;   "Send the last pressed key as a meta escape sequence to the terminal."
;;   (interactive)
;;   (term-send-raw-string (concat "\e" (string (event-basic-type last-input-event)))))

;; 3. Use evil-collection-define-key to bind the keys
;; (with-eval-after-load 'evil-collection-term
;;   (evil-collection-define-key 'insert 'term-raw-map
;;     [?\M-h] #'term-send-raw-meta
;;     ;; [?\M-j] #'term-send-raw-meta
;;     ;; [?\M-k] #'term-send-raw-meta
;;     [?\M-l] #'term-send-raw-meta))

;; 2. Bind the keys in the native term-raw-map
;; (with-eval-after-load 'term
;;   (define-key term-raw-map [?\M-h] #'term-send-raw-meta)
;;   ;; (define-key term-raw-map [?\M-j] #'term-send-raw-meta)
;;   ;; (define-key term-raw-map [?\M-k] #'term-send-raw-meta)
;;   (define-key term-raw-map [?\M-l] #'term-send-raw-meta))


;;; term setup

;; (with-eval-after-load 'term
;;   (with-eval-after-load 'evil
;;     ;; 1. Prevent Evil from intercepting ESC in native term-raw-map
;;     (evil-define-key 'insert term-raw-map (kbd "ESC") nil)
;;     (evil-define-key 'insert term-raw-map (kbd "<escape>") nil)
;;
;;     ;; 2. Assign a new key to exit insert state and return to normal state
;;     ;; C-\ is used here as an example, but you can change it to any preferred key
;;     (evil-define-key 'insert term-raw-map (kbd "C-\\") #'evil-normal-state))
;;
;;   (with-eval-after-load 'evil-collection-term
;;     ;; 3. Ensure evil-collection also releases the ESC key
;;     (evil-collection-define-key 'insert 'term-raw-map (kbd "ESC") nil)
;;     (evil-collection-define-key 'insert 'term-raw-map (kbd "<escape>") nil)
;;     (evil-collection-define-key 'insert 'term-raw-map (kbd "C-\\") #'evil-normal-state)))

;; (defun my-term-send-m-h ()
;;   "Send M-h to the terminal."
;;   (interactive)
;;   (term-send-raw-string "\eh"))
;;
;; (defun my-term-send-m-j ()
;;   "Send M-j to the terminal."
;;   (interactive)
;;   (term-send-raw-string "\ej"))
;;
;; (defun my-term-send-m-k ()
;;   "Send M-k to the terminal."
;;   (interactive)
;;   (term-send-raw-string "\ek"))
;;
;; (defun my-term-send-m-l ()
;;   "Send M-l to the terminal."
;;   (interactive)
;;   (term-send-raw-string "\el"))
;;
;; (with-eval-after-load 'term
;;   (define-key term-raw-map [?\M-h] #'my-term-send-m-h)
;;   (define-key term-raw-map [?\M-j] #'my-term-send-m-j)
;;   (define-key term-raw-map [?\M-k] #'my-term-send-m-k)
;;   (define-key term-raw-map [?\M-l] #'my-term-send-m-l))
;;
;; (with-eval-after-load 'evil
;;   (evil-define-key '(insert emacs) term-raw-map
;;     [?\M-h] #'my-term-send-m-h
;;     [?\M-j] #'my-term-send-m-j
;;     [?\M-k] #'my-term-send-m-k
;;     [?\M-l] #'my-term-send-m-l))

;; (defun my-ansi-term-send-m-h ()
;;   "Send M-h directly to the ansi-term process."
;;   (interactive)
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (when proc (process-send-string proc "\eh"))))
;;
;; (defun my-force-ansi-term-evil-bindings ()
;;   "Force Evil insert state bindings for ansi-term buffers."
;;   ;; Apply directly to the local Evil map using vector notation
;;   (evil-local-set-key 'insert [?\M-h] #'my-ansi-term-send-m-h))
;;
;; ;; Attach this to term-mode-hook so it runs every time a new terminal opens
;; (add-hook 'term-mode-hook #'my-force-ansi-term-evil-bindings)

;; ;; 2. Define your meta function
;; (defun term-send-raw-meta ()
;;   "Send the last pressed key as a meta escape sequence to the terminal."
;;   (interactive)
;;   (term-send-raw-string (concat "\e" (string (event-basic-type last-input-event)))))
;;
;; (with-eval-after-load 'term
;;   ;; 1. Define the keys in the native term-raw-map
;;   (define-key term-raw-map [?\M-h] #'term-send-raw-meta)
;;   (define-key term-raw-map [?\M-j] #'term-send-raw-meta)
;;   (define-key term-raw-map [?\M-k] #'term-send-raw-meta)
;;   (define-key term-raw-map [?\M-l] #'term-send-raw-meta))
;;
;; (with-eval-after-load 'evil
;;   ;; 2. Explicitly define the keys in Evil's insert state map for term-raw-map.
;;   ;; We use vector notation [?\M-h] to prevent the "ESC prefix" error.
;;   (evil-define-key 'insert term-raw-map
;;     [?\M-h] #'term-send-raw-meta
;;     [?\M-j] #'term-send-raw-meta
;;     [?\M-k] #'term-send-raw-meta
;;     [?\M-l] #'term-send-raw-meta))
;;
;; ;; 3. Use evil-collection-define-key to bind the keys
;; (with-eval-after-load 'evil-collection-term
;;   (evil-collection-define-key 'insert 'term-raw-map
;;     [?\M-h] #'term-send-raw-meta
;;     [?\M-j] #'term-send-raw-meta
;;     [?\M-k] #'term-send-raw-meta
;;     [?\M-l] #'term-send-raw-meta))
;;
;; (defun my-ansi-term-send-escape ()
;;   "Send an escape character directly to the ansi-term process."
;;   (interactive)
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (when proc (process-send-string proc "\e"))))

(defun my-term-setup ()
  "Configuration for term and `ansi-term' buffers."
  (let ((inhibit-message t))
    (toggle-truncate-lines 1))
  (display-line-numbers-mode 0)

  (setq-local nobreak-char-display nil)
  (setq-local line-spacing 0)

  ;; Disable `hscroll-margin' in shell buffers to prevent visual jumping when
  ;; the cursor approaches the left or right edges of the window.
  (setq-local hscroll-margin 0)

  ;; Setting scroll-margin to 0 prevents visual glitching and cursor jumping
  ;; when using a terminal emulator inside Emacs.
  (setq-local scroll-margin 0)

  (setq-local scroll-conservatively most-positive-fixnum)
  (setq-local fast-but-imprecise-scrolling t)

  (setq-local mode-line-format nil)

  (when (fboundp 'yas-minor-mode)
    (yas-minor-mode -1))

  (when (fboundp 'yas-minor-mode)
    (yas-minor-mode -1))

  (when (fboundp 'evil-snipe-local-mode)
    (evil-snipe-local-mode -1))

  (when (fboundp 'electric-pair-local-mode)
    (electric-pair-mode -1))

  (when (fboundp 'electric-indent-local-mode)
    (electric-indent-local-mode -1))

  ;; (evil-local-set-key 'insert (kbd "<escape>") 'my-ansi-term-send-escape)
  ;; (evil-local-set-key 'insert (kbd "ESC") 'my-ansi-term-send-escape)
  ;; (evil-local-set-key 'insert (kbd "C-[") 'my-ansi-term-send-escape)

  ;; Bind C-c to return to normal mode, since Escape is now consumed by the terminal
  (evil-local-set-key 'insert (kbd "C-c ESC") 'evil-normal-state)
  (evil-local-set-key 'insert (kbd "C-c <escape>") 'evil-normal-state)

  ;; C-s and C-r for terminal history search
  ;; Applied to both insert and normal states so it always works
  ;; (evil-local-set-key 'insert (kbd "C-s") 'my-ansi-term-send-ctrl-s)
  ;; (evil-local-set-key 'normal (kbd "C-s") 'my-ansi-term-send-ctrl-s)
  ;; (evil-local-set-key 'insert (kbd "C-r") 'my-ansi-term-send-ctrl-r)
  ;; (evil-local-set-key 'normal (kbd "C-r") 'my-ansi-term-send-ctrl-r)

  ;; (setq-local transient-mark-mode nil)
  ;; (auto-fill-mode -1)

  ;; ;; Disable UI elements that interfere with terminal rendering
  ;; (display-line-numbers-mode -1)
  ;; (hl-line-mode -1)
  ;;
  ;; ;; Bind paste to work correctly in raw mode
  ;; ;; (define-key term-raw-map (kbd "C-y") 'term-paste)
  ;;
  ;; ;; Toggle easily between line mode and char (raw) mode
  ;; (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
  ;; (define-key term-mode-map (kbd "C-c C-k") 'term-char-mode)
  )

(add-hook 'term-mode-hook #'my-term-setup t)

;;; org

;; TODO patch?
(defun my-org-move-subtree-preserve-column-advice (orig-fun &rest args)
  "Advice to preserve the column when moving Org subtrees.
ORIG-FUN and ARGS is the function and its arguments."
  (let ((col (current-column)))
    (condition-case nil
        (apply orig-fun args)
      (error nil))
    (move-to-column col)))

;; Apply the advice to both up and down movements
(with-eval-after-load 'org
  (advice-add 'org-move-subtree-up :around #'my-org-move-subtree-preserve-column-advice)
  (advice-add 'org-move-subtree-down :around #'my-org-move-subtree-preserve-column-advice))

;;; Only yank visible text

;; (defun evilbuffer-buffer-substring-visible (beg end)
;;   "Return the visible text between BEG and END, excluding invisible regions."
;;   (let (parts)
;;     (save-excursion
;;       (goto-char beg)
;;       (while (< (point) end)
;;         (let ((next (next-single-char-property-change (point) 'invisible nil end)))
;;           (unless (invisible-p (point))
;;             (push (buffer-substring-no-properties (point) next) parts))
;;           (goto-char next))))
;;     (apply #'concat (nreverse parts))))

;; TODO article
(defun evilbuffer-buffer-substring-visible (beg end)
  "Return the visible text between BEG and END, excluding invisible regions."
  (let ((text ""))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let* ((next (next-single-char-property-change (point) 'invisible nil end))
               (invis (invisible-p (point))))
          (unless invis
            (setq text (concat text (buffer-substring-no-properties (point) next))))
          (goto-char next))))
    text))

(defun evilbuffer-substring--filter-visible (beg end &optional delete)
  "Filter for `filter-buffer-substring-function' that preserves visible text.

BEG and END specify the region bounds. If DELETE is non-nil, the region is
deleted and its text is returned. Otherwise, the function returns only the
visible text between BEG and END, excluding regions with the invisible text
property.

This function also respects the obsolete wrapper hook
`filter-buffer-substring-functions' via `with-wrapper-hook'. No filtering occurs
unless a wrapper hook is active."
  (subr--with-wrapper-hook-no-warnings
   filter-buffer-substring-function (beg end delete)
   (cond
    (delete
     (save-excursion
       (goto-char beg)
       (delete-and-extract-region beg end)))

    (t
     (evilbuffer-buffer-substring-visible beg end)))))

;; Useful for yanking =text= in org mode. I do not want to include invisible
;; text
;; Disable
;; (setq filter-buffer-substring-function 'evilbuffer-substring--filter-visible)

;;; better outline

;;; outline M-j and M-k

(defun my-patched-outline-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p")
  ;; TODO: PATCH3: Restore column
  (when (and (fboundp 'outline-back-to-heading)
             (fboundp 'outline-end-of-subtree)
             (fboundp 'outline-end-of-heading)
             (fboundp 'outline-hide-subtree))
    (let ((column (current-column)))
      (unwind-protect
          (progn
            (outline-back-to-heading)
            (let* ((movfunc (if (> arg 0) 'outline-get-next-sibling
                              'outline-get-last-sibling))
                   ;; Find the end of the subtree to be moved as well as the point
                   ;; to move it to, adding a newline if necessary, to ensure
                   ;; these points are at bol on the line below the subtree.
                   (end-point-func (lambda ()
                                     (let ((outline-blank-line nil))
                                       (outline-end-of-subtree))
                                     (if (eq (char-after) ?\n) (forward-char 1)
                                       (if (and (eobp) (not (bolp))) (insert "\n")))
                                     (point)))
                   (beg (point))
                   (folded (save-match-data
                             (outline-end-of-heading)
                             (outline-invisible-p)))
                   (end (save-match-data
                          (funcall end-point-func)))
                   (ins-point (make-marker))
                   (cnt (abs arg)))
              ;; Find insertion point, with error handling.
              (goto-char beg)
              (while (> cnt 0)
                (or (funcall movfunc)
                    (progn (goto-char beg)
                           (user-error "Cannot move past superior level")))
                (setq cnt (1- cnt)))
              (if (> arg 0)
                  ;; Moving forward - still need to move over subtree.
                  (funcall end-point-func))
              (move-marker ins-point (point))
              (insert (delete-and-extract-region beg end))
              (goto-char ins-point)
              (if folded (outline-hide-subtree))
              (move-marker ins-point nil)))
        ;; TODO: PATCH3: Restore column
        (unless (= (current-column) column)
          (move-to-column column))))))

(defun my-patched-outline-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG sibling headlines.
Preserve the cursor column and adjust any trailing blank space after the moved
heading."
  (interactive "p")
  (my-patched-outline-move-subtree-down (- (prefix-numeric-value arg))))

;; Bind these to your preferred keys in org-mode-map
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-<up>") 'org-move-subtree-up)
  (define-key org-mode-map (kbd "M-<down>") 'org-move-subtree-down))

(defun my-setup-outline-minor-mode-keymap ()
  "Setup `outline-minor-mode'."
  (cond
   ((derived-mode-p 'org-mode)
    (evil-define-key 'normal 'local (kbd "M-k") 'org-move-subtree-up)
    (evil-define-key 'normal 'local (kbd "M-j") 'org-move-subtree-down))

   ((bound-and-true-p outline-indent-minor-mode)
    (evil-define-key 'normal 'local (kbd "M-k") 'outline-indent-move-subtree-up)
    (evil-define-key 'normal 'local (kbd "M-j") 'outline-indent-move-subtree-down))

   ((bound-and-true-p outline-minor-mode)
    ;; Set `M-k` and `M-j` to move indented blocks up and down
    (evil-define-key 'normal 'local (kbd "M-k") 'my-patched-outline-move-subtree-up)
    (evil-define-key 'normal 'local (kbd "M-j") 'my-patched-outline-move-subtree-down))))

(add-hook 'org-mode-hook #'my-setup-outline-minor-mode-keymap)
(add-hook 'outline-indent-minor-mode-hook #'my-setup-outline-minor-mode-keymap)
(add-hook 'outline-minor-mode-hook #'my-setup-outline-minor-mode-keymap)

;;; Provide

(provide 'my-config-evil)

;; Local variables:
;; byte-compile-warnings: (not lexical free-vars)
;; End:

;;; my-config-evil.el ends here

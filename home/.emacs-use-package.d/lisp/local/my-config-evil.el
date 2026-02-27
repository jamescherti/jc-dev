;;; my-config-evil.el --- Config evil -*- lexical-binding: t -*-

;;; Commentary:

;; Author: James Cherti
;; URL: https://github.com/jamescherti/jc-dev
;;
;; Distributed under terms of the MIT license.
;;
;; Copyright (C) 2004-2026 James Cherti
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'evil)

;;; Better evil

(defun my-save-buffers-kill-emacs ()
  "Handle quitting Emacs with daemon-aware frame management."
  (interactive)
  (if (and (daemonp)
           (fboundp 'easysession-save-session-and-close-frames))
      (easysession-save-session-and-close-frames)
    (save-buffers-kill-emacs)))

(defun evilbuffer-switch-to-scratch-and-clear ()
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
        ;;   (visual-line-fill-column-mode)
        ;;   (setq-local my-was-visual-line-fill-column-mode nil))
        (let ((inhibit-message t))
          ;; Wrap
          (toggle-truncate-lines 0)))
    ;; When the text is truncated
    ;; (visual-line-mode -1)
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
    ;; when (yes-or-no-p "Are you sure you want to erase the buffer?")
    (erase-buffer)

    (cond
     ((string-prefix-p "*Ollama" buffer-name)
      (cond
       ((derived-mode-p 'org-mode)
        (insert "* "))

       ((or (derived-mode-p 'markdown-ts-mode)
            (derived-mode-p 'markdown-mode))
        (insert "# ")))

      (when (fboundp 'evil-insert-state)
        (evil-insert-state))))))

(defun my-clear-highlights ()
  "Clear highlight and related state in the buffer."
  ;; Clear lazy highlights
  (lazy-highlight-cleanup)

  ;; Clear Evil mode highlights
  (when (fboundp 'evil-ex-nohighlight)
    (evil-ex-nohighlight))

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

(defun my-evil-save ()
  "Save."
  (if (fboundp 'buffer-guardian-save-buffer)
      (buffer-guardian-save-buffer)
    (save-buffer)))

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

(defun my-project-root-dir (&optional path)
  "Search up the PATH for `project-root-markers'."
  (when (fboundp 'project-root)
    (when-let* ((project (project-current nil path))
                (project-root (when project
                                (project-root project))))
      (directory-file-name project-root))))

;; TODO lightemacs?
(defun my-bash-stdops-sre ()
  "Call sre."
  (interactive)
  (let ((project-dir (expand-file-name (my-project-root-dir))))
    (unless project-dir
      (user-error "Unable to find the project path: %s" project-dir))
    (when (fboundp 'bash-stdops-project-sre)
      (bash-stdops-project-sre nil nil project-dir))))

(with-eval-after-load 'evil
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
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal 'global (kbd "<leader>er") 'evileval-region)
    (evil-define-key 'normal 'global (kbd "<leader>eb") 'evileval-buffer))

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
    (when (and (fboundp 'evil-shift-right)
               (fboundp 'evil-normal-state)
               (fboundp 'evil-visual-restore))
      (evil-shift-right evil-visual-beginning evil-visual-end 1 nil)
      (evil-normal-state)
      (evil-visual-restore)))

  (defun lightemacs-evil-shift-left ()
    "Shift the selected region to the left, preserving the selection."
    (interactive)
    (when (and (fboundp 'evil-shift-left)
               (fboundp 'evil-normal-state)
               (fboundp 'evil-visual-restore))
      (evil-shift-left evil-visual-beginning evil-visual-end 1 nil)
      (evil-normal-state)
      (evil-visual-restore)))
  (when (fboundp 'evil-define-key)
    (evil-define-key 'visual 'global
      (kbd ">") 'lightemacs-evil-shift-right
      (kbd "<") 'lightemacs-evil-shift-left))

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
    (when (fboundp 'evil-join)
      (save-excursion
        (evil-join (line-beginning-position) (line-end-position)))))
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal 'global (kbd "J") 'evilcursor-join-normal)
    (evil-define-key 'visual 'global (kbd "J") 'evil-join))

  ;; M-[ and M-]: Previous and next section
  (defun my-evil-forward-section-end ()
    "Move to the next section."
    (interactive)
    (execute-kbd-macro (read-kbd-macro "]]")))
  (defun my-evil-backward-section-end ()
    "Move to the previous section."
    (interactive)
    (execute-kbd-macro (read-kbd-macro "[[")))
  (when (fboundp 'evil-define-key)
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
      'my-evil-forward-section-end))

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

  (define-key evil-normal-state-map (kbd "C-s") 'my-evil-save)
  (define-key evil-insert-state-map (kbd "C-s") 'my-evil-save)
  (define-key evil-visual-state-map (kbd "C-s") 'my-evil-save)

  (defun evilclipboard-select-pasted ()
    "Visually select last pasted text."
    (interactive)
    (when (and (fboundp 'evil-goto-mark)
               (fboundp 'evil-visual-char))
      (evil-goto-mark ?\[)
      (evil-visual-char)
      (evil-goto-mark ?\])))
  (with-no-warnings
    (define-key evil-normal-state-map (kbd "gp") 'evilclipboard-select-pasted)
    (define-key evil-normal-state-map (kbd "<leader>gp") 'evilclipboard-select-pasted))

  (setopt evil-want-Y-yank-to-eol t)

  (when (fboundp 'evil-set-leader)
    (evil-set-leader 'normal (kbd ","))
    (evil-set-leader 'visual (kbd ",")))

  (with-eval-after-load 'eldoc
    (eldoc-add-command-completions "evilcursor-"))

  (when (fboundp 'evil-define-key)
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
    (evil-define-key 'normal 'global (kbd "<leader>ib") #'ibuffer))
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
  (when (fboundp 'evil-fill-and-move)
    (with-no-warnings
      (evil-define-operator my-evil-fill-and-move-operator (beg end)
        "Fill text and move point to the end of the filled region BEG and END.
This enhancement prevents the cursor from moving."
        :move-point nil
        :type line
        :restore-point t
        (save-excursion
          (evil-fill-and-move beg end))))
    (define-key evil-normal-state-map "gq" 'my-evil-fill-and-move-operator))

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

  ;; (evil-select-search-module 'evil-search-module 'evil-search)

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
  (define-key evil-normal-state-map (kbd "gdp") 'delete-pair)
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
  (define-key evil-normal-state-map (kbd "<leader>ww") 'my-wip)
  (define-key evil-normal-state-map (kbd "<leader>W")  'my-wip)

  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "gs") 'evilbuffer-switch-to-scratch-and-clear))

  ;; (when (fboundp 'my-dabbrev-completion-backwards)
  ;;   (setq evil-complete-next-func #'my-dabbrev-completion-backwards))
  ;;
  ;; (when (fboundp 'my-dabbrev-completion-forward)
  ;;   (setq evil-complete-previous-func #'my-dabbrev-completion-forward))
  ;; TODO use cape-dabbrev
  ;; (define-key evil-insert-state-map (kbd "C-p") 'my-dabbrev-completion-backwards)
  ;; (define-key evil-insert-state-map (kbd "C-n") 'my-dabbrev-completion-forward)

  (defun my-dabbrev-completion-forward-all-buffers (arg)
    (with-no-warnings
      (let ((dabbrev-check-all-buffers t))
        (dabbrev-completion arg)))))

(with-eval-after-load 'vertico
  (when (fboundp 'evil-define-key)
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
    (evil-define-key 'normal vertico-map (kbd "M-j") 'vertico-next))

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
             (with-eval-after-load 'evil
               (when (fboundp 'wgrep-finish-edit)
                 ;; TODO: save and restore the cursor: wgrep-finish-edit
                 (when (fboundp 'evil-define-key)
                   (evil-define-key 'normal 'local (kbd "C-s")
                     #'(lambda()
                         (interactive)
                         (lightemacs-save-window-start
                           (lightemacs-save-window-hscroll
                             (save-mark-and-excursion
                               (wgrep-finish-edit)
                               (wgrep-change-to-wgrep-mode))))))))))

         ;; Emacs >= 31
         (when (fboundp 'grep-change-to-grep-edit-mode)
           (grep-change-to-grep-edit-mode)
           (with-eval-after-load 'evil
             (when (fboundp 'grep-edit-save-changes)
               ;; TODO: save and restore the cursor: wgrep-finish-edit
               ;; (evil-define-key 'normal 'local (kbd "C-s")
               ;;   #'(lambda()
               ;;       (interactive)
               ;;       (grep-edit-save-changes)))
               ))))))

  (with-eval-after-load 'evil
    (when (fboundp 'evil-define-key)
      (evil-define-key 'normal 'global (kbd "<leader>ee") 'embark-dwim)
      (evil-define-key 'normal 'global (kbd "<leader>ew") 'embark-act))))



(with-eval-after-load 'vterm
  (with-eval-after-load 'evil
    (when (fboundp 'evil-define-key)
      (evil-define-key 'insert vterm-mode-map (kbd "M-H") 'my-vterm--send-Alt-Shift-H)
      (evil-define-key 'insert vterm-mode-map (kbd "M-L") 'my-vterm--send-Alt-Shift-L)))

  ;; Useful for nano
  ;;(define-key vterm-mode-map (kbd "C-c") 'vterm--self-insert)
  ;;(define-key vterm-mode-map (kbd "C-g") 'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-x") 'vterm--self-insert)

  (define-key vterm-mode-map (kbd "C-c C-c") 'vterm--self-insert)

  (with-eval-after-load 'evil
    (define-key vterm-mode-map (kbd "M-j") 'vterm--self-insert)
    (define-key vterm-mode-map (kbd "M-k") 'vterm--self-insert)
    (when (fboundp 'evil-define-key)
      (evil-define-key 'insert vterm-mode-map (kbd "M-j") 'vterm--self-insert)
      (evil-define-key 'insert vterm-mode-map (kbd "M-k") 'vterm--self-insert)))

  (define-key vterm-mode-map (kbd "M-H") 'my-vterm--send-Alt-Shift-H)
  (define-key vterm-mode-map (kbd "M-L") 'my-vterm--send-Alt-Shift-L))

;;; evil jump

(with-eval-after-load 'evil
  (defun eviljump-goto-definition-try-imenu-first (imenu-only)
    "Improved `evil-goto-definition` to open folds correctly in outline mode.
When IMENU-ONLY is nil it only uses imenu."
    (require 'xref)
    (if (and (fboundp 'evil-goto-definition)
             (fboundp 'xref-push-marker-stack))
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
              (evil-goto-definition))))
      (error "Undefined required functions")))

  (defun eviljump-goto-definition (&optional force-all)
    "Find definition and scroll line to top.
When FORCE-ALL is non-nil, use all functions."
    (interactive)
    (lightemacs-recenter-if-out-of-view
      (cond ((and (fboundp 'eglot-managed-p) (eglot-managed-p))
             (xref-find-definitions (thing-at-point 'symbol t)))

            ((and (boundp 'lsp-mode) lsp-mode (fboundp 'lsp-find-definition))
             (lsp-find-definition))

            ((and (not force-all) (derived-mode-p 'emacs-lisp-mode))
             ;; Do not jump to emacs.d. Only use imenu.
             (eviljump-goto-definition-try-imenu-first t))

            (t (eviljump-goto-definition-try-imenu-first nil)))))

  (defun eviljump-goto-definition-force ()
    "Go to definition."
    (interactive)
    (eviljump-goto-definition t))

  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal 'global (kbd "<leader>d") 'eviljump-goto-definition)
    (evil-define-key 'normal 'global (kbd "<leader>D") 'eviljump-goto-definition-force)
    ;; Causes bugs
    ;; (evil-define-key 'normal 'global (kbd "gd") 'eviljump-goto-definition)
    ;; (evil-define-key 'normal 'global (kbd "gD") 'eviljump-goto-definition-force)
    ))

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
  (if (and (fboundp 'evil-get-register)
           (fboundp 'evil-visual-paste)
           (fboundp 'evil-paste-before)
           (fboundp 'evil-set-register))
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
              (evil-set-register ?a original-register-contents)))))
    (error "Undefined required functions")))

(defun evilclipboard-paste-with-current-indentation-restore-point ()
  "Paste text from the clipboard with the current line's indentation.
This function also restores window start and point when pasting multiple lines."
  (interactive)
  (if (fboundp 'evil-paste-before)
      (if (minibufferp)
          (let ((evil-move-cursor-back nil))
            (evil-paste-before 1))
        (lightemacs-save-window-hscroll
          (lightemacs-save-window-start
            (save-mark-and-excursion
              (evilclipboard-paste-with-current-indentation)))))
    (error "Undefined required functions")))

;; (define-key evil-insert-state-map (kbd "C-a p") 'evilclipboard-paste-with-current-indentation-restore-point)
;; (define-key evil-insert-state-map (kbd "C-a C-p") 'evilclipboard-paste-with-current-indentation-restore-point)

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key 'insert 'global (kbd "C-v") 'evilclipboard-paste-with-current-indentation-restore-point)))

;;; Copy with without indentation

(defun evilclipboard-evil-yank-region-unindented ()
  "Copy the region, un-indented by the length of its minimum indent.
If numeric prefix argument PAD is supplied, indent the resulting
text by that amount."
  (interactive)
  (if (and (fboundp 'evil-yank)
           (fboundp 'evil-get-register)
           (fboundp 'evil-set-register))
      (when (use-region-p)
        (evil-yank (region-beginning) (region-end))
        (dolist (register '(?\" ?*))
          (let ((original-contents (evil-get-register register t)))
            (when original-contents
              (evil-set-register
               register (evil-clipboard--string-unindent
                         (substring-no-properties original-contents)))))))
    (error "Undefined required functions")))

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key 'visual 'global (kbd "C") 'evilclipboard-evil-yank-region-unindented)))

;;; evilwindow: split and select

(defun evilwindow-split-and-select-new-window (split-direction)
  "Split the window in the specified direction then switch to the new window.
SPLIT-DIRECTION is the direction (v or h).
By using this function, Emacs and Evil can mimic the behavior of Vim when the
user presses Ctrl-w v and Ctrl-w s. It prohibits Emacs from altering the cursor
position when the user presses Ctrl-v or Ctrl-s to create a new split and
guarantees that the new window is selected, as in Vim."
  ;; Save current buffer's cursor position and view
  (when (and (fboundp 'evil-window-vsplit)
             (fboundp 'evil-window-split))
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
      )))

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
             (fboundp 'kirigami-open-fold)
             (fboundp 'evil-goto-line))
    (let ((file-buffer (find-buffer-visiting file-path)))
      (if file-buffer (switch-to-buffer file-buffer)
        (find-file file-path)
        (kirigami-close-folds)
        (kirigami-open-fold)
        (evil-goto-line 1)))))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-w v") 'evilwindow-split-select-right)
  (define-key evil-normal-state-map (kbd "C-w s") 'evilwindow-split-select-below))

;;; evil: browse-url

(defun my-evil-browse-url-copy-to-clipboard (url &optional _args)
  "Copy the URL to the clipboard instead of opening it."
  (when (fboundp 'evil-set-register)
    (dolist (register '(?\" ?*))
      (evil-set-register register url))
    (message "URL copied to clipboard: %s" url)))

(with-eval-after-load 'evil
  ;; Override the browse-url function
  (setq browse-url-browser-function 'my-evil-browse-url-copy-to-clipboard))

;;; evil consult

(defun my-consult-fd-project ()
  "Run `consult-fd` in the root directory of the current project."
  (interactive)
  (require 'consult)
  (let* ((project (project-current nil "."))
         (project-root (when (and project
                                  (fboundp 'project-root))
                         (project-root project)))
         (consult-fd-args (concat (if (boundp 'consult-fd-args)
                                      consult-fd-args
                                    "")
                                  " --threads "
                                  (number-to-string (num-processors)))))
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

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "<leader>ff") 'my-consult-imenu)
  (define-key evil-normal-state-map (kbd "<leader>m") 'consult-recent-file)
  (define-key evil-normal-state-map (kbd "<leader>b") 'consult-recent-file)
  ;; (define-key evil-normal-state-map (kbd "<leadrr>B") 'switch-to-buffer)
  (define-key evil-normal-state-map (kbd "<leader>B") 'consult-buffer)
  (define-key evil-normal-state-map (kbd "M-/") 'consult-line)

  (define-key evil-normal-state-map (kbd "C-p") 'my-consult-fd-project)

  ;; (define-key evil-normal-state-map (kbd "C-p") 'consult-fd)
  )

(defun my-consult-grep-dir (&optional dir)
  "Execute ripgrep in the current directory, using the selection if available.
DIR is the directory."
  (interactive)
  (require 'consult)
  (let ((selection (when (use-region-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))))
        (consult-ripgrep-args (concat (if (fboundp 'consult-ripgrep-args)
                                          consult-ripgrep-args
                                        "")
                                      " --threads "
                                      (number-to-string (num-processors)))))
    (when selection
      (save-excursion
        (deactivate-mark)))

    (save-some-buffers t)
    ;; (buffer-guardian-save-all-buffers)
    (when (fboundp 'consult-ripgrep)
      (consult-ripgrep (or dir (buffer-cwd)) selection)
      (error "Undefined: consult-ripgrep"))))

(defun my-consult-grep-project ()
  "Run `consult-fd` in the root directory of the current project."
  (interactive)
  (require 'consult)
  (let* ((project (project-current nil "."))
         (project-root (when (and project (fboundp 'project-root))
                         (project-root project))))
    (my-consult-grep-dir (or project-root (buffer-cwd)))))

(with-eval-after-load 'evil
  (when (and (fboundp 'evil-define-key)
             (fboundp 'evil-define-key*))
    ;; (evil-define-key 'normal 'global (kbd "gR") 'my-consult-grep-project)
    ;; (evil-define-key 'normal 'global (kbd "gr") 'my-consult-grep-dir)
    (evil-define-key 'normal 'global (kbd "<leader>gR") 'my-consult-grep-dir)
    (evil-define-key 'normal 'global (kbd "<leader>gr") 'my-consult-grep-project)))

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
  (when (fboundp 'evil-delete-back-to-indentation)
    (let ((cur-point (point)))
      (save-excursion
        (if (re-search-backward
             "^\\(*+\\)\\( +\\)\\([A-Z]\\{2,\\}\\)?\\( +\\)?"
             (line-beginning-position) t)
            (let ((delete-start (match-end 0)))
              (delete-region delete-start cur-point))
          (evil-delete-back-to-indentation))))))

(with-eval-after-load 'org
  (with-eval-after-load 'evil
    (when (fboundp 'evil-set-initial-state)
      (evil-set-initial-state 'org-agenda-mode 'motion)
      (evil-set-initial-state 'org-agenda-mode 'normal))

    (when (fboundp 'evil-define-key)
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

            (when (fboundp 'evilbuffer-clear-highlights)
              (evilbuffer-clear-highlights))))

      (evil-define-key 'insert org-mode-map (kbd "C-u") 'my-evil-delete-to-heading-star)

      ;; Equivalent to C-c C-q
      (evil-define-key 'normal org-mode-map (kbd "<leader>oo") 'org-set-tags-command)
      (evil-define-key 'normal org-mode-map (kbd "<leader>xx") 'org-babel-execute-maybe)
      (evil-define-key 'normal org-mode-map (kbd "<leader>cd") 'my-org-todo-and-toggle)
      ;; (evil-define-key 'normal org-mode-map (kbd "<leader>xx") 'org-edit-src-code)
      ;; (evil-define-key 'normal org-src-mode-map (kbd "<leader>xx") 'org-edit-src-exit)
      )))

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

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key 'insert 'global (kbd "C-p") 'cape-dabbrev)
    (evil-define-key 'insert 'global (kbd "C-n") 'cape-dabbrev)))

;;; cape: evil

(with-eval-after-load 'evil
  (defun my-minibuffer-setup-dabbrev-evil ()
    "Bind `C-p' and `C-n' to dabbrev completion in minibuffer using evil."
    (when (fboundp 'evil-define-key)
      (evil-define-key 'insert 'local
        (kbd "C-p") 'cape-dabbrev
        (kbd "C-n") 'cape-dabbrev))

    ;; (when (and (boundp 'completion-at-point-functions)
    ;;            (listp completion-at-point-functions))
    ;;   (add-hook 'completion-at-point-functions #'cape-dabbrev nil t))
    )

  (with-eval-after-load 'cape
    (when (fboundp 'evil-define-key)
      (evil-define-key 'insert 'global (kbd "C-x C-f") 'cape-file))))

(with-eval-after-load 'cape
  ;; Emulate Vim's C-x C-f
  ;; TODO lightemacs?
  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-dabbrev-evil)

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

(with-eval-after-load 'corfu
  ;; (add-hook 'evil-mode-hook
  ;;           #'(lambda()
  ;;               (with-eval-after-load "evil"
  ;;                 ;; Emulate Vim's C-x C-f
  ;;                 (evil-define-key 'insert 'global (kbd "C-x C-f") 'cape-file))))
  (with-eval-after-load "evil"
    (when (fboundp 'evil-define-key)
      (evil-define-key 'insert 'global (kbd "C-SPC") 'completion-at-point))

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
      (when (fboundp 'evil-insert)
        (with-current-buffer python-buffer
          (evil-insert 1))))))

(defun evilinferior-mode-clear ()
  "Equivalent to clear the screen in a terminal."
  (interactive)
  ;; (evil-insert-state)
  (recenter 0))

(with-eval-after-load 'evil
  (defun evilinferior-setup ()
    "Set up keybinding for recentering in inferior modes."
    ;; (evil-define-key 'normal 'local (kbd "M-k") 'comint-previous-input)
    ;; (evil-define-key 'normal 'local (kbd "M-j") 'comint-next-input)
    (when (fboundp 'evil-define-key)
      (evil-define-key 'normal 'local (kbd "C-l") 'evilinferior-mode-clear)
      (evil-define-key 'insert 'local (kbd "C-l") 'evilinferior-mode-clear)))

  ;; Python
  (add-hook 'inferior-python-mode-hook 'evilinferior-setup)
  (add-hook 'ielm-mode-hook 'evilinferior-setup)

  ;; M-DEL and C-<backspace> in Emacs modify the kill ring. The following
  ;; makes both key mappings use the Evil version that does not modify the
  ;; kill ring.
  (when (fboundp 'evil-delete-backward-word)
    (global-set-key (kbd "C-<backspace>") #'evil-delete-backward-word)
    (global-set-key (kbd "M-DEL") #'evil-delete-backward-word))

  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal 'global (kbd "<leader>ep") 'evilinferior-run-python)
    (evil-define-key 'normal 'global (kbd "<leader>el") 'ielm)))

;;; evil intercept (TODO replace with global?)

(with-eval-after-load 'evil
  (if (and (fboundp 'evil-make-intercept-map)
           (fboundp 'evil-get-auxiliary-keymap)
           (fboundp 'evil-define-key))
      (progn
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
          (kbd "M-RET") 'toggle-term-tmux
          (kbd "M-<enter>") 'toggle-term-tmux
          (kbd "M-<return>") 'toggle-term-tmux
          (kbd "M-o") 'my-previous-interesting-buffer
          (kbd "M-i") 'my-next-interesting-buffer
          ;; (kbd "M-=") 'global-text-scale-adjust
          (kbd "C--") 'text-scale-decrease
          (kbd "C-+") 'text-scale-increase
          (kbd "C-S-k") 'my-tab-bar-move-tab-backward
          (kbd "C-S-j") 'my-tab-bar-move-tab
          (kbd "C-k") 'my-tab-previous
          (kbd "C-j") 'my-tab-next)

        (if (fboundp 'my-intercept-mode)
            (my-intercept-mode 1)
          (error "Undefined required functions")))
    (error "Undefined required functions")))

;;; flyspell region or buffer

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

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key 'visual 'global (kbd "<leader>fs") 'my-flyspell-region)
    (evil-define-key 'normal 'global (kbd "<leader>fb") 'my-flyspell-buffer)
    (evil-define-key 'normal 'global (kbd "<leader>fc") 'my-flyspell-clear)))

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

(with-eval-after-load 'evil
  (define-key evil-visual-state-map (kbd "M-j") 'move-region-down)
  (define-key evil-visual-state-map (kbd "M-k") 'move-region-up))

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
                (when (fboundp 'evil-define-key)
                  (evil-define-key 'normal 'global (kbd "gV")
                    #'my-elisp-mode-select-sexp))

                (add-hook 'evil-insert-state-exit-hook
                          #'(lambda() (when (and
                                             (fboundp 'evil-insert-state-p)
                                             (evil-insert-state-p))
                                        (my-check-parens-no-jump t)))
                          nil t))
              (add-hook 'after-save-hook #'my-check-parens-no-jump -99 t)))

;;; markdown mode

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
  (visual-line-mode 1)

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
  (if (and (fboundp 'evil-ex-make-search-pattern)
           (fboundp 'evil-push-search-history)
           (fboundp 'evil-ex-delete-hl)
           (fboundp 'evil-ex-search-next))
      (progn
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
    (error "Undefined required evil functions")))

(defun evilbuffer-search-symbol (&optional direction)
  "Search for the symbol at point using Evil search.
The DIRECTION argument can be either \='forward' or \='backward, determining the
search direction (default: \='forward)."
  (interactive)
  (if (and (fboundp 'evil-visual-state-p)
           (fboundp 'evil-exit-visual-state)
           (fboundp 'evil-ex-search))
      (progn
        ;; Both `save-window-excursion' and `save-excursion' are used here to ensure
        ;; that performing the search does not move the cursor or change the visible
        ;; portion of the buffer (`window-start'). This allows Evil's search functions
        ;; to update highlights and internal search state while leaving the user's
        ;; point and window view completely unchanged.
        (save-window-excursion ; Preserve window-start
          (save-excursion ; Preserve point and mark
            (let* ((visual-p (evil-visual-state-p))
                   (count 1)
                   (direction (or direction 'forward))
                   (text (if visual-p
                             (let ((selection (buffer-substring-no-properties
                                               (region-beginning)
                                               (region-end))))
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
                         (string= regex (and evil-ex-search-pattern
                                             (car evil-ex-search-pattern))))
                    ;; Instead of calling le-evil--search-regexp for the same
                    ;; keyword, just highlight the current keyword
                    (condition-case err
                        (progn
                          (evil-ex-search 1))
                      (search-failed
                       nil))
                  (progn
                    (if (eq direction 'forward)
                        (goto-char (point-min))
                      (goto-char (point-max)))

                    ;; let ((lightemacs-maybe-recenter-after-jump nil))
                    (le-evil--search-regexp regex
                                            direction
                                            count))
                  t))))))
    (error "Undefined required functions")))

(defun le-evil--search-symbol-backwards ()
  "Search for the symbol at point using Evil search.
The DIRECTION argument can be either `forward' or `backward', determining the
search direction (default: \='forward)."
  (interactive)
  (evilbuffer-search-symbol 'backward))

(with-eval-after-load 'evil
  ;; Key mappings
  (define-key evil-normal-state-map (kbd "*") 'evilbuffer-search-symbol)
  (define-key evil-visual-state-map (kbd "*") 'evilbuffer-search-symbol)
  (define-key evil-normal-state-map (kbd "#") 'le-evil--search-symbol-backwards)
  (define-key evil-visual-state-map (kbd "#") 'le-evil--search-symbol-backwards)
  (define-key evil-visual-state-map (kbd "?") 'le-evil--search-symbol-backwards))

;;; Provide

(provide 'my-config-evil)

;; Local variables:
;; byte-compile-warnings: (not lexical free-vars)
;; End:

;;; my-config-evil.el ends here

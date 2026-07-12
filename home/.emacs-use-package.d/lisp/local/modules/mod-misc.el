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

;; Miscellaneous.

;;; Code:

;;; Require

(require 'my-defun)
(require 'cl-lib)
(eval-and-compile
  (require 'lightemacs-use-package))
(require 'seq)
(require 'my-defun)

;;; Modeline

(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))
(setq mode-line-percent-position nil)

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
                ;; Inclusion of major and minor modes
                ;; "  |  "
                ;; mode-line-modes
                (:eval
                 (let ((ref (bound-and-true-p diff-hl-reference-revision)))
                   (when (and (bound-and-true-p diff-hl-mode) ref)
                     (format "  |  diff-hl-ref:%s" ref))))
                "  |  "
                (:eval (my-gc-cons-threshold-mode-line))
                ;; mode-line-modes
                ;; Slow eval
                (:eval (mode-line-right))))

;;; Other modules

(unless noninteractive
  ;; Optional
  (require 'mod-misc2 nil t))

(unless noninteractive
  (with-eval-after-load 'evil
    (with-eval-after-load 'evil-collection
      (require 'my-config-evil))))

;; Automatically resizes all windows proportionally when splitting or deleting a
;; window. This prevents new windows from taking all the space from the current
;; window, maintaining a balanced layout across your frame.
(setq window-combination-resize t)

;; Disable displaying a bookmark icon on the fringe. Removing this icon reduces
;; visual clutter in the margins, especially if you use bookmarks frequently and
;; prefer a minimalist interface.
(setq bookmark-fringe-mark nil)

;; t is bad for accessibility HTML email in dark themes. Disabling custom colors
;; in HTML rendering ensures the text uses your active theme's colors,
;; preventing unreadable situations like dark text on a dark background.
(setq shr-use-colors nil)

;; Use `variable-pitch-mode' instead. Disabling custom fonts ensures HTML
;; documents do not override your preferred Emacs typography, maintaining a
;; consistent reading experience.
(setq shr-use-fonts nil)

;; (setq lazy-count-prefix-format "(%s/%s) ")

;; When stepping through search results, pressing the up or down arrow normally
;; interrupts the search. This setting allows directional keys to automatically
;; reverse or advance the search direction.
(setq isearch-motion-changes-direction t)

;; TODO minimal-emacs
;; Adds an explicit count of matches (e.g., [3/12]) directly into your
;; minibuffer search line, exactly like modern text editors or web browsers.
(setq isearch-lazy-count t
      lazy-count-suffix-format " (%s/%s)")

;; (setq isearch-allow-motion t)

;; Prevents isearch from stubbornly freezing at the end of a buffer match before
;; wrapping. It keeps navigation continuous and allows you to use standard
;; scrolling actions while remaining inside a search block.
(setq isearch-wrap-pause 'no
      isearch-allow-scroll 'unlimited)

(setq resize-mini-windows t)

;; (setq switch-to-buffer-obey-display-actions t)

;; Setting this to nil prevents Emacs from "snapping" the viewport to fit the
;; entire line when point moves to a partially visible line.
;; Pros: Enables true smooth/pixel scrolling; prevents jarring UI jumps
;;       when navigating past large images or long wrapped blocks of text.
;; Cons: The cursor can technically be on a line that is only half-visible
;;       at the very top or bottom edge of the window.
;; Recently disabled. Causes issues?
;; (setq make-cursor-line-fully-visible t)

;; The Problem: If you scroll down a file and land on a line that is only 90%
;; visible at the bottom of the window, vanilla Emacs will violently "snap" the
;; entire screen to force that line into full view. This destroys smooth
;; scrolling.
;; Why it has no tradeoff: Note: Your snippet had this set to t, which is the
;; vanilla default. Changing it to nil is universally preferred by users who
;; want modern, predictable scrolling behavior, especially when dealing with
;; large images in Org-mode or long wrapped paragraphs.
;; TODO minimal emacs?
(setq make-cursor-line-fully-visible nil)

;; also useful for org
(setq imenu-max-items 30)

;; (setq lazy-highlight-initial-delay 0.5)

;; (ls-lisp-use-insert-directory-program nil)
;; (ls-lisp-use-insert-directory-program t)      ;; use external ls

;; (setq fit-window-to-buffer-horizontally t)

;; Prevent version control async commands (like "git pull --stat") from popping
;; up new windows when upgrading Emacs packages. This works by temporarily
;; setting the internal vc-dispatcher variable 'vc--inhibit-async-window' to t
;; strictly during the execution of package upgrade commands.
(defun my-inhibit-vc-async-window-around-advice (orig-fun &rest args)
  "Inhibit VC async windows during package upgrades.
ORIG-FUN is the original upgrade function, and ARGS are its arguments."
  (let ((vc--inhibit-async-window t))
    (ignore vc--inhibit-async-window)
    (apply orig-fun args)))

;; Apply the advice to the built-in package and package-vc upgrade commands.
(with-eval-after-load 'package
  (advice-add 'package-upgrade :around #'my-inhibit-vc-async-window-around-advice)
  (advice-add 'package-upgrade-all :around #'my-inhibit-vc-async-window-around-advice))

(with-eval-after-load 'package-vc
  (advice-add 'package-vc-upgrade :around #'my-inhibit-vc-async-window-around-advice)
  (advice-add 'package-vc-upgrade-all :around #'my-inhibit-vc-async-window-around-advice))

;;; delete outdated early-init.elc

(with-eval-after-load 'compile-angel
  ;; TODO Remove this from the compile-angel defaults
  (setq compile-angel-excluded-path-suffixes
        (delete "/early-init.el" compile-angel-excluded-path-suffixes)))

(let ((early-init-el (expand-file-name "early-init.el" lightemacs-user-directory))
      (early-init-elc (expand-file-name "early-init.elc" lightemacs-user-directory)))
  (when (and (file-exists-p early-init-elc)
             ;; (file-exists-p early-init-el) ; Not necessary
             (file-newer-than-file-p early-init-el early-init-elc))
    (message "[AUTO DELETE] %s" early-init-elc)
    (delete-file early-init-elc)))

;;; compile-angel timer (test)

;; (defun my-disable-compile-angel-after-delay ()
;;   "Disable `compile-angel-on-load-mode' 60 seconds after Emacs startup."
;;   (run-at-time
;;    (* 30 60) nil
;;    (lambda ()
;;      (when (bound-and-true-p compile-angel-on-load-mode)
;;        (when (fboundp 'compile-angel-on-load-mode)
;;          (compile-angel-on-load-mode -1)
;;          (let ((inhibit-message t))
;;            (message "compile-angel-on-load-mode disabled after 60 seconds")))))))
;;
;; ;; Activate the timer at startup
;; (add-hook 'emacs-startup-hook #'my-disable-compile-angel-after-delay)

;;; compile-angel on save local hooks

;; TODO lightemacs target hooks
;; (with-eval-after-load 'le-compile-angel
;;   (add-hook 'lightemacs-after-init-hook 'compile-angel-on-save-mode))

;;; compile-angel function report

;; TODO: Useless?
;; (defun compile-angel--get-list-non-native-compiled-functions ()
;;   "Return a list of loaded Elisp function symbols that are not natively compiled."
;;   (let (result)
;;     (mapatoms
;;      (lambda (sym)
;;        (when (and (fboundp sym)
;;                   (functionp sym))
;;          (let ((def (symbol-function sym)))
;;            (when (and (not (eq (car-safe def) 'autoload))
;;                       (not (subr-primitive-p def))
;;                       (if (fboundp 'subr-native-elisp-p)
;;                           (not (subr-native-elisp-p def))
;;                         t))
;;              (push sym result))))))
;;     (sort result #'string-lessp)))

;; TODO add to compile angel
(defun compile-angel--get-list-non-native-compiled-functions ()
  "Return a list of loaded Elisp function symbols that are not natively compiled."
  (let (result)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (functionp sym))
         (let ((sym-name (symbol-name sym))
               (def (indirect-function sym))) ; Resolve aliases to their true definition
           (when (and def
                      ;; Filter out generalized variable setters
                      (not (string-prefix-p "(setf " sym-name))
                      ;; Filter out runtime dynamically generated function patterns
                      (not (string-prefix-p "vterm-send-" sym-name))
                      (not (string-prefix-p "orgtbl-hijacker-command-" sym-name))
                      (not (string-prefix-p "recentf-open-most-recent-file-" sym-name))
                      ;; Filter out macros and autoload stubs
                      (not (eq (car-safe def) 'autoload))
                      (not (eq (car-safe def) 'macro))
                      ;; Filter out C primitives
                      (not (subr-primitive-p def))
                      ;; Verify native compilation status status
                      (if (fboundp 'subr-native-elisp-p)
                          (not (subr-native-elisp-p def))
                        t))
             (push sym result))))))
    (sort result #'string-lessp)))

;;;###autoload
(defun compile-angel-report-functions ()
  "Create a buffer listing all loaded Elisp functions that are not native compiled."
  (interactive)
  (with-current-buffer (get-buffer-create "*compile-angel:functions-report*")
    (let ((inhibit-read-only t))
      (read-only-mode 1)
      (erase-buffer)
      (insert "Non-natively compiled functions:\n")
      (insert "--------------------------------\n\n")
      (goto-char (point-min))

      (pop-to-buffer (current-buffer))

      (save-excursion
        (goto-char (point-max))
        (let ((count 0))
          (dolist (func (compile-angel--get-list-non-native-compiled-functions))
            (setq count (1+ count))
            (insert (format "- %s\n" func)))

          (if (= count 0)
              (insert "(All loaded Elisp functions have been natively compiled.)")
            (insert (format
                     "\n(%s function%s %s NOT successfully natively compiled.)"
                     count
                     (if (< count 2) "" "s")
                     (if (< count 2) "was" "were")))))))))

;;; testing

;; Warns about undefined commands in the prompt (Emacs 29.1)
(setq shell-highlight-undef-enable t)

;; Automatically cleans up dead shell buffers (Emacs 29.1)
(setq shell-kill-buffer-on-exit t)

(setq ;; completion-styles '(partial-completion flex initials)
 completion-ignore-case t
 read-buffer-completion-ignore-case t
 ;; Ignore case in file and buffer completions
 read-file-name-completion-ignore-case t
 completions-format 'one-column
 completions-max-height 20
 completion-show-help nil
 completion-show-inline-help nil)
(setq icomplete-separator "\n")
(setq icomplete-delay-completions-threshold 0)
(setq icomplete-compute-delay 0)
(setq icomplete-prospects-height 10)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-with-completion-tables t)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-max-delay-chars 0)
(setq icomplete-tidy-shadowed-file-names t)
(setq icomplete-scroll t)
(setq icomplete-in-buffer t)
(setq completion-auto-select nil  ; Alternative: 'second-tab
      completions-detailed t
      completions-group t
      completions-group-sort 'alphabetical)
;; (setq icomplete-vertical-in-buffer-adjust-list t)
;; (setq icomplete-vertical-prospects-height (/ (frame-height) 5))
;; (setq icomplete-vertical-render-prefix-indicator t)
;; (setq icomplete-vertical-selected-prefix-indicator   " @ ")
;; (setq icomplete-vertical-unselected-prefix-indicator "   ")

;; Text properties inflate the size of recentf's files, and there is no purpose
;; in persisting them.
(with-eval-after-load 'recentf
  (add-to-list 'recentf-filename-handlers #'substring-no-properties -80))

;; If you spend any time on a Windows machine, the default file I/O layer is
;; punishing. Turning off true file attributes and boosting the pipe buffer size
;; dramatically increases responsiveness for sub-processes like Git, compilers,
;; and LSPs.
(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)))

;; The benefit of visual-order-cursor-movement t is that when editing text
;; containing both left-to-right and right-to-left scripts, cursor
;; movement aligns with how the text is visually presented on the screen.
;; This means pressing C-f moves the cursor to the character visually to
;; the right, and C-b moves it to the character visually to the left,
;; regardless of the underlying logical (buffer) order. It provides a
;; navigation model that matches human reading habits in mixed-script
;; documents, reducing cognitive load and making cursor movement
;; predictable in visually complex bidirectional contexts.
;;
;; NOTE: Setting visual-order-cursor-movement to t forces Emacs to do extra
;; computational work every time you move the cursor.
;; (setq visual-order-cursor-movement nil)

;; C-x =
;; What it does: When you press C-x = (what-cursor-position), Emacs displays
;; technical details about the character under the point in the echo area.
;; Setting this to t appends the official, full Unicode name of the character to
;; that string. Verdict: Nice to have, but unnecessary. If you are tracking down
;; weird invisible spacing bugs or non-ASCII quotes (like ’ vs '), it helps.
;; Otherwise, it just adds verbosity to the echo line.
(setq what-cursor-show-names t)

(setq help-clean-buttons t)
(setq help-enable-variable-value-editing t)

;; Allow drag and drop out of dired into other apps (e.g. browser)
(setq dired-mouse-drag-files t)

;; Tell dired-x to not bind "I" key to `dired-info' or "N" to `dired-man'
(setq dired-bind-info nil)
(setq dired-bind-man nil)

(when (display-graphic-p)
  (set-display-table-slot standard-display-table 'vertical-border ?\u2502)
  (set-display-table-slot standard-display-table 'truncation ?\u2192))

(setq save-silently t)

;; Automatically enable ANSI color support in compilation buffers
;; by parsing and applying ANSI escape sequences during output filtering.
(setq ansi-color-for-compilation-mode t)

;; Alternatively, explicitly add the ANSI color filter to the compilation filter hook
;; to apply colors immediately during compilation output processing.
;; This is equivalent to the above `setq`, but does not depend on `compilation-mode` being loaded.
;; (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; (setq compilation-context-lines 10)  ; not good
;; (setq compilation-skip-threshold 2)
;; (setq compilation-window-height 100)
;; (setq compilation-scroll-output nil)

;; (setq read-process-output-max ; Increase single chunk bytes to read from subprocess (def. 4096)
;;       (condition-case nil
;;           (with-temp-buffer ; On GNU/Linux systems, the value should not exceed `pipe-max-size'
;;             (insert-file-contents "/proc/sys/fs/pipe-max-size")
;;             (string-to-number (buffer-string)))
;;         (error (* 1024 1024))))

;; TODO minimal-emacs?
;; (setq bookmark-watch-bookmark-file 'silent)
;; (setq bookmark-use-annotations nil)
;; (setq bookmark-automatically-show-annotations nil)

;; (setq rainbow-delimiters-max-face-count 5)

;; TODO are these useful?
;; (unless *sys/win32*
;;   (set-selection-coding-system 'utf-8)
;;   (prefer-coding-system 'utf-8)
;;   (set-language-environment "UTF-8")
;;   (set-default-coding-systems 'utf-8)
;;   (set-terminal-coding-system 'utf-8)
;;   (set-keyboard-coding-system 'utf-8)
;;   (setq locale-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first, compound text next
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq next-error-message-highlight 'keep)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
;; TODO: add it to minimal-emacs.d
;; (when (display-graphic-p)
;;   (setq x-select-request-type
;; (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; This variable specifies a function for splitting a window, in order
;; to make a new window for displaying a buffer. It is used by the
;; display-buffer-pop-up-window action function to actually split the
;; window.  The value must be a function that takes one argument, a
;; window, and returns either a new window (which will be used to
;; display the desired buffer) or nil (which means the splitting
;; failed). The default value is split-window-sensibly, which is
;; documented next.
(setq split-window-preferred-function nil)

;; (setq read-minibuffer-restore-windows t) ; Emacs 28

;; GPM mouse support is strictly for TTY consoles.
(with-eval-after-load 't-mouse
  (when (bound-and-true-p gpm-mouse-mode)
    (gpm-mouse-mode -1))
  (setq-default gpm-mouse-mode nil))

;; Useless for Evil users: This mode modifies minibuffer syntax tables for regex
;; navigation. Since Evil provides its own regex tools and operators that
;; operate independently of these minibuffer-specific highlighting side-effects,
;; this mode is redundant and can interfere with custom Evil keybindings.
(with-eval-after-load 'minibuffer
  (when (bound-and-true-p minibuffer-regexp-mode)
    (minibuffer-regexp-mode -1))
  (setq-default minibuffer-regexp-mode nil))

;; In standard (vanilla) Emacs, you do not select text by shifting into a visual
;; mode. Instead, you drop an anchor called the "mark" by pressing C-SPC, and
;; then move your cursor. The text between the mark and your cursor becomes your
;; active selection.
;;
;; By default, Emacs uses `transient-mark-mode' to highlight this selection
;; visually, making it look like a standard modern text editor.
;;
;; If you use Evil (Vim bindings), this native highlighting gets in the way.
;; Evil handles text selection through its own Visual states (v, V, C-v). If
;; Emacs is also trying to highlight text in the background based on where your
;; last mark was dropped, the two systems create conflicting visual noise.
;;
;; Disabling `transient-mark-mode' stops Emacs from painting the screen with
;; highlights. Pressing C-SPC goes back to being a silent utility: it just drops
;; an invisible location bookmark that you can jump back to later, while you
;; leave all the actual visual text selection to Evil.
(setq-default transient-mark-mode nil)
;; (with-eval-after-load 'simple
;;   (when (bound-and-true-p transient-mark-mode)
;;     (transient-mark-mode -1)))

(setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)

;; TODO is eager better?
;; compose is broken. It makes erases the content of the message the help
;; buffer.
;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose)

(setq electric-quote-comment nil)
(setq electric-quote-string nil)

(setq diff-add-log-use-relative-names t)

;; (setq package-native-compile t)

;; TODO patch dumb-jump
;; Push the current position to the Xref marker stack right before dumb-jump
;; calculates a definition search. This guarantees that your original location
;; is saved to the history (allowing you to return via M-,), even if external
;; completion frameworks or incomplete backend metadata interrupt the native
;; Xref history mechanism.
(with-eval-after-load 'dumb-jump
  (cl-defmethod xref-backend-definitions :before ((_backend (eql dumb-jump))
                                                  _identifier)
    (when (fboundp 'xref-push-marker-stack)
      (xref-push-marker-stack))))

(setq archive-hidden-columns '(Mode Ids Date&Time Ratio))
(setq archive-alternate-hidden-columns '())

(setq redisplay-skip-fontification-on-input nil)

;; Maximizes screen real estate by hiding the mode-line.
(setq-local redisplay-skip-fontification-on-input nil)
(setq fast-but-imprecise-scrolling nil)
;; (setq scroll-step 1)

(setq font-lock-maximum-decoration t)

;; Auto-scroll to bottom only when you type, not when output arrives

;; Expands history commands like !! or !$ before execution
(setq-default comint-input-autoexpand 'input)

(setq-default comint-scroll-to-bottom-on-input t)
;; (setq-default comint-scroll-to-bottom-on-output nil)

;; Prevent duplicates in your shell history
(setq comint-input-ignoredups t)

;; Leaves the cursor at the end of the newly duplicated text block.
(setq duplicate-line-final-position -1 ; both are Emacs 29
      duplicate-region-final-position -1)

;; NOTE disabled recently
;;
;; Emacs packages are very verbose. Language servers, version control
;; operations, and background linters (like Flymake) constantly print to the
;; *Messages* buffer.
;;
;; The Real Benefit: If you run Emacs as a daemon or keep it open for days, the
;; default 1000-line limit is overwritten quickly. If you experience a sudden UI
;; freeze or a package fails silently, a limit of 16384 ensures the error trace
;; from three hours ago is still there for you to read. It turns your log into a
;; reliable diagnostic tool instead of a fleeting ticker.
(setq message-log-max 16384)

;; Emacs drops a mark in the global ring every time you jump across files, such
;; as when using xref-find-definitions to trace Python or Elisp functions.
;;
;; The Real Benefit: If you are exploring a large codebase and following a call
;; stack through a dozen files, the default limit of 16 means your earliest
;; marks are erased. A limit of 512 turns your global mark ring into an infinite
;; "back button" for your entire project. You can pop the mark continuously to
;; retrace your steps and return to your exact starting point, regardless of how
;; many files you visited.
(setq global-mark-ring-max 512)

;; In Emacs, almost every deletion command (such as killing a word, killing a
;; line, or deleting a sentence) saves the text to the clipboard history.
;;
;; The Real Benefit: If you copy a block of code to paste elsewhere, but then
;; delete 70 individual lines to clean up a buffer, the default limit of 60 will
;; push your copied code out of memory before you can paste it. A limit of 1024
;; prevents this data loss. It allows you to use completion frameworks to search
;; for text you cut hours ago, treating your clipboard as a safe, long-term
;; scratchpad rather than a fragile queue.
(setq kill-ring-max 1024)
(setq mark-ring-max 32)

;; If this variable is t, splitting a window tries to get the space
;; proportionally from all windows in the same combination.  This also
;; allows splitting a window that is otherwise too small or of fixed size.
;; Resizing and deleting a window proportionally resize all windows in the
;; same combination.
;;
;; Emacs can balance window sizes automatically, but you can turn this behavior
;; off by setting even-window-sizes to nil: This prevents Emacs from resizing
;; existing windows to match the size of a new split window.
(setq even-window-sizes nil)
(setq even-window-sizes 'height-only)

;; Kills the entire line plus the newline character
;; kills the entire line plus the newline
;; (setq kill-whole-line t)

;; Deletes all contiguous whitespace characters at once, preventing repeated
;; backspace presses.
;;
;; 'hungry' deletes all consecutive whitespace characters at once
;; Alternatives is: 'all (remove all consecutive whitespace characters, even newlines).
(setq backward-delete-char-untabify-method 'hungry)

;; Prevent prompting for identifier when running xref-find-* commands, including
;; xref-find-references, which enables faster navigation to all usages of the
;; symbol at point without manual input
;;
;; Default: (not xref-find-definitions xref-find-definitions-other-window
;; xref-find-definitions-other-frame)
;;
;; The Problem: When you place your cursor over a function and press M-?
;; (xref-find-references), vanilla Emacs stops and prompts you in the
;; minibuffer: Find references for: [Function Name]. You have to press RET to
;; confirm, adding an unnecessary keystroke to a highly repetitive action.
;;
;; Why it has no tradeoff: It makes code navigation immediate. If you ever need
;; to search for a symbol that is not under your cursor, you can simply call the
;; command with a prefix argument (C-u M-?), and Emacs will prompt you normally.
;; TODO: minimal-emacs.d
(setq xref-prompt-for-identifier
      '(not xref-find-definitions
            xref-find-definitions-other-window
            xref-find-definitions-other-frame
            ;; This adds the following
            ;; This setting skips the redundant prompt asking you to confirm the
            ;; identifier under the cursor, making code navigation immediate.
            xref-find-references))

;; Decrease verbosity and use faster connection methods
;; (setq tramp-verbose 2)
;; (setq tramp-use-connection-share t)
;; (setq tramp-use-scp-direct-remote-copying t)

;; TODO: package-upgrade
(defun my-enable-package-review-policy ()
  "Enable package review policy."
  (setq ; package-review-policy t
   package-review-diff-command '("git" "--no-pager" "diff"
                                 "--no-ext-diff"
                                 "--no-index"
                                 "--color=never"
                                 "--diff-filter=d")))

(add-hook 'after-init-hook #'my-enable-package-review-policy)

(setq-default search-invisible nil)

;; Enable automatic buffer refresh when VC-controlled files change externally.
;; (setq auto-revert-check-vc-info t)

;;; Default modes that I disabled

(when (bound-and-true-p global-eldoc-mode)
  (global-eldoc-mode -1))
(setq-default global-eldoc-mode nil)

(when (bound-and-true-p show-paren-mode)
  (show-paren-mode -1))
(setq-default show-paren-mode nil)

;; Disable Remote File Checks if Not Needed
(setq-default tramp-mode nil)

;;; Disabled defaults

;; Truncate the compilation buffer to avoid excessive memory use by limiting its
;; size. It removes lines from the beginning of the buffer when it exceeds
;; `comint-buffer-maximum-size'.
(autoload 'comint-truncate-buffer "comint" nil t)
(add-hook 'compilation-filter-hook #'comint-truncate-buffer)

;; (setq comint-completion-autolist t)
;; (setq comint-input-ignoredups t)

;; When you initiate a compilation or a Git commit, Emacs prompts you to save
;; every modified buffer across all active directories. This restricts the save
;; prompt strictly to files belonging to the current project root.
;; TODO
;; (setq save-some-buffers-default-predicate #'save-some-buffers-root)

;; Show unprettified symbol under cursor (when in `prettify-symbols-mode')
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; (setq completions-sort (if (>= emacs-major-version 30) 'historical 'alphabetical))
;; (setq completions-sort nil)

;; (tramp-show-ad-hoc-proxies t)
;; (remote-file-name-access-timeout 10 "Timeout when restoring session with remote file. In seconds")

;; (setq lazy-highlight-cleanup nil)
;; (setq lazy-highlight-max-at-a-time nil)
;; (setq lazy-highlight-no-delay-length 2)
;; (setq lazy-highlight-interval 0.0625)

;; (setq
;;  search-whitespace-regexp ".*?"
;;  isearch-yank-on-move 'shift
;;  isearch-repeat-on-direction-change t
;;  )

;; (setq tab-bar-auto-width-min '(10 4))
;; (setq tab-bar-auto-width-max '(50 5))

;; (setq tramp-connection-timeout (* 60 10)) ; seconds

;; It causes scrolling issues in Magit and potentially other modes. Activate it
;; only in specific Emacs modes to avoid these problems.

;; When word-or-paren-or-punct, complete unless the next character is part of a
;; word, parenthesis, or punctuation.

;; (use-package minibuffer
;;   :ensure nil
;;   :demand t
;;   :config
;;   (setq
;;    completion-auto-help 'always
;;    completion-flex-nospace nil
;;    completions-header-format nil
;;    completions-highlight-face 'completions-highlight
;;    enable-recursive-minibuffers t
;;    completions-sort 'historical
;;    )
;;   :bind (:map minibuffer-local-map
;;               ("C-p" . minibuffer-previous-completion)
;;               ("C-n" . minibuffer-next-completion))
;;   :bind (:map completion-in-region-mode-map
;;               ("C-p" . minibuffer-previous-completion)
;;               ("C-n" . minibuffer-next-completion)
;;               ("RET" . minibuffer-choose-completion)))

;; Info-fontify-maximum-menu-size t
;; grep-use-headings t
;; next-error-message-highlight 'keep
;; smiley-style t
;; tar-mode-show-date t
;; tramp-allow-unsafe-temporary-files t
;; visual-order-cursor-movement t
;; view-read-only t

;; TODO: minimal emacs?


;; (setq completion-auto-help t)
;; (setq completion-cycle-threshold nil)

;; (setq
;;  ;; Non-nil means automatically provide help for invalid completion input.
;;  completion-auto-help nil
;;  ;; Non-nil means show help message in *Completions* buffer.
;;  ;; completion-cycle-threshold t
;;  ;; completion-pcm-complete-word-inserts-delimiters t
;;  ;; completion-category-defaults nil
;;  ;; completion-flex-nospace nil
;;  )
;;

;; (setq completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
;; (setq completions-highlight-face 'completions-highlight)
;; (setq minibuffer-completion-auto-choose t)
;; (setq completions-sort 'historical)


;; browse-url-firefox-new-window-is-tab t
;; comint-history-isearch 'dwim

;; (setq shell-font-lock-keywords
;;       '(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-builtin-face)
;;         ("^[^ \t\n]+:.*" . font-lock-string-face)
;;         ("^\\[[1-9][0-9]*\\]" . font-lock-constant-face)))
;; (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
;; (setq shell-input-autoexpand 'input)

;; (setq shell-has-auto-cd nil) ; Emacs 29.1
;; (setq shell-get-old-input-include-continuation-lines t) ; Emacs 30.1
;; (setq shell-completion-fignore '("~" "#" "%"))
;; (setq-default comint-scroll-to-bottom-on-input t)
;; (setq-default comint-scroll-to-bottom-on-output nil)

;; Removed from minimal-emacs.d

;; ripgrep

;; (defun my-jump-to-first-compilation-info ()
;;   "Jump to the first occurrence of compilation-info face in the current buffer."
;;   (interactive)
;;   (let ((found nil)
;;         (pos (point-min)))
;;     (save-excursion
;;       (goto-char pos)
;;       (while (and (not found) (< pos (point-max)))
;;         (let ((face-prop (get-text-property pos 'face)))
;;           (when (or (eq face-prop 'compilation-info)
;;                     (and (listp face-prop)
;;                          (memq 'compilation-info face-prop)))
;;             (setq found pos)))
;;         (unless found
;;           (setq pos (next-property-change pos (current-buffer) (point-max))))))
;;     (if found
;;         (progn
;;           (goto-char found)
;;           (message "Jumped to first compilation-info face"))
;;       (message "No compilation-info face found in buffer"))))
;;
;; (add-hook 'grep-mode-hook #'my-jump-to-first-compilation-info)

;; (defun my-next-error-recenter (&rest _args)
;;   "Call `next-error' and recenter the buffer."
;;   (recenter))
;;
;; (advice-add 'next-error :after #'my-next-error-recenter)

;; wgrep/embark export and `compile-goto-error'
;; (setq next-error-recenter nil)

;; TODO: should it be moved to evilcursor?
;; (add-hook 'next-error-hook #'my-next-error-show-all-recenter)

;; New code

;; (setq normal-erase-is-backspace t)

;; (setq org-persist--refresh-gc-lock-timer t) ;; Disable it

;; Why this is the best default:
;; 🧹 Strips clutter: Removes properties that are rarely useful when yanked
;; (e.g., read-only, highlighting, clickable behavior).
;;
;; 🧠 Preserves semantics: Keeps properties like face, display, and composition
;; that affect how the text looks or behaves in modes where that's important
;; (e.g., org-mode, markdown-mode, emacs-lisp-mode).
;;
;; 🛠️ Safe and predictable: Avoids unintended side effects of setting it to t,
;; which may silently break display or behavior in some buffers.
;;
;; Others:
;; - category: Classification tag; not useful when pasted.
;; - field: Used for structured editing; can disrupt input fields.
;; - follow-link: Makes text clickable; unnecessary when yanked.
;; - fontified: Marks text as syntax-highlighted; irrelevant after copy.
;; - font-lock-face: Defines syntax styles; usually cosmetic.
;; - help-echo: Tooltip text; not shown when pasted.
;; - intangible: Prevents cursor entry; breaks editing if preserved.
;; - invisible: Hides text; causes missing content on paste.
;; - keymap: Binds keys to region; not portable.
;; - local-map: Buffer-local keymap; not useful elsewhere.
;; - mouse-face: Mouse hover effect; purely visual.
;; - read-only: Prevents editing; undesirable in pasted text.
;; - yank-handler: Custom paste behavior; not reusable.
;; - face: Font style (bold, color); strip for plain text.
;; - display: Visual replacement (e.g., icons); obscures raw content.
;; - composition: Ligature rendering; not needed in plain copy.
;; - line-prefix: Visual indentation; rarely useful in paste.
;; - wrap-prefix: Line-wrap decoration; cosmetic only.
;; - cursor-sensor-functions: Cursor-triggered actions; irrelevant when yanked.
;; - syntax-table: Syntax hints; unnecessary unless parsing the text.

;; Apply the following settings if you also want scrolling with an ordinary
;; mouse to be almost as smooth as scrolling with a touchpad, on systems other
;; than X:
;; (setq pixel-scroll-precision-large-scroll-height 40.0)

;; Configure Emacs to ask for confirmation before exiting

;; (setq switch-to-buffer-in-dedicated-window 'pop)

;; (customize-set-variable 'completion-cycle-threshold 3)
;; (customize-set-variable 'completion-category-overrides
;;                         '((file (styles . (partial-completion)))))

;;; Misc (previously part of mod-same-window, but not useful for it)

;; (defun lightemacs-user-post-init ()
;;   "User post init."
;;   (my-add-packages-to-load-path))

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
        ;; toml-ts-mode-hook
        ;; conf-toml-mode-hook
        ;; markdown-mode-hook
        ))

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
        (buffer-guardian               . "melpa")

        (markdown-mode                 . "melpa")

        (dumb-jump                 . "melpa")
        ;; Latest
        (vterm                         . "melpa")

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

;;; user post init

(defun lightemacs-user-post-init ()
  "This function is executed right before loading modules."
  ;; pre early init

  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "•")

  ;; I find the default prefix for smerge-mode C-c^ cumbersome so I have changed
  ;; it to C-cv
  (setq smerge-command-prefix "\C-xc")
  (setq smerge-diff-buffer-name "*smerge-diff*")
  (setq smerge-refine-shadow-cursor nil)

  (unless noninteractive
    (global-set-key (kbd "M-o") 'my-previous-interesting-buffer)
    (global-set-key (kbd "M-i") 'my-next-interesting-buffer)
    ;; (global-set-key (kbd "M-=") 'global-text-scale-adjust)
    ;; (global-set-key (kbd "M-RET") 'toggle-term-tmux)
    ;; (global-set-key (kbd "M-<enter>") 'toggle-term-tmux)
    ;; (global-set-key (kbd "M-<return>") 'toggle-term-tmux)

    (global-set-key (kbd "C--") 'text-scale-decrease)
    (global-set-key (kbd "C-+") 'text-scale-increase)
    (global-set-key (kbd "C-S-k") 'my-tab-bar-move-tab-backward)
    (global-set-key (kbd "C-S-j") 'my-tab-bar-move-tab)
    (global-set-key (kbd "C-k") 'my-tab-previous)
    (global-set-key (kbd "C-j") 'my-tab-next))

  (setq hs-hide-comments-when-hiding-all nil)
  (setq hs-isearch-open t)  ;; Open both comments and code

  ;; Code folding
  ;; Note: html-mode usually relies on specialized packages like sgml-mode or web-mode folding

  ;; (add-hook 'web-mode-hook 'hs-minor-mode)

  ;; (add-hook 'js-ts-mode-hook 'hs-minor-mode)

  ;; (add-hook 'lisp-interaction-mode-hook #'outline-minor-mode)

  ;; This fixes the skipping when scrolling long org documents
  ;; NOTE: MANAGED BY MINIMAL-EMACS
  (setq scroll-conservatively most-positive-fixnum)
  ;; (setq scroll-conservatively 20)

  (setq eldoc-idle-delay 0.5)
  (setq eldoc-echo-area-display-truncation-message nil)
  ;; (setq eldoc-echo-area-prefer-doc-buffer nil)
  (setq eldoc-echo-area-use-multiline-p nil)

  (with-eval-after-load 'recentf
    (setq recentf-exclude
          (append recentf-exclude
                  (list
                   "^~/\\.bin/"

                   "^~/\\.emacs"
                   "^~/\\.src"
                   "^~/src/forks/"
                   "^~/\\.[a-z-]*-?emacs.d/"
                   "^/opt/local/"

                   ;; Archives and Compressed Files
                   "\\.tar$" "\\.gz$" "\\.zip$" "\\.7z$" "\\.rar$" "\\.xz$" "\\.bz2?$"
                   ;; "\\.tbz2?$" "\\.tgz$" "\\.gzip$"
                   ;; "\\.zpaq$" "\\.lz$" "\\.lrz$" "\\.lzo$" "\\.lzma$"
                   ;; "\\.shar$" "\\.kgb$" "\\.Z$" "\\.zst$"
                   ;; "\\.tzst$" "\\.lz4$" "\\.br$" "\\.cpio$" "\\.cab$" "\\.arj$"
                   ;; "\\.lzh$" "\\.lha$"

                   ;; Packages, Images, and Ecosystem Archives
                   ;; "\\.deb$" "\\.rpm$" "\\.apk$" "\\.dmg$" "\\.iso$"
                   ;; "\\.jar$" "\\.war$" "\\.ear$" "\\.whl$"

                   ;; Images and Media
                   "\\.\\(?:gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                   ;; "\\.mkv$" "\\.mp[34]$" "\\.avi$" "\\.wav$"

                   ;; Exclude OS temporary directories
                   "^/tmp/"
                   "^/var/tmp/"

                   "/elpa/.*\\'"
                   "/tramp.*\\'"
                   "/ssh\\(x\\)?:"

                   ;; Exclude local bypass and root
                   ;; "^/\\(?:su\\|sudo\\)?:"

                   ;; Version Control (keeps out COMMIT_EDITMSG, MERGE_MSG,
                   ;; etc.)
                   "/\\.git/.*\\'"
                   ;; "/\\.hg/.*\\'"
                   ;; "/\\.svn/.*\\'"

                   ;; Emacs cache files
                   ;; "/recentf\\'"   ; Don't track recentf itself
                   ;; "/bookmarks\\'" ; Don't track bookmarks

                   ;; Include
                   ;; "^/usr/include/"
                   ;; "/TAGS\\'"
                   ;; "/GTAGS\\'"
                   ;; "/GRAGS\\'"
                   ;; "/GPATH$\\'"

                   ;; "/\\.loaddefs\\.elc?\\'"
                   ;; "-autoloads\\.el$"
                   ;; "autoload\\.el$"

                   ;; "/\\.cache/"
                   ;; "/\\.git/"
                   ;; "/\\.svn/"

                   ;; "emacs/src/"
                   ;; "/usr/share/emacs/"
                   ;; "/usr/local/share/emacs/"

                   ;; Exclude all remote files (prevents TRAMP hangs)
                   ;; #'file-remote-p

                   ;; binary
                   ;; "\\.docx?$"
                   ;; "\\.xlsx?$"
                   ;; sub-titles
                   ;; "\\.sub$"
                   ;; "\\.srt$"
                   ;; "\\.ass$"

                   ;; "/eln-cache/"

                   ;; TODO add lightemacs dir
                   ;; ".cask"
                   ;; "/G?TAGS$"
                   ;; "\\.?cache"
                   ;; "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                   ;; "\\.revive$"
                   ;; "^/ssh:"
                   ;; "/persp-confs/"
                   ;; Emacs state/cache files (Fixing the TODO)
                   ;; "url"
                   ;; tramp-file-name-regexp
                   ))))

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-mixed-fonts nil)

  (setq kirigami-enhance-outline-open t)
  (setq kirigami-enhance-outline-close-all t)
  (setq kirigami-preserve-visual-position t)

  (setq savehist-autosave-interval 650)
  (setq tab-bar-history-limit 15)

  (setq outline-blank-line t)

  (setq vim-tab-bar-update-group-name-function #'(lambda(name)
                                                   (concat " [" name "] ")))
  (setq vim-tab-bar-show-groups nil)

  (setq bufferfile-use-vc t)
  ;; (setq bufferfile-delete-switch-to 'previous-buffer)
  (setq bufferfile-delete-switch-to 'parent-directory)

  (setq grep-use-null-device nil
        grep-use-null-filename-separator nil
        grep-use-headings nil)
  (with-eval-after-load 'le-core-cli-tools
    (setq grep-use-null-device nil)
    (setq grep-command-position nil)
    (setq grep-command
          (concat "rg"
                  ;; Include hidden files
                  " --hidden"
                  ;; Exclude VC
                  " -g !.git -g !.svn -g !.hg"
                  ;; Default
                  " --null --line-buffered --color=never --max-columns=1000"
                  " --path-separator / --smart-case --no-heading"
                  " --with-filename --line-number --search-zip"))
    ;; Synchronize grep-template with the custom ripgrep command defined above.
    ;; The <R> and <F> tokens serve as dynamic placeholders that Emacs replaces
    ;; with the search regular expression and target file patterns at runtime.
    ;; This setup ensures that built-in recursive search utilities like rgrep
    ;; and lgrep automatically inherit the same optimized ripgrep options
    ;; without duplicating the configuration string.
    (setq grep-template (concat grep-command " <R> <F>")))

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
              (evil-collection-eat-toggle-send-escape)))))
      (add-hook 'eat-mode-hook 'evil-collection-enable-eat-toggle-send-escape)))

  (add-hook 'emacs-lisp-mode-hook
            #'(lambda()
                (setq-local dabbrev-case-fold-search t)
                (setq-local case-fold-search t)))

  (setq epg-gpg-program "gpg2")
  ;; (setq epa-pinentry-mode 'loopback)  ;; Obsolete

  (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)\\(~\\|\\.~[0-9]+~\\)?\\'")

  ;; Manually update the handler alist to recognize the new extension immediately
  (setq file-name-handler-alist
        (cons (cons epa-file-name-regexp #'epa-file-handler)
              file-name-handler-alist))

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
        '(("en_US"
           "[[:alpha:]]"
           "[^[:alpha:]]"
           "['‘’]"
           ;; When set to nil: A word can contain the defined "other characters"
           ;; (like an apostrophe), but only one at a time between valid word
           ;; characters. For example, FFmpeg's is parsed as a single word, but
           ;; if a typo like FFmpeg''s occurs, the parser will split it at the
           ;; consecutive apostrophes.
           ;;
           ;; When set to t: The parser allows multiple consecutive instances of
           ;; the defined "other characters" inside a word without breaking the
           ;; word boundary.
           ;;
           ;; For a standard English dictionary configuration, nil is the
           ;; correct and expected value, because standard English grammar does
           ;; not use consecutive apostrophes within a single word.
           nil
           ("-d" "en_US")
           nil
           utf-8)))

  ;; (setq ispell-extra-args '("--add-word-chars='")
  (setq flyspell-default-dictionary "en_US")
  (setq ispell-dictionary "en_US")

  ;; Non-nil means suppress messages in ispell-word.
  (setq ispell-quietly t)

  ;; If non-nil, add correction to abbreviation table.
  ;; (setq flyspell-abbrev-p nil)
  ;; (setq flyspell-use-global-abbrev-table-p t)

  ;; (with-eval-after-load 'flyspell
  ;;   ;; Remove strings from Flyspell
  ;;   (setq flyspell-prog-text-faces (delq 'font-lock-string-face
  ;;                                        flyspell-prog-text-faces))
  ;;
  ;;   ;; Remove doc from Flyspell
  ;;   (setq flyspell-prog-text-faces (delq 'font-lock-doc-face
  ;;                                        flyspell-prog-text-faces)))

  (setq ispell-program-name "aspell")
  (setq ispell-local-dictionary "en_US")

  ;; Configures Aspell's suggestion mode to "ultra", which provides more
  ;; aggressive and detailed suggestions for misspelled words.
  ;;
  ;; The language is set to "en_US" for US English, which can be replaced with
  ;; your desired language code (e.g., "en_GB" for British English, "de_DE" for
  ;; German).
  (setq ispell-extra-args '(;; This flag changes the internal algorithm Aspell
                            ;; uses to find replacement words when it detects a
                            ;; typo. Aspell has multiple modes (ultra, fast,
                            ;; normal, and bad-spellers).
                            ;;
                            ;; Benefits:
                            ;; Zero UI Blocking: It prioritizes execution speed
                            ;; above all else. When Flyspell requests suggestions,
                            ;; Aspell returns them instantly. This prevents the
                            ;; single-threaded Emacs UI from locking up while you
                            ;; are typing.
                            ;; Lower CPU Usage: It prevents Emacs from spiking
                            ;; your CPU when running bulk checks over large
                            ;; files.
                            ;;
                            ;; Drawbacks:
                            ;; Reduced Accuracy: Because the search algorithm is
                            ;; shallow, it is less forgiving of heavy typos or
                            ;; phonetic mistakes. If a word is severely
                            ;; misspelled, the correct replacement might not
                            ;; appear in the generated suggestion list.
                            "--sug-mode=ultra"
                            ;; Ignore 1 and 2 characters words
                            "--ignore=2"
                            ;; "--ignore-case"
                            ;; This flag instructs Aspell to accept words formed
                            ;; by combining two or more valid dictionary words
                            ;; without spaces, treating the resulting string as
                            ;; valid.
                            ;;
                            ;; Benefits: Excellent for Source Code: Code is
                            ;; heavily populated with compound variable names
                            ;; and technical terms (e.g., filepath, buffername,
                            ;; checkbox). This flag stops the spell checker from
                            ;; highlighting every combined word as an error,
                            ;; significantly reducing false positives and visual
                            ;; noise in your programming buffers.
                            ;;
                            ;; Drawback: Masks Real Typos in Prose: It makes the
                            ;; spell checker too lenient for standard text or
                            ;; markdown files. If you accidentally miss a space
                            ;; while typing regular sentences (e.g., typing
                            ;; "andthe" instead of "and the"), Aspell will
                            ;; consider it a valid run-together string and fail
                            ;; to flag the typo.
                            "--run-together"
                            "--lang=en_US"))

  (add-hook 'text-mode-hook
            #'(lambda()
                (setq-local ispell-extra-args
                            (remove "--run-together" ispell-extra-args))))

  ;; (defun my-ispell-perl-mode-setup ()
  ;;   "Remove the --run-together argument from Aspell in text modes."
  ;;   (setq-local ispell-extra-args (append '("--mode=perl") ispell-extra-args)))
  ;; (add-hook 'bash-ts-mode-hook #'my-ispell-perl-mode-setup)
  ;; (add-hook 'sh-mode-hook #'my-ispell-perl-mode-setup)

  (with-eval-after-load 'which-key
    (when (bound-and-true-p which-key-buffer-name)
      (add-to-list 'winner-boring-buffers which-key-buffer-name)))

  (with-eval-after-load 'le-aggressive-indent
    (add-hook 'lua-mode-hook 'aggressive-indent-mode))

  (my-update-package-pinned-packages my-package-pinned-packages)

  ;; Abbrev
  (add-hook 'markdown-mode-hook #'abbrev-mode)
  (add-hook 'markdown-ts-mode-hook #'abbrev-mode)
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

  ;; This seems to change ediff
  (setq diff-default-read-only t)

  ;; TODO minimal-emacs?
  (fringe-mode (frame-char-width))
  ;; (if (fboundp 'fringe-mode) (fringe-mode '20))

  ;; TODO try
  ;; (setq-default fringes-outside-margins t)

  ;; Use reliable file-based syntax highlighting when available and hunk-based
  ;; syntax highlighting otherwise as a fallback.
  ;;
  ;; The Problem: When you look at a diff in Emacs, added lines are strictly
  ;; green and removed lines are strictly red. You lose all of the programming
  ;; language's native syntax highlighting, making the code harder to read.
  ;;
  ;; Why it has no tradeoff: Emacs will apply the correct major mode syntax
  ;; highlighting (like Python or Elisp keywords) inside the diff hunk itself,
  ;; blending the diff colors with your code colors perfectly.
  ;;
  ;; TODO add to minimal-emacs.d
  (setq diff-font-lock-syntax 'hunk-also)

  ;; (setq diff-advance-after-apply-hunk t)

  ;; This sometimes interacts poorly with the undo mechanism
  ;; (setq diff-update-on-the-fly t)

  ;; Set this to nil if you want to do it on demand
  ;; (setq diff-refine nil)

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
                      "^\*Ediff"
                      "^\*Compile-Log\*"
                      "^\*ansible-doc"))
      (push regexp consult-buffer-filter)))

  (when debug-on-error
    ;; TODO le-default config?
    (push 'search-failed debug-ignored-errors)

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

                   ;; scan-error: "Unbalanced parentheses"
                   scan-error

                   ;; hideshow
                   "Already at end of element"

                   imenu-unavailable

                   "Bad diff region number"))
      (push err debug-ignored-errors)))

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

  (setq read-process-output-max (* 32 1024 1024))

  (when (and (not (daemonp))
             (not (display-graphic-p)))
    (xterm-mouse-mode 1))

  (unless noninteractive
    ;; (windmove-default-keybindings)

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
  ;; (setq-default line-spacing 0.05)
  (setq enhanced-evil-paredit-handle-paste t)
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq history-delete-duplicates t)
  (setq widget-image-enable nil)
  (setq mode-line-collapse-minor-modes t)  ;; Emacs 31
  (setq abbrev-suggest t)
  (setq copy-directory-create-symlink t)
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

  ;; Plain Text Pasting (Fixing "Org-Mode Bleed")
  ;;
  ;; Copying text from an Org buffer often results in unwanted colors,
  ;; backgrounds, or text weights bleeding into the destination buffer.
  ;;
  ;; By default, vanilla Emacs preserves explicit text formatting (face
  ;; properties) when copying and pasting to support rich-text environments.
  ;; While standard syntax highlighting (font-lock-face) is automatically
  ;; stripped, modes like org-mode rely heavily on the face property for their
  ;; visual styling.
  ;;
  ;; Benefits of (push 'face yank-excluded-properties):
  ;; - Prevents visual formatting bleed between different major modes.
  ;; - Unlike the common workaround of stripping all text properties entirely
  ;;   (setq yank-excluded-properties t), this method is surgical. It only
  ;;   removes visual properties, ensuring that functional text properties
  ;;   remain fully intact.
  ;;
  ;; This configuration intentionally disables the ability to copy and paste
  ;; rich-text formatting. If you specifically require the preservation of text
  ;; colors or weights across buffers (for example, when using enriched-mode or
  ;; composing HTML emails), you should omit this setting.
  (push 'face yank-excluded-properties)

  ;; Shows all options when running apropos. For more info,
  (setq calendar-week-start-day 1)

  (setq echo-keystrokes 0)  ;; Do not show keystrokes in the mini buffer

  (setq delete-pair-blink-delay 0)

  ;; Other things
  (setq tab-bar-close-tab-select 'right)

  (setq history-length 200)

  ;; testing
  (setq transient-detect-key-conflicts t)

  ;; TODO minimal-emacs
  ;; tramp-copy-size-limit (* 2 1024 1024) ; 1mb
  ;; tramp-use-scp-direct-remote-copying t
  ;; tramp-completion-reread-directory-timeout 60

  (setq remote-file-name-inhibit-auto-save t)

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



  (when auto-save-default
    (let ((auto-save-dir (file-name-directory auto-save-list-file-prefix)))
      (unless (file-exists-p auto-save-dir)
        (with-file-modes #o700
          (make-directory auto-save-dir t)))))
  ;;-------------------------------------> BLOCK CHANGE AUTO SAVE PATH

  ;; (setq easysession-debug t)
  (setq easysession-refresh-tab-bar t)

  )

(add-hook 'lightemacs-after-modules-hook #'lightemacs-user-post-init)

;;; Useful functions

;;; Startup time

(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs loaded in %.2f seconds (Init only: %.2fs) with %d garbage collections."
           (time-to-seconds (time-since before-init-time))
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(add-hook 'lightemacs-emacs-startup-hook #'display-startup-time 200)

;;; Ignored errors

;; Org + vertico preview error: Debugger entered--Lisp error: (error "rx '**'
;; range error")

;;; Other settings

;; Control ^ = Control
;; Command = Fn/Globe
;; Fn/Globe = Command
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)

(setq enable-local-variables :safe)

;;; ibuffer

(setq ibuffer-filter-group-name-face '(:inherit (success bold)))
(setq
 ;; ibuffer-hidden-filter-groups nil

 ;; The number of hours before a buffer is considered "old".
 ibuffer-old-time 5

 ;; If non-nil, then forward and backwards movement commands cycle.
 ibuffer-movement-cycle nil

 ibuffer-show-empty-filter-groups nil

 ;; If non-nil, don't ask for confirmation of "dangerous" operations.
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
        (derived-mode-p 'markdown-ts-mode)
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

(add-hook 'grep-mode-hook #'my-setup-display-line-numbers-mode)
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

;; t is slow. Use nil.
;; Scroll profiling: 7% + display-line-numbers-update-width
(setq display-line-numbers-grow-only nil)

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

  ;; Python
  (cond
   ((executable-find "ruff")
    ;; Ruff

    ;; Create one super-formatter that sorts imports (I) and fixes PEP 8 (E, W)
    (setf (alist-get 'ruff apheleia-formatters)
          '("ruff" "check"
            "-n"
            "--select" "I,E,W"   ;; I = isort, E/W = pycodestyle (autopep8)
            "--fix" "--fix-only"
            "--stdin-filename" filepath
            "-"))

    ;; (setf (alist-get 'ruff apheleia-formatters)
    ;;       '("ruff" "format"
    ;;         "--line-length=79"         ;; Set your strict line length
    ;;         ;; "--target-version=py311"   ;; Example: Tell Ruff your Python version
    ;;         ;; "--skip-magic-trailing-comma" ;; Example: Another formatting tweak
    ;;         "--silent"                 ;; Keep Apheleia quiet
    ;;         "-"))
    ;; (setf (alist-get 'ruff-isort apheleia-formatters)
    ;;       '("ruff" "check" "-n" "--select" "I" "--fix" "--fix-only"
    ;;         "--stdin-filename" filepath "-"))

    (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff)))

   (t
    ;; Legacy
    (setf (alist-get 'autopep8 apheleia-formatters)
          '("autopep8"
            "--max-line-length=79"
            ;; --aggressive is too aggressive when it comes to max-line-length
            ;; "--aggressive"
            ;; "--aggressive"
            "-"))

    (setf (alist-get 'isort apheleia-formatters) '("isort" "--stdout" "-"))

    (setf (alist-get 'python-mode apheleia-mode-alist) '(isort autopep8))
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(isort autopep8))))

  ;; SH/Bash shell scripts
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "--binary-next-line"
          "-filename" filepath
          (when (and apheleia-formatters-respect-indent-level
                     (boundp 'sh-basic-offset))
            (list "-i" (number-to-string sh-basic-offset)))
          "-"))

  (setf (alist-get 'sh-mode apheleia-mode-alist) '(shfmt))
  (setf (alist-get 'bash-ts-mode apheleia-mode-alist) '(shfmt))

  (defun my-apheleia-sh-mode-setup ()
    "Enable shfmt in `sh-mode' only if the shell is Bash."
    ;; To prevent it, for example, from replacing `cmd` with $(cmd)
    (if (eq sh-shell 'bash)
        (setq-local apheleia-formatter '(shfmt))
      (setq-local apheleia-formatter nil)))

  (add-hook 'bash-ts-mode-hook 'my-apheleia-sh-mode-setup)
  (add-hook 'sh-mode-hook 'my-apheleia-sh-mode-setup))

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


;; elisp evil-imenu

(with-eval-after-load 'lisp-mode
  (add-to-list 'lisp-imenu-generic-expression
               `("Evil Definitions"
                 ,(rx line-start
                      (* space)
                      "("
                      (or "evil-define-avy-motion"
                          "evil-define-command"
                          "evil-define-interactive-code"
                          "evil-define-key"
                          "evil-define-key*"
                          "evil-define-keymap"
                          "evil-define-local-var"
                          "evil-define-minor-mode-key"
                          "evil-define-motion"
                          "evil-define-operator"
                          "evil-define-state"
                          "evil-define-text-object"
                          "evil-define-type"
                          "evil-define-visual-selection")
                      (+ space)
                      (* (any "'" "#"))
                      (* "(")
                      (group
                       (+ (or word (syntax symbol)))))
                 1)
               t))

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

;; (defvar-local my-window-redisplay-last-text-scale-amount nil
;;   "Store the last text scale amount for window redisplay checks.")
;;
;; (defun my-window-redisplay ()
;;   "Redisplay window."
;;   (when (bound-and-true-p text-scale-mode-amount)
;;     (let ((amount text-scale-mode-amount))
;;       (when (or (null my-window-redisplay-last-text-scale-amount)
;;                 (/= amount my-window-redisplay-last-text-scale-amount))
;;         (setq my-window-redisplay-last-text-scale-amount amount)
;;         (let ((window (selected-window)))
;;           (if (<= emacs-major-version 27)
;;               (run-hook-with-args 'window-size-change-functions window)
;;             (run-hook-with-args 'window-state-change-functions window))))))
;;   (run-hooks 'window-configuration-change-hook))
;;
;; (defun my-persist-text-scale-adjust ()
;;   "Ensure the window is updated.
;; I have identified an issue that affects Emacs packages such as eat (terminal)
;; and visual-fill-column. Functions like `text-scale-increase`,
;; `text-scale-decrease`, and `text-scale-set` do not trigger hooks like
;; `window-configuration-change-hook`. As a result, the eat package does not
;; immediately update the window when the text scale is changed, and
;; visual-fill-column does not update the margin right away (it updates only after
;; the window is resized). This function fixes these issues."
;;   (when (or (derived-mode-p 'eat-mode)
;;             (bound-and-true-p visual-fill-column-mode))
;;     (my-window-redisplay)))
;;
;; ;; Force windows update
;; (add-hook 'text-scale-mode-hook #'my-persist-text-scale-adjust)

(defvar my-window-redisplay-last-text-scale-amount nil)

(defun my-persist-text-scale-function ()
  "Return the persist text scale category."
  ;; Corfu context menu adjusts the text size based on the size of the
  ;; window from which the text completion is triggered. It should be
  ;; ignored.
  (let ((buffer-name (buffer-name)))
    (cond
     ;; Temporary / Popup Buffers to Ignore
     ((or (string= buffer-name " *transient*")
          (string= buffer-name " *which-key*")  ; TODO add to official package
          (string= buffer-name " *lv*")                  ; Hydra/LV popups
          (string= buffer-name "*Ediff Control Panel*")
          (string-match-p "\\` \\*posframe" buffer-name)   ; Posframe popups
          (string-match-p "\\` \\*company" buffer-name))   ; Company popups
      :ignore)

     ;;((string-prefix-p ))

     ;; TODO: add to the official one
     ((string-prefix-p "*Embark Export:" buffer-name)
      "c:embark-export")

     ((string-prefix-p "*Org Src" buffer-name)
      "c:org-src")

     ((string-prefix-p "*magit" buffer-name)
      "c:magit")

     ((string-prefix-p "*helpful" buffer-name)
      "c:helpful")

     ((string-prefix-p "*sdcv:" buffer-name)
      "c:sdcv"))))

(with-eval-after-load 'persist-text-scale
  (setq persist-text-scale-buffer-category-function
        #'my-persist-text-scale-function))

;;; DISABLED: sh-mode: Highlight sh-mode and bash-ts-mode $variables

;; (progn
;;   (defun sh-script-match-variables (limit)
;;     "Search forward for env vars up to LIMIT, skipping comments and quotes."
;;     (catch 'found
;;       ;; The regex now matches ${anything_except_newline_and_closing_brace}
;;       ;; or standard unbracketed variables like $var, $1, $#
;;       (while (re-search-forward "\\$\\({[^}\n]+}\\|[[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\|[[:digit:]]+\\)" limit t)
;;         (let ((state (syntax-ppss)))
;;           ;; (nth 4 state) is non-nil if we are inside a comment
;;           ;; (nth 3 state) is the string delimiter character (e.g., ?\')
;;           (unless (or (nth 4 state)
;;                       (eq (nth 3 state) ?\'))
;;             (throw 'found t))))
;;       nil))
;;
;;   ;; This one works, but it also highlights the variable that are in comments
;;   (defvar sh-script-extra-font-lock-keywords
;;     '((sh-script-match-variables
;;        (0 font-lock-variable-name-face prepend))))
;;
;;   (defun sh-script-extra-font-lock-activate ()
;;     "Activate additional font-locking for variables in double-quoted strings."
;;     (font-lock-add-keywords nil sh-script-extra-font-lock-keywords))
;;
;;   (add-hook 'sh-mode-hook #'sh-script-extra-font-lock-activate)
;;   (add-hook 'bash-ts-mode-hook #'sh-script-extra-font-lock-activate))

;; NOTE: DEPRECATED
;; Highlight $variables
;; This one works, but it also highlights the variable that are in comments
;; (defvar sh-script-extra-font-lock-keywords
;;   '(("\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\|[[:digit:]]+\\)"
;;      (2 font-lock-variable-name-face prepend))))
;; (defun sh-script-extra-font-lock-activate ()
;;   "Activate additional font-locking for variables in double-quoted strings."
;;   (font-lock-add-keywords nil sh-script-extra-font-lock-keywords))
;; (add-hook 'sh-mode-hook #'sh-script-extra-font-lock-activate)
;; (add-hook 'bash-ts-mode-hook #'sh-script-extra-font-lock-activate)



;;; Better highlight sh-mode and bash-ts-mode variables
(defvar my-sh-builtin-keywords
  '("cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find" "git" "grep"
    "kill" "less" "ln" "ls" "make" "mkdir" "mv" "pgrep" "pkill" "pwd" "rm"
    "sleep" "sudo" "touch")
  "A list of common shell commands to be fontified especially in `sh-mode'.")

;; Custom functions required to replace Doom's autoloaded regex matchers
(defun my-sh--match-variables-in-quotes (limit)
  "Match shell variables inside double quotes up to LIMIT."
  (catch 'found
    (while (re-search-forward "\\(\\$\\)\\([a-zA-Z0-9_]+\\|{[^}]*}\\)" limit t)
      (when (eq (nth 3 (syntax-ppss)) ?\")
        (throw 'found t)))))

(defun my-sh--match-command-subst-in-quotes (limit)
  "Match command substitutions inside double quotes up to LIMIT."
  (catch 'found
    ;; Replaced slow non-greedy "\\(\\$(.*?)\\)" with a fast
    ;; character class [^)]+
    (while (re-search-forward "\\(\\$\\([^)]+\\)\\)" limit t)
      (when (eq (nth 3 (syntax-ppss)) ?\")
        (throw 'found t)))))

(with-eval-after-load 'sh-script
  ;; Replace (modulep! +lsp) with standard feature check
  ;; (when (featurep 'lsp-mode)
  ;;   (add-hook 'sh-mode-hook #'lsp append))

  ;; TODO minimal emacs?
  (setq sh-indent-after-continuation 'always)

  ;; (add-hook 'sh-mode-hook (lambda () (setq mode-name "Sh")))

  ;; recognize function names with dashes in them
  ;; (add-to-list 'sh-imenu-generic-expression
  ;;              '(sh (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
  ;;                   (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1)))

  ;; `sh-set-shell' is verbose about setting up indentation
  (when (fboundp 'sh-set-shell)
    (advice-add #'sh-set-shell :around
                (lambda (orig-fn &rest args)
                  (let ((inhibit-message t))
                    (apply orig-fn args)))))

  ;; 1. Fontifies variables in double quotes
  ;; 2. Fontify command substitution in double quotes
  ;; 3. Fontify built-in/common commands (see `my-sh-builtin-keywords')
  (defun my-sh-init-extra-fontification-handler ()
    (font-lock-add-keywords
     nil `((my-sh--match-variables-in-quotes
            (1 'font-lock-constant-face prepend)
            (2 'font-lock-variable-name-face prepend))
           (my-sh--match-command-subst-in-quotes
            (1 'sh-quoted-exec prepend))
           (,(regexp-opt my-sh-builtin-keywords 'symbols)
            ;; Changed `append` to `keep` so it doesn't overwrite comments
            (0 'font-lock-type-face keep)))))

  (when (fboundp 'my-sh-init-extra-fontification-handler)
    ;; Removed bash-ts-mode-hook to prevent severe performance penalties
    (add-hook 'sh-mode-hook #'my-sh-init-extra-fontification-handler))

  ;; autoclose backticks Ensure this only runs if smartparens is installed and
  ;; loaded
  ;; (with-eval-after-load 'smartparens
  ;;   (sp-local-pair 'sh-mode "`" "`" :unless '(sp-point-before-word-p sp-point-before-same-p)))
  )
;;; ansible


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

;;; Enable `smerge'

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

;;; DISABLED: Disable `smerge' mode paredit

;; (defun my-disable-modes-on-smerge ()
;;   "Disable `paredit-mode' when a git conflict happens."
;;   (when (bound-and-true-p smerge-mode)
;;     (when (and (derived-mode-p 'emacs-lisp-mode)
;;                (bound-and-true-p paredit-mode)
;;                (fboundp 'paredit-mode))
;;       (message "The `paredit-mode' has been disabled")
;;       (paredit-mode -1))))
;; (add-hook 'smerge-mode-hook #'my-disable-modes-on-smerge)

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

;;; server

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
    (lightemacs-recenter-maybe)))

(defun lightemacs-default-settings--advice-recenter-always (fn &rest args)
  "FN is the advised function. ARGS are the function arguments."
  (unwind-protect
      (apply fn args)
    (when (and (eq (current-buffer) (window-buffer))
               (not (pos-visible-in-window-p (point))))
      (recenter))))

;; (with-eval-after-load 'saveplace  ; TODO test more
;;   (add-hook 'save-place-after-find-file-hook
;;             #'lightemacs-default-settings--recenter-maybe 70))
;;
;; (with-eval-after-load 'imenu  ; TODO test more
;;   (add-hook 'imenu-after-jump-hook
;;             #'lightemacs-default-settings--recenter-maybe 70))
;;
;; (with-eval-after-load 'consult  ; TODO test more
;;   (add-hook 'consult-after-jump-hook
;;             #'lightemacs-default-settings--recenter-maybe 70))
;;
;; (with-eval-after-load 'org-agenda  ; TODO test more
;;   (add-hook 'org-agenda-after-show-hook
;;             #'lightemacs-default-settings--recenter-maybe 70))
;;
;; (with-eval-after-load 'bookmark  ; TODO test more
;;   (add-hook 'bookmark-after-jump-hook
;;             #'lightemacs-default-settings--recenter-maybe 70))

(with-eval-after-load 'evil-commands
  (advice-add 'evil-goto-last-change-reverse :around
              #'lightemacs-default-settings--advice-recenter-maybe)
  (advice-add 'evil-goto-last-change :around
              #'lightemacs-default-settings--advice-recenter-maybe))

(with-eval-after-load 'diff-hl
  (advice-add 'diff-hl-next-hunk :around
              #'lightemacs-default-settings--advice-recenter-always)
  (advice-add 'diff-hl-next-hunk :around
              #'lightemacs-default-settings--advice-recenter-always))

(with-eval-after-load 'git-gutter
  (advice-add 'git-gutter:previous-hunk :around
              #'lightemacs-default-settings--advice-recenter-maybe)
  (advice-add 'git-gutter:next-hunk :around
              #'lightemacs-default-settings--advice-recenter-maybe))

;; TODO use a loop to add to hooks and advice functions
(with-eval-after-load 'git-gutter
  (advice-add 'git-gutter:previous-hunk :around
              #'lightemacs-default-settings--advice-recenter-maybe)
  (advice-add 'git-gutter:next-hunk :around
              #'lightemacs-default-settings--advice-recenter-maybe))

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

(defun my-flymake-execution-only-when-code-checker-allowed (orig-fun &rest args)
  "Execute ORIG-FUN with ARGS only if it is allowed.
This function is intended for use as :around advice."
  (when (and (fboundp 'my-code-checker-allowed-p)
             (my-code-checker-allowed-p))
    (when (bound-and-true-p my-buffer-enable-flymake)
      (apply orig-fun args))))

(defun my-apheleia-execution-only-when-code-checker-allowed (orig-fun &rest args)
  "Execute ORIG-FUN with ARGS only if it is allowed.
This function is intended for use as :around advice."
  (when (and (fboundp 'my-code-checker-allowed-p)
             (my-code-checker-allowed-p))
    (when (bound-and-true-p my-buffer-enable-apheleia)
      (apply orig-fun args))))

(with-eval-after-load 'le-aggressive-indent
  (advice-add 'aggressive-indent-mode :around
              #'my-prevent-execution-only-when-code-checker-allowed))

(with-eval-after-load 'le-stripspace
  (advice-add 'stripspace-local-mode :around
              #'my-prevent-execution-only-when-code-checker-allowed))

(with-eval-after-load 'le-flymake-ansible-lint
  (advice-add 'flymake-ansible-lint-setup :around
              #'my-prevent-execution-only-when-code-checker-allowed))

(with-eval-after-load 'le-apheleia
  (advice-add 'apheleia-mode :around
              #'my-apheleia-execution-only-when-code-checker-allowed))

(with-eval-after-load 'le-flymake
  (advice-add 'flymake-mode :around
              #'my-flymake-execution-only-when-code-checker-allowed))

;;; Lazy loader

(lightemacs-use-package lazy-loader
  :ensure nil
  :commands lazy-loader-mode
  :hook
  (lightemacs-after-init . lazy-loader-mode)
  :init
  (setq lazy-loader-verbose t)
  (setq lazy-loader-files (delq nil
                                (list (when (bound-and-true-p file-path-todo)
                                        file-path-todo))))
  (setq lazy-loader-modules '(org
                              vterm
                              org-appear
                              vterm
                              aggressive-indent
                              yasnippet
                              apheleia
                              dired

                              ;; advice
                              ;; annalist
                              ;; ansi-color
                              ;; ansi-osc
                              ;; apheleia-dp
                              ;; apheleia-formatter-context
                              ;; apheleia-formatters
                              ;; apheleia-log
                              ;; apheleia-rcs
                              ;; apheleia-utils
                              ;; autorevert
                              ;; avl-tree
                              ;; bibtex
                              ;; bookmark
                              ;; buffer-terminator
                              ;; wizard
                              ;; byte-opt
                              ;; c++-ts-mode
                              ;; c-ts-common
                              ;; c-ts-mode
                              ;; cal-loaddefs
                              ;; cal-menu
                              ;; calendar
                              ;; char-fold
                              ;; color
                              ;; comint
                              ;; comp
                              ;; comp-common
                              ;; comp-cstr
                              ;; comp-run
                              ;; compile
                              ;; compile-angel
                              ;; corfu
                              ;; corfu-prescient
                              ;; cursor-sensor
                              ;; derived
                              ;; diff-mode
                              ;; dig
                              ;; dired-loaddefs
                              ;; disp-table
                              ;; display-fill-column-indicator
                              ;; display-line-numbers
                              ;; doc-view
                              ;; dom
                              ;; dtrt-indent
                              ;; easy-escape
                              ;; edit-indirect
                              ;; edmacro
                              ;; ef-melissa-dark-theme
                              ;; ef-themes
                              ;; ehelp
                              ;; elec-pair
                              ;; enhanced-evil-paredit
                              ;; epa
                              ;; epg
                              ;; epg-config
                              ;; evil
                              ;; evil-collection
                              ;; evil-collection-bookmark
                              ;; evil-collection-buff-menu
                              ;; evil-collection-calendar
                              ;; evil-collection-comint
                              ;; evil-collection-compile
                              ;; evil-collection-corfu
                              ;; evil-collection-custom
                              ;; evil-collection-diff-mode
                              ;; evil-collection-dired
                              ;; evil-collection-doc-view
                              ;; evil-collection-eldoc
                              ;; evil-collection-elisp-mode
                              ;; evil-collection-epa
                              ;; evil-collection-eww
                              ;; evil-collection-finder
                              ;; evil-collection-flymake
                              ;; evil-collection-gnus
                              ;; evil-collection-help
                              ;; evil-collection-hideshow
                              ;; evil-collection-image
                              ;; evil-collection-imenu
                              ;; evil-collection-indent
                              ;; evil-collection-kmacro
                              ;; evil-collection-markdown-mode
                              ;; evil-collection-message
                              ;; evil-collection-minibuffer
                              ;; evil-collection-outline
                              ;; evil-collection-package-menu
                              ;; evil-collection-process-menu
                              ;; evil-collection-python
                              ;; evil-collection-replace
                              ;; evil-collection-sh-script
                              ;; evil-collection-simple
                              ;; evil-collection-tab-bar
                              ;; evil-collection-tabulated-list
                              ;; evil-collection-term
                              ;; evil-collection-unimpaired
                              ;; evil-collection-vc-git
                              ;; evil-collection-vertico
                              ;; evil-collection-vterm
                              ;; evil-command-window
                              ;; evil-commands
                              ;; evil-common
                              ;; evil-core
                              ;; evil-ex
                              ;; evil-integration
                              ;; evil-jumps
                              ;; evil-macros
                              ;; evil-maps
                              ;; evil-matchit-evil-setup
                              ;; evil-repeat
                              ;; evil-search
                              ;; evil-snipe
                              ;; evil-states
                              ;; evil-surround
                              ;; evil-types
                              ;; evil-vars
                              ;; eww
                              ;; executable
                              ;; exif
                              ;; face-remap
                              ;; filenotify
                              ;; files-x
                              ;; find-func
                              ;; finder
                              ;; flymake
                              ;; format-spec
                              ;; gcmh
                              ;; gcsentinel
                              ;; generator
                              ;; gmm-utils
                              ;; gnus
                              ;; gnus-art
                              ;; gnus-cloud
                              ;; gnus-group
                              ;; gnus-int
                              ;; gnus-range
                              ;; gnus-spec
                              ;; gnus-start
                              ;; gnus-sum
                              ;; gnus-undo
                              ;; gnus-util
                              ;; gnus-win
                              ;; gnutls
                              ;; help-fns
                              ;; highlight-defined
                              ;; hl-line
                              ;; ibuf-macs
                              ;; ietf-drums
                              ;; image-mode
                              ;; imenu
                              ;; inhibit-mouse
                              ;; inline
                              ;; iso8601
                              ;; jinx
                              ;; jka-compr
                              ;; kinsoku
                              ;; kirigami-evil
                              ;; kmacro
                              ;; lazy-loader
                              ;; let-alist
                              ;; lisp-mnt
                              ;; mail-parse
                              ;; mail-prsvr
                              ;; mail-source
                              ;; mail-utils
                              ;; mailabbrev
                              ;; mailheader
                              ;; marginalia
                              ;; markdown-mode
                              ;; mb-depth
                              ;; message
                              ;; mm-bodies
                              ;; mm-decode
                              ;; mm-encode
                              ;; mm-url
                              ;; mm-util
                              ;; mm-uu
                              ;; mm-view
                              ;; mml
                              ;; mml-sec
                              ;; mml-smime
                              ;; mml2015
                              ;; modus-themes
                              ;; my-config-evil
                              ;; my-evil-outline
                              ;; nnheader
                              ;; nnimap
                              ;; nnmail
                              ;; nnoo
                              ;; nnselect
                              ;; outline-indent
                              ;; package-lint
                              ;; package-lint-flymake
                              ;; paredit
                              ;; parse-time
                              ;; pcase
                              ;; pcomplete
                              ;; persist-text-scale
                              ;; pixel-fill
                              ;; prescient
                              ;; project
                              ;; pulse
                              ;; puny
                              ;; python
                              ;; radix-tree
                              ;; range
                              ;; recentf
                              ;; rect
                              ;; reveal
                              ;; rfc2045
                              ;; rfc2047
                              ;; rfc2231
                              ;; rfc6068
                              ;; rfc822
                              ;; ring
                              ;; rx
                              ;; savehist
                              ;; saveplace
                              ;; sendmail
                              ;; server
                              ;; sh-script
                              ;; shell
                              ;; shell-pop
                              ;; shr
                              ;; smie
                              ;; smime
                              ;; stripspace
                              ;; sub-better-evil
                              ;; svg
                              ;; tabify
                              ;; term
                              ;; term/xterm
                              ;; terminal-themes
                              ;; terminal-themes-frame-color
                              ;; terminal-themes-vterm
                              ;; text-property-search
                              ;; thingatpt
                              ;; time
                              ;; time-date
                              ;; time-stamp
                              ;; track-changes
                              ;; tramp
                              ;; tramp-cache
                              ;; tramp-cmds
                              ;; tramp-compat
                              ;; tramp-integration
                              ;; tramp-loaddefs
                              ;; tramp-message
                              ;; trampver
                              ;; tree-widget
                              ;; undo-fu-session
                              ;; url-file
                              ;; url-queue
                              ;; utf7
                              ;; vc-dispatcher
                              ;; vc-git
                              ;; vertico
                              ;; vertico-prescient
                              ;; vtable
                              ;; vterm-module
                              ;; warnings
                              ;; winner
                              ;; xml
                              ;; xterm
                              ;; yaml-ts-mode
                              ;; yank-media
                              ))
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

;; Lazy loader report for new features

(defvar lazy-loader-initial-features nil
  "A copy of the features list captured right after Emacs initialization.")

(defun lazy-loader-save-initial-features ()
  "Capture the state of loaded features post-init."
  (setq lazy-loader-initial-features (copy-sequence features)))

;; Automatically capture features after the init file finishes loading
(add-hook 'after-init-hook #'lazy-loader-save-initial-features)

(defun lazy-loader-compare-features ()
  "Compare current features against the stored post-init version.
Opens a split window showing the added and removed features."
  (interactive)
  ;; Fallback for testing in the current session if Emacs wasn't restarted
  (unless lazy-loader-initial-features
    (when (y-or-n-p "Initial features not recorded. Snapshot current features as baseline? ")
      (lazy-loader-save-initial-features)))

  (if (not lazy-loader-initial-features)
      (message "Comparison canceled.")
    (let ((added (seq-remove (lambda (f)
                               (seq-contains-p lazy-loader-initial-features f))
                             features))
          (removed (seq-remove (lambda (f)
                                 (seq-contains-p features f))
                               lazy-loader-initial-features))
          (buf (get-buffer-create "*Feature Diff*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "=== Emacs Feature Diff Report ===\n\n")
          (insert (format "Initial features count: %d\n"
                          (length lazy-loader-initial-features)))
          (insert (format "Current features count: %d\n\n"
                          (length features)))

          (insert "--- Added Features (Loaded since init) ---\n")
          (if added
              (dolist (f (sort added #'string-lessp))
                (insert (format "%s\n" f)))
            (insert "  (None)\n"))

          (insert "\n--- Removed Features (Unloaded since init) ---\n")
          (if removed
              (dolist (f (sort removed #'string-lessp))
                (insert (format "%s\n" f)))
            (insert "  (None)\n"))
          (special-mode)))
      ;; Pop to the buffer, which naturally splits the window
      (pop-to-buffer buf))))

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

;;; Golden-ratio

;; package cl is deprecated
;; NOTE: For some reason, this is always reinstalled.
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

;;; Disabled packages

;; (lightemacs-use-package ws-butler
;;   :commands ws-butler-mode
;;   :init
;;   (defun my-setup-ws-butler ()
;;     "Setup `lsp-mode'."
;;     (when (and (fboundp 'ws-butler-mode)
;;                (not (my-code-checker-allowed-p)))
;;       (ws-butler-mode)))
;;   (add-hook 'find-file-hook #'my-setup-ws-butler))

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

;; (lightemacs-use-package shell-pop
;;   :commands shell-pop
;;   :bind (("C-c t" . shell-pop))
;;   :config
;;   ;; The key sequence used to toggle the shell window.
;;   (setopt shell-pop-universal-key "C-c t")
;;   (setopt shell-pop-shell-type '("vterm" "*vterm*"
;;                                  (lambda ()
;;                                    (when (fboundp 'vterm)
;;                                      (let* ((vterm-shell shell-pop-term-shell))
;;                                        (vterm))))))
;;   ;; (setopt shell-pop-shell-type '("eat" "*eat*"
;;   ;;                         (lambda ()
;;   ;;                           (when (fboundp 'eat)
;;   ;;                             (eat shell-pop-term-shell)))))
;;   ;; (setopt shell-pop-shell-type '("ansi-term"
;;   ;;                                "*ansi-term*"
;;   ;;                                (lambda ()
;;   ;;                                  (ansi-term shell-pop-term-shell))))
;;
;;   :init
;;   ;; (setq shell-pop-term-shell "/usr/bin/env bash")
;;   ;; (setq shell-pop-window-position "full")
;;   (setq shell-pop-window-position "bottom")
;;   (setq shell-pop-full-span nil)
;;   (setq shell-pop-autocd-to-working-dir nil)
;;   (setq shell-pop-term-shell "tmux-session emacs")
;;   (setq shell-pop-window-size 80)
;;   (setq shell-pop-restore-window-configuration t))

;; shell-pop: Change the default directory
;; NOTE replaced
(defun my-around-shell-pop (fn &rest args)
  "FN is the advised function. ARGS are the function arguments."
  (my-update-bash-lastdir)
  (apply fn args))
(with-eval-after-load 'shell-pop
  (advice-add 'shell-pop :around #'my-around-shell-pop))

;; Ensure switching to insert mode
(defun my-shell-pop-evil-insert-state ()
  "Ensure the terminal is in char-mode and Evil is in insert state."
  ;; If using term/ansi-term, this lets keys pass to the shell
  ;; (when (and (fboundp 'term-char-mode)
  ;;            (derived-mode-p 'term-mode))
  ;;   (term-char-mode))

  ;; (my-save-all-buffers)

  ;; Force Evil into insert state
  (when (fboundp 'evil-insert-state)
    (evil-insert-state))

  ;; Fix issue that causes the cursor to move to the top-left of the screen
  (when (and (derived-mode-p 'vterm-mode)
             (fboundp 'vterm-reset-cursor-point))
    (vterm-reset-cursor-point)))

(add-hook 'shell-pop-in-after-hook #'my-shell-pop-evil-insert-state)

;;; Auto update lastdir

(defun my-update-bash-lastdir (&rest _)
  "Update Bash lastdir."
  (let* ((directory (buffer-cwd))
         (file "~/.bash_lastdir")
         (file-lastdir (when (file-exists-p file)
                         (let ((line (let ((coding-system-for-read 'utf-8-emacs)
                                           (file-coding-system-alist nil))
                                       (with-temp-buffer
                                         (insert-file-contents file)
                                         (thing-at-point 'line)))))
                           (when line
                             line)))))
    (when (or (not file-lastdir)
              (not (string= directory file-lastdir)))
      (with-temp-buffer
        (insert (expand-file-name default-directory))
        ;; Force Emacs to read and write the exact internal byte representation
        ;; of the text without attempting any implicit encoding or decoding
        ;; conversions.
        (let ((coding-system-for-write 'utf-8-emacs)
              (write-region-annotate-functions nil)
              (write-region-post-annotation-function nil))
          (let ((inhibit-quit t))
            (write-region (point-min) (point-max) file
                          nil 'silent)))))))

;; (add-hook 'find-file-hook #'my-update-bash-lastdir)
(add-hook 'window-buffer-change-functions #'my-update-bash-lastdir)

;;; shell-pop: shell pop per project

(defun my-shell-pop-set-global-type (&rest _args)
  "Set `shell-pop-shell-type' as a global variable for the current project."
  (when (fboundp 'my-project-name)
    (let* ((proj-name (or (my-project-name) "misc"))
           (buf-name (format "*vterm:%s*" proj-name)))
      ;; (setopt shell-pop-shell-type (list "ansi-term"
      ;;                                    "*ansi-term*"
      ;;                                    `(lambda ()
      ;;                                       (let* ((shell-cmd
      ;;                                               (format "tmux-session emacs-%s"
      ;;                                                       (replace-regexp-in-string
      ;;                                                        "[^a-z0-9]+" "-"
      ;;                                                        (shell-quote-argument ,proj-name)))))
      ;;                                         (ansi-term shell-cmd)))))
      (setopt shell-pop-shell-type
              (list "vterm"
                    buf-name
                    `(lambda ()
                       (let* ((vterm-shell
                               (format "tmux-session emacs-%s"
                                       (replace-regexp-in-string
                                        "[^a-z0-9]+" "-"
                                        (shell-quote-argument ,proj-name))))
                              (tmux-buffer (vterm ,buf-name))))))))))

(with-eval-after-load 'shell-pop
  ;; Apply the new global setting advice
  (advice-add 'shell-pop :before #'my-shell-pop-set-global-type))

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
;; Force Emacs to read and write the exact internal byte representation
;; of the text without attempting any implicit encoding or decoding
;; conversions.
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
;; (add-hook 'lightemacs-after-init-hook #'global-so-long-mode)

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
;;    programs to output raw 24-bit RGB ANSI escape sequences, entirely
;;    bypassing the color limitations of the active terminfo profile. Emacs can
;;    natively parse and render these 24-bit codes.
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

;;; embark / vertico

;; (require 'vertico-multiform)
;; (vertico-multiform-mode 1)
;; ;; Force all Vertico sessions to use the 'buffer' display style
;; (setq vertico-multiform-commands
;;       '((t buffer)))

;; Lightemacs?
;; (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

;; is this good?
;; (setq display-buffer-alist
;;       '(("\\*vertico\\*"
;;          (display-buffer-reuse-window
;;           display-buffer-at-bottom)
;;          (window-height . 0.5)
;;          (preserved-parameters . (window-height)))))

;; (use-package vertico-buffer
;;   :ensure nil
;;   :defer t
;;   :commands (vertico-buffer-mode
;;              vertico-buffer--redisplay)
;;   :hook (vertico-mode . vertico-buffer-mode)
;;
;;   ;; causes issues, maybe also because of stillness-mode
;;   ;; :preface
;;   ;; (defun my-fit-minibuffer-to-content (win)
;;   ;;   "Try to fit the minibuffer window height to its content before redisplay."
;;   ;;   (ignore-errors
;;   ;;     (fit-window-to-buffer win)))
;;   ;;
;;   ;; :config
;;   ;; ;; TODO: Make it grow only
;;   ;; (advice-add #'vertico-buffer--redisplay
;;   ;;             :after #'my-fit-minibuffer-to-content)
;;
;;   :init
;;   (setq vertico-buffer-display-action '((display-buffer-at-bottom)
;;                                         (window-height . 0.38))))

;;   ;; (defun my-update-consult-async-settings (&rest _)
;;   ;;   (if (string= (battery-angel-get-charging-state) "AC")
;;   ;;       (setq consult-async-input-debounce 0.02
;;   ;;             consult-async-input-throttle 0.05
;;   ;;             consult-async-refresh-delay 0.02)
;;   ;;     (setq consult-async-input-debounce 0.2
;;   ;;           consult-async-input-throttle 0.5
;;   ;;           consult-async-refresh-delay 0.2)))
;;
;;   ;; (my-update-consult-async-settings)
;;
;;   ;; (advice-add 'consult-ripgrep :before #'my-update-consult-async-settings)
;;   ;; (advice-add 'consult-fd :before #'my-update-consult-async-settings)
;;   ;; (advice-add 'consult-recent-file :before #'my-update-consult-async-settings)
;;
;;   ;; (with-eval-after-load 'evil
;;   ;;   (evil-define-key '(insert normal) consult-async-map (kbd "M-s") 'consult-history))
;;   )

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

;;; git gutter

(lightemacs-use-package git-gutter
  :commands (git-gutter-mode)

  :init
  (setq git-gutter:added-sign "|"
        git-gutter:deleted-sign "-"
        git-gutter:ask-p nil
        git-gutter:diff-option "-w"
        git-gutter:handled-backends '(git)
        git-gutter:disabled-modes '(image-mode fundamental-mode)
        git-gutter:hide-gutter nil
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

;;; le-undo-fu

(with-eval-after-load 'le-undo-fu-session
  (setq undo-fu-session-incompatible-files
        '(;; ".*\\.age$"
          ;; "/\\.authinfo\\'"
          ;; "/\\.netrc\\'"
          ;; "/SQUASH_MSG\\'"
          ;; "/TAG_EDITMSG\\'"
          ;; "/PULLREQ_EDITMSG\\'"
          "^/tmp/"
          "^/var/tmp/")))

;;; savefold

;; TODO kill emacs hook issue when editing .asc file, it asks for the password
;; (lightemacs-use-package savefold
;;   :init
;;   ;; Removed org (buggy)
;;   (setq savefold-backends '(outline hideshow treesit-fold markdown))
;;
;;   ;; (setq savefold-directory (locate-user-emacs-file "savefold"))
;;   (setq savefold-directory
;;         (expand-file-name "savefold" my-shared-user-emacs-directory))
;;
;;   (setq org-startup-folded 'showeverything)
;;
;;   :preface
;;   ;; Fixes: https://github.com/jcfk/savefold.el/issues/7
;;   (defun my-savefold-utils--get-attr-table-fpath (fpath)
;;     "Return the FPATH of the attribute table file for FPATH.
;;   This naively replaces path slashes with ! (/a/b/c -> !a!b!c) leading to a
;;   chance of collision."
;;     (let* ((fpath (expand-file-name fpath))
;;            (fpath (string-replace "/" "!" fpath))
;;            (fpath (string-replace ":" "!" fpath))  ; For windows
;;            (old-fpath (expand-file-name fpath savefold-directory))
;;            (new-fpath (expand-file-name (concat fpath ".savefold")
;;                                         savefold-directory)))
;;       (if (and (file-exists-p old-fpath)
;;                (not (file-exists-p new-fpath)))
;;           old-fpath
;;         new-fpath)))
;;   (advice-add 'savefold-utils--get-attr-table-fpath :override
;;               #'my-savefold-utils--get-attr-table-fpath)
;;
;;   :config
;;   (savefold-mode 1))

;; Bug fix: https://github.com/jcfk/savefold.el/issues/7
(defun my-savefold-append-ext-advice (return-path)
  "Append `savefold' extension to the RETURN-PATH."
  (concat return-path ".savefold"))

(with-eval-after-load 'savefold
  (advice-add 'savefold-utils--get-attr-table-fpath
              :filter-return
              #'my-savefold-append-ext-advice))

(defun my-save-buffer-savefold-advice (&rest _)
  "Advise `save-buffer' to persist folds when the buffer is unmodified.
Standard save hooks handle persistence when the buffer is modified."
  (when (and (bound-and-true-p savefold-mode)
             (not (buffer-modified-p)))
    (dolist (backend (bound-and-true-p savefold-backends))
      (let ((save-func (intern-soft (format "savefold-%s--save-folds" backend)))
            (pred-func (intern-soft (format "savefold-%s--bufferp" backend))))
        (when (and save-func (fboundp save-func)
                   pred-func (fboundp pred-func)
                   (funcall pred-func))
          (funcall save-func))))))

;; (with-eval-after-load 'buffer-guardian
;;   (with-eval-after-load 'savefold
;;     (advice-add 'buffer-guardian-save-buffer :before
;;                 #'my-save-buffer-savefold-advice)
;;     (advice-add 'buffer-guardian-save-buffer-maybe :before
;;                 #'my-save-buffer-savefold-advice)))

;;; ghostel

;; (lightemacs-use-package ghostel
;;   :bind (("C-c g" . ghostel)
;;          ("C-c p" . ghostel-project)
;;          ("C-c o" . ghostel-other))
;;   :custom
;;   (ghostel-term "xterm-ghostty")
;;   (ghostel-scroll-on-input t)
;;   (ghostel-enable-url-detection t)
;;   (ghostel-enable-file-detection t)
;;   (ghostel-query-before-killing 'auto)
;;   (ghostel-max-scrollback (* 5 1024 1024))
;;   (ghostel-module-directory "~/.config/emacs/ghostel-bin/")
;;   (ghostel-module-auto-install nil)
;;   :config
;;   (setq ghostel-shell-integration t)
;;   (ghostel-sync-theme)
;;
;;   ;; (with-eval-after-load 'project
;;   ;;   (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t))
;;   ;;
;;   ;; (use-package ghostel-compile
;;   ;;   :bind (("C-c c" . ghostel-compile)
;;   ;;          ("C-c r" . ghostel-recompile)))
;;
;;   ;; (use-package ghostel-eshell
;;   ;;   :after eshell
;;   ;;   :config
;;   ;;   (add-hook 'eshell-load-hook #'ghostel-eshell-visual-command-mode))
;;   )
;;
;; ;; Evil-mode integration for tracking terminal state transitions
;; (lightemacs-use-package evil-ghostel
;;   :after (ghostel evil)
;;   :hook (ghostel-mode . evil-ghostel-mode)
;;   :custom
;;   ;; Configures the default entry state when initializing a new terminal buffer
;;   ;; Options include 'insert, 'normal, or 'emacs
;;   (evil-ghostel-initial-state 'insert)
;;
;;   ;; Dictates how the dynamic module handles the ESC key inside insert state
;;   ;; 'auto routes ESC to the shell if an alt-screen TUI (like vim) is running,
;;   ;; otherwise it switches the buffer frame to evil normal state.
;;   (evil-ghostel-escape 'auto))

;;; diff-hl setup

(defun my-diff-hl-set-upstream-reference (&rest _)
  "Set `diff-hl-reference-revision' to the default branch's upstream.
This function uses `vc' to detect the default branch and its upstream (which
properly handles remote files over Tramp), applying the setting only if
`diff-hl-mode' is enabled."
  (interactive)
  (let (target-remote
        reference
        (file-name (buffer-file-name (buffer-base-buffer))))
    ;; Only proceed if the current directory is controlled by Git
    (when (and (fboundp 'vc-git-command)
               (eq (vc-backend file-name) 'Git))
      (with-temp-buffer
        ;; Determine the target remote (prefer 'upstream' over 'origin')
        (when (ignore-errors
                (vc-git-command (current-buffer) 0 nil "remote")
                t)
          (let ((remotes (split-string (buffer-string) "\n" t)))
            (setq target-remote (cond ((member "upstream" remotes) "upstream")
                                      ((member "origin" remotes) "origin")))))

        (when target-remote
          (erase-buffer)
          ;; Detect the default branch on the target remote
          (if (ignore-errors
                (vc-git-command (current-buffer) 0 nil
                                "symbolic-ref" "--short"
                                (format "refs/remotes/%s/HEAD" target-remote))
                t)
              (progn
                (goto-char (point-min))
                (when (looking-at "[^\n]+")
                  (setq reference (match-string 0))))
            ;; Fallback if remote HEAD is not set locally: look for main or
            ;; master
            (erase-buffer)
            (when (ignore-errors
                    (vc-git-command (current-buffer) 0 nil
                                    "branch" "-r" "--format=%(refname:short)")
                    t)
              (goto-char (point-min))
              (when (re-search-forward
                     (format "^%s/\\(main\\|master\\)$" target-remote) nil t)
                (setq reference (match-string 0))))))

        ;; Fallback to local main or master if no upstream reference was found
        (unless reference
          (erase-buffer)
          (let (current-branch)
            ;; Get the current local branch
            (when (ignore-errors
                    (vc-git-command (current-buffer) 0 nil
                                    "rev-parse" "--abbrev-ref" "HEAD")
                    t)
              (goto-char (point-min))
              (when (looking-at "[^\n]+")
                (setq current-branch (match-string 0))))
            ;; Only search for a fallback if the current branch is not main or
            ;; master
            (unless (member current-branch '("main" "master"))
              (erase-buffer)
              (when (ignore-errors
                      (vc-git-command (current-buffer) 0 nil
                                      "branch" "--format=%(refname:short)")
                      t)
                (goto-char (point-min))
                (when (re-search-forward "^\\(main\\|master\\)$" nil t)
                  (setq reference (match-string 0))))))))

      ;; Set the local variable and update diff-hl
      (when (called-interactively-p 'any)
        (message "Update Git reference to: %s" reference))

      (if (not reference)
          (setq-local diff-hl-reference-revision nil)
        (setq-local diff-hl-reference-revision reference)
        (when (and (bound-and-true-p diff-hl-mode)
                   (fboundp 'diff-hl-update))
          (diff-hl-update))))))

(defun my-setup-diff-hl-mode ()
  "Setup diff-hl mode if the buffer is backed by a suitable file."
  (when (and (fboundp 'diff-hl-mode)
             (not (bound-and-true-p diff-hl-mode)))
    (let ((file (buffer-file-name))
          expanded-file)
      (when (and file
                 (setq expanded-file (expand-file-name file))
                 (not (string= (file-name-nondirectory expanded-file) "todo.org"))
                 (not (and (bound-and-true-p diff-hl-disable-on-remote)
                           (file-remote-p expanded-file)))
                 (vc-backend expanded-file))
        ;; (git-gutter-mode 1)
        (diff-hl-mode 1)
        ;; (diff-hl-flydiff-mode 1)
        ))))

(with-eval-after-load 'diff-hl
  (if (fboundp 'diff-hl-set-reference-rev)
      (define-key diff-hl-mode-map (kbd "C-x v b") #'diff-hl-set-reference-rev)
    (error "Undefined: diff-hl-set-reference-rev"))
  ;; (advice-add 'diff-hl-mode :before #'my-diff-hl-set-upstream-reference)
  )

(add-hook-text-editing-modes 'my-setup-diff-hl-mode)

;; (setq-default diff-hl-reference-revision "origin/main")
(setq diff-hl-ask-before-revert-hunk t)
(setq diff-hl-disable-on-remote t)
(setq diff-hl-draw-borders nil)
(setq diff-hl-next-previous-hunk-auto-recenter nil)
(setq diff-hl-autohide-margin t)
(setq diff-hl-bmp-max-width 16)
(setq diff-hl-global-modes '(not image-mode pdf-view-mode))

;;; Disabled: Emacs Consult / Vertico

;; (setq consult-imenu-config
;;       '((emacs-lisp-mode :toplevel "Functions"
;;                          :types ((?f "Functions" font-lock-function-name-face)
;;                                  (?m "Macros"    font-lock-function-name-face)
;;                                  (?p "Packages"  font-lock-constant-face)
;;                                  (?t "Types"     font-lock-type-face)
;;                                  (?v "Variables" font-lock-variable-name-face)))))

;;; Disabled: consult flymake

;; (with-eval-after-load 'consult
;;   (with-eval-after-load 'flymake
;;     (require 'consult-flymake)))

;; TODO fix vertico after enabling this
;; (lightemacs-use-package vertico-repeat
;;   ;; Vertico repeat last command
;;   :ensure nil
;;   :after vertico
;;   :commands (vertico-repeat-last
;;              vertico-repeat
;;              vertico-repeat-save)
;;   :hook
;;   (minibuffer-setup . vertico-repeat-save)
;;   :init
;;   (evil-define-key 'normal 'global (kbd "<leader>vr") #'vertico-repeat-last))

;;(lightemacs-use-package vertico-quick
;;  :ensure nil
;;  :after (vertico)
;;  :custom
;;  (vertico-quick1 "aoeuid")
;;  (vertico-quick2 "htns")
;;  :commands (vertico-quick-insert
;;             vertico-quick-exit
;;             vertico-quick-jump)
;;  :general
;;  (emacs-map
;;   'vertico-map
;;   "M-f" #'vertico-quick-insert
;;   "M-," #'vertico-quick-insert
;;   "M-." #'vertico-quick-exit))

;; (lightemacs-use-package vertico-directory
;;   :ensure nil
;;   :after vertico
;;   :defer t
;;   :commands vertico-directory-tidy
;;   ;; More convenient directory navigation commands
;;   :bind (:map vertico-map
;;               ("DEL" . vertico-directory-delete-char)
;;               ("M-DEL" . vertico-directory-delete-word))
;;   ;; Tidy shadowed file names
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;; Lazy autorevert

(lightemacs-use-package lazy-autorevert
  :ensure nil
  :commands lazy-autorevert-mode
  :hook (lightemacs-on-first-file . lazy-autorevert-mode)
  :init
  (setq lazy-autorevert-debug nil))

;;; eldoc

(defun my-setup-eldoc-mode ()
  "Setup eldoc mode."
  (unless (bound-and-true-p eldoc-mode)
    (setq-local eldoc-idle-delay 0.4)
    (eldoc-mode 1)))

(lightemacs-use-package eldoc
  :ensure nil
  :commands (global-eldoc-mode
             eldoc-mode)
  :hook ((lisp-interaction-mode . my-setup-eldoc-mode)
         (bash-ts-mode . my-setup-eldoc-mode)
         (sh-mode . my-setup-eldoc-mode)
         (emacs-lisp-mode . my-setup-eldoc-mode)
         (python-mode . my-setup-eldoc-mode)
         (python-ts-mode . my-setup-eldoc-mode))

  :init
  ;; (setq eldoc-message-commands-table-size 63)
  (setq eldoc-idle-delay most-positive-fixnum))

;;; corfu history

;; Corfu: Corfu candidate lists are typically shorter, so its default sorting is
;; simpler. To get it to sort by recency, you must explicitly enable
;; corfu-history-mode.
;;
;; Comparison with Prescient: While corfu-history sorts purely by recency (your
;; most recent selection goes to the top), Prescient tracks both recency and
;; frequency using an exponential decay algorithm. With corfu-history, selecting
;; a rare candidate once will immediately place it above your most frequently
;; used candidates. Prescient prevents this by keeping high-frequency candidates
;; near the top even if they were not the absolute last item selected.
(with-eval-after-load 'corfu
  (require 'corfu-history)
  (when (fboundp 'corfu-history-mode)
    (corfu-history-mode 1)))

;;; pathaction

;; (defun pathaction-vterm (command name)
;;   "Run COMMAND in `vterm' named NAME."
;;   (if (require 'vterm nil t)
;;       (when (fboundp 'vterm)
;;         (let* ((inhibit-redisplay t)
;;                ;; Override the shell to run the command directly
;;                (vterm-shell command)
;;                (term-buffer (vterm name)))
;;           (ignore vterm-shell)
;;           (pop-to-buffer term-buffer)
;;           term-buffer))
;;     (error "vterm is not available")))

(with-eval-after-load 'pathaction
  (if (fboundp 'pathaction-vterm)
      (setq pathaction-term-function #'pathaction-vterm)
    (error "Undefined: pathaction-vterm")))

;;; VC

(setq vc-git-log-switches '("--stat"))

;; The function that is called by default is `vc-shrink-buffer-window',
;; which calls `shrink-window-if-larger-than-buffer' when BUFFER is visible.
;; This function shrinks height of WINDOW if its buffer doesn't need so many
;; lines. More precisely, shrink WINDOW vertically to be as small as possible,
;; while still showing the full contents of its buffer. WINDOW must be a live
;; window and defaults to the selected one.
(setq vc-diff-finish-functions nil)
(setq vc-git-diff-switches '("--histogram"  ; Faster algorithm
                             "--stat"
                             "--textconv"

                             ;; "--ignore-cr-at-eol"

                             ;; Ignore changes in amount of white space.
                             ;; For example these would be considered the same:
                             ;; -foo    bar
                             ;; +foo bar
                             ;; "--ignore-space-change"

                             ;; Ignore all white space.
                             ;; "--ignore-all-space"
                             ;; "-w"

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

;;; VC: Support git-crypt repositories

;; TODO use a .dir-locals.el variable fo this

;; (defun mod-better-vc-git-crypt-support ()
;;   "Make git use --textconv if the current buffer is in a git-crypt repository.
;; This function checks whether the current buffer is under Git version control and
;; whether the repository contains a .git/git-crypt directory, which indicates
;; that git-crypt is being used. If both conditions are met and --textconv is
;; not already present in `vc-git-diff-switches', it appends --textconv
;; buffer-locally. This ensures that custom diff drivers defined in
;; .gitattributes (e.g., for decrypting files) are correctly invoked during diff
;; operations."
;;   (let ((file-name (buffer-file-name (buffer-base-buffer))))
;;     (when (and (memq 'Git vc-handled-backends)
;;                (not (file-remote-p file-name))
;;                (eq (vc-backend file-name) 'Git))
;;       (let* ((top-level-dir (vc-call-backend 'Git 'root file-name)))
;;         (when (and top-level-dir
;;                    (file-directory-p (expand-file-name ".git/git-crypt"
;;                                                        top-level-dir)))
;;           (unless (member "--textconv" vc-git-diff-switches)
;;             (setq-local vc-git-diff-switches
;;                         (append vc-git-diff-switches '("--textconv")))))))))
;;
;; (add-hook 'find-file-hook #'mod-better-vc-git-crypt-support 90)

;; Prevent Emacs from closing or shrinking the *vc-diff* window after showing a
;; diff.
;;
;; By default, when you run a version control diff (e.g., with `C-x v =`), Emacs
;; will try to clean up the *vc-diff* window after the diff is done. This means
;; it might close the window or make it smaller, depending on how the diff was
;; opened.
;;
;; That behavior can be annoying if you want to keep the diff visible to review
;; it longer or if you rely on a custom window layout.
;;
;; Setting `vc-diff-finish-functions' to nil disables that automatic cleanup, so
;; the *vc-diff* window stays open and its size is left unchanged.

;; Use a concise log format that shows only the file being logged, not the full
;; repository history.
;;
;; By default, when you run `vc-print-log` (e.g., with `C-x v l`), Emacs may
;; show the full Git commit history for the entire repository, even if you're
;; only interested in changes to a specific file.
;;
;; Setting `vc-log-short-style` to '(file) tells Emacs to restrict the log to
;; just the file associated with the current buffer. This results in a much
;; shorter and more relevant log output, especially in large repositories where
;; global history is overwhelming or unnecessary.
;; (setq vc-log-short-style '(file))  ; Default: '(directory)

;; Display `vc-annotate` output using a color scale.
;; (setq vc-annotate-display-mode 'scale)

;; Limit the summary line length in Git commit messages to encourage concise
;; messages.
;; (setq vc-git-log-edit-summary-target-len 50)
;; (setq vc-git-log-edit-summary-max-len 70)

;; Configure how Emacs formats and parses Git commit logs in the VC root log
;; view. Specifies the Git log output format and a regex pattern to extract
;; commit hash, author, date, refs, and message from each line, including
;; support for --graph decorations. This enables proper display and interaction
;; with commit entries.

;; Other
;; (setq vc-git-resolve-conflicts nil)

;; Silence the vc-resolve-conflicts issue when opening a file containing
;; conflicts. Why?
;; (setq vc-resolve-conflicts nil)

;;; highlight-numbers

;; (lightemacs-use-package highlight-numbers
;;   :commands highlight-numbers-mode
;;   :hook (emacs-lisp-mode . highlight-numbers-mode))

;;; Org: Font Lock Deferral

;; TODO: Bug emacs? This causes other buffers to have a slow jit lock.

;; (defun my-setup-defer-font-lock ()
;;   "Set jit-lock defer and stealth parameters when buffer is large.
;; Especially useful for large Org files with complex structure."
;;   (when (> (buffer-size) 100000) ;; Increased threshold to 100KB
;;     (setq-local jit-lock-defer-time 0.1
;;                 jit-lock-stealth-time 2
;;                 ;; Process fewer chars during idle time
;;                 jit-lock-stealth-load 200
;;                 ;; Process in larger chunks
;;                 jit-lock-chunk-size 10000
;;                 ;; Lower CPU usage during stealth fontification
;;                 jit-lock-stealth-nice 0.5)
;;     ;; Text is sometimes not highlighted. Ensure it is highlighted.
;;     ;; (font-lock-ensure)
;;     ))
;;
;; (add-hook 'org-mode-hook #'my-setup-defer-font-lock)

;;; Org

;; (setq org-attach-store-link-p 'attached     ; store link after attaching files
;;       org-attach-use-inheritance t) ; inherit properties from parent nodes

;; ;; Autoload all these commands that org-attach doesn't autoload itself
;; (use-package! org-attach
;;               :commands (org-attach-delete-one
;;                          org-attach-delete-all
;;                          org-attach-new
;;                          org-attach-open
;;                          org-attach-open-in-emacs
;;                          org-attach-reveal-in-emacs
;;                          org-attach-url
;;                          org-attach-set-directory
;;                          org-attach-sync)
;;               :config
;;               (unless org-attach-id-dir
;;                 ;; Centralized attachments directory by default
;;                 (setq-default org-attach-id-dir (expand-file-name ".attach/" org-directory)))
;;               (after! projectile
;;                       (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir)))

;; ;; Allow inline image previews of http(s)? urls or data uris.
;; ;; `+org-http-image-data-fn' will respect `org-display-remote-inline-images'.
;; (setq org-display-remote-inline-images 'download) ; TRAMP urls
;; (org-link-set-parameters "http"  :image-data-fun #'+org-http-image-data-fn)
;; (org-link-set-parameters "https" :image-data-fun #'+org-http-image-data-fn)
;; (org-link-set-parameters "img"   :image-data-fun #'+org-inline-image-data-fn)

;; (+org-define-basic-link "org" 'org-directory)

;; (org-link-set-parameters
;;  "file" :face (lambda (path)
;;                 (if (or
;;                      ;; file uris is not a valid path on windows
;;                      ;; ref https://lists.gnu.org/archive/html/bug-gnu-emacs/2024-05/threads.html#00729
;;                      ;; emacs <= 29 crashes for (file-exists-p "file://whatever")
;;                      (if (featurep :system 'windows) (string-prefix-p "//" path))
;;                      (file-remote-p path)
;;                      ;; filter out network shares on windows (slow)
;;                      (if (featurep :system 'windows) (string-prefix-p "\\\\" path))
;;                      (file-exists-p path))
;;                     'org-link
;;                   '(warning org-link))))

;; (pushnew! org-link-abbrev-alist
;;           '("github"      . "https://github.com/%s")
;;           '("youtube"     . "https://youtube.com/watch?v=%s")
;;           '("google"      . "https://google.com/search?q=")
;;           '("gimages"     . "https://google.com/images?q=%s")
;;           '("gmap"        . "https://maps.google.com/maps?q=%s")
;;           '("kagi"        . "https://kagi.com/search?q=%s")
;;           '("duckduckgo"  . "https://duckduckgo.com/?q=%s")
;;           '("wikipedia"   . "https://en.wikipedia.org/wiki/%s")
;;           '("wolfram"     . "https://wolframalpha.com/input/?i=%s")
;;           `("emacsdir"    . ,(doom-path doom-emacs-dir "%s"))
;;           )


;; Allow creating new parent nodes when refiling, but ask for confirmation;
;; provides flexibility in organization but adds an extra prompt that may
;; interrupt workflow.
;; TODO: Add back?
;; (setq org-refile-allow-creating-parent-nodes 'confirm)

;; Do not fontify the entire block delimiter line; prevents color bleeding
;; when folding headings or blocks.
;; TODO add back?
;; (setq org-fontify-whole-block-delimiter-line nil)

;; Log completion time; provides audit trail but adds automatic notes that may
;; clutter logs.
;; TODO: Add back?
;; (setq org-log-done 'time)

;; Disable highlighting LaTeX fragments and related elements; avoids interfering
;; with elisp regex in source blocks
;; (setq org-highlight-latex-and-related nil)

;; Do not automatically adjust indentation based on outline structure;
;; preserves original formatting.
;; TODO: add back?
;; (setq org-adapt-indentation nil)


;; Prevent marking a to do with checkboxes as DONE if any checkboxes are
;; incomplete; preserves logical consistency but may frustrate users if
;; subtasks are partially complete.
;; TODO add back?
;; (setq org-enforce-todo-checkbox-dependencies t)

;; Prevents clutter in agenda by skipping already done scheduled tasks.
;; TODO add back
;; (setq org-agenda-skip-scheduled-if-done t)

;; Reduces clutter for completed tasks with deadlines.
;; TODO add back
;; (setq org-agenda-skip-deadline-if-done t)

;; TODO add this?
;; (with-eval-after-load 'org
;;   (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit))

;; (require 'org)
;; (require 'org-agenda)

;; (with-eval-after-load 'org-indent
;;   (setq org-indent-agent-timer
;;   	    (run-with-idle-timer 0.01 t #'org-indent-initialize-agent))
;;   )


;; TODO contrib org?
;; (with-eval-after-load 'org-indent
;;   (defun my-around-org-indent-mode (fn &rest args)
;;     "FN is the advised function. ARGS are the function arguments."
;;     (apply fn args)
;;     (when (bound-and-true-p org-mode)
;;       (when org-indent-agent-timer
;;         (cancel-timer org-indent-agent-timer))
;;       (setq org-indent-agent-timer
;; 	          (run-with-idle-timer 0 t #'org-indent-initialize-agent))))
;;
;;   (advice-add 'org-indent-mode :around #'my-around-org-indent-mode))



;; Keep new notes at the end of the entry rather than the beginning; preserves
;; chronological order but may make recent notes less immediately visible.
;; (setq org-reverse-note-order nil)

;; When you run an agenda command, Org visits agenda files that are not yet
;; visited. When finding a file for the first time, Org checks the startup
;; options and apply them to the buffer: those options are either globally set
;; through the org-startup-* variables or on a per-file basis through the
;; #+STARTUP keyword.
;;
;; Especially, Org will honor the startup visibility status, as set by
;; org-startup-folded or #+STARTUP: folded.
;;
;; This may slow down the operation of visiting a file very much, and the
;; process of selecting agenda entries consequently.
;;

;; (org-startup-indented nil)     ;; No auto-indentation

;; Setting org-hide-emphasis-markers to t causes jumping issues in org-mode

;; Speed

;; org-catch-invisible-edits 'show-and-error

;; Disable automatic inheritance of properties; keeps properties local to the
;; entry
;; (setq org-use-property-inheritance nil)


;; (setq org-footnote-section nil) ; place footnotes locally
;; (setq org-footnote-auto-adjust nil) ; renumber footnotes

;; Logging


;; (org-log-state-notes-insert-after-drawers nil)
;; (org-log-redeadline 'time)
;; (org-log-reschedule 'time)


;; Movement

;; (setq org-todo-keywords '((sequence
;;                            "TODO(t)"
;;                            "WIP(p)"
;;                            "DONE(d)")))


;; (org-persist-directory (expand-file-name "org-persist/" emacs-var-dir))

;; Searching

;; Tasks
;; (org-refile-allow-creating-parent-nodes 'confirm)
;; (org-refile-use-cache t)
;; (org-reverse-note-order nil)
;; (org-todo-keywords
;;  '((sequence "TODO(t)" "|" "CANCEL(c@)" "DONE(d!)")
;;    (sequence "COACH(k)" "|" "COACHED(K!)")))
;; (org-todo-keyword-faces
;;  '(("CANCEL" . prot/org-bold-done)))
;; (org-track-ordered-property-with-tag t)
;; (org-highest-priority ?A)
;; (org-lowest-priority ?C)
;; (org-default-priority ?A)
;; (org-priority-faces nil)
;; (org-hide-macro-markers nil)
;; (org-link-context-for-files t)
;; (org-link-keep-stored-after-insertion nil)
;; (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; (add-hook 'org-mode-hook 'turn-on-auto-fill)

;; org-persist is a feature in Org mode that caches various Org-related data
;; to improve performance. It stores serialized data, such as parsed Org
;; buffers and computed results, in a persistent storage directory
;; (org-persist-directory). This speeds up operations like opening large Org
;; files.
;;
;; Disabling `org-persist` can slow down Org mode operations, especially for
;; large files, as it prevents caching of parsed data, leading to frequent
;; reparsing and slower agenda or refile actions.
;;
;; For someone who only reads `.org` files, the main drawback of disabling
;; `org-persist` is that Org mode will need to re-parse the file every time it
;; is opened, which can lead to slower performance, especially for larger
;; files. However, if the files are small, this impact may be minimal.
;; (setq org-element-use-cache nil)
;; (setq org-element-cache-persistent nil)


;; (add-hook 'org-capture-prepare-finalize-hook #'my-org-capture-switch-insert)
;; (add-hook 'org-capture-after-finalize-hook #'my-org-capture-switch-insert)
;; (add-hook 'org-capture-before-finalize-hook #'my-org-capture-switch-insert)

;; (setq org-appear-delay 0.4)
;; (setq org-appear-autoemphasis  t)
;; (setq org-appear-autosubmarkers t)
;; (setq org-appear-trigger 'always)
;; (setq org-appear-autolinks t)

;; (use-package evil-org-agenda
;;   :after (evil org-agenda)
;;   :ensure nil
;;   :config
;;   (evil-org-agenda-set-keys)
;;
;;   ;; (define-key org-agenda-keymap (kbd "C-k") nil)
;;   ;; (define-key org-agenda-keymap (kbd ",") nil)
;;   ;; (define-key org-agenda-keymap (kbd ",t") 'my-tab-split)
;;   ;; (define-key org-agenda-keymap (kbd "M-/") 'org-agenda-filter)
;;   ;; (define-key org-agenda-keymap (kbd "C-k") 'tab-previous)
;;   ;; (define-key org-agenda-keymap (kbd "C-j") 'tab-next)
;;   ;; (define-key org-agenda-keymap (kbd ",t") 'my-tab-split)
;;   )

;; (setq org-deadline-warning-days 5)

;; Most people want deadlines shown in the agenda by default.
;; (setq org-agenda-include-deadlines t)
;; (setq org-agenda-tags-column 0)
;; (setq org-agenda-confirm-kill nil)
;; (setq org-agenda-show-outline-path nil)
;; (setq org-agenda-todo-list-sublevels t)
;; (setq org-agenda-start-with-clockreport-mode t)
;; (setq org-agenda-clockreport-parameter-plist '(:link nil
;;                                                      :maxlevel 6
;;                                                      :fileskip0 t
;;                                                      :compact nil))
;; Makes agenda blocks visually more compact; helps readability.
;; (setq org-agenda-compact-blocks t)

;; (setq org-agenda-block-separator ?—)
;; (setq org-agenda-current-time-string
;;       "◀── now ─────────────────────────────────────────────────")
;;
;; (setq org-agenda-time-grid
;;       '((daily today require-timed)
;;         (0900 01000 1100 1200 1300 1400 1500 1600 1700 1800)
;;         "-"
;;         "────────────────"))
;; (setq org-agenda-prefix-format '((agenda . "%5c %4e %?-12t %s")
;;                                  (todo   . " %4e %-12c")
;;                                  (tags   . " %-22c")
;;                                  (search . " %-12c")))

;; (with-eval-after-load 'org-agenda
;;   ;; (setq org-agenda-start-day nil)
;;   ;; org-agenda-time-grid
;;   ;; '((daily today require-timed)
;;   ;;   (800 1000 1200 1400 1600 1800 2000)
;;   ;;   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
;;
;;
;;   ;; (org-agenda-span 'week)
;;   ;; (org-agenda-start-on-weekday nil)
;;   ;; (org-agenda-show-all-dates t)
;;   ;; (org-agenda-skip-comment-trees t)
;;   ;; (org-agenda-menu-show-matcher t)
;;   ;; (org-agenda-menu-two-columns nil)
;;   ;; (org-agenda-sticky nil)
;;   ;; (org-agenda-custom-commands-contexts nil)
;;   ;; (org-agenda-max-entries nil)
;;   ;; (org-agenda-max-todos nil)
;;   ;; (org-agenda-max-tags nil)
;;   ;; (org-agenda-max-effort nil)
;;
;;   ;; (org-agenda-fontify-priorities 'cookies)
;;   ;; (org-agenda-category-icon-alist nil)
;;   ;; (org-agenda-remove-times-when-in-prefix nil)
;;   ;; (org-agenda-remove-timeranges-from-blocks nil)
;;   ;; (org-agenda-compact-blocks nil)
;;   ;;
;;   ;; ;;;;; Agenda marks
;;   ;; (org-agenda-bulk-mark-char "#")
;;   ;; (org-agenda-persistent-marks nil)
;;   ;;
;;   ;; ;;;;; Agenda diary entries
;;   ;; (org-agenda-insert-diary-strategy 'date-tree)
;;   ;; (org-agenda-insert-diary-extract-time nil)
;;   ;; (org-agenda-include-diary nil)
;;   ;; ;; I do not want the diary, but there is no way to disable it
;;   ;; ;; altogether.  This creates a diary file in the /tmp directory.
;;   ;; (diary-file (make-temp-file "emacs-diary-"))
;;   ;; (org-agenda-diary-file 'diary-file) ; TODO 2023-05-20: review Org diary substitute
;;   ;;
;;   ;; ;;;;; Agenda follow mode
;;   ;; (org-agenda-start-with-follow-mode nil)
;;   ;; (org-agenda-follow-indirect t)
;;   ;;
;;   ;; ;;;;; Agenda multi-item tasks
;;   ;; (org-agenda-dim-blocked-tasks t)
;;   ;; (org-agenda-todo-list-sublevels t)
;;   ;;
;;   ;; ;;;;; Agenda filters and restricted views
;;   ;; (org-agenda-persistent-filter nil)
;;   ;; (org-agenda-restriction-lock-highlight-subtree t)
;;   ;;
;;   ;; ;;;;; Agenda items with deadline and scheduled timestamps
;;   ;; (org-agenda-include-deadlines t)
;;   ;; (org-deadline-warning-days 0)
;;   ;; (org-agenda-skip-scheduled-if-done nil)
;;   ;; (org-agenda-skip-scheduled-if-deadline-is-shown t)
;;   ;; (org-agenda-skip-timestamp-if-deadline-is-shown t)
;;   ;; (org-agenda-skip-deadline-if-done nil)
;;   ;; (org-agenda-skip-deadline-prewarning-if-scheduled 1)
;;   ;; (org-agenda-skip-scheduled-delay-if-deadline nil)
;;   ;; (org-agenda-skip-additional-timestamps-same-entry nil)
;;   ;; (org-agenda-skip-timestamp-if-done nil)
;;   ;; (org-agenda-search-headline-for-time nil)
;;   ;; (org-scheduled-past-days 365)
;;   ;; (org-deadline-past-days 365)
;;   ;; (org-agenda-move-date-from-past-immediately-to-today t)
;;   ;; (org-agenda-show-future-repeats t)
;;   ;; (org-agenda-prefer-last-repeat nil)
;;   ;; (org-agenda-timerange-leaders
;;   ;;  '("" "(%d/%d): "))
;;   ;; (org-agenda-scheduled-leaders
;;   ;;  '("Scheduled: " "Sched.%2dx: "))
;;   ;; (org-agenda-inactive-leader "[")
;;   ;; (org-agenda-deadline-leaders
;;   ;;  '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
;;   ;; ;; Time grid
;;   ;; (org-agenda-time-leading-zero t)
;;   ;; (org-agenda-timegrid-use-ampm nil)
;;   ;; (org-agenda-use-time-grid t)
;;   ;; (org-agenda-show-current-time-in-grid t)
;;   ;; (org-agenda-current-time-string (concat "Now " (make-string 70 ?.)))
;;   ;; (org-agenda-time-grid
;;   ;;  '((daily today require-timed)
;;   ;;    ( 0500 0600 0700 0800 0900 1000
;;   ;;      1100 1200 1300 1400 1500 1600
;;   ;;      1700 1800 1900 2000 2100 2200)
;;   ;;    "" ""))
;;   ;; (org-agenda-default-appointment-duration nil)
;;   ;;
;;   ;; ;;;;; Agenda global to-do list
;;   ;; (org-agenda-todo-ignore-with-date t)
;;   ;; (org-agenda-todo-ignore-timestamp t)
;;   ;; (org-agenda-todo-ignore-scheduled t)
;;   ;; (org-agenda-todo-ignore-deadlines t)
;;   ;; (org-agenda-todo-ignore-time-comparison-use-seconds t)
;;   ;; (org-agenda-tags-todo-honor-ignore-options nil)
;;   ;;
;;   ;; ;;;;; Agenda tagged items
;;   ;; (org-agenda-show-inherited-tags t)
;;   ;; (org-agenda-use-tag-inheritance
;;   ;;  '(todo search agenda))
;;   ;; (org-agenda-hide-tags-regexp nil)
;;   ;; (org-agenda-remove-tags nil)
;;   ;; (org-agenda-tags-column -100)
;;   ;;
;;   ;; ;;;;; Agenda entry
;;   ;; ;; NOTE: I do not use this right now.  Leaving everything to its
;;   ;; ;; default value.
;;   ;; (org-agenda-start-with-entry-text-mode nil)
;;   ;; (org-agenda-entry-text-maxlines 5)
;;   ;; (org-agenda-entry-text-exclude-regexps nil)
;;   ;; (org-agenda-entry-text-leaders "    > ")
;;   ;;
;;   ;; ;;;;; Agenda logging and clocking
;;   ;; ;; NOTE: I do not use these yet, though I plan to.  Leaving everything
;;   ;; ;; to its default value for the time being.
;;   ;; (org-agenda-log-mode-items '(closed clock))
;;   ;; (org-agenda-clock-consistency-checks
;;   ;;  '((:max-duration "10:00" :min-duration 0 :max-gap "0:05" :gap-ok-around
;;   ;;                   ("4:00")
;;   ;;                   :default-face ; This should definitely be reviewed
;;   ;;                   ((:background "DarkRed")
;;   ;;                    (:foreground "white"))
;;   ;;                   :overlap-face nil :gap-face nil :no-end-time-face nil
;;   ;;                   :long-face nil :short-face nil)))
;;   ;; (org-agenda-log-mode-add-notes t)
;;   ;; (org-agenda-start-with-log-mode nil)
;;   ;; (org-agenda-start-with-clockreport-mode nil)
;;   ;; (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2))
;;   ;; (org-agenda-search-view-always-boolean nil)
;;   ;; (org-agenda-search-view-force-full-words nil)
;;   ;; (org-agenda-search-view-max-outline-level 0)
;;   ;; (org-agenda-search-headline-for-time t)
;;   ;; (org-agenda-use-time-grid t)
;;   ;; (org-agenda-cmp-user-defined nil)
;;   ;; (org-agenda-sort-notime-is-late t) ; Org 9.4
;;   ;; (org-agenda-sort-noeffort-is-high t) ; Org 9.4
;;   ;;
;;   ;; ;;;;; Agenda column view
;;   ;; ;; NOTE I do not use these, but may need them in the future.
;;   ;; (org-agenda-view-columns-initially nil)
;;   ;; (org-agenda-columns-show-summaries t)
;;   ;; (org-agenda-columns-compute-summary-properties t)
;;   ;; (org-agenda-columns-add-appointments-to-effort-sum nil)
;;   ;; (org-agenda-auto-exclude-function nil)
;;   ;; (org-agenda-bulk-custom-functions nil)
;;
;;   ;; Old options
;;
;;   ;; (define-key org-agenda-keymap (kbd "C-w c") 'buffer-terminator-close-window)
;;   ;; (define-key org-agenda-keymap (kbd "C-w C-c") 'buffer-terminator-close-window)
;;   ;; (define-key org-agenda-keymap (kbd "C-k") nil)
;;   ;; (define-key org-agenda-keymap (kbd ",") nil)  ; Evil leader
;;   ;; (define-key org-agenda-keymap (kbd ", t") 'my-tab-split)  ; Evil leader
;;   ;; (define-key org-agenda-keymap (kbd "h") #'left-char)
;;   ;; (define-key org-agenda-keymap (kbd "j") #'org-agenda-next-line)
;;   ;; (define-key org-agenda-keymap (kbd "k") #'org-agenda-previous-line)
;;   ;; (define-key org-agenda-keymap (kbd "l") #'right-char)
;;   ;; (define-key org-agenda-keymap (kbd "C-f") #'scroll-up-command)
;;   ;; (define-key org-agenda-keymap (kbd "C-b") #'scroll-down-command)
;;   )

;;; Font lock deferral

;; JIT Lock Defer Time
;; -------------------
;; (setq fast-but-imprecise-scrolling nil)

;; NOTE: This adds timers, for some reason...
;; In addition to that, it causes issues with dired
;; (setq jit-lock-defer-time 0)  ; 0 = Fontification deferred while there is input

;; (setq jit-lock-defer-time 0.04)
;; (setq jit-lock-defer-time 0.1)
;; (setq jit-lock-defer-time 0)
;; (setq jit-lock-defer-time 0.25)

;; Override the value in dired since deleting in dired requires immediate font
;; lock update
;; (add-hook 'dired-mode-hook #'(lambda()
;;                                (setq-local jit-lock-defer-time nil)
;;                                (when (bound-and-true-p font-lock-mode)
;;                                  (font-lock-ensure))))

;; The number of lines to try scrolling a window by when point moves out.
;; If that fails to bring point back on frame, point is centered instead.
;; If this is zero, point is always centered after it moves off frame.
;; If you want scrolling to always be a line at a time, you should set
;; 'scroll-conservatively' to a large value rather than set this to 1.
;; (setq scroll-step 0)

;; Scroll Aggressively
;; - scroll-up-aggressively: Controls how far Emacs lets the point get from the
;;   bottom of the window before it scrolls.
;; - scroll-down-aggressively: Controls how far the point can get from the top
;;   of the window before scrolling.
;;
;; 0.01 means “scroll as soon as the point starts moving away from the edge.”.
;; 1 would mean “wait until the point is almost off the screen.”
;;
;; What happens with 0.01: When you scroll down (e.g., pressing C-n or C-v),
;; Emacs will try to keep the point near the top of the screen. When you scroll
;; up, Emacs will keep the point near the bottom.
;;
;; In Emacs, when you move the cursor (called the point) up or down, sometimes
;; the screen scrolls to keep the point visible. The variables
;; scroll-up-aggressively and scroll-down-aggressively control how soon that
;; scrolling happens.
;;
;; Benefits:
;; - Provides a more "stick-to-edge" scrolling experience, which many users find
;;   helpful when reading or editing code sequentially from top to bottom (e.g.,
;;   in programming).
;; - Avoids excessive recentring, giving a more predictable and less jumpy scroll.
;;
;; Also emacs by default will jump around a lot when scrolling a buffer with
;; images. Set the following variables to prevent that:
;; (setq scroll-up-aggressively 0.0
;;       scroll-down-aggressively 0.0)
;; (setq scroll-up-aggressively 0.01
;;       scroll-down-aggressively 0.01)

;; To speed up Emacs when working with large files, increasing
;; jit-lock-chunk-size to a larger value, such as 2000, will generally improve
;; performance by processing larger chunks of text at once, thereby reducing the
;; number of context-switching operations. However, be mindful of the tradeoff
;; between responsiveness and performance.
;; (setq jit-lock-chunk-size 1700)

;; The benefit of stealth fontification in Emacs, controlled by variables like
;; jit-lock-stealth-time, is that it optimizes the responsiveness of syntax
;; highlighting in large buffers by deferring font-lock updates until they are
;; absolutely necessary. This approach minimizes the impact of frequent syntax
;; re-evaluations on system performance, particularly when typing or scrolling
;; through large files. By reducing the frequency of font-lock recalculations,
;; stealth fontification ensures smoother user interactions, such as faster
;; scrolling and more immediate cursor movements, without sacrificing the
;; overall accuracy of syntax highlighting. This makes it especially beneficial
;; in large, complex files where real-time updates might otherwise cause
;; noticeable lag or slowdowns.

;; This controls the amount of time (in milliseconds) that jit-lock-mode will
;; wait before triggering a re-scan of the buffer for syntax highlighting. A
;; lower value like 16 means Emacs will update more quickly, while a higher
;; value would result in less frequent updates, possibly improving performance
;; but at the cost of delayed highlighting. jit-lock-stealth-time 16
;; (setq jit-lock-stealth-time 1)
;; jit-lock-stealth-time 0.2
;; jit-lock-stealth-time 1.0
;; jit-lock-stealth-time 2

;; This controls the priority given to jit-lock-mode when deciding whether to
;; perform syntax highlighting. Lower values (like 0.5) make jit-lock-mode more
;; "stealthy," meaning it will try to avoid interrupting the user's workflow
;; unless absolutely necessary, especially when performing other tasks.
;; (setq jit-lock-stealth-nice 0.2)

;; This enables or disables the contextual re-evaluation of font-locking. When
;; set to t, Emacs will try to do context-aware font-locking, adjusting the
;; syntax highlighting based on the context of the text, which can improve
;; highlighting accuracy for some languages.
;; (setq jit-lock-contextually t)

;; (with-no-warnings
;;   ;; Deprecated
;;   (setq jit-lock-defer-contextually jit-lock-contextually))

;; jit-lock-stealth-nice 0.1
;; jit-lock-stealth-nice 0.5

;; jit-lock-stealth-verbose nil

;; (setq jit-lock-stealth-load 10)
;; (setq jit-lock-stealth-verbose t)

;; Decrease the idle delay before context fontification happens. Setting this to
;; 0.05 (50ms) makes it nearly instantaneous without hitting performance.
;; (setq-default jit-lock-context-time 0.05)

;; Alternatively, you can completely disable deferred context fontification
;; if your machine handles syntax highlighting easily.
;; (setq jit-lock-contextually nil)

;;; Warnings

;; (unless (fboundp 'json-serialize)
;;   (warn "Native JSON is *not* available"))

(unless (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
  (warn "Native compilation is *not* available"))

;;; showparen

;; BUG never turn this variable on. it will break lispy AND smartparens
;; (setq show-paren-context-when-offscreen nil)
;; (setq show-paren-ring-bell-on-mismatch nil)
;; (setq show-paren-data-function #'show-paren--default)

;;; hippie expand

;; (setq hippie-expand-try-functions-list '(try-expand-dabbrev
;;                                          try-expand-dabbrev-from-kill
;;                                          try-expand-dabbrev-all-buffers
;;                                          try-complete-file-name-partially
;;                                          try-complete-file-name
;;                                          try-expand-all-abbrevs
;;                                          try-expand-list
;;                                          try-expand-line
;;                                          try-complete-lisp-symbol-partially
;;                                          try-complete-lisp-symbol))

;; (setq hippie-expand-try-functions-list
;;       '(try-complete-file-name-partially
;;         try-complete-file-name
;;         try-expand-dabbrev
;;         try-expand-dabbrev-all-buffers
;;         try-expand-dabbrev-from-kill))

;;; Papyrus Help

(defvar papyrus-help-hook nil
  "Hook run after describing an Elisp symbol at point.
This hook is executed after the function
`papyrus-help-elisp-symbol-at-point' describes the symbol at point as a
variable or function. It can be used to perform additional actions, such as
updating the display, highlighting defined functions and variables...")

(defun papyrus-help-elisp-symbol-at-point ()
  "Describe the symbol at point as either a variable or a function.

This function determines whether the symbol at point is a variable or a
function. If the symbol is identified as a function or a variable exclusively,
it will automatically describe it. If both types are detected or neither is
detected, the user is prompted to choose whether to describe it as a variable or
a function.

After describing the symbol, if a help buffer is active, it will rename the help
buffer to include the name of the symbol.

The function displays messages if the symbol at point is not a variable or
function or if an invalid choice is made."
  (interactive)

  (let* ((symbol (symbol-at-point))
         (is-function (and symbol (fboundp symbol)))
         (is-variable (and symbol (boundp symbol)))
         (choice nil))

    ;; Automatically select function or variable if only one is at point,
    ;; otherwise ask the user.
    (cond
     (is-function
      (setq choice ?f))
     (is-variable
      (setq choice ?v))
     (t
      (setq choice (read-char "Describe (v)ariable or (f)unction? "))))

    ;; Handle the chosen option for describing a function or variable.
    (cond
     ((eq choice ?v)
      (describe-variable symbol))
     ((eq choice ?f)
      (describe-function symbol))
     ((not choice)
      (message "No function or variable at point."))
     (t
      (message
       "Invalid choice, press 'v' for variable or 'f' for function.")))

    (when (or (eq choice ?v)
              (eq choice ?f))
      (let ((help-buffer (get-buffer "*Help*")))
        (when help-buffer
          (with-current-buffer help-buffer
            (rename-buffer
             (format "*Help:%s*" (symbol-name symbol)) t)
            (run-hooks 'papyrus-help-hook)))))))

(defun my-setup-evil-papyrus-help ()
  "Setup papyrus help."
  (setq-local evil-lookup-func #'papyrus-help-elisp-symbol-at-point))

(add-hook 'emacs-lisp-mode-hook #'my-setup-evil-papyrus-help)

(add-hook 'papyrus-help-hook #'(lambda()
                                 (when (fboundp 'highlight-defined-mode)
                                   (highlight-defined-mode))
                                 (when (bound-and-true-p font-lock-mode)
                                   (font-lock-ensure))))

;;; Load specific local variables

;; (when (bound-and-true-p lightemacs-package-manager)
;;   (put 'lightemacs-package-manager 'safe-local-variable
;;        (lambda (val) (memq val '(use-package straight elpaca)))))

;; TODO article
;; (defun my-hack-local-variables-apply ()
;;   "Remove all file-local variables except `no-byte-compile' and `lexical-binding'.
;; They are removed from `file-local-variables-alist'. This ensures that only these
;; variables are retained, preventing unwanted local variable settings."
;;   (unless (boundp 'file-local-variables-alist)
;;     (error (concat "`file-local-variables-alist` is not bound. "
;;                    "This may indicate that file-local variables are not "
;;                    "initialized.")))
;;   (setq file-local-variables-alist
;;         (cl-remove-if-not
;;          (lambda (entry)
;;            (memq (car entry) '(no-byte-compile
;;                                lexical-binding
;;                                package-lint-batch-fail-on-warnings
;;                                byte-compile-warnings)))
;;          file-local-variables-alist)))
;; (add-hook 'before-hack-local-variables-hook #'my-hack-local-variables-apply)

;;; TODO bufferfile delete

;; (defun my-dired-do-delete-advice (orig-fn &rest args)
;;   "Delete current file or all marked files.
;; ORIG-FN is the advised function. ARGS are the function arguments."
;;   (interactive "P" dired-mode)
;;   (let ((marked-files (dired-get-marked-files nil (car args))))
;;     (let ((buffer (when (and marked-files
;;                              (> (length marked-files) 0))
;;                     (get-file-buffer (car marked-files)))))
;;       (if (buffer-live-p buffer)
;;           (progn
;;             (bufferfile-delete buffer)
;;             (dired-post-do-command))
;;         (apply orig-fn args)))))
;;
;; (advice-add 'dired-do-rename :around 'my-dired-do-rename-advice)

;;; dabbrev

;; Control whether dabbrev searches should ignore case.
;; Any other non-nil version means case is not significant.
;; nil means case is significant.
(setq dabbrev-case-fold-search nil)

;; Whether dabbrev applies the abbreviations's case pattern to the expansion.
;; A value of nil means preserve the expansion's case pattern.
(setq dabbrev-case-replace nil)

(setq dabbrev-check-all-buffers nil)

;; It configures dabbrev (dynamic abbreviation expansion) to search only in the
;; current buffer when expanding abbreviations, instead of searching in other
;; buffers as well.
;; (setq dabbrev-check-other-buffers t)  ;; Default t

;; Set this variable to "\\sw" if you want ordinary words or "\\sw\\|\\s_" if
;; you want symbols (including characters whose syntax is "symbol" as well as
;; those whose syntax is "word"). The abbreviation is from point to the start
;; of the previous sequence of characters matching this variable.
;; (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

;; (dabbrev-ignored-buffer-names '("*Messages*" "*Ibuffer*"))
;; (dabbrev-limit 1000)
(setq dabbrev-case-distinction nil)

;; (setq dabbrev-case-fold-search nil)
;; (setq dabbrev-case-replace 'case-replace)
;; (setq dabbrev-check-other-buffers t)
;; (setq dabbrev-eliminate-newlines t)
;; (setq dabbrev-upcase-means-case-search t)

;; :init
;;
;; (when (fboundp 'my-setup-dabbrev)
;;   (add-hook 'bash-ts-mode-hook #'my-setup-dabbrev)
;;   (add-hook 'sh-mode-hook #'my-setup-dabbrev)
;;   (add-hook 'php-mode-hook #'my-setup-dabbrev))

;;; dabbrev boundaries 1

;; dabbrev-abbrev-char-regexp: Defines WHAT you are completing. By setting this
;; to "\\sw\\|\\s_", you tell Emacs which characters belong to the string (e.g.,
;; allowing underscores and hyphens).
;;
;; By default, dabbrev frequently stops at standard word boundaries. If you have
;; a variable named get_user_data elsewhere in your file, and you type get_u and
;; press M-/, vanilla Emacs might only look at the u as the target word. It will
;; complete it to user, leaving you with the broken string get_user.
;;
;; By explicitly adding \\s_ to the regex, you instruct Emacs to treat the
;; entire programming symbol (including underscores and hyphens) as a single,
;; unbroken block. Typing get_u and pressing M-/ will correctly grab the full
;; context and expand the string to get_user_data.
;; (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

;;; DISABLED: dabbrev boundaries 2

;; my-cape--dabbrev-bounds: Defines WHERE the completion applies relative to
;; your cursor. By overriding this, you dictate that the completion engine must
;; never overwrite text to the right of your cursor.

;; (defun my-setup-dabbrev ()
;;   ;; Skip $ (variables)
;;   (setq-local dabbrev-abbrev-skip-leading-regexp  "\\\\$"))

;; (setq-default dabbrev-abbrev-skip-leading-regexp  "\\\\$")

;; This configuration sets `dabbrev-abbrev-skip-leading-regexp` to a regular
;; expression matching any of the characters `$`, `*`, `/`, `=`, `~`, or `'` at
;; the beginning of a word. When dabbrev expands abbreviations, it uses this
;; regexp to ignore such leading characters, preventing them from being included
;; in the expanded text. This behavior is useful in programming contexts—such as
;; Bash, Python, and YAML—where these characters often prefix identifiers or
;; tokens, ensuring that dabbrev completes only the meaningful part of the word
;; without unwanted special characters.
;; (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")

;; (setq dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")

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
              (when (bound-and-true-p dabbrev-abbrev-skip-leading-regexp)
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

;;; DISABLED: PDF tools

;; (use-package pdf-tools
;;   :defer t
;;   :commands (pdf-view-themed-minor-mode
;;              pdf-isearch-minor-mode
;;              pdf-tools-install
;;              pdf-view-mode)
;;   :functions (pdf-view-refresh-themed-buffer)
;;   :magic ("%PDF" . pdf-view-mode)
;;   :custom
;;   (pdf-view-use-scaling t)
;;   :config
;;   (pdf-tools-install :no-query)
;;   :hook
;;   ((pdf-view-mode . pdf-view-themed-minor-mode)
;;    (pdf-view-mode . pdf-isearch-minor-mode)))

;;; DISABLED: tempel

;; (lightemacs-use-package tempel
;;   :commands (tempel-complete
;;              tempel-insert)
;;   :bind (("S-SPC" . tempel-complete) ;; expand or insert template
;;          ("M-SPC" . tempel-insert))  ;; prompt for template
;;   ;; :init
;;   ;; Add Tempel to completion-at-point
;;   ;; (add-to-list 'completion-at-point-functions #'tempel-complete)
;;
;;   ;; Set the template file location
;;   ;; (setq tempel-path (expand-file-name "templates.eld"
;;   ;;                                     (file-name-directory load-file-name)))
;;   )
;;
;; (lightemacs-use-package tempel-collection
;;   :after tempel)

;;; DISABLED: vundo

;; (lightemacs-use-package vundo
;;   :commands vundo
;;   :bind (("C-x u" . my-vundo-open))
;;   :init
;;   (setq vundo-glyph-alist vundo-unicode-symbols
;;         vundo-compact-display t)
;;   :config
;;   (defun my-vundo-open ()
;;     "Open vundo in a non-dedicated window."
;;     (interactive)
;;     (let ((display-buffer-alist
;;            '(("\\*vundo-tree\\*"
;;               (display-buffer-in-side-window)
;;               (side . right)
;;               (window-width . 0.3)
;;               (dedicated . nil)))))
;;       (vundo))))

;;; DISABLED: Flyspell lazy

;; (lightemacs-use-package flyspell-lazy
;;   :commands flyspell-lazy-mode
;;   :config
;;   :hook ((after-init . flyspell-lazy-mode))
;;   :init
;;   (setq flyspell-lazy-idle-seconds 1
;;         flyspell-lazy-window-idle-seconds 3)
;;   (flyspell-lazy-mode +1))

;;; DISABLED: disproject

;; (lightemacs-use-package disproject
;;   :bind ( :map ctl-x-map
;;           ("p" . disproject-dispatch)
;;           :map global-map
;;           ("C-c p" . disproject-dispatch)))

;;; DISABLED: erefactor

;; (lightemacs-use-package erefactor
;;   :commands (erefactor-highlight-current-symbol
;;              erefactor-eval-current-defun
;;              erefactor-add-current-defun
;;              erefactor-change-prefix-in-buffer
;;              erefactor-rename-symbol-in-buffer
;;              erefactor-rename-symbol-in-package
;;              erefactor-lint-by-emacsen
;;              erefactor-lint
;;              erefactor-check-eval-mode
;;              erefactor-lazy-highlight-turn-on)
;;   :no-require t)

;; (with-eval-after-load 'erefactor
;;   (remove-hook 'emacs-lisp-mode-hook 'erefactor-lazy-highlight-turn-on)
;;   (remove-hook 'lisp-interaction-mode-hook 'erefactor-lazy-highlight-turn-on))

;;; DISABLED: dirvish

;; (use-package dirvish
;;   :ensure nil
;;   :commands dirvish-override-dired-mode)

;;; DISABLED: org-modern

;; (use-package org-modern
;;   :defer t
;;   :commands org-modern-mode
;;   ;; :hook
;;   ;; (org-mode . org-modern-mode)
;;   ;; :config
;;   ;; (setq
;;   ;;  ;; Agenda styling
;;   ;;  org-agenda-tags-column 0
;;   ;;  org-agenda-block-separator ?─
;;   ;;  org-agenda-time-grid
;;   ;;  '((daily today require-timed)
;;   ;;    (800 1000 1200 1400 1600 1800 2000)
;;   ;;    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
;;   ;;  org-agenda-current-time-string
;;   ;;  "◀── now ─────────────────────────────────────────────────")
;;
;;   ;; (setq org-modern-block-name '("" . "")
;;   ;;       ;; org-modern-list '((43 . "•")
;;   ;;       ;;                   (45 . "-")
;;   ;;       ;;                   (42 . "↪"))
;;   ;;       ;; org-modern-radio-target    '("❰" t "❱")
;;   ;;       ;; org-catch-invisible-edits 'show-and-error
;;   ;;       ;; org-modern-internal-target '("↪ " t "")
;;   ;;       ;; org-modern-todo t
;;   ;;       ;; org-modern-tag t
;;   ;;       ;; org-modern-timestamp t
;;   ;;       ;; org-modern-statistics nil
;;   ;;       ;; org-modern-progress nil
;;   ;;       ;; org-modern-priority t
;;   ;;       ;; org-modern-horizontal-rule "──────────"
;;   ;;       ;; org-modern-hide-stars "·"
;;   ;;       ;; org-modern-star ["⁖"]
;;   ;;       ;; org-modern-keyword "‣"
;;   ;;       ;; org-modern-todo-faces
;;   ;;       ;; '(("[-]"  . +org-todo-active)
;;   ;;       ;;   ("NEXT" . +org-todo-active)
;;   ;;       ;;   ("STARTED" . +org-todo-active)
;;   ;;       ;;   ("WAITING" . +org-todo-onhold)
;;   ;;       ;;   ("CANCELED" . +org-archived)
;;   ;;       ;;   ("PROJ" . +org-todo-project)
;;   ;;       ;;   ("DONE"   . +org-todo-cancel))
;;   ;;       )
;;
;;   )

;;; DISABLED: eldoc box

;; (use-package eldoc-box
;;   :commands (eldoc-box-hover-mode
;;              eldoc-box-help-at-point
;;              eldoc-box-quit-frame
;;              eldoc-box-reset-frame
;;              eldoc-box-scroll-up
;;              eldoc-box-scroll-down
;;              eldoc-box-hover-at-point-mode)
;;
;;   ;; :config
;;   ;; (defun rex/eldoc-box-scroll-up ()
;;   ;;   "Scroll up in `eldoc-box--frame'"
;;   ;;   (interactive)
;;   ;;   (with-current-buffer eldoc-box--buffer
;;   ;;     (with-selected-frame eldoc-box--frame
;;   ;;       (scroll-down 3))))
;;   ;; (defun rex/eldoc-box-scroll-down ()
;;   ;;   "Scroll down in `eldoc-box--frame'"
;;   ;;   (interactive)
;;   ;;   (with-current-buffer eldoc-box--buffer
;;   ;;     (with-selected-frame eldoc-box--frame
;;   ;;       (scroll-up 3))))
;;   ;; :general
;;   ;; (:keymaps 'eglot-mode-map
;;   ;;           "C-k" 'rex/eldoc-box-scroll-up
;;   ;;           "C-j" 'rex/eldoc-box-scroll-down
;;   ;;           "M-h" 'eldoc-box-eglot-help-at-point)
;;   )

;;; DISABLED: difftastic

;; (lightemacs-use-package difftastic
;;   :commands (difftastic-files
;;              difftastic-buffers
;;              difftastic-file-buffer
;;              ;; Other functions
;;              difftastic-rerun
;;              difftastic-dired-diff
;;              difftastic-magit-diff-buffer-file
;;              difftastic-forge-pullreq-show-diff
;;              difftastic-forge-create-pulreq-show-diff
;;              difftastic-magit-show
;;              difftastic-magit-diff
;;              difftastic-git-diff-range)
;;   :config
;;   (difftastic-bindings-mode))

;;; DISABLED: auto-highlight-symbol

;; Default: M-→/← moves to the next/previous instance of the currently
;; highlighted word
;; (lightemacs-use-package auto-highlight-symbol
;;   :commands (global-auto-highlight-symbol-mode
;;              auto-highlight-symbol-mode)
;;   :init
;;   (setq ahs-default-range 'ahs-range-whole-buffer)
;;   ;; (auto-highlight-symbol-mode-map (make-sparse-keymap))
;;   ;; TODO: Fix exclusions, they don't seem to work.
;;   ;; (ahs-exclude '((ruby-mode . "\_<\(end\|def\|class\|module\)\_>")))
;;
;;   (setq ahs-case-fold-search nil  ;; TODO function + same as dabbrev
;;         ahs-inhibit-face-list '(
;;                                 font-lock-comment-delimiter-face
;;                                 font-lock-comment-face
;;                                 font-lock-doc-face
;;                                 font-lock-doc-string-face
;;                                 font-lock-string-face
;;                                 tree-sitter-hl-face:comment
;;                                 tree-sitter-hl-face:doc
;;                                 tree-sitter-hl-face:string)
;;         )
;;   (setq ahs-idle-interval 0.15)
;;   (setq ahs-default-range 'ahs-range-whole-buffer)
;;
;;   ;; (bind-keys :map auto-highlight-symbol-mode-map
;;   ;;            ("M-S-<right>")
;;   ;;            ("M-S-<left>")
;;   ;;            ("M--")
;;   ;;            ("M-<left>")
;;   ;;            ("M-<right>"))
;;
;;   ;; Better?
;;   ;; (bind-keys
;;   ;;  :map auto-highlight-symbol-mode-map
;;   ;;  ("M-<"     . ahs-backward)
;;   ;;  ("M->"     . ahs-forward)
;;   ;;  ("M--"     . ahs-back-to-start)
;;   ;;  ("C-x C-'" . ahs-change-range)
;;   ;;  ("C-x C-a" . ahs-edit-mode))
;;   )

;; This breaks Org Exports; e.g.,
;; C-c C-e h o  ⇒  Match data clobbered by buffer modification hooks

;;; DISABLED: cov / underdover

;; (lightemacs-use-package cov
;;   :defer t
;;   :commands (cov-mode
;;              cov-clear-overlays
;;              cov-visit-coverage-file
;;              cov-set-overlays
;;              cov-update)
;;   :custom
;;   (cov-coverage-mode t)
;;   :config
;;   ;; (custom-set-faces
;;   ;;  '(cov-none-face  ((((class color)) :foreground "red")))
;;   ;;  '(cov-light-face ((((class color)) :foreground "orange")))
;;   ;;  '(cov-med-face   ((((class color)) :foreground "yellow")))
;;   ;;  '(cov-heavy-face ((((class color)) :foreground "green"))))
;;
;;   ;; (face-spec-set 'cov-none-face
;;   ;;                '((((class color)) :foreground "red")))
;;   ;;
;;   ;; (face-spec-set 'cov-light-face
;;   ;;                '((((class color)) :foreground "orange")))
;;   ;;
;;   ;; (face-spec-set 'cov-med-face
;;   ;;                '((((class color)) :foreground "yellow")))
;;   ;;
;;   ;; (face-spec-set 'cov-heavy-face
;;   ;;                '((((class color)) :foreground "green")))
;;
;;   (set-face-attribute 'cov-none-face nil :foreground "red")
;;   (set-face-attribute 'cov-light-face nil :foreground "orange")
;;   (set-face-attribute 'cov-med-face nil :foreground "yellow")
;;   (set-face-attribute 'cov-heavy-face nil :foreground "green"))

;; (lightemacs-use-package undercover
;;   :defer t
;;   :commands undercover)

;;; DISABLED: combobulate

;; (use-package combobulate
;;   :defer t
;;   :commands combobulate
;;   :init
;;   (with-eval-after-load 'evil
;;     (evil-define-key 'normal 'global (kbd "<leader>co") #'combobulate)))
;; (eldoc-add-command-completions "combobulate-")

;;; DISABLED: desktop

;; (lightemacs-use-package desktop
;;   :ensure nil
;;   :demand t
;;   :commands (desktop-save-mode desktop-read)
;;
;;   :preface
;;   (eval-and-compile (defvar emacs-base-dir))
;;
;;   :custom
;;   (desktop-save t)
;;   (desktop-lazy-idle-delay 1)
;;   (desktop-lazy-verbose nil)
;;   (desktop-dirname emacs-var-dir)  ;; The directory in which the desktop file should be saved.
;;   (desktop-auto-save-timeout 120)
;;   (desktop-base-lock-name ".emacs.desktop.lock")
;;   (desktop-base-file-name "emacs-desktop")
;;   (desktop-load-locked-desktop t)
;;   (desktop-missing-file-warning nil)
;;   (desktop-restore-eager t)
;;   (desktop-restore-frames t)
;;   (desktop-restore-in-current-display t)
;;   (desktop-restore-reuses-frames t)  ;; If t, restoring frames reuses existing frames.
;;   (desktop-restore-in-current-display t)  ;; NEW: is set to nil to exclude settings related to the current display
;;   (desktop-path (list (file-name-as-directory emacs-var-dir)))
;;   (desktop-files-not-to-save (concat "\\(" "\\`/[^/:]*:"
;;                                      "\\|(ftp)\\'"
;;                                      "\\|\\.asc"
;;                                      "\\|\\.gpg"
;;                                      "\\)$"))
;;
;;   :init
;;   (setf (alist-get 'background-color frameset-filter-alist) :never)
;;   (setf (alist-get 'foreground-color frameset-filter-alist) :never)
;;   (setf (alist-get 'cursor-color frameset-filter-alist) :never)
;;   (setf (alist-get 'GUI:font frameset-filter-alist) :never)
;;   (setf (alist-get 'font frameset-filter-alist) :never)
;;   (setf (alist-get 'background-mode frameset-filter-alist) :never)
;;   (setf (alist-get 'ns-appearance frameset-filter-alist) :never)
;;
;;   :config
;;   (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
;;   (add-hook 'emacs-startup-hook #'desktop-save-mode)
;;   (add-hook 'emacs-startup-hook #'desktop-read))

;;; DISABLED: projectile

;; (use-package projectile
;;   :defer t
;;   :commands (projectile-mode projectile-mode-map project-switch-project)
;;
;;   :hook
;;   (after-init . projectile-mode)
;;
;;   :preface
;;   (eval-and-compile (defvar emacs-base-dir))
;;
;;   :custom
;;   (projectile-known-projects-file (concat emacs-var-dir "projectile-auto.eld"))
;;   (projectile-auto-discover t)
;;   (projectile-enable-caching nil)
;;   ;; (projectile-enable-caching (not noninteractive))
;;   ;; (projectile-file-exists-local-cache-expire (* 5 60))
;;   ;; (projectile-file-exists-remote-cache-expire (* 10 60))
;;   (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
;;   (projectile-globally-ignored-files '(".DS_Store" "TAGS"))
;;   (projectile-kill-buffers-filter 'kill-only-files)
;;   (projectile-project-search-path '("~/src"))
;;   (projectile-require-project-root nil)
;;   (projectile-switch-project-action #'projectile-dired)
;;   (projectile-track-known-projects-automatically t)
;;   (projectile-verbose nil)
;;   (projectile-project-root-files '(".project"
;;                                    "LICENSE"
;;                                    "COPYING"
;;                                    "README.md"
;;                                    "requirements.txt"
;;                                    "setup.py"))
;;
;;   ;; For Personal Workflow Enhancement: If your workflow benefits from seeing
;;   ;; the projects you've actively interacted with recently,
;;   ;; (projectile-sort-order 'recentf) is likely more useful. It aligns well
;;   ;; with workflows where engagement with specific files dictates project
;;   ;; relevance.
;;   ;;
;;   ;; For Active Development Focus: If you're more interested in seeing which
;;   ;; projects have changed recently, regardless of whether those changes were
;;   ;; made by you or by others (e.g., in a collaborative environment where
;;   ;; files might be updated by colleagues), then (projectile-sort-order
;;   ;; 'modification-time) might be more beneficial.
;;   ;; (projectile-sort-order 'modification-time)
;;   (projectile-sort-order 'recentf)
;;
;;   (projectile-project-root-functions
;;    '(
;;      ;; The buffer-local variable projectile-project-root.
;;      ;; Typically you'd set this variable via .dir-locals.el
;;      projectile-root-local
;;
;;      ;; Look for .projectile (projectile-dirconfig-file).
;;      projectile-root-marked
;;
;;      ;; This searches for project markers (like .git, .hg, etc.) defined in
;;      ;; projectile-project-root-files. Return the top-most (farthest from the
;;      ;; current directory) match.
;;      ;; (It's configurable via projectile-project-root-files)
;;      projectile-root-top-down
;;
;;      ;; This also searches for project markers, but it starts from your
;;      ;; current location and goes up the directory tree (think climbing the
;;      ;; tree from the top). It returns the first matching directory it finds,
;;      ;; assuming the project marker only appears once at the project's root.
;;      projectile-root-bottom-up
;;
;;      ;; Look for project markers that can appear at every level of a project
;;      ;; (e.g. Makefile or .svn) and will return the top-most match for those.
;;      projectile-root-top-down-recurring))
;;
;;   :config
;;   (add-to-list 'projectile-globally-ignored-directories emacs-base-dir)
;;
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;
;;   (with-eval-after-load 'evil
;;     ;; (evil-define-key 'normal 'global (kbd "<leader>epf") 'projectile-find-dir)
;;     ;; (evil-define-key 'normal 'global (kbd "<leader>ef") 'projectile-find-file) ;; conflict with treemacs
;;     ;; (evil-define-key 'normal 'global (kbd "<leader>ep") 'projectile-switch-project)
;;     (evil-define-key '(normal insert visual) 'global (kbd "M-p") 'projectile-switch-project))
;;
;;   (when (string= choice-minibuffer "ivy-counsel")
;;     (setq projectile-completion-system 'ivy))
;;
;;   (when my-rg-exists
;;     (setq-default projectile-generic-command my-projectile-rg-command)))

;;; Provide

(provide 'mod-misc)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-misc.el ends here

;;; sub-org.el --- sub-org -*- lexical-binding: t -*-

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

(eval-and-compile
  (require 'lightemacs-use-package))

(require 'org)

;;; Testing

;; Hide markers like * / _ = ~; cleaner view but markers are not visible for
;; editing emphasis.
;; TODO add again
;; Setting org-hide-emphasis-markers to t causes jumping issues in org-mode
(setq org-hide-emphasis-markers nil)

;;; Defaults

;; Enable modules on an opt-in basis to reduce initial Org load latency.
(setq org-modules nil)

(setq org-tags-column 0)

;; Define refile targets up to maxlevel in the current file and agenda files;
;; allows flexible refiling but may slow completion in very large files and
;; requires remembering hierarchy.
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

;; Use the full outline path when refiling; makes it easier to select the
;; correct target but requires remembering or seeing the full path.
(setq org-refile-use-outline-path t)

;; Ctrl-A/E moves to beginning/end of heading instead of line; improves
;; navigation.
(setq org-special-ctrl-a/e t)

;; Indent text according to heading level; makes outline visually clearer but
;; can misalign code blocks or tables.
(setq org-startup-indented t)

;; Color DONE headlines; quickly identifies completed tasks
(setq org-fontify-done-headline t)

;; Color to do headlines; improves task visibility
(setq org-fontify-todo-headline t)

;; Fontify the whole heading line; improves readability but may affect
;; alignment of inline content.
;; - Benefit: The whole heading line looks consistent and visually distinct.
;; - Drawback: Inline elements may be harder to read or lose their usual
;;             highlighting, especially if the heading face color is very dark
;;             or clashes with your theme.
(setq org-fontify-whole-heading-line t)

;; Fontify quote and verse blocks; highlights these blocks but may interfere
;; with other syntax highlighting.
(setq org-fontify-quote-and-verse-blocks t)

;; Disable sub/superscript interpretation (_ and ^)
(setq org-use-sub-superscripts '{})

;; Indentation per heading level; controls visual hierarchy but tight spacing
;; may feel cramped.
;; NOTE: Same as default
;; (setq org-indent-indentation-per-level 2)

;; Set ellipsis for folded sections; improves folding visibility but may not
;; suit all fonts.
(setq org-ellipsis lightemacs-ellipsis)

;; Allow alphabetical lists; flexible list styles but may confuse automatic
;; numbering.
(setq org-list-allow-alphabetical t)

;; More comprehensive imenu
(setq org-imenu-depth 6)

;; Disable stepwise path completion; direct path completion but may be harder
;; to navigate long hierarchies.
(setq org-outline-path-complete-in-steps nil)

;; Prevent marking a parent to do as done if its child tasks are incomplete;
;; ensures task consistency but may slow task completion when some subtasks
;; are still pending.
(setq org-enforce-todo-dependencies t)

;; Insert new headings after the current subtree instead of at point;
;; maintains logical structure
(setq org-insert-heading-respect-content t)

;; When nil, it will go to the end of the line before making a new line.
(setq org-M-RET-may-split-line nil)

;; No need to ask. Just exercise caution.
(setq org-confirm-babel-evaluate t  ; Security
      ;; Do not ask for confirmation before executing Emacs Lisp links.
      org-link-elisp-confirm-function nil)

;; Set tag column to 0 (tags appear immediately after heading); simplifies
;; layout but may make long headings with tags harder to read.
;;
;; Setting this to t will fold  stuff
;; (setq org-hide-block-startup nil) ; default nil
(setq org-startup-folded nil)

(setq org-hide-leading-stars t
      ;; showeverything is Org's default, but it ignores
      ;; org-hide-block-startup (#+startup: hideblocks), archived trees,
      ;; hidden drawers, and VISIBILITY properties. Setting it to nil has the
      ;; same effect functionally, but respects these settings.
      org-image-actual-width nil
      org-priority-faces
      '((?A . error)
        (?B . warning)
        (?C . shadow)))

(setq org-todo-keywords '((sequence "TODO" "DONE")))
;; (setq org-todo-keywords
;;       '((sequence
;;          "TODO(t)"
;;          "PROJ(p)"  ; A project
;;          "LOOP(r)"  ; A recurring task
;;          "STRT(s)"  ; A task that is in progress
;;          "WAIT(w)"  ; Something external is holding up this task
;;          "HOLD(h)"  ; This task is paused/on hold because of me
;;          "IDEA(i)"  ; An unconfirmed and unapproved task or notion
;;          "|"
;;          "DONE(d)"  ; Task successfully completed
;;          "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
;;         (sequence
;;          "TBOX(T)"  ; A task that needs doing
;;          "WBOX(S)"  ; Task is in progress
;;          "QBOX(W)"  ; Task is being held up or paused
;;          "|"
;;          "XBOX(D)") ; Task was completed
;;         (sequence
;;          "|"
;;          "OKAY(o)"
;;          "YES(y)"
;;          "NO(n)"))
;;       org-todo-keyword-faces
;;       '(("WBOX" . +org-todo-active)
;;         ("STRT" . +org-todo-active)
;;         ("QBOX" . +org-todo-onhold)
;;         ("WAIT" . +org-todo-onhold)
;;         ("HOLD" . +org-todo-onhold)
;;         ("PROJ" . +org-todo-project)
;;         ("NO"   . +org-todo-cancel)
;;         ("KILL" . +org-todo-cancel)))

;;; org

(defun my-hide-rng-what-schema-message (orig-fun &rest args)
  "Hide `rng-what-schema' message.
ORIG-FUN is the function and ARGS are its arguments."
  ;; (cl-letf* ((old-msg (symbol-function 'message))
  ;;            ((symbol-function 'message)
  ;;             (lambda (format-string &rest msg-args)
  ;;               (unless (and (stringp format-string)
  ;;                            (string-match-p "Using .*schema" format-string))
  ;;                 (apply old-msg format-string msg-args)))))
  ;;   (apply orig-fun args))
  (let ((inhibit-message t))
    (apply orig-fun args)))

(with-eval-after-load 'rng-loc
  (advice-add 'rng-what-schema :around #'my-hide-rng-what-schema-message))

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

        (let ((moving t)
              (last-point -1))
          (while moving
            (if (= (point) last-point)
                (setq moving nil) ;; Break loop if movement failed silently
              (setq last-point (point))
              (let ((is-next-done
                     (save-excursion
                       (if (org-forward-heading-same-level 1)
                           (let ((state (org-get-todo-state)))
                             (member
                              (if state (substring-no-properties state) "")
                              (bound-and-true-p org-done-keywords)))
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
             (fboundp 'org-get-todo-state)
             (fboundp 'org-back-to-heading)
             (fboundp 'org-at-heading-p))
    (let ((column (current-column)))
      (unwind-protect
          (save-excursion
            (org-back-to-heading)
            (when (org-at-heading-p)
              (let ((current-state (substring-no-properties
                                    (let ((state (org-get-todo-state)))
                                      (if state state "")))))
                (if (string= current-state "DONE")
                    (org-todo "TODO")
                  (org-todo "DONE")
                  (when (string= (substring-no-properties
                                  (let ((state (org-get-todo-state)))
                                    (if state state "")))
                                 "DONE")
                    (my-org-move-todo-before-first-done)))))
            (org-hide-entry))
        (move-to-column column)))))

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
          (evil-insert-state))
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
  ;;  ;; `org-fontify-done-headline' is set. If applies to the part of the headline
  ;;  ;; after the DONE keyword.
  ;;  '(org-headline-done ((t (:strike-through t)))))

  ;; (set-face-attribute 'org-done nil :strike-through t)
  ;; (set-face-attribute 'org-headline-done nil :strike-through t)

  (face-spec-set 'org-done
                 '((t (:strike-through t))))

  (face-spec-set 'org-headline-done
                 '((t (:strike-through t)))))

(defun my-org-mode-setup ()
  "When active, indent text according to outline structure."
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

  ;; TODO bug. Send a patch to org?
  ;; When jumping to org file from org agenda todo list, org-indent-mode is not
  ;; enabled by default.
  (when (and (not (bound-and-true-p org-indent-mode))
             (fboundp 'org-indent-mode))
    (org-indent-mode 1))

  ;; It makes o not auto indent after a bullet list like * or -
  (setq-local evil-auto-indent nil)

  ;; (setq-local indent-line-function nil)
  ;; (custom-set-faces `(org-block ((t (:height 0.7)))))
  ;; (custom-set-faces `(org-block-begin-line ((t (:height 0.6)))))
  ;; (custom-set-faces `(org-block-end-line ((t (:height 0.6 :extend t)))))
  )

(add-hook 'org-mode-hook #'my-org-mode-setup)

(setq org-clock-report-include-clocking-task t)

;; Do not insert empty lines between collapsed sections; makes folded view
;; denser but reduces visual separation between headings.
;; This keeps your files compact by removing empty lines between folded
;; headings.
(setq org-cycle-separator-lines 0)

;; Display descriptive text for links instead of raw URLs; improves
;; readability
;; (setq org-link-descriptive t)

;; RET follows links; intuitive navigation but may conflict with normal line
;; breaks.
(setq org-return-follows-link t)

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

;; Fast todo selection without popup; efficient for experts but hides guidance
;; for beginners.
(setq org-use-fast-todo-selection 'expert)

;; Source block settings
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

;;; org-agenda

(setq org-agenda-start-on-weekday 1)  ; Monday

;; To prevent agenda commands to honor startup options when visiting an agenda
;; file for the first time, use this:
;; https://orgmode.org/worg/agenda-optimization.html
(setq org-agenda-inhibit-startup t)

(setq org-agenda-skip-unavailable-files t
      org-agenda-start-day "-3d"
      org-agenda-span 10)

(defun my-org-agenda-switch-to-todos ()
  "Open the Org Agenda directly showing all TODO items."
  (interactive)
  (let ((buffer (get-buffer "*Org Agenda*")))
    (if buffer
        (switch-to-buffer buffer)
      (org-agenda nil "t"))))

(defun my-org-agenda-goto-in-same-window ()
  "`org-agenda-goto` that opens the target buffer in the current window."
  (interactive)
  (require 'cl-lib)
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
    (when (fboundp 'org-agenda-goto)
      (org-agenda-goto))))

(with-eval-after-load 'org-agenda
  (unless noninteractive
    (define-key org-agenda-keymap (kbd "<tab>") #'ignore))

  (setq org-agenda-file-regexp (replace-regexp-in-string
                                "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                org-agenda-file-regexp)))

;;; org-src

(unless noninteractive
  (define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit))

;; (with-eval-after-load 'org-src
;;   (add-to-list 'org-src-lang-modes '("md" . markdown)))

;; Make TAB behave according to the language mode inside source blocks;
;; consistent editing experience
(setq org-src-tab-acts-natively t)

;; (setq org-src-lang-modes '(("python" . python)
;;                            ("sh" . sh)
;;                            ("bash" . sh)
;;                            ("elisp" . emacs-lisp)))

;; Use native major-mode indentation
(setq org-src-preserve-indentation t)

;; Enforce zero indentation for code within Org source blocks. This prevents Org
;; mode from adding artificial leading spaces, ensuring that code copied
;; directly from the file remains correctly aligned and syntactically valid.
(setq org-src-content-indentation 0)

(with-no-warnings
  ;; The `with-no-warnings' macro maintains compatibility with older Org
  ;; versions where the variable was named `org-edit-src-content-indentation'.
  (setq org-edit-src-content-indentation 0))

;; org-src modes
(defvar my-org-src-minor-mode-alist
  '((emacs-lisp-mode . aggressive-indent-mode))
  "Alist mapping major modes to minor modes for Org source buffers.
Each element is a cons cell of the form (MAJOR-MODE . MINOR-MODE).
When an Org source buffer is initialized with MAJOR-MODE, the
corresponding MINOR-MODE is enabled.")

(defun my-org-src-apply-minor-modes ()
  "Apply minor modes to the current Org source buffer.

This function checks `my-org-src-minor-mode-alist' and activates
any minor mode associated with the current `major-mode'."
  (dolist (entry my-org-src-minor-mode-alist)
    (when (eq major-mode (car entry))
      (let ((minor-mode (cdr entry)))
        (when (fboundp minor-mode)
          (funcall minor-mode 1))))))

(add-hook 'org-src-mode-hook #'my-org-src-apply-minor-modes)

;; (lightemacs-use-package consult-org
;;   :ensure nil
;;   :commands consult-org-heading
;;   :init
;;   (with-eval-after-load 'org
;;     (add-hook 'org-mode-hook
;;               #'(lambda()
;;                   (evil-define-key 'normal 'local (kbd "C-c /") #'consult-org-heading)))))

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;; consult-org

;;; org-capture

;; org-capture: Disable saving a bookmark when capturing; avoids cluttering the
;; bookmark list but loses the ability to quickly return to the capture
;; location.
(with-eval-after-load 'org
  (if (boundp 'org-bookmark-names-plist)
      (setf (plist-get org-bookmark-names-plist :last-capture) nil)
    (setq org-bookmark-names-plist
          '(;; Bookmark names
            :last-capture nil
            :last-refile "org-refile-last-stored"
            :last-capture-marker "org-capture-last-stored-marker"))))
(with-suppressed-warnings ((obsolete org-capture-bookmark))
  (setq org-capture-bookmark nil))

;; org-capture: Switch to insert mode on org capture.
;; TODO lightemacs?
(defun my-org-capture-switch-insert ()
  "Switch to insert mode on org capture."
  (when (and (bound-and-true-p evil-local-mode)
             (fboundp 'evil-insert-state))
    (evil-insert-state)))
(add-hook 'org-capture-mode-hook #'my-org-capture-switch-insert)

;; org-capture: Move cursor to end line.
(defun my-org-capture-move-cursor-end-line ()
  "Move cursor to end line."
  (when (eq major-mode 'org-mode)
    (goto-char (line-end-position))))
(when (fboundp 'my-org-capture-move-cursor-end-line)
  (add-hook 'org-capture-before-finalize-hook
            #'my-org-capture-move-cursor-end-line))

;;; org-ibullets

(lightemacs-use-package org-ibullets
  :straight (org-ibullets
             :type git
             :host github
             :repo "jamescherti/org-ibullets.el")
  ;; :vc (:url "https://github.com/jamescherti/org-ibullets.el"
  ;;           :rev :newest)
  :after org
  :commands org-ibullets-mode
  :hook (org-mode . org-ibullets-mode))

;; :custom
;; (org-ibullets-bullet-list '("●" "◉" "○" "♦" "▶" "♣" "♠"))


;;; Move subtree up and down while preserving the column position

;; TODO org patch
;; This implementation differs from the default Org mode behavior. Standard
;; Org commands restore the cursor position upon successful execution, but
;; they fail to retain the column state if the movement signals an error.
(defun my-org-preserve-column-around-advice (orig-fun &rest args)
  "Preserve the current column when moving Org subtrees.
Executes ORIG-FUN with ARGS, ensuring the column is restored
via `unwind-protect' even if the movement signals an error."
  (let ((col (current-column)))
    (unwind-protect
        (apply orig-fun args)
      (move-to-column col))))

(with-eval-after-load 'org
  (advice-add 'org-move-subtree-up :around #'my-org-preserve-column-around-advice)
  (advice-add 'org-move-subtree-down :around #'my-org-preserve-column-around-advice))

;;; DISABLED: Org: Font Lock Deferral

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

;;; DISABLED: Org

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

;; (setq org-agenda-block-separator ?-)
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

;;; Provide

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'sub-org)

;;; sub-org.el ends here

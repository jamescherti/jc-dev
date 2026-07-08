;;; mod-org.el --- mod-org -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;;; Require

(eval-and-compile
  (require 'lightemacs-use-package))

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

;; TODO lightemacs?
(defun my-org-capture-switch-insert ()
  "Switch to insert mode on org capture."
  (when (and (bound-and-true-p evil-local-mode)
             (fboundp 'evil-insert-state))
    (evil-insert-state)))
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
  ;;  ;; `org-fontify-done-headline' is set. If applies to the part of the headline
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
    (goto-char (line-end-position))))

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
        (org-agenda-goto))))

  (setq org-agenda-file-regexp (replace-regexp-in-string
                                "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                org-agenda-file-regexp)))


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

;;; org

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

  ;; (when (derived-mode-p 'org-mode)
  ;;   ;; It makes o not auto indent after a bullet list like * or -
  ;;   (setq-local evil-auto-indent nil)
  ;;   ;; (setq-local indent-line-function nil)
  ;;   ;; (custom-set-faces `(org-block ((t (:height 0.7)))))
  ;;   ;; (custom-set-faces `(org-block-begin-line ((t (:height 0.6)))))
  ;;   ;; (custom-set-faces `(org-block-end-line ((t (:height 0.6 :extend t)))))
  ;;   )
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

;; Hide markers like * / _ = ~; cleaner view but markers are not visible for
;; editing emphasis.
;; TODO add again
(setq org-hide-emphasis-markers t)

;; Fast todo selection without popup; efficient for experts but hides guidance
;; for beginners.
(setq org-use-fast-todo-selection 'expert)

;; Source block settings
(setq org-edit-src-persistent-message nil)
(setq org-modules '())
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

;; Set tag column to 0 (tags appear immediately after heading); simplifies
;; layout but may make long headings with tags harder to read.
;;
;; Setting this to t will fold  stuff
(setq org-hide-block-startup nil)

;;; org-agenda

(setq org-agenda-start-on-weekday 1)  ; Monday

;;; org-src

;; (setq org-src-lang-modes '(("python" . python)
;;                            ("sh" . sh)
;;                            ("bash" . sh)
;;                            ("elisp" . emacs-lisp)))

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

(provide 'mod-org)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;; consult-org

;;; Move subtree up and down while preserving the column position

;; TODO org patch
;; This implementation differs from the default Org mode behavior. Standard
;; Org commands restore the cursor position upon successful execution, but
;; they fail to retain the column state if the movement signals an error.
(defun my/org-preserve-column-around-advice (orig-fun &rest args)
  "Preserve the current column when moving Org subtrees.
Executes ORIG-FUN with ARGS, ensuring the column is restored
via `unwind-protect' even if the movement signals an error."
  (let ((col (current-column)))
    (unwind-protect
        (apply orig-fun args)
      (move-to-column col))))

(advice-add 'org-move-subtree-up :around #'my/org-preserve-column-around-advice)
(advice-add 'org-move-subtree-down :around #'my/org-preserve-column-around-advice)

;;; mod-org.el ends here

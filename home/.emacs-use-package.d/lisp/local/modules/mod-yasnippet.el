;;; mod-yasnippet.el --- mod-yasnippet -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;;; Require

(eval-and-compile
  (require 'lightemacs-use-package))


;;; auto insert if new file

;; This is called by main.el
(defun my/autoinsert-yas-expand()
  "Replace text with Yasnippet template."
  (when (fboundp 'yas-expand-snippet)
    (condition-case nil
        (progn
          (yas-expand-snippet (buffer-string) (point-min) (point-max))
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


;;; yasnippet: Clear highlights after changing the theme

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

;;; yasnippet: final new line

;; Prevent adding new lines
(defun my-snippet-mode-disable-final-newline ()
  "Disable the final newline in Yasnippet `snippet-mode'."
  (setq-local mode-require-final-newline nil))
(if (fboundp 'my-snippet-mode-disable-final-newline)
    (add-hook 'snippet-mode-hook
              #'my-snippet-mode-disable-final-newline)
  (error "Undefined: my-snippet-mode-disable-final-newline"))

(defun my-snippet-remove-final-newline ()
  "Remove all final newlines at the end of the buffer."
  (save-excursion
    (goto-char (point-max))
    (while (and (not (bobp))
                (eq (char-before) ?\n))
      (delete-char -1))))

(defun my-snippet-mode-setup ()
  "Configure `snippet-mode' to prevent and remove final newlines."
  (setq-local require-final-newline nil)
  (setq-local mode-require-final-newline nil)
  (add-hook 'before-save-hook #'my-snippet-remove-final-newline nil t))

(add-hook 'snippet-mode-hook #'my-snippet-mode-setup)

;;; yasnippet

;; Ensure it also applies when yasnippet is first loaded
(with-no-warnings
  (add-hook 'yas-minor-mode-hook #'my-clear-yasnippet-field-highlight))

(setq yas-snippet-dirs '())
(add-to-list 'yas-snippet-dirs
             (expand-file-name "yasnippet/snippets" "~/.emacs-data/etc"))
(add-to-list 'yas-snippet-dirs
             (expand-file-name "yasnippet/snippets-auto" "~/.emacs-data/etc"))

;; TODO fix when enter is pressed, yas it does not behave as well
;; as without this
;; (defun my-yas-next-field-or-corfu ()
;;   "Insert the selected Corfu candidate or move to the next Yasnippet field."
;;   (interactive)
;;   (if (and (bound-and-true-p corfu-mode)
;;            (fboundp 'corfu-insert)
;;            (>= corfu--index 0))
;;       (corfu-insert)
;;     (when (fboundp 'yas-next-field)
;;       (yas-next-field))))

(with-eval-after-load 'yasnippet
  ;; (define-key yas-keymap (kbd "RET") 'my-yas-next-field-or-corfu)
  ;; (define-key yas-keymap (kbd "<return>") 'my-yas-next-field-or-corfu)

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

;;; Provide

(provide 'mod-yasnippet)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-yasnippet.el ends here

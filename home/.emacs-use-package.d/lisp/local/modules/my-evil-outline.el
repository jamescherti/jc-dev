;;; my-evil-outline.el --- my-evil-outline -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(require 'evil)
(require 'outline)

;;; Outline Keybindings

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

;;; outline move subtree up and down (M-j and M-k)

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
  (when (boundp 'org-mode-map)
    (define-key org-mode-map (kbd "M-<up>") 'org-move-subtree-up)
    (define-key org-mode-map (kbd "M-<down>") 'org-move-subtree-down)))

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

(provide 'my-evil-outline)
;;; my-evil-outline.el ends here

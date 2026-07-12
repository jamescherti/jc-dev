;;; init-counsel.el --- packages  -*- lexical-binding: t; -*-

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

;;; DEPRECATED: Legacy unmaintained code. Safe to remove if it causes
;;; regressions.

;;; Code:

(require 'alternatives)
(require 'init-defun)
(eval-when-compile
  (require 'evil))

;;; Code:
;; (when (string= choice-minibuffer "ivy-counsel")
;;   (with-eval-after-load 'evil
;;     (with-eval-after-load 'counsel
;;       (evil-define-key 'insert 'global (kbd "C-S-SPC") #'counsel-company))))

(use-package ivy-prescient
  :after (prescient counsel)
  :commands ivy-prescient-mode
  :config
  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode 1))

(use-package counsel
  :commands (counsel-M-x
             counsel-fzf
             counsel-rg
             counsel-company
             counsel-recentf
             counsel-ibuffer
             ivy-mode
             ivy-resume
             ivy-call
             ivy-dispatching-call
             ivy-insert-current
             ivy-beginning-of-buffer
             ivy-end-of-buffer
             ivy-scroll-up-command
             ivy-scroll-down-command)

  :hook
  (on-first-input . ivy-mode)

  :preface
  (defun my-ivy-preview-and-jump-to-other-window ()
    "Ivy: preview the current selection and jump to the other window."
    (interactive)
    (ivy-call)
    (other-window 1)
    (recenter))

  (defun my-ivy-preview ()
    "Ivy: preview the current selection without jumping to the other window."
    (interactive)
    (my-ivy-preview-and-jump-to-other-window)
    (other-window -1))

  (defun tab-new-ivy-call ()
    "Open ivy-call in a new tab."
    (interactive)
    (tab-new-func-buffer-from-other-window #'ivy-call))

  (defun my-counsel-rg-current-dir ()
    "Call counsel rg in the current directory"
    (interactive)
    (counsel-rg "" (buffer-cwd)))

  :init
  (global-set-key (kbd "M-x") 'counsel-M-x)

  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global (kbd "M-/") 'swiper)
    (evil-define-key 'normal 'global (kbd "<leader>ff") 'counsel-imenu)
    (evil-define-key 'normal 'global (kbd "<leader>m") #'counsel-recentf)
    (evil-define-key 'normal 'global (kbd "<leader>b") #'counsel-recentf)
    (evil-define-key 'normal 'global (kbd "<leader>B") #'counsel-ibuffer)
    (evil-define-key 'normal 'global (kbd "<leader>gr") #'my-counsel-rg-current-dir)
    (when (string= fuzzy-file-finder-system "counsel-fzf")
      (define-key evil-normal-state-map (kbd "C-p") #'counsel-fzf)))

  :config
  (setq ivy-initial-inputs-alist
        '((counsel-package . "^+")
          (counsel-org-capture . "^")
          (counsel-minor . "^+")
          ;; (counsel-M-x . "^")
          (counsel-describe-symbol . "^")
          (org-refile . "^")
          (org-agenda-refile . "^")
          (org-capture-refile . "^")
          (Man-completion-table . "^")
          (woman . "^")))
  (setq ivy-use-virtual-buffers t)  ; Without this, fd does not work
  (setq ivy-height 15)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))

  ;; (set-face-attribute 'ivy-posframe nil :foreground "white" :background "DarkSlateBlue")
  ;; How to highlight ivy candidates in the manner of hl-line?
  ;; https://emacs.stackexchange.com/questions/20778/how-to-highlight-ivy-candidates-in-the-manner-of-hl-line
  (setq counsel-rg-base-command my-counsel-rg-command)

  ;; https://www.reddit.com/r/emacs/comments/9o6inu/sort_ivys_counselrecentf_results_by_timestamp/
  ;; Sort results by timestamp
  ;; (add-to-list 'ivy-sort-functions-alist
  ;;              '(counsel-recentf . file-newer-than-file-p))

  (setq ivy-more-chars-alist '((counsel-grep . 2)
                               (counsel-rg . 2)
                               (counsel-fzf . 3)
                               (t . 3)))

  (define-key ivy-minibuffer-map (kbd "M-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "M-k") 'ivy-previous-line)
  (keymap-set ivy-minibuffer-map "C-t" #'tab-new-ivy-call)

  (with-eval-after-load 'evil
    (evil-define-key 'normal ivy-minibuffer-map (kbd "E") #'my-ivy-preview-and-jump-to-other-window)
    (evil-define-key 'normal ivy-minibuffer-map (kbd "t") 'tab-new-ivy-call)
    (evil-define-key '(insert normal) ivy-minibuffer-map (kbd "C-t") 'tab-new-ivy-call)

    (evil-define-key 'insert ivy-minibuffer-map (kbd "C-e") #'my-ivy-preview)
    (evil-define-key 'normal ivy-minibuffer-map (kbd "C-e") #'my-ivy-preview)
    (evil-define-key 'normal ivy-minibuffer-map (kbd "e") #'my-ivy-preview)

    (evil-define-key 'insert ivy-minibuffer-map (kbd "C-p") 'ivy-insert-current)
    (evil-define-key 'insert ivy-minibuffer-map (kbd "C-n") 'ivy-insert-current)
    (evil-define-key 'insert ivy-minibuffer-map (kbd "C-o") #'ivy-dispatching-call)

    (evil-define-key 'insert ivy-minibuffer-map (kbd "TAB") #'ivy-insert-current)
    (evil-define-key 'normal ivy-minibuffer-map (kbd "gg") #'ivy-beginning-of-buffer)
    (evil-define-key 'normal ivy-minibuffer-map (kbd "G") #'ivy-end-of-buffer)
    (evil-define-key 'insert ivy-minibuffer-map (kbd "C-f") #'ivy-scroll-up-command)
    (evil-define-key 'insert ivy-minibuffer-map (kbd "C-b") #'ivy-scroll-down-command)
    (evil-define-key 'normal ivy-minibuffer-map (kbd "C-f") #'ivy-scroll-up-command)
    (evil-define-key 'normal ivy-minibuffer-map (kbd "C-b") #'ivy-scroll-down-command)))

(provide 'pkg-counsel)

;;; pkg-counsel.el ends here
